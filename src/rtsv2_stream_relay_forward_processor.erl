-module(rtsv2_stream_relay_forward_processor).

-behaviour(workflow_processor).

-include_lib("kernel/include/inet.hrl").
-include_lib("id3as_common/include/id3as_message_bus.hrl").
-include_lib("id3as_common/include/common.hrl").
-include_lib("id3as_media/include/id3as_workflow.hrl").
-include_lib("id3as_rtc/include/rtp.hrl").
-include("./rtsv2_slot_media_source_publish_processor.hrl").

%% Workflow API
-export([ spec/1
        , initialise/1
        , process_input/2
        , handle_info/2
        , flush/1
        , is_meter/0
        , ioctl/2
        ]).

-define(state, ?MODULE).

-type destination_key() :: {inet:ip_address(), inet:port_number()}.

-record(?state,
        { slot_id :: term()
        , publication_count = 0 :: non_neg_integer()
        , destinations = #{} :: maps:map(destination_key(), gen_udp:socket())
        , destinations_by_socket = #{} :: maps:map(gen_udp:socket(), destination_key())
        }).

%%------------------------------------------------------------------------------
%% Workflow API
%%------------------------------------------------------------------------------
spec(_Processor) ->
  #processor_spec{ consumes = [ ?rtp_sequences, ?parsed_rtps ]
                 , is_pure = true
                 }.

initialise(#processor{config = SlotId}) ->
  { ok, #?state{slot_id = SlotId} }.

process_input(#rtp_sequence{ rtps = RTPs }, State = #?state{ destinations = Destinations, publication_count = PublicationCount }) ->
  Packets = [ rtp:unparse(avp, RTP) || RTP <- RTPs ],

  _ =
    maps:fold(fun(_AddrPort, Socket, {Failed, Succeeded}) ->
                  lists:foldl(fun(Packet, {FailedInner, SucceededInner}) ->
                                  case gen_udp:send(Socket, Packet) of
                                    ok ->
                                      {FailedInner, SucceededInner + 1};
                                    _ ->
                                      {FailedInner + 1, SucceededInner}
                                  end
                              end,
                              {Failed, Succeeded},
                              Packets
                             )
              end,
              {0, 0},
              Destinations
             ),

  {ok, State #?state{ publication_count = PublicationCount + 1 }};

process_input(#rtp{} = RTP, State = #?state{ destinations = Destinations, publication_count = PublicationCount }) ->

  Packet = rtp:unparse(avp, RTP),

  _ =
    maps:fold(fun(_AddrPort, Socket, {Failed, Succeeded}) ->
                  case gen_udp:send(Socket, Packet) of
                    ok ->
                      {Failed, Succeeded + 1};
                    _ ->
                      {Failed + 1, Succeeded}
                  end
              end,
              {0, 0},
              Destinations
             ),

  {ok, State #?state{ publication_count = PublicationCount + 1 }}.

handle_info({udp_error, Socket, econnrefused},
            State = #?state{ slot_id = SlotId
                           , destinations = Destinations
                           , destinations_by_socket = DestinationsBySocket
                           }
           ) ->

  case maps:take(Socket, DestinationsBySocket) of
    {RelayKey, NewDestinationsBySocket} ->
      NewDestinations = maps:remove(RelayKey, Destinations),

      ?SLOG_INFO("UDP error sending to Destination",
                 #{ slot_id => SlotId
                  , stream_relay => RelayKey
                  , reason => econnrefused
                  , socket => Socket
                  }
                ),

      { noreply
      , State#?state{ destinations = NewDestinations
                    , destinations_by_socket = NewDestinationsBySocket
                    }
      };

    error ->
      ?SLOG_INFO("UDP econnrefused for unknown socket",
                 #{ slot_id => SlotId
                  , reason => econnfused
                  , socket => Socket
                  }
                ),

      {noreply, State}
  end.


ioctl(read_meter, State = #?state{ publication_count = PublicationCount }) ->

  Metrics =
    [ #counter_metric{name = publication_count, value = PublicationCount, display_name = <<"Publication Count">>}
    ],

  {ok, #status{metrics = Metrics}, State};

ioctl({set_destinations, UnresolvedDestinationList},
      State = #?state{}
     ) ->

  {ResolvedDestinationList, ResolveFailedList} =
    lists:foldl(fun(#{ server := Host, port := Port }, { ResolvedDestinationListIn, ResolveFailedListIn }) ->
                  case inet:gethostbyname(?to_list(Host)) of
                    {ok, #hostent{ h_addr_list = [ Addr | _ ] }} ->
                      ResolvedDestination = {Addr, Port},

                      { [ ResolvedDestination | ResolvedDestinationListIn ]
                      , ResolveFailedListIn
                      };

                    Other ->
                      Failure = { { Host, Port}, Other },

                      { ResolvedDestinationListIn
                      , [ Failure | ResolveFailedListIn ]
                      }
                  end
                end,
                {[], []},
                UnresolvedDestinationList
               ),

  ValidDestinations = maps:from_list([{ResolvedDestination, true} || ResolvedDestination <- ResolvedDestinationList]),

  NewState1 =
    lists:foldl(fun({ Addr, Port }, Acc) ->
                    ensure_destination(Addr, Port, Acc)
                end,
                State,
                ResolvedDestinationList
               ),

  NewState2 =
    lists:foldl(fun(DestinationKey, StateIn) ->
                    case maps:is_key(DestinationKey, ValidDestinations) of
                      true ->
                        StateIn;
                      false ->
                        drop_destination(DestinationKey, StateIn)
                    end
                end,
                NewState1,
                maps:keys(NewState1#?state.destinations)
               ),

  {ok, ResolveFailedList, NewState2}.

ensure_destination(Addr,
                   Port,
                   #?state{ destinations = Destinations
                          , destinations_by_socket = DestinationsBySocket
                          } = State
                  ) ->

  MapKey = {Addr, Port},

  case maps:find(MapKey, Destinations) of
    {ok, _Socket} ->
      State;

    error ->
      {ok, Socket} = gen_udp:open(0, [binary]),

      %% Errors are asynchronous
      gen_udp:connect(Socket, Addr, Port),

      NewDestinations = maps:put(MapKey, Socket, Destinations),
      NewDestinationsBySocket = maps:put(Socket, MapKey, DestinationsBySocket),

      State#?state{ destinations = NewDestinations
                  , destinations_by_socket = NewDestinationsBySocket
                  }
  end.

drop_destination(MapKey,
                 #?state{ destinations = Destinations
                        , destinations_by_socket = DestinationsBySocket
                        } = State
                ) ->

  {ok, Socket} = maps:find(MapKey, Destinations),

  _ = gen_udp:close(Socket),

  NewDestinations = maps:remove(MapKey, Destinations),
  NewDestinationsBySocket = maps:remove(Socket, DestinationsBySocket),

  State#?state{ destinations = NewDestinations
              , destinations_by_socket = NewDestinationsBySocket
              }.

is_meter() ->
  true.

flush(State) ->
  {flush_complete, State}.
