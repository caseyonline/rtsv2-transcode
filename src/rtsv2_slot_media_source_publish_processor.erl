-module(rtsv2_slot_media_source_publish_processor).

-behaviour(workflow_processor).

-include_lib("kernel/include/inet.hrl").
-include_lib("id3as_common/include/id3as_message_bus.hrl").
-include_lib("id3as_common/include/common.hrl").
-include_lib("id3as_media/include/id3as_workflow.hrl").
-include_lib("id3as_rtc/include/rtp.hrl").
-include("./rtsv2_slot_media_source_publish_processor.hrl").

%% API
-export([ maybe_slot_configuration/2
        ]).

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

-type stream_relay_key() :: {inet:ip_address(), inet:port_number()}.

-record(?state,
        { slot_id :: non_neg_integer()
        , name :: term()                        %
        , publication_count = 0 :: non_neg_integer()
        , stream_relays = #{} :: maps:map(stream_relay_key(), gen_udp:socket())
        , stream_relays_by_socket = #{} :: maps:map(gen_udp:socket(), stream_relay_key())
        }).

-define(metadata, rtsv2_slot_media_source_publish_processor_metadata).

-record(?metadata,
        { slot_configuration :: rtsv2_slot_configuration:slot_configuration()
        }).

%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------
maybe_slot_configuration(SlotId, SlotRole) ->
  case gproc:lookup_local_properties({metadata, name(SlotId, SlotRole)}) of
    [] ->
      undefined;

    [{_Pid, #?metadata{ slot_configuration = SlotConfiguration }}] ->
      SlotConfiguration
  end.

%%------------------------------------------------------------------------------
%% Workflow API
%%------------------------------------------------------------------------------
spec(_Processor) ->
  #processor_spec{ consumes = [ ?rtp_sequences, ?parsed_rtps ]
                 , is_pure = true
                 }.

initialise(#processor{config =
                        #rtsv2_slot_media_source_publish_processor_config{ slot_name = SlotId
                                                                         , slot_role = SlotRole
                                                                         , slot_configuration = SlotConfiguration
                                                                         }
                     }) ->

  Name = name(SlotId, SlotRole),

  _ = gproc:add_local_property({metadata, Name},
                               #?metadata{ slot_configuration = SlotConfiguration }
                              ),
  { ok
  , #?state{ slot_id = SlotId
           , name = Name
           }
  }.

process_input(#rtp_sequence{ rtps = RTPs }, State = #?state{ stream_relays = StreamRelays, publication_count = PublicationCount }) ->
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
              StreamRelays
             ),

  {ok, State #?state{ publication_count = PublicationCount + 1 }};

process_input(#rtp{} = RTP, State = #?state{ stream_relays = StreamRelays, publication_count = PublicationCount }) ->
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
              StreamRelays
             ),

  {ok, State #?state{ publication_count = PublicationCount + 1 }}.

handle_info({udp_error, Socket, econnrefused}, State = #?state{ slot_id = SlotId
                                                              , stream_relays = StreamRelays
                                                              , stream_relays_by_socket = StreamRelaysBySocket }) ->

  case maps:take(Socket, StreamRelaysBySocket) of
    error ->
      ?SLOG_INFO("UPD error: socket not registered", #{ slot_id => SlotId
                                                      , socket => Socket }),
      {noreply, State};

    {RelayKey, NewStreamRelaysBySocket} ->
      NewStreamRelays = maps:remove(RelayKey, StreamRelays),

      ?SLOG_INFO("UDP error sending to StreamRelay", #{ slot_id => SlotId
                                                      , stream_relay => RelayKey
                                                      , reason => econnrefused}),

      {noreply, State#?state{ stream_relays = NewStreamRelays
                            , stream_relays_by_socket = NewStreamRelaysBySocket}}
  end.

ioctl(read_meter, State = #?state{slot_id = SlotId, publication_count = PublicationCount}) ->

  Metrics = [ #text_metric{name = slot_name, value = SlotId, display_name = <<"Slot Name">>, update_frequency = low}
            , #counter_metric{name = publication_count, value = PublicationCount, display_name = <<"Publication Count">>}
            ],

  {ok, #status{metrics = Metrics}, State};

ioctl({register_stream_relay, Host, Port}, State = #?state{ stream_relays = StreamRelays
                                                          , stream_relays_by_socket = StreamRelaysBySocket }) ->
  case inet:gethostbyname(?to_list(Host)) of
    {ok, #hostent{ h_addr_list = [ Addr | _ ] }} ->

      MapKey = {Addr, Port},

      case maps:find(MapKey, StreamRelays) of
        {ok, _Socket} ->
          {ok, {error, already_registered}, State};

        error ->
          {ok, Socket} = gen_udp:open(0, [binary]),

          %% Errors are asynchronous
          gen_udp:connect(Socket, Addr, Port),

          NewStreamRelays = maps:put(MapKey, Socket, StreamRelays),
          NewStreamRelaysBySocket = maps:put(Socket, MapKey, StreamRelaysBySocket),

          {ok, State#?state{ stream_relays = NewStreamRelays
                           , stream_relays_by_socket = NewStreamRelaysBySocket}}
      end;

    Other ->
      {ok, {error, {dns_resolve_failed, Other}}, State}
  end;

ioctl({deregister_stream_relay, Host, Port}, State = #?state{ slot_id = SlotId
                                                            , stream_relays = StreamRelays
                                                            , stream_relays_by_socket = StreamRelaysBySocket }) ->
  case inet:gethostbyname(?to_list(Host)) of
    {ok, #hostent{ h_addr_list = [ Addr | _ ] }} ->

      MapKey = {Addr, Port},

      case maps:find(MapKey, StreamRelays) of
        {ok, Socket} ->
          _ = gen_udp:close(Socket),
          NewStreamRelays = maps:remove(MapKey, StreamRelays),
          NewStreamRelaysBySocket = maps:remove(Socket, StreamRelaysBySocket),

          {ok, State#?state{ stream_relays = NewStreamRelays
                           , stream_relays_by_socket = NewStreamRelaysBySocket}};

        error ->
          ?SLOG_INFO("Deregistering: stream relay not registered", #{ slot_id => SlotId
                                                                    , stream_relay_host => Host
                                                                    , stream_relay_port => Port}),
          {ok, State}
      end;

    Other ->
      {ok, {error, {dns_resolve_failed, Other}}, State}
  end.


is_meter() ->
  true.

flush(State) ->
  {flush_complete, State}.

%%------------------------------------------------------------------------------
%% Private Functions
%%------------------------------------------------------------------------------
name(SlotId, SlotRole) ->
  { slot_media_source, SlotId, SlotRole }.
