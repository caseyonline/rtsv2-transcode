-module(rtsv2_slot_media_source_publish_processor).

-behaviour(workflow_processor).

-include_lib("kernel/include/inet.hrl").
-include_lib("id3as_common/include/id3as_message_bus.hrl").
-include_lib("id3as_common/include/common.hrl").
-include_lib("id3as_media/include/id3as_workflow.hrl").
-include_lib("id3as_rtc/include/rtp.hrl").
-include("./rtsv2_slot_media_source_publish_processor.hrl").

%% API
-export([ maybe_slot_configuration/1
        ]).

%% Workflow API
-export([ spec/1
        , initialise/1
        , process_input/2
        , flush/1
        , is_meter/0
        , ioctl/2
        ]).

-define(state, ?MODULE).

-record(?state,
        { slot_name :: binary_string()
        , name :: term()                        %
        , publication_count = 0 :: non_neg_integer()
        , stream_relays = #{} :: maps:map({inet:ip_address(), inet:port_number()}, gen_udp:socket())
        }).

-define(metadata, rtsv2_slot_media_source_publish_processor_metadata).

-record(?metadata,
        { slot_configuration :: rtsv2_slot_configuration:slot_configuration()
        }).

%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------
maybe_slot_configuration(SlotName) ->
  case gproc:lookup_local_properties({metadata, name(SlotName)}) of
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
                        #rtsv2_slot_media_source_publish_processor_config{ slot_name = SlotName
                                                                         , slot_configuration = SlotConfiguration
                                                                         }
                     }) ->

  Name = name(SlotName),

  _ = gproc:add_local_property({metadata, Name},
                               #?metadata{ slot_configuration = SlotConfiguration }
                              ),
  { ok
  , #?state{ slot_name = SlotName
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

ioctl(read_meter, State = #?state{slot_name = SlotName, publication_count = PublicationCount}) ->

  Metrics = [ #text_metric{name = slot_name, value = SlotName, display_name = <<"Slot Name">>, update_frequency = low}
            , #counter_metric{name = publication_count, value = PublicationCount, display_name = <<"Publication Count">>}
            ],

  {ok, #status{metrics = Metrics}, State};

ioctl({register_stream_relay, Host, Port}, State = #?state{ stream_relays = StreamRelays }) ->
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

          {ok, State#?state{ stream_relays = NewStreamRelays}}
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
name(SlotName) ->
  { slot_media_source, SlotName }.
