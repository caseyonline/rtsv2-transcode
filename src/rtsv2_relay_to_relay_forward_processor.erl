-module(rtsv2_relay_to_relay_forward_processor).

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
        , flush/1
        , is_meter/0
        , ioctl/2
        ]).

-define(state, ?MODULE).

-record(?state,
        { publication_count = 0 :: non_neg_integer()
        , relays = #{} :: maps:map({inet:ip_address(), inet:port_number()}, gen_udp:socket())
        }).

%%------------------------------------------------------------------------------
%% Workflow API
%%------------------------------------------------------------------------------
spec(_Processor) ->
  #processor_spec{ consumes = [ ?rtp_sequences, ?parsed_rtps ]
                 , is_pure = true
                 }.

initialise(#processor{}) ->
  { ok, #?state{} }.

process_input(#rtp_sequence{ rtps = RTPs }, State = #?state{ relays = Relays, publication_count = PublicationCount }) ->
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
              Relays
             ),

  {ok, State #?state{ publication_count = PublicationCount + 1 }};

process_input(#rtp{} = RTP, State = #?state{ relays = Relays, publication_count = PublicationCount }) ->
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
              Relays
             ),

  {ok, State #?state{ publication_count = PublicationCount + 1 }}.

ioctl(read_meter, State = #?state{ publication_count = PublicationCount }) ->

  Metrics =
    [ #counter_metric{name = publication_count, value = PublicationCount, display_name = <<"Publication Count">>}
    ],

  {ok, #status{metrics = Metrics}, State};

ioctl({register_relay, Host, Port}, State = #?state{ relays = Relays }) ->
  case inet:gethostbyname(?to_list(Host)) of
    {ok, #hostent{ h_addr_list = [ Addr | _ ] }} ->

      MapKey = {Addr, Port},

      case maps:find(MapKey, Relays) of
        {ok, _Socket} ->
          {ok, {error, already_registered}, State};

        error ->
          {ok, Socket} = gen_udp:open(0, [binary]),

          %% Errors are asynchronous
          gen_udp:connect(Socket, Addr, Port),

          NewRelays = maps:put(MapKey, Socket, Relays),

          {ok, State#?state{ relays = NewRelays}}
      end;

    Other ->
      {ok, {error, {dns_resolve_failed, Other}}, State}
  end.


is_meter() ->
  true.

flush(State) ->
  {flush_complete, State}.
