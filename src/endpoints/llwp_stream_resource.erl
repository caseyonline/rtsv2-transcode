-module(llwp_stream_resource).

-define(ID3AS_COMMON_USE_LOGGER, true).

-include_lib("id3as_common/include/common.hrl").
-include_lib("id3as_media/include/mp4.hrl").
-include_lib("id3as_media/include/receive_from_bus_generator.hrl").
-include_lib("id3as_media/include/fun_processor.hrl").

-export([init/2,
         allowed_methods/2,
         service_available/2,
         content_types_provided/2,
         get_stream/2,
         info/3,
         terminate/3
        ]).

-record(state, {
          bus_name :: term(),
          bus_pid :: pid(),
          stream_metadata :: undefined | maps:map(),
          program_details :: program_details()
         }).

init(Req, MakeStreamAndVariant) ->

  StreamId = cowboy_req:binding(stream_id, Req),
  VariantId = cowboy_req:binding(variant_id, Req),

  ?SLOG_INFO("LLWP Stream opening", #{stream_id => StreamId,
                                      variant_id => VariantId}),

  { cowboy_rest, Req, #state { bus_name = {ingest, (MakeStreamAndVariant(StreamId))(VariantId) }}}.

service_available(Req, State = #state{bus_name = BusName}) ->

  case pubsub:read_bus_metadata(BusName) of
    {ok, #live_stream_metadata{program_details = ProgramDetails}} when ProgramDetails /= undefined ->
      {true, Req, State#state{program_details = ProgramDetails}};

    _ ->
      {false, Req, State}
  end.

allowed_methods(Req, State) ->
  { [<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
  {[{ <<"application/vnd.id3as.media+llwp">>, get_stream}], Req, State}.

get_stream(Req, State = #state { bus_name = BusName }) ->

  {ok, BusPid} = pubsub:register_listener(BusName, self()),

  Req2 = cowboy_req:stream_reply(200, Req),

  ok = i_cowboy_helpers:set_idle_timeout(infinity, Req2),

  {{switch_handler,  cowboy_loop}, Req2, State#state{bus_pid = BusPid}}.

terminate(Reason, _Req, #state { bus_name = BusName,
                                 bus_pid = undefined}) ->
  ?SLOG_DEBUG("LLWP Stream closing (no listener)", #{bus_name => BusName,
                                                     reason => Reason}),
  ok;

terminate(Reason, _Req, #state { bus_name = BusName,
                                 bus_pid = BusPid}) ->

  ?SLOG_DEBUG("LLWP Stream closing", #{bus_name => BusName,
                                      reason => Reason}),
  _ = pubsub:deregister_listener(BusPid),
  ok.

info(Frame = #frame{dts = Dts}, Req, State = #state {
                                                program_details = ProgramDetails,
                                                stream_metadata = undefined
                                               }) ->
  {ok, Map } =  send_program_details(ProgramDetails, Dts, Req),
  info(Frame, Req, State#state { stream_metadata = Map });

info(Frame = #frame{}, Req, State = #state { stream_metadata = Map }) ->
  {ok, Map2 } = send_frame(Frame, Req, Map),
  {ok, Req, State#state { stream_metadata = Map2 } }.

send_frame(Frame = #frame{stream_metadata = #stream_metadata{stream_id = StreamId}}, Req, Map) ->

  CurrentMetadata = maps:get(StreamId, Map, undefined),

  {Data, NewMetadata} = llwp_utils:frame_to_binary(Frame, CurrentMetadata),

  ok = cowboy_req:stream_body(Data, nofin, Req),

  Map2 = maps:put(StreamId, NewMetadata, Map),

  {ok, Map2 }.

send_program_details(ProgramDetails, Dts, Req) ->
  send_frame(ProgramDetails#frame {
    dts = Dts,
    pts = Dts
    %% Old behaviour, come here if it's broken
%% stream_metadata = #stream_metadata { stream_id = 1 }
   }, Req, #{}).
