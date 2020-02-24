-module(llnw_edge_session_handler).


-export([ init/1
        , handle_media_frame/2
        , handle_info/2
        ]).


-define(state, ?MODULE).


-include_lib("kernel/include/logger.hrl").
-include_lib("id3as_common/include/id3as_types.hrl").
-include_lib("id3as_common/include/id3as_message_bus.hrl").
-include_lib("id3as_media/include/frame.hrl").


-record(?state,
        { account :: binary_string()
        , stream_name :: binary_string()
        }).


init([Account, StreamName]) ->

  ?I_SUBSCRIBE_BUS_MSGS({ Account, StreamName }),

  #?state{ account = Account
         , stream_name = StreamName
         }.


handle_media_frame(_Frame, State) ->
  {ok, State}.


handle_info(#frame{ type = _FrameType } = Frame, State) ->
  {ok, Frame, State}.
