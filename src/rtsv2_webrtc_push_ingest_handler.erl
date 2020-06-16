-module(rtsv2_webrtc_push_ingest_handler).

-behaviour(webrtc_session_handler).

-export([ init/1
        , handle_media_frame/2
        , handle_info/2
        ]).

-include_lib("id3as_media/include/frame.hrl").
-include_lib("id3as_media/include/libav.hrl").
-include_lib("id3as_media/include/libjpeg_encoder.hrl").
-include_lib("id3as_media/include/transcode.hrl").
-include_lib("id3as_media/include/receive_from_bus_generator.hrl").
-include_lib("id3as_media/include/send_to_bus_processor.hrl").
-include_lib("id3as_media/include/fun_processor.hrl").
-include_lib("id3as_media/include/h264.hrl").

-define(state, ?MODULE).

-record(?state,
        { workflow_handle :: id3as_workflow:workflow_handle()
        }).

init([ SlotId, SlotRole, ProfileName, SlotProfile ]) ->
  #?state{ workflow_handle = start_workflow(SlotId, SlotRole, ProfileName, SlotProfile) }.

handle_media_frame(Frame, State = #?state{ workflow_handle = WorkflowHandle }) ->
  ok = id3as_workflow:process_input(Frame, WorkflowHandle),
  {ok, State}.

handle_info(#workflow_output{message = #workflow_data_msg{data = Frame = #frame{}}}, State) ->
  {ok, Frame, State};

handle_info(#workflow_output{message = #workflow_data_msg{data = _Other}}, State) ->
  {ok, State};

handle_info({lost_ownership, _OperatorId}, State) ->
  {stop, normal, State}.

start_workflow(SlotId, SlotRole, ProfileName, SlotProfile) ->
  Key = {ingestKey, SlotId, SlotRole, ProfileName},

  WorkflowDefinition =
    #workflow{ name = ?MODULE
             , generators = []
             , processors = [
                              rtsv2_ingest_processor:ingest_processor(Key, ProfileName, SlotProfile, outside_world)
                            ]
             },

  {ok, Pid} = id3as_workflow:start_link(WorkflowDefinition),
  {ok, Handle} = id3as_workflow:workflow_handle(Pid),
  Handle.
