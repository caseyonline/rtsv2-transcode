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

%% init should be part of negotiation so it can construct a workflow
%%      based on the negotiated stream?
init([ SlotId, SlotRole, ProfileName ]) ->
  #?state{ workflow_handle = start_workflow(SlotId, SlotRole, ProfileName) }.

handle_media_frame(Frame, State = #?state{ workflow_handle = WorkflowHandle }) ->
  ok = id3as_workflow:process_input(Frame, WorkflowHandle),
  {ok, State}.

handle_info(#workflow_output{message = #workflow_data_msg{data = Frame = #frame{}}}, State) ->
  {ok, Frame, State};

handle_info(#workflow_output{message = #workflow_data_msg{data = _Other}}, State) ->
  {ok, State};

handle_info({lost_ownership, _OperatorId}, State) ->
  {stop, normal, State}.

start_workflow(SlotId, SlotRole, ProfileName) ->
  BusName = {rtsv2_webrtc_ingest_bus, SlotId, SlotRole, ProfileName},

  %% TODO - don't need the other workflow in webrtcingest.erl - this can do it all...
  %% TODO - handle failure

  WorkflowDefinition =
    #workflow{ name = ?MODULE
             , generators = []
             , processors = [ #processor{ name = video
                                        , display_name = <<"Receive Video from WebRTC">>
                                        , subscribes_to = {outside_world, ?video_frames}
                                        , module = passthrough_processor
                                        }

                            , #processor{ name = video_with_correct_stream_id
                                        , display_name = <<"Update Video Stream Id">>
                                        , subscribes_to = video
                                        , module = set_stream_id
                                        , config = 256
                                        }

                            , #processor{ name = audio
                                        , display_name = <<"Receive Audio from WebRTC">>
                                        , subscribes_to = {outside_world, ?audio_frames}
                                        , module = passthrough_processor
                                        }

                            , #processor{ name = audio_with_correct_stream_id
                                        , display_name = <<"Update Audio Stream Id">>
                                        , subscribes_to = audio
                                        , module = set_stream_id
                                        , config = 257
                                        }

                            , #processor{ name = despatch_media
                                        , display_name = <<"Despatch WebRTC to Encoder Workflow">>
                                        , subscribes_to = [ audio_with_correct_stream_id
                                                          , video_with_correct_stream_id
                                                          ]
                                        , module = send_to_bus_processor
                                        , config = #send_to_bus_processor_config{ bus_name = BusName }
                                        }
                            ]
             },

  {ok, Pid} = id3as_workflow:start_link(WorkflowDefinition),
  {ok, Handle} = id3as_workflow:workflow_handle(Pid),
  Handle.
