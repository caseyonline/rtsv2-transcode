-module(rtsv2_webrtc_session_handler).

-behaviour(webrtc_session_handler).

-export([ init/1
        , handle_media_frame/2
        , handle_info/2
        , handle_cast/2
        ]).

-include_lib("id3as_rtc/include/rtp.hrl").
-include_lib("id3as_rtc/include/rtp_engine.hrl").

-define(state, ?MODULE).

-record(?state,
        {
         session_id,
         audio_ready,
         video_ready
        }).

%% init should be part of negotiation so it can construct a workflow
%%      based on the negotiated stream?
init(SessionId) ->
  ?DEBUG("Session handler started for session ~p", [SessionId]),
  #?state{session_id = SessionId}.


handle_media_frame(#rtp_engine_msg{message = #rtp_engine_ready{}, snapshot = Snapshot},
                   State) ->

  NewState =
    case rtp_engine:type(Snapshot) of
      audio ->
        State#?state{ audio_ready = true };
      video ->
        State#?state{ video_ready = true }
    end,

  {ok, NewState};

handle_media_frame(_Frame, #?state{} = State) ->
  {ok, State}.

handle_info(#rtp_sequence{} = Sequence, State) ->
  {ok, Sequence, State}.

%% handle_cast(notify_socket_disconnect, State = #?state{audio_ready = true,
%%                                                       video_ready = true}) ->
%%   %% Would normally stop here, but we want to continue creating output for load test purposes
%%   {noreply, State};

handle_cast(notify_socket_disconnect, State) ->
  {stop, normal, State}.
