%% this guy might better just be a random gen server, but maybe we want

-module(rtsv2_hls_master_playlist_processor).

-include_lib("id3as_media/include/id3as_workflow.hrl").
-include_lib("id3as_media/include/frame.hrl").

-include_lib("id3as_common/include/common.hrl").

-include("./rtsv2_slot_profiles.hrl").

-include_lib("id3as_media/include/m3u8.hrl").

-behaviour(workflow_processor).

-export([
         spec/1,
         initialise/1,
         process_input/2,
         handle_info/2,
         flush/1,
         ioctl/2
        ]).

-define(state, ?MODULE).

-record(?state,
        {
          config :: hls_master_playlist_processor_config()
        }).


%%------------------------------------------------------------------------------
%% Workflow API
%%------------------------------------------------------------------------------
spec(_Processor) ->
  #processor_spec{
     consumes = ?all,
     generates = ?all,
     supports_synchronous_mode = true,
     is_pure = true
    }.

initialise(_Processor = #processor{ config = Config }) ->
  %% when, wait for what
  timer:send_after(10000, publish_playlists),
  {ok, #?state{
    config = Config
  }}.

process_input(_Input, State) ->
  {ok, State}.

handle_info(publish_playlists, State = #?state{ config = #hls_master_playlist_processor_config{
                                           slot_id = SlotId,
                                           profiles = Profiles,
                                           push_details = PushDetails = [ #{ putBaseUrl := PutBaseUrl, auth := #{ type := <<"basic">>, username := Username, password := Password } }]
                                         }}) ->
  ?INFO("Building and sending playlists, pushdetails = ~p", [PushDetails]),

  Playlist = m3u8:master_playlist(build_playlists(rtsv2_types:uuid_to_string(SlotId), Profiles, PushDetails)),
  ?INFO("Master playlist:~n~s", [Playlist]),

  rtsv2_internal_playlist_publish:send({Username, Password}, PutBaseUrl, [{<<"master.m3u8">>, Playlist}], primary_playlists_published, <<"version">>),
  {noreply, State};
handle_info(primary_playlists_published, State = #?state{ config = #hls_master_playlist_processor_config{} }) ->
  ?INFO("Playlists published"),
  {noreply, State}.

flush(State) ->
  {flush_complete, State}.

ioctl(_, State) ->
  {ok, State}.

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------


build_playlists(SlotId, Profiles = [_ |_], [PushDetail = #{}|_]) ->

  #master_playlist {
     rendition_groups = [ ],
     variant_streams = [ variant_stream(SlotId, PushDetail, Profile) || Profile <- Profiles ]
    }.


variant_stream(SlotId, #{ playbackBaseUrl := PlaybackBaseUrl }, Profile = #{ bitrate := BitRate, profileName := ProfileName}) ->
  VideoCodec = #h264_codec_descriptor { profile = main, level = 3.0 },
  AudioCodec = #aac_codec_descriptor{ profile = main },

  ?INFO("making variant for slotid=~p with ~p", [SlotId, Profile]),
  #variant_stream {
     name = ProfileName,
     program_id = 1,
     codecs = [ VideoCodec, AudioCodec ],
     peak_bandwidth = BitRate,
     audio_rendition_group_id = <<"aac">>,
     uri = << PlaybackBaseUrl/binary,
              ProfileName/binary, "/playlist.m3u8" >>
    }.
