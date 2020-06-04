%% this guy might better just be a random gen server, but maybe we want

-module(rtsv2_hls_master_playlist_processor).

-include_lib("id3as_media/include/id3as_workflow.hrl").
-include_lib("id3as_media/include/frame.hrl").
-include_lib("id3as_media/include/libav_constants.hrl").

-include_lib("id3as_common/include/common.hrl").

-include("./rtsv2_master_playlist_processor.hrl").

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

-define(AUDIO_PROFILE_WAIT_MS, 200).

-type profile_key() :: { frame_source_id(), stream_id() }.

-record(?state,
        {
          config :: hls_master_playlist_processor_config(),
          profiles = #{} :: maps:map(profile_key(), av_profile()),
          published_playlist = false :: boolean(),
          timer
        }).


%%------------------------------------------------------------------------------
%% Workflow API
%%------------------------------------------------------------------------------
spec(_Processor) ->
  #processor_spec{
     consumes = ?frames,
     generates = ?all,
     supports_synchronous_mode = true,
     is_pure = true
    }.

initialise(_Processor = #processor{ config = Config }) ->
  {ok, #?state{
    config = Config
  }}.

process_input(#frame{type = Type, profile = Profile, source_metadata = #source_metadata {source_id = SourceId}, stream_metadata = #stream_metadata{ stream_id = StreamId }}, State = #?state{ profiles = Profiles, published_playlist = false,  timer = Timer }) ->
  Key = {SourceId, StreamId},
  Profiles2 = maps:put(Key, Profile, Profiles),
  State2 = State#?state{ profiles = Profiles2 },
  ProfilesReady = check_profiles_ready(State2),
  State3 =
    case ProfilesReady of
      all_ready ->
        self() ! publish_playlists,
        State2#?state{ published_playlist = true };
      not_ready -> State2;
      video_ready ->
        if Type =:= video andalso Timer == undefined
          -> 
            {ok, Ref} = timer:send_after(?AUDIO_PROFILE_WAIT_MS, wait_for_audio_profile),
            State2#?state{ timer = Ref };
          ?otherwise -> State2
        end
    end,
  {ok, State3};
process_input(_, State = #?state{ published_playlist = true }) ->
  {ok, State}.

handle_info(wait_for_audio_profile, State = #?state{ published_playlist = true }) ->
  {noreply, State};

handle_info(wait_for_audio_profile, State = #?state{ published_playlist = false }) ->
  self() ! publish_playlists,
  State2 = State#?state{ published_playlist = true },
  {noreply, State2};

handle_info(publish_playlists, State = #?state{
                                          config = #hls_master_playlist_processor_config{
                                           audio_only = AudioOnly,
                                           slot_id = SlotId,
                                           profiles = Profiles,
                                           push_details = PushDetails = [ #{ putBaseUrl := PutBaseUrl, auth := #{ type := <<"basic">>, username := Username, password := Password } }]
                                          },
                                          profiles = AvProfiles
                                        }) ->
  ?INFO("Building and sending playlists, pushdetails = ~p", [PushDetails]),

  Playlist = m3u8:master_playlist(build_playlists(rtsv2_types:uuid_to_string(SlotId), AvProfiles, Profiles, PushDetails, AudioOnly)),
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
check_profiles_ready(_State = #?state{ profiles = FoundProfiles,
                                       config = #hls_master_playlist_processor_config{
                                                   profiles = ConfigProfiles,
                                                   audio_only = AudioOnly
                                      }
                                    }) ->
  AvProfilesList = maps:values(FoundProfiles),
  VideoProfiles = lists:filtermap(fun (Prof = #video_profile{}) -> {true, Prof};
                                      (_) -> false
                                   end, AvProfilesList),
  AudioProfiles = lists:filtermap(fun (Prof = #audio_profile{}) -> {true, Prof};
                                      (_) -> false
                                   end, AvProfilesList),
  case {length(ConfigProfiles), length(VideoProfiles), length(AudioProfiles)} of
    {N, N, N} ->
      ?INFO("Profiles are ready!~n~p", [FoundProfiles]),
      all_ready;
    {N, _, N} when AudioOnly->
      ?INFO("Profiles are ready!~n~p", [FoundProfiles]),
      all_ready;
    {N, N, _} ->
      video_ready;
    {Expected, ActualVideo, ActualAudio} when Expected > ActualVideo orelse Expected > ActualAudio -> not_ready;
    {Expected, ActualVideo, ActualAudio} ->
      ?ERROR("Found ~p/~p A/V profiles when only expecting ~p", [ActualVideo, ActualAudio, Expected]),
      not_ready
  end.


build_playlists(SlotId, AvProfiles, Profiles, [PushDetail = #{}|_], AudioOnly) ->
  #master_playlist {
     rendition_groups = [ ],
     variant_streams = lists:filtermap(fun (Profile) -> variant_stream(SlotId, AvProfiles, PushDetail, Profile, AudioOnly) end, Profiles)
  }.


variant_stream(SlotId, AvProfiles, _PushDetail, Profile = #{ bitrate := BitRate, profileName := ProfileName}, AudioOnly) ->
  AvProfilesList = maps:to_list(AvProfiles),
  VideoProfiles = lists:filtermap(fun ({{P, _Pid}, Prof = #video_profile{}}) when P =:= ProfileName -> {true, Prof};
                                       (_) -> false
                                   end, AvProfilesList),
  AudioProfiles = lists:filtermap(fun ({{P, _Pid}, Prof = #audio_profile{}}) when P =:= ProfileName -> {true, Prof};
                                       (_) -> false
                                   end, AvProfilesList),
  if
    length(VideoProfiles) /= 1 and not AudioOnly ->
      ?ERROR("Expected 1 video profile on ~p", [ProfileName]),
      false;
    length(AudioProfiles) > 1 ->
      ?ERROR("Expected 1 audio profile on ~p", [ProfileName]),
      false;
    ?otherwise ->
      AudioProfile = case AudioProfiles of
                      [FoundProfile] -> FoundProfile;
                      [] -> #audio_profile{codec_profile_level = #codec_profile_level { profile = ?LIBAV_AAC_PROFILE_MAIN } }
                     end,
      case AudioOnly of
        true -> 
          ?INFO("making variant for slotid=~p with ~p, av profiles=~p", [SlotId, Profile, AudioProfile]),
          {true, #variant_stream {
            name = ProfileName,
            program_id = 1,
            codecs = [
              audio_profile_to_codec_descriptor(AudioProfile)
            ],
            peak_bandwidth = BitRate,
            uri = << ProfileName/binary, "/playlist.m3u8" >>
            }
          };
        false -> 
          [VideoProfile = #video_profile { width = Width, height = Height } = VideoProfile] = VideoProfiles,

          ?INFO("making variant for slotid=~p with ~p, av profiles=~p, ~p", [SlotId, Profile, VideoProfile, AudioProfile]),
          {true, #variant_stream {
            name = ProfileName,
            program_id = 1,
            codecs = [
              video_profile_to_codec_descriptor(VideoProfile),
              audio_profile_to_codec_descriptor(AudioProfile)
            ],
            peak_bandwidth = BitRate,
            resolution = { Width, Height },
            uri = << ProfileName/binary, "/playlist.m3u8" >>
            }
          }
        end
  end.


audio_profile_to_codec_descriptor(#audio_profile{codec_profile_level = #codec_profile_level { profile = Profile } }) ->
  #aac_codec_descriptor{ profile = case Profile of
                                    ?LIBAV_AAC_PROFILE_LOW -> low_complexity;
                                    ?LIBAV_AAC_PROFILE_HE -> high_efficiency;
                                    ?LIBAV_AAC_PROFILE_MAIN -> main
                                  end
                        }.
video_profile_to_codec_descriptor(#video_profile {
                                                    codec_profile_level = #codec_profile_level {
                                                                            profile = VideoProfile,
                                                                            level = VideoLevel
                                                                          }
                                                 }) ->
  #h264_codec_descriptor { profile = VideoProfile, level = VideoLevel }.
