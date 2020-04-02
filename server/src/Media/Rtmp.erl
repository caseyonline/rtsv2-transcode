-module(media_rtmp@foreign).

-include_lib("id3as_common/include/common.hrl").

-export([
         foreignToPursImpl/2
        ]).

foreignToPursImpl(Metadata, Map) ->
  lists:foldl(fun({Name, Value}, Acc) ->
                  add_value_to_map(Name, Value, Acc)
              end,
              Map,
              Metadata).

add_value_to_map(Name, Value, Map) ->
  try
    add_value_to_map_(Name, Value, Map)
  catch
    _:_ ->
      ?SLOG_INFO("Failed to add RTMP metadata", #{ name => Name
                                                 , value => Value}),
      Map
  end.

%% Audio
add_value_to_map_(<<"audiocodecid">>, Value, Map = #{audio := Audio}) ->
  maps:put(audio, maps:put(codecId, {just, map_audio_codec_id(Value)}, Audio), Map);

add_value_to_map_(<<"audiodatarate">>, Value, Map = #{audio := Audio}) ->
  maps:put(audio, maps:put(dataRate, {just, Value}, Audio), Map);

add_value_to_map_(<<"audiosamplesize">>, Value, Map = #{audio := Audio}) ->
  maps:put(audio, maps:put(sampleSize, {just, trunc(Value)}, Audio), Map);

add_value_to_map_(<<"audiosamplerate">>, Value, Map = #{audio := Audio}) ->
  maps:put(audio, maps:put(sampleRate, {just, trunc(Value)}, Audio), Map);

add_value_to_map_(<<"stereo">>, Value, Map = #{audio := Audio}) ->
  maps:put(audio, maps:put(stereo, {just, Value}, Audio), Map);

add_value_to_map_(<<"audiodevice">>, Value, Map = #{audio := Audio}) ->
  maps:put(audio, maps:put(device, {just, Value}, Audio), Map);

add_value_to_map_(<<"audiochannels">>, Value, Map = #{audio := Audio}) ->
  maps:put(audio, maps:put(channels, {just, trunc(Value)}, Audio), Map);

add_value_to_map_(<<"audioinputvolume">>, Value, Map = #{audio := Audio}) ->
  maps:put(audio, maps:put(inputVolume, {just, Value}, Audio), Map);

%% Video
add_value_to_map_(<<"videocodecid">>, Value, Map = #{video := Video}) ->
  maps:put(video, maps:put(videoCodecId, {just, map_video_codec_id(Value)}, Video), Map);

add_value_to_map_(<<"videodatarate">>, Value, Map = #{video := Video}) ->
  maps:put(video, maps:put(videoDataRate, {just, Value}, Video), Map);

add_value_to_map_(<<"framerate">>, Value, Map = #{video := Video}) ->
  maps:put(video, maps:put(frameRate, {just, Value}, Video), Map);

add_value_to_map_(<<"width">>, Value, Map = #{video := Video}) ->
  maps:put(video, maps:put(width, {just, trunc(Value)}, Video), Map);

add_value_to_map_(<<"height">>, Value, Map = #{video := Video}) ->
  maps:put(video, maps:put(height, {just, trunc(Value)}, Video), Map);

add_value_to_map_(<<"videodevice">>, Value, Map = #{video := Video}) ->
  maps:put(video, maps:put(device, {just, Value}, Video), Map);

add_value_to_map_(<<"videokeyframe_frequency">>, Value, Map = #{video := Video}) ->
  maps:put(video, maps:put(keyFrameFrequency, {just, Value}, Video), Map);

add_value_to_map_(<<"avcprofile">>, Value, Map = #{video := Video}) ->
  maps:put(video, maps:put(avcProfile, {just, map_avc_profile(Value)}, Video), Map);

add_value_to_map_(<<"avclevel">>, Value, Map = #{video := Video}) ->
  maps:put(video, maps:put(avcLevel, {just, Value}, Video), Map);

%% Misc
add_value_to_map_(<<"duration">>, Value, Map) ->
  maps:put(duration, {just, Value}, Map);

add_value_to_map_(<<"encoder">>, Value, Map) ->
  maps:put(encoder, {just, Value}, Map);

add_value_to_map_(<<"filesize">>, Value, Map) ->
  maps:put(filesize, {just, Value}, Map);

add_value_to_map_(Name, Value, Map = #{other := Other}) ->
  maps:put(other, [#{name => Name,
                     value => rtmp_metadata_value_to_purs(Value)} | Other], Map).

map_audio_codec_id(AudioCodecId) when is_number(AudioCodecId) ->
  case trunc(AudioCodecId) of
    0 -> {uncompressed};
    1 -> {aDPCM};
    2 -> {mP3};
    5 -> {nellymoser8Mono};
    6 -> {nellymoser};
    10 -> {aAC};
    11 -> {speex}
  end;

map_audio_codec_id(AudioCodecId) ->
  case AudioCodecId of
    <<".mp3">> -> {mP3};
    <<"mp4a">> -> {aAC}
  end.

map_video_codec_id(VideoCodecId) when is_number(VideoCodecId) ->
  case trunc(VideoCodecId) of
    2 -> {sorensonH263};
    3 -> {screen};
    4 -> {on2VP6};
    5 -> {on2VP6Transparency};
    7 -> {h264};
    <<"avc1">> -> {h264}
  end;

map_video_codec_id(VideoCodecId) ->
  case VideoCodecId of
    <<"avc1">> -> {h264}
  end.

map_avc_profile(AvcProfile) ->
  case trunc(AvcProfile) of
    100 -> {high};
    77 -> {main};
    66 -> {baseline}
  end.

rtmp_metadata_value_to_purs(Value) when Value == true;
                                       Value == false ->
  {rtmpBool, Value};

rtmp_metadata_value_to_purs(Value) when is_integer(Value) ->
  {rtmpInt, Value};

rtmp_metadata_value_to_purs(Value) when is_float(Value) ->
  {rtmpFloat, Value};

rtmp_metadata_value_to_purs(Value) when is_binary(Value) ->
  {rtmpString, Value}.
