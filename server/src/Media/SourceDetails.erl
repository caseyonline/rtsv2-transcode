-module(media_sourceDetails@foreign).

-include_lib("id3as_avp/include/avp_ingest_source_details_extractor.hrl").

-export([
         foreignToPursImpl/1
        ]).

foreignToPursImpl(#source_info{ audio_streams = AudioStreams
                              , video_streams = VideoStreams}) ->
  #{ audio_streams => map_audio_streams(AudioStreams)
   , videoStreams => map_video_streams(VideoStreams)
   }.

map_audio_streams(AudioStreams) ->
  lists:foldl(fun(#audio_stream_info{ stream_id = StreamId
                                    , format = Format
                                    , sample_rate = SampleRate
                                    , channel_layout = ChannelLayout
                                    , language = Language
                                    , in_fillin_mode = InFillinMode
                                    , dvb_stream_id = DvbStreamId
                                    }, Acc) ->

                  [#{ streamId => StreamId
                    , codec => map_audio_format(Format)
                    , sampleRate => to_maybe(SampleRate)
                    , channelLayout => map_channel_layout(ChannelLayout)
                    , language => to_maybe(Language)
                    , inFillinMode => ?null_coalesce(InFillinMode, false)
                    , dvbStreamId => to_maybe(DvbStreamId)
                    } | Acc]
              end,
              [],
              AudioStreams).

map_video_streams(VideoStreams) ->
  lists:foldl(fun(#video_stream_info{ stream_id = StreamId
                                    , format = Format
                                    , width = Width
                                    , height = Height
                                    , pixel_aspect_ratio = PixelAspectRatio
                                    , interlaced = Interlaced
                                    , resolution_name = ResolutionName
                                    , frame_rate_name = FrameRateName
                                    , language = Language
                                    , in_fillin_mode = InFillinMode
                                    }, Acc) ->

                  [#{ streamId => StreamId
  %% , codec :: Maybe VideoCodec
  %% , width :: Maybe Width
  %% , height :: Maybe Height
  %% , pixelAspectRatio :: Maybe PixelAspectRatio
  %% , interlaced :: Maybe Boolean
  %% , resolutionName :: Maybe String
  %% , frameRateName :: Maybe String
  %% , language :: Maybe String
  %% , inFillinMode :: Boolean
                    } | Acc]
              end,
              [],
              VideoStreams).

map_audio_format(undefined) ->
  {nothing};

map_audio_format(Format) ->
  {just, case Format of
           1 -> 1
         end}.

map_channel_layout(_) ->
  {nothing}.

to_maybe(undefined) ->
  {nothing};

to_maybe(Value) ->
  {just, Value}.
