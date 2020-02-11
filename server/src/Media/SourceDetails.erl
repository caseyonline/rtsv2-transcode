-module(media_sourceDetails@foreign).

-include_lib("id3as_avp/include/avp_ingest_source_details_extractor.hrl").

-export([
         foreignToPursImpl/1
        ]).

foreignToPursImpl(#source_info{ audio_streams = AudioStreams
                              , video_streams = VideoStreams}) ->
  #{ audioStreams => map_audio_streams(AudioStreams)
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
                    , codec => map_video_format(Format)
                    , width => to_maybe(Width)
                    , height => to_maybe(Height)
                    , pixelAspectRatio => map_pixel_aspect_ratio(PixelAspectRatio)
                    , interlaced => to_maybe(Interlaced)
                    , resolutionName => to_maybe(ResolutionName)
                    , frameRateName => to_maybe(FrameRateName)
                    , language => to_maybe(Language)
                    , inFillinMode => ?null_coalesce(InFillinMode, false)
                    } | Acc]
              end,
              [],
              VideoStreams).

map_audio_format(undefined) ->
  {nothing};

map_audio_format(Format) ->
  {just, case Format of
           raw -> {'rawAudio'};
           adpcm -> {'aDPCM'};
           mp2 -> {'mP2'};
           mp3 -> {'mP3'};
           linear_pcm -> {'linearPCM'};
           nellymoser -> {'nellymoser'};
           alaw -> {'aLaw'};
           ulaw -> {'uLaw'};
           aac -> {'aAC'};
           speex -> {'speex'};
           ac3 -> {'aC3'};
           eac3 -> {'eAC3'};
           g722 -> {'g722'};
           opus -> {'opus'}
         end}.

map_channel_layout(undefined) ->
  {nothing};

map_channel_layout(Layout) ->
  {just, case Layout of
           mono -> {'mono'};
           stereo -> {'stereo'}
         end}.

map_video_format(undefined) ->
  {nothing};

map_video_format(Format) ->
  {just, case Format of
           raw -> {'rawVideo'};
           vc1 -> {'vC1'};
           h263 -> {'h263'};
           h264 -> {'h264'};
           h265 -> {'h265'};
           mpeg2video -> {'mPEG2'};
           jpeg -> {'jPEG'};
           jpeg2000 -> {'jPEG2000'}
         end}.

map_pixel_aspect_ratio(undefined) ->
  {nothing};

map_pixel_aspect_ratio({Width, Height}) ->
  {just, {tuple, Width, Height}}.

to_maybe(undefined) ->
  {nothing};

to_maybe(Value) ->
  {just, Value}.
