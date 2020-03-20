-ifndef(rtsv2_rtp_hrl).
-define(rtsv2_rtp_hrl, 1).


-define(OPUS_ENCODING_ID, 111).
-define(H264_ENCODING_ID, 125).


-define(SSRC_AUDIO_TAG, 1).
-define(SSRC_VIDEO_TAG, 2).


-define(PROFILE_INDEX_RESERVED_EGEST, 0).
-define(PROFILE_INDEX_STANDARD_OFFSET, 16).


-define(make_audio_ssrc(ProfileIndex, StreamIndex), (ProfileIndex bsl 16) bor (?SSRC_AUDIO_TAG bsl 8) bor StreamIndex).
-define(make_video_ssrc(ProfileIndex, StreamIndex), (ProfileIndex bsl 16) bor (?SSRC_VIDEO_TAG bsl 8) bor StreamIndex).


-endif.
