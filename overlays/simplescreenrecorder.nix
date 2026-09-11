# simplescreenrecorder overlay
#
# Upstream 0.4.4 still reads AVCodec.pix_fmts / AVCodec.sample_fmts directly.
# FFmpeg 8 removed both fields (replaced by avcodec_get_supported_config), so
# building against the current default ffmpeg fails in AV/AVWrapper.cpp.
# Pin to ffmpeg 7 until upstream ports to the new API.
(final: prev: {
  simplescreenrecorder = prev.simplescreenrecorder.override {
    ffmpeg = prev.ffmpeg_7;
  };
})
