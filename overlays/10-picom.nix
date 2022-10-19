final: prev: {
  picom = prev.picom.overrideAttrs (old: {
    src = prev.fetchFromGitHub {
      owner = "yshui";
      repo = "picom";
      rev = "8a373c38a631e0344c38d3b19ab673aacfbaf1f5";
      sha256 = "mKtop5/80YIAZqogshk5Tm9PR+93rLWSh8hqtNFjlPE=";
    };
  });
}
