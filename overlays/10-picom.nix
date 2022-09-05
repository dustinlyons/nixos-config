final: prev: {
  picom = prev.picom.overrideAttrs (old: {
    src = prev.fetchFromGitHub {
      owner = "dustinlyons";
      repo = "picom";
      rev = "1f69af3174bd45324487087c667ea3545cbf2d58";
      sha256 = "HIZ9/zKlpgwogue/ranOlVpWSCoZ+xhLVx245QlgTIU=";
    };
  });
}
