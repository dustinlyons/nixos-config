final: prev: {
  picom = prev.picom.overrideAttrs (old: {
    src = prev.fetchFromGitHub {
      owner = "mebenstein";
      repo = "picom";
      rev = "b68488273e3274436eda1b714d1fa57824cf9e4a";
      sha256 = "HIZ9/zKlpgwogue/ranOlVpWSCoZ+xhLVx245QlgTIU=";
    };
  });
}
