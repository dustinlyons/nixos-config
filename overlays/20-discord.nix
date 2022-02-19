self: super: {
  discord = super.discord.overrideAttrs (_: { src = builtins.fetchTarball { url = https://discord.com/api/download?platform=linux&format=tar.gz; sha256 = "0hdgif8jpp5pz2c8lxas88ix7mywghdf9c9fn95n0dwf8g1c1xbb"; };
   }
 );
}
