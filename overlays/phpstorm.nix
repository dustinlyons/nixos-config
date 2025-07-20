(final: prev: {
  jetbrains = prev.jetbrains // {
    phpstorm = prev.jetbrains.phpstorm.override {
      # Force using stock JDK (skip building JetBrains runtime)
      jdk = prev.jdk21;
    };
  };
})
