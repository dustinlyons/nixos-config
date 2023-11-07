# Overlays

Files in this directory run automatically as part of each build. Some common ways I've used overlays in the past:
* Applying patches
* Downloading different versions of files (locking to a version or trying a fork)
* Workarounds and stuff I need to run temporarily

Here are some previous examples.

### Overriding a package with a specific hash from Github
To get the sha256, I just made something up and tried to build it; Nix will complain with the actual sha256.
```nix
final: prev: {
  picom = prev.picom.overrideAttrs (old: {
    src = prev.fetchFromGitHub {
      owner = "pijulius";
      repo = "picom";
      rev = "982bb43e5d4116f1a37a0bde01c9bda0b88705b9";
      sha256 = "YiuLScDV9UfgI1MiYRtjgRkJ0VuA1TExATA2nJSJMhM=";
    };
  });
}
```

### Override a file or attribute of a package
In Nix, we get to just patch things willy nilly. This is an old patch I used to get the `cypress` package working; it tidied me over until a proper fix was in `nixpkgs`.

```nix
# When Cypress starts, it copies some files locally from the Nix Store, but
# fails to remove the read-only flag.
#
# Luckily, the code responsible is a plain text script that we can easily patch:
#
final: prev: {
  cypress = prev.cypress.overrideAttrs (oldAttrs: {
    installPhase = let
      matchForChrome = "yield utils_1.default.copyExtension(pathToExtension, extensionDest);";
      appendForChrome = "yield fs_1.fs.chmodAsync(extensionBg, 0o0644);"; # We edit this line

      matchForFirefox = "copyExtension(pathToExtension, extensionDest)";
      replaceForFirefox = "copyExtension(pathToExtension, extensionDest).then(() => fs.chmodAsync(extensionBg, 0o0644))"; # We edit this line
    in ''
      sed -i '/${matchForChrome}/a\${appendForChrome}' \
          ./resources/app/packages/server/lib/browsers/chrome.js

      sed -i 's/${matchForFirefox}/${replaceForFirefox}/' \
          ./resources/app/packages/server/lib/browsers/utils.js
    '' + oldAttrs.installPhase;
  });
}
```
