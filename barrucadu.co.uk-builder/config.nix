{ pkgs }:
{
  # pandoc-sidenote is marked broken, but version 0.20.0 works
  allowBroken = true;

  packageOverrides = pkgs: {
    haskellPackages = pkgs.haskellPackages.override {
      overrides = hsSelf: hsSuper: {
        pandoc-sidenote = pkgs.haskell.lib.overrideCabal hsSuper.pandoc-sidenote (drv: rec {
          version = "0.20.0";
          src = pkgs.fetchgit {
            url = "https://github.com/jez/pandoc-sidenote";
            sha256 = "0pkzri1x6nq9xr0rlgl79g14n1bd5l839h2g0mpdvavyzgbwnc9n";
            rev = version;
          };
        });
      };
    };
  };
}
