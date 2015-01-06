{ pkgs ? import <nixpkgs> {},
  src ? builtins.filterSource (path: type:
    type != "unknown" &&
    baseNameOf path != ".git" &&
    baseNameOf path != "result" &&
    baseNameOf path != "dist" &&
    baseNameOf path != ".nhc") ./.
}:
let
  hdevtoolsSrc = pkgs.fetchgit {
    url = "https://github.com/schell/hdevtools.git";
    rev = "4ff36c55ed64a774067697d9830a490bb9028263";
    sha256 = "612946494aef28a0a9bf0e48b7e93f878f02791ec0735019ee49ae6e4eb04daf";
  };
  hdevtools = pkgs.haskellPackages.buildLocalCabal hdevtoolsSrc "hdevtools";
in
pkgs.haskellPackages.buildLocalCabalWithArgs {
  inherit src;
  name = "cron-syslog-viewer";
  cabalDrvArgs = {
    buildTools = [
      hdevtools
      pkgs.gnuplot
    ];
  };
}
