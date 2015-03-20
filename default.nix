{
  pkgs ?
    let
      bootstrap_pkgs = import <nixpkgs> {};
      nixpkgsGit = bootstrap_pkgs.fetchgit {
        url = "https://github.com/nixos/nixpkgs";
        rev = "5f7d37480d7fb31d1850652739dd66d41e8f25d9";
        sha256 = "1x00xnd4y4mi8ddzh2kb66k9m4r9hfhnls5y92ny287ckd2n9dl3";
      };
    in import nixpkgsGit {},
  src ? builtins.filterSource (path: type:
    type != "unknown" &&
    baseNameOf path != ".git" &&
    baseNameOf path != "result" &&
    baseNameOf path != "dist" &&
    baseNameOf path != ".nhc") ./.
}: pkgs.haskellPackages.buildLocalCabalWithArgs {
  inherit src;
  name = "cron-syslog-viewer";
  cabalDrvArgs = {
    buildTools = [
      pkgs.gnuplot
    ];
  };
}
