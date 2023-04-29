{
  description = "A very basic flake";

  inputs.miso = {
    url = "https://github.com/dmjio/miso/archive/refs/tags/1.8.3.tar.gz";
    flake = false;
  };

  outputs = { self, nixpkgs, miso }:
    with
    (import miso { system = "x86_64-linux"; });
    let
      client = pkgs.haskell.packages.ghcjs86.callCabal2nix "haskell-miso" ./. { };
      server = pkgs.haskell.packages.ghc865.callCabal2nix "haskell-miso" ./. { };
    in
    {

      packages.x86_64-linux.client = client;
      packages.x86_64-linux.server = server;

    };
}
