{ compiler ? "default", doBenchmark ? false }:

let

  pkgs = import ../kassir/nix/pkgs.nix;

  f = { mkDerivation, aeson, base, containers, hpack, hspec
      , http-client, process, servant, servant-client, silently, stdenv
      , text, time
      }:
      mkDerivation {
        pname = "toxiproxy-haskell";
        version = "0.2.1.0";
        src = ./.;
        libraryHaskellDepends = [
          aeson base containers http-client servant servant-client text
        ];
        libraryToolDepends = [ hpack ];
        testHaskellDepends = [
          base containers hspec http-client process servant servant-client
          silently time
        ];
        preConfigure = "hpack";
        homepage = "https://github.com/jpittis/toxiproxy-haskell#readme";
        description = "Client library for Toxiproxy: a TCP failure testing proxy";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});
  withToxi = ride: ride.overrideAttrs (_: 
   { buildInputs = with pkgs; ride.buildInputs ++ [ toxiproxy coreutils ];
   });

in

  if pkgs.lib.inNixShell then withToxi drv.env else withToxi drv
