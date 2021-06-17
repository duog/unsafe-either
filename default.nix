 { pkgs ? import ./nix {}
, compilerVersion ? null
, doCheck ? false
, lib ? pkgs.lib
, this ? "unsafe-either"
, inNixShell ? false
}: with lib;
let
  newpkgs = pkgs.extend (self: super: let
    baseHaskellPackages = if compilerVersion == null then super.haskellPackages else super.haskell.${compilerVersion}.packages;
  in {
    haskellPackages = baseHaskellPackages.extend (with self.haskell.lib; let
      in  hself: hsuper: {
        "${this}" = (hself.callCabal2nix "${this}" ./. {}).overrideAttrs (drv: {
          passthru = drv.passthru or {} // {
            pkgs =  self;
            haskellPackages = hself;
            shell = hself.shellFor {
              packages = p: [ p.${this} ];
              buildInputs = [
                hself.haskell-language-server
                hself.cabal-install
              ];
            };
          };
        });
      });
  });
  this' = newpkgs.haskellPackages.${this};
in if inNixShell then this'.shell else this'
