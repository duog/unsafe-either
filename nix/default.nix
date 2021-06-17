with builtins;
{ sources ? import ./sources.nix
, config ? {}
, overlays ? []
, system ? currentSystem
}: import sources.nixpkgs {
  inherit config overlays system;
}
