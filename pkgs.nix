{
  extras = hackage:
    { packages = { obiwan = ./obiwan.nix; }; };
  resolver = "lts-14.10";
  modules = [ { packages = {}; } ];
  }