{
  extras = hackage:
    { packages = { obiwan = ./obiwan.nix; }; };
  resolver = "lts-14.7";
  modules = [ { packages = {}; } ];
  }