{
  extras = hackage:
    { packages = { obiwan = ./obiwan.nix; }; };
  resolver = "lts-14.16";
  modules = [ { packages = {}; } ];
  }