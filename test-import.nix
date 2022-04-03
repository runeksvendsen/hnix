let
  a = import ./test-simple.nix;
in { y = a.x; }