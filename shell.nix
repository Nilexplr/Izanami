let
  srcs = import nix/sources.nix;

  pkgs = import srcs.nixpkgs {
    # llvm-hs-pretty is still marked as broken in nixpkgs, even though
    # the incompatibility with LLVM 8 has been addressed in 0.6.2.0:
    # https://github.com/llvm-hs/llvm-hs-pretty/issues/71
    #
    # Working around this the ugly way. (TODO: replace by an override)
    config = { allowBroken = true; };
  };
in
  with pkgs; mkShell {
    buildInputs = [
      cabal-install
      (haskellPackages.ghcWithPackages(p: with p;
        [llvm-hs
         llvm-hs-pure
         llvm-hs-pretty
         text]))
      llvm_8
    ];
  }