{
  inputs.ghc-wasm-meta.url = "gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org";

  outputs =
    { self
    , ghc-wasm-meta
    ,
    }:
    ghc-wasm-meta.inputs.flake-utils.lib.eachSystem
      [
        "x86_64-linux"
        "aarch64-darwin"
        "aarch64-linux"
      ]
      (
        system:
        let
          pkgs = import ghc-wasm-meta.inputs.nixpkgs { inherit system; };
        in
        {
          devShells.default = pkgs.mkShellNoCC {
            nativeBuildInputs = with pkgs; [
              brotli
              ghcid
              git
              python3
              wabt
              ghc-wasm-meta.outputs.packages."${system}".all_9_12
            ];
          };
        }
      );
}
