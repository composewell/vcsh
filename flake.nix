{
  description = "vcsh Haskell port development environment";

  inputs = {
    basepkgs.url = "github:composewell/streamly-packages/69728978adc44f53b3dd907acb2eb5bd2415fd60";
    nixpkgs.follows = "basepkgs/nixpkgs";
    nixpkgs-darwin.follows = "basepkgs/nixpkgs-darwin";
  };

  outputs = { self, nixpkgs, nixpkgs-darwin, basepkgs }:
    basepkgs.nixpack.mkOutputs {
      inherit nixpkgs nixpkgs-darwin basepkgs;
      name = "vcsh";
      sources = import ./sources.nix;
      packages = import ./packages.nix;
    };
}
