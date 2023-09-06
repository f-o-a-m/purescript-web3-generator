# Example

We use `purescript-web3-generator` in the absence of template-purescript. Suggested usage is as follows:


In your project, create a dedicated `spago.dhall` with `web3-generator` as a dependency and a source directory that mimics the setup of this example, e.g. a single module `Main` with the example `main` function.

1. You can now execute the generator with whatever arguments you want. In the case of this repo example, it might look like
```bash
> spago run --node-args="--abis ../abi-data/truffle --truffle true"
```

2. If you don't know what to do, simply run
```bash
spago run --node-args='--help'
```

As of the time of this README, you should see something like:
```bash
➜  example git:(purs-0.15) ✗ spago run --node-args='--help'
ps-web3-generator - generate Purescript bindings to your solidity contracts

Usage: run.js --abis ABIS [--dest DEST] [--prefix PREFIX] [--module MODULE]
              [--truffle TRUFFLE]
  Purescript Web3 Generator

Available options:
  --abis ABIS              The abi source directory.
  --dest DEST              The destination directory for purescript
                           code. (default: "./src")
  --prefix PREFIX          The expression prefix for the generated purescript
                           code (used to get around solidity identifiers that
                           generate invalide Purescript).
  --module MODULE          The module name prefix for the generated purescript
                           code.
  --truffle TRUFFLE        Are the abi files truffle artifacts
  -h,--help                Show this help text
```