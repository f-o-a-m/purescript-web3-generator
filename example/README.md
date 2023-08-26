# Example

We use `purescript-web3-generator` in the absence of template-purescript. Suggested usage is as follows:


In your project, create a dedicated `spago.dhall` with `web3-generator` as a dependency and a source directory that mimics the setup of this example, e.g. a single module `Generator` with the example `main` function. 

1. Run the following command to generate an executable for `purescript-web3-generator`
```bash
> npx spago bundle-app -m Generator -t generator.js          
```

2. You can now execute the generator with whatever arguments you want. In the case of this repo example, it might look like
```bash
> node generator.js --abis ../abi-data/truffle --truffle true
```

3. If you don't know what to do, simply run
```bash
node generator.js --help
```

As of the time of this README, you should see something like:
```bash
➜  example git:(optparse) ✗ node generator.js --help                                   
ps-web3-generator - generate Purescript bindings to your solidity contracts

Usage: generator.js --abis ABIS [--dest DEST] [--prefix PREFIX]
                    [--module MODULE] [--truffle TRUFFLE]
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