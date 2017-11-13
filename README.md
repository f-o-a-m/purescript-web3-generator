# purescript-web3-generator
<img src=https://github.com/f-o-a-m/purescript-web3/blob/master/purescript-web3-logo.png width="75">

[![Latest release](http://img.shields.io/github/release/f-o-a-m/purescript-web3-generator.svg?branch=master)](https://github.com/f-o-a-m/purescript-web3-generator/releases)
[![Build status](https://travis-ci.org/f-o-a-m/purescript-web3-generator.svg?branch=master)](https://travis-ci.org/f-o-a-m/purescript-web3-generator?branch=master)

Generats purescript modules from Solidity ABIs

## Requirements

- `npm`
- `psc-package`

## Getting Started
```
> git clone
> cd purescript-web3-generator
> npm install
> pulp build
> pulp test
```

## How to use it

For a complete example that follows the steps below, see [`purescript-web3-example`](https://github.com/f-o-a-m/purescript-web3-example).

We use `purescript-web3-generator` in the absence of template-purescript. Suggested usage is as follows:

1. Create a directory `generator/` in your project with a file that looks like this

```purescript

module Generator where

import Data.GeneratorMain (generatorMain)

main = generatorMain

```

2. From there, add a build step 

```sh

pulp -m Generator --src-path generator run -- --abis <abis> --dest src/Contracts ...

```

(note that we specify both a different source directory than `src` and a different module `Generator` that `purs` is looking for `main` in)


Until [this issue](https://github.com/purescript-contrib/pulp/issues/309) is fixed, we have to temporarily replace the step above with something like this

```sh
pulp -m Generator --src-path generator build --to generator.js
node generator.js --abis <abis> --dest src/Contracts ...
rm generator.js
```

3. Now you should have created contract modules for each contract into your `src/Contracts` directory that your code can depend on.

## Documentation

Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-web3-generator).

## Resources

 - [purescript-web3](https://github.com/f-o-a-m/purescript-web3)
 - [web3.js repo](https://github.com/ethereum/web3.js)
 - [web3 Javascript API wiki](https://github.com/ethereum/wiki/wiki/JavaScript-API)

