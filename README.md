# marlowe-actus-labs

Web app which deploys ACTUS contracts using Marlowe on the Cardano blockchain. WIP.

## Installation

Devel env `shell.nix` doesn't cover `Js` deps so in order to build the app you have to perform `$ npm install` manually.

## CI

* Testing:
  ```shell
  $ spago test
  ```

* Formatting:
  ```shell
  $ purs-tidy format-in-place 'src/**/*.purs'
  ```

## Staging server

We deploy staging servers using `nixpacks` (it derives the build steps from `package.json` - `scripts.start`, `engines`). To test the deployment setup you can run:

```
$ nixpacks build ./path/to/app --name my-app
```
