# Changelog

## [0.3.1](https://github.com/liuyinz/mini-echo.el/compare/v0.3.0..v0.3.1) - 2023-10-19

### Refactoring

- rename internal var mini-echo--orig-mdf - ([3f4ab02](https://github.com/liuyinz/mini-echo.el/commit/3f4ab02959d121d24b903003a940aa33229d71d0))
- rewrite macro mini-echo-define-segment - ([f9ce7e8](https://github.com/liuyinz/mini-echo.el/commit/f9ce7e8d99bfa06fd2e95cac11af979b833201a8))

## [0.3.0](https://github.com/liuyinz/mini-echo.el/compare/v0.2.0..v0.3.0) - 2023-10-18

### Bug Fixes

- require pakcages to silent compile warning - ([be2ecf7](https://github.com/liuyinz/mini-echo.el/commit/be2ecf7f6f3936cc2627fe127ebcbc3f37a376db))

### Documentation

- **(README)** update todo list - ([e7f485f](https://github.com/liuyinz/mini-echo.el/commit/e7f485f6e2fd017f9cb96dec7e71d6551e719ca7))

### Features

- add option mini-echo-buffer-status-style - ([7676b1f](https://github.com/liuyinz/mini-echo.el/commit/7676b1f562595428914bd819148f74eea7d3b79d))
- add segment of profiler - ([5aa6ff7](https://github.com/liuyinz/mini-echo.el/commit/5aa6ff7dbf155e5ace11869db8afe228b5d08bcc))
- add var mini-echo-toggle-segments - ([8b5c99a](https://github.com/liuyinz/mini-echo.el/commit/8b5c99ae3ecc3df4ee26d2fc4e9c7827c2daaf39))

### Performance

- set mini-echo-update-interval to 0.3 - ([67d7569](https://github.com/liuyinz/mini-echo.el/commit/67d75690cd9374724444e86369118e21118afd6c))

### Refactoring

- internal function of set window border color - ([2cda5ce](https://github.com/liuyinz/mini-echo.el/commit/2cda5ce9bfebb3cb8fd05883d75a941e9dca5d87))

## [0.2.0](https://github.com/liuyinz/mini-echo.el/compare/v0.1.1..v0.2.0) - 2023-10-17

### Bug Fixes

- get meow status only through meow--indicator - ([30bd072](https://github.com/liuyinz/mini-echo.el/commit/30bd072b87fd76a54add645c36a964efab15465d))
- update overlays with timer too as long as it's not hide - ([b0e7b4b](https://github.com/liuyinz/mini-echo.el/commit/b0e7b4b8d420938a4d55b8ecf4bff51e7ecfa520))

## [0.1.1](https://github.com/liuyinz/mini-echo.el/compare/v0.1.0..v0.1.1) - 2023-10-16

### Bug Fixes

- return empty string if not in a project - ([d54a03b](https://github.com/liuyinz/mini-echo.el/commit/d54a03b91a99c134da13eaaf8c81279edd5175b6))
- mini-echo-define-segment macro error - ([4b9fcca](https://github.com/liuyinz/mini-echo.el/commit/4b9fcca1773d2a0abd8e423f5d353a77e363d021))

## [0.1.0] - 2023-10-15

### Bug Fixes

- repeat insertion in minibuf - ([3026c86](https://github.com/liuyinz/mini-echo.el/commit/3026c865b010d1a28789b00d7480d817ed1c6a13))
- select right frame when calculate frame width - ([2639e4e](https://github.com/liuyinz/mini-echo.el/commit/2639e4ef96a911a37afba72d75c3d01e95cef75a))
- add flymake exception mark before diagnostic numbers - ([ea1d9b6](https://github.com/liuyinz/mini-echo.el/commit/ea1d9b6f957c0a76a5893504dc163269ff55e05b))
- use minibuffer-window width rather than frame-width - ([7d57493](https://github.com/liuyinz/mini-echo.el/commit/7d57493fa1dc2c0f557816d8c3d25c1bcfa1038b))
- update overlays before print message - ([1620870](https://github.com/liuyinz/mini-echo.el/commit/1620870b304992477ebb1bebe3b16cd4fbd6292a))
- save and restore mode-line-format per buffer - ([1dde02f](https://github.com/liuyinz/mini-echo.el/commit/1dde02f1d167e4938b29aea3af940de01d9d166d))
- update echo info after resize frame size - ([93dfbb5](https://github.com/liuyinz/mini-echo.el/commit/93dfbb56b14026b3e35e487395715249950e3283))

### Documentation

- **(README)** update todo list - ([01de9cb](https://github.com/liuyinz/mini-echo.el/commit/01de9cba55a2a2dcce7aec760aed9a7fa2b43be0))
- **(README)** update package informations - ([00e6f78](https://github.com/liuyinz/mini-echo.el/commit/00e6f7844ae8e64b7b3a96ab61905ddfdcd65c59))
- add todo list - ([8821aaa](https://github.com/liuyinz/mini-echo.el/commit/8821aaa5183bd22d02ca1da88060f90abf1ae8e9))

### Features

- add segment macro-record - ([666fba7](https://github.com/liuyinz/mini-echo.el/commit/666fba7f3b1d1f75a1da6572f3fa6e5d51c128bd))
- add option for full/short style - ([1fbde32](https://github.com/liuyinz/mini-echo.el/commit/1fbde32c69b61305c685f183c44e4e32e9614bf5))
- add option mini-echo-right-padding - ([864a9de](https://github.com/liuyinz/mini-echo.el/commit/864a9de60c63bdf2e29ee09a0ef89523a0c035cd))
- add segment vcs - ([76c631a](https://github.com/liuyinz/mini-echo.el/commit/76c631a23b97fd539147f4f8bdb08b56fd6fe13b))
- add option mini-echo-short-segments-prdicate - ([bafcc78](https://github.com/liuyinz/mini-echo.el/commit/bafcc7858d76346cfc802c38c7bb8d5f2c6c7727))
- add segment of narrow buffer-status - ([c56bac9](https://github.com/liuyinz/mini-echo.el/commit/c56bac9185a95b5df4d4ef28d800f0b40afca40f))
- simplify magit-blob-mode buffer name - ([4ddc8a4](https://github.com/liuyinz/mini-echo.el/commit/4ddc8a4a89b120d5ba7cc31b2d9de23fc89e6cc1))
- add option mini-echo-window-divider-args - ([8f5734c](https://github.com/liuyinz/mini-echo.el/commit/8f5734c72e8aa14c370c0d1ea4ff374e325194fe))
- add option mini-echo-window-border-color - ([42719d5](https://github.com/liuyinz/mini-echo.el/commit/42719d5b8b271c796432f51c1b770e911cbafd8a))

### Miscellaneous Chores

- **(changelog)** add cliff.toml - ([72ee0bd](https://github.com/liuyinz/mini-echo.el/commit/72ee0bd0f0ebacac56efe95141996385468dc194))
- **(dependency)** require emacs version >=28.1 - ([f61f30c](https://github.com/liuyinz/mini-echo.el/commit/f61f30cc3ac9b919f2f189d20b3f09e78233c3fd))
- add gitignore pattern - ([76b0403](https://github.com/liuyinz/mini-echo.el/commit/76b04033d2cb04fedfa479d443a5a54ef626f557))

### Refactoring

- rename internal vars - ([600ffad](https://github.com/liuyinz/mini-echo.el/commit/600ffadf205d69f464e725b51cc856daa692a349))
- macro and narrow segment - ([d1879ef](https://github.com/liuyinz/mini-echo.el/commit/d1879ef3aaaf412ce406be63879dc8d76c7e5050))
- bind buffer-status to buffer-name - ([56dabf8](https://github.com/liuyinz/mini-echo.el/commit/56dabf8ba1c4cc45daa5c238dd351c99b286c268))
- mini-echo-hide-mode-line - ([026f24a](https://github.com/liuyinz/mini-echo.el/commit/026f24adf228907a747bee3d50d63f115a8798b4))

<!-- generated by git-cliff -->
