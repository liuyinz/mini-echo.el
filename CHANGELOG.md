# Changelog

## [0.5.0](https://github.com/liuyinz/mini-echo.el/compare/v0.4.1..v0.5.0) - 2023-10-28

### Bug Fixes

- **(mini-echo-toggle)** put toggled segments in the beginning - ([9dd46e7](https://github.com/liuyinz/mini-echo.el/commit/9dd46e78e492ab40a0b0acf35c6c4d8aa7de1f70))
- **(mini-echo-toggle)** only available when mini-echo-mode is enabled - ([bbb4893](https://github.com/liuyinz/mini-echo.el/commit/bbb48939ceef47eeacdbb2e58dde6fd91335de5c))
- wrong arguments error in flymake segment - ([c68e930](https://github.com/liuyinz/mini-echo.el/commit/c68e930410a1d202ce8c2022a43989ffe9b0b372))
- remove properties of vcs segment to avoid mess - ([42d90e9](https://github.com/liuyinz/mini-echo.el/commit/42d90e9d59ec26f8d6b4bbde176fd3e32d002136))
- wrong type in defcustom - ([6050e8c](https://github.com/liuyinz/mini-echo.el/commit/6050e8cfbe28737232237707d86f900ecb0cd5c6))

### Documentation

- **(README)** update todo list and add FAQ section - ([29a1770](https://github.com/liuyinz/mini-echo.el/commit/29a17705d6293b5a2190817cf8b4847892debfbe))
- **(README)** update FAQ section - ([f6ce3e2](https://github.com/liuyinz/mini-echo.el/commit/f6ce3e23071a44ce4d1d14f3be4965a05f305d05))
- **(README)** update todo list - ([a8b30a9](https://github.com/liuyinz/mini-echo.el/commit/a8b30a91986be60e718963e2c78cae3c3a3c3b76))
- **(README)** update Usage - ([fbf72d7](https://github.com/liuyinz/mini-echo.el/commit/fbf72d7ac8d6a8638cf687434284a2e2affb4363))
- **(README)** update Customization info to newest - ([b56b6ec](https://github.com/liuyinz/mini-echo.el/commit/b56b6eced47fc5dce48772efebc77114cadd87e4))
- improve description of macro - ([8de162e](https://github.com/liuyinz/mini-echo.el/commit/8de162ebf9c7e003798845deb879a1a261d24390))

### Features

- add segment evil - ([56a9c45](https://github.com/liuyinz/mini-echo.el/commit/56a9c459b1254e148951cc94335c33054725abf4))
- add segment word-count - ([48c62bd](https://github.com/liuyinz/mini-echo.el/commit/48c62bdc4c0edc452536e223f8444ea914473fef))
- add segment last-command - ([02ba873](https://github.com/liuyinz/mini-echo.el/commit/02ba873f827db073391410fd7e8cf058b1c6d833))

### Performance

- only check valid segments when building info first time - ([facbbca](https://github.com/liuyinz/mini-echo.el/commit/facbbca4afe3e135a68cf28d9db9128332b9ded1))

### Refactoring

- rewrite macro keywords names - ([7ea50ef](https://github.com/liuyinz/mini-echo.el/commit/7ea50ef4f10dc7af73560b832a6e3304b675a8c2))
- [**breaking**] lots of improvements - ([894d471](https://github.com/liuyinz/mini-echo.el/commit/894d47189dc8f783896c66d2acc1c8d6ed5545c2))
- internal function mini-echo-get-segments - ([b1bee0f](https://github.com/liuyinz/mini-echo.el/commit/b1bee0f11f9997e6fd7b6504755668faa2dc23c7))

## [0.4.1](https://github.com/liuyinz/mini-echo.el/compare/v0.4.0..v0.4.1) - 2023-10-25

### Bug Fixes

- autoload macro mini-echo-define-segment - ([906d2fb](https://github.com/liuyinz/mini-echo.el/commit/906d2fbc4e2aa1950fe3e9055d1de86b0a106b99))
- command mini-echo-toggle failed problem - ([83b6443](https://github.com/liuyinz/mini-echo.el/commit/83b644320d71a1c381259a52c97b072854be5ff8))

### Documentation

- **(README)** improve features section - ([185c1e1](https://github.com/liuyinz/mini-echo.el/commit/185c1e1e8a618d81e6918ed28c0aa7b672ebfe9e))

### Refactoring

- do not add hook repeatly when init echo area - ([08899f4](https://github.com/liuyinz/mini-echo.el/commit/08899f4eae063a33990e6372a5e4aa4f40dec4a7))

## [0.4.0](https://github.com/liuyinz/mini-echo.el/compare/v0.3.2..v0.4.0) - 2023-10-25

### Bug Fixes

- do not add duplicate segments - ([394ddcd](https://github.com/liuyinz/mini-echo.el/commit/394ddcd0c7df4166ce2d1a85ed4fd002ed9df04a))
- generate segment when load file instead of compile - ([960d5f4](https://github.com/liuyinz/mini-echo.el/commit/960d5f4bca3bf02633ae3d93dba6b4323c8606e6))
- wrong result of calculate project root - ([0573de2](https://github.com/liuyinz/mini-echo.el/commit/0573de274b515683e5d976ec446e3a60d243f558))

### Documentation

- **(README)** update todo list - ([4e0d7dc](https://github.com/liuyinz/mini-echo.el/commit/4e0d7dc4ea9f7482b72c675c6ef296c5346cbc49))
- **(README)** Update README for publish to Melpa - ([deefef7](https://github.com/liuyinz/mini-echo.el/commit/deefef7fa05a586bf6cbedecd6add0307b6ed3db))
- improve docstrings of segments - ([a7ad583](https://github.com/liuyinz/mini-echo.el/commit/a7ad583ae597dad6d43b9fabdc6d741b4c10d611))

### Features

- add option mini-echo-minibuffer-background - ([02f29f5](https://github.com/liuyinz/mini-echo.el/commit/02f29f52ba91204b81aceaba423b80743dfd45b0))
- add hook mini-echo-current-segments-hook - ([2775d28](https://github.com/liuyinz/mini-echo.el/commit/2775d287edac5dbef4534336a71e33bdafc6a0c0))
- add segment keycast - ([73dfa4f](https://github.com/liuyinz/mini-echo.el/commit/73dfa4fbf4fc53c1365073289f3b012a684e0869))
- add option mini-echo-keycast-format - ([9f22fad](https://github.com/liuyinz/mini-echo.el/commit/9f22fad7f633d148b5f96f75befc661f9a4984fc))
- add command mini-echo-toggle - ([2247eeb](https://github.com/liuyinz/mini-echo.el/commit/2247eeb714e912f4954f48a7d255d3c093800afa))

### Refactoring

- rename option mini-echo-window-divider-color - ([f7770c7](https://github.com/liuyinz/mini-echo.el/commit/f7770c79b65f8168b462a8ea8af23d03ba87e102))
- insert space in minibuf-0 only once - ([5ffb568](https://github.com/liuyinz/mini-echo.el/commit/5ffb56853b3840992a67d425e110841c818d0814))
- use faces instead of colors - ([11ee769](https://github.com/liuyinz/mini-echo.el/commit/11ee769250b2cc9754abcf491eb525e616618eb1))
- use cl-defstruct to define segments - ([84bd3e2](https://github.com/liuyinz/mini-echo.el/commit/84bd3e2760b8aefbab4178b3f98cba84d2fb1491))
- macro mini-echo-define-segment - ([81b9afd](https://github.com/liuyinz/mini-echo.el/commit/81b9afddb0ad17d60244f929d43bafdf0116b13d))
- remove single segment toggle cmd - ([3c65ae0](https://github.com/liuyinz/mini-echo.el/commit/3c65ae03bd636b5647cf951a10539b8fcb8e07fd))
- better default value for default/short styles - ([03647dd](https://github.com/liuyinz/mini-echo.el/commit/03647dd67b2a9237bca1f14842e94083e528a093))

## [0.3.2](https://github.com/liuyinz/mini-echo.el/compare/v0.3.1..v0.3.2) - 2023-10-19

### Bug Fixes

- do not use plistp for compatiblility with emacs 28 - ([1797ddb](https://github.com/liuyinz/mini-echo.el/commit/1797ddbc4f68a7d6c8c83d17166b023d2829e9bd))
- warning of unknown function - ([786c38e](https://github.com/liuyinz/mini-echo.el/commit/786c38ee2e40fabca438e1d0640f7e7fef35983b))

### Features

- **(segment)** add new segment - time - ([93e3d8b](https://github.com/liuyinz/mini-echo.el/commit/93e3d8b511833c0a5a62a13076782907c4ecbacf))

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
