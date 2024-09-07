# Changelog

## [0.13.1](https://github.com/liuyinz/mini-echo.el/compare/v0.13.0..v0.13.1) - 2024-09-07

### Bug Fixes

- byte warning of missing variable - ([931f440](https://github.com/liuyinz/mini-echo.el/commit/931f440ce2264a13599780811e8fc27494cfb202))
- typos in option names - ([288fd3b](https://github.com/liuyinz/mini-echo.el/commit/288fd3bb6fe9b2653babcd797e06909be8e01fa2))
- silent compile warning - ([c08646b](https://github.com/liuyinz/mini-echo.el/commit/c08646bc6c11191ceaaf49dc5e45d1849e60cc1e))

### Refactoring

- **(segment)** enhance buffer name segments info - ([80062f8](https://github.com/liuyinz/mini-echo.el/commit/80062f821efbb923f5159cd614ae478b796784f1))
- remove segment ide - ([6637a9e](https://github.com/liuyinz/mini-echo.el/commit/6637a9e5cba7ab141259f27a8291fc73a039f008))
- adjust face of dired segments - ([f377035](https://github.com/liuyinz/mini-echo.el/commit/f37703572c879d40484a57a1ea429f22b6288830))

## [0.13.0](https://github.com/liuyinz/mini-echo.el/compare/v0.12.1..v0.13.0) - 2024-09-03

### Bug Fixes

- **(project)** return project correctly even if non-file buffers - ([724ffc7](https://github.com/liuyinz/mini-echo.el/commit/724ffc7b1b418181ed5fa9be37b6b9af3ed1f131))
- update atomic-chrome segments rule - ([1451178](https://github.com/liuyinz/mini-echo.el/commit/1451178a3e983ec95f3b3b5d17f574bf14ae67c7))
- typos in mini-echo-default-rules - ([ddb2510](https://github.com/liuyinz/mini-echo.el/commit/ddb25108bb09b61461aba582df97f16725de6a23))
- remove major-mode from diff and dired segments - ([c83ad5c](https://github.com/liuyinz/mini-echo.el/commit/c83ad5c228044242fa7a07e3a4c6eb02ed86edc5))
- typos in description and documentation - ([7efd7e3](https://github.com/liuyinz/mini-echo.el/commit/7efd7e3dda7b7d273abda7c9165b1282982501b7))

### Features

- **(helpful)** add segment helpful - ([28a623b](https://github.com/liuyinz/mini-echo.el/commit/28a623b8c0a9ceb949c5065d54094f3022d02084))
- **(mini-echo-rule-detect)** [**breaking**] use function rather than var to handle special conditions - ([15b2897](https://github.com/liuyinz/mini-echo.el/commit/15b28973e2cf498660e7bc4ac6b82eafd407be66))
- **(mini-echo-rules-function)** rename mini-echo-rule-detect to mini-echo-rules-function - ([7eb4b0b](https://github.com/liuyinz/mini-echo.el/commit/7eb4b0bd2f3b741b16e56f38fb741447155663ed))
- add segments of diff and atomic-chrome - ([862f6fa](https://github.com/liuyinz/mini-echo.el/commit/862f6facee14f12cb662f6e8b4651c8d3af98e8a))
- add rule for magit related buffers - ([db876b1](https://github.com/liuyinz/mini-echo.el/commit/db876b16c0c8b83e87ae86ef0fb89aabde636d14))
- add segment blame - ([535c2fb](https://github.com/liuyinz/mini-echo.el/commit/535c2fba6783df72871418c9c17853f00c4dcd3d))

### Refactoring

- only add timer to check buffers in older versions emacs - ([371fbaf](https://github.com/liuyinz/mini-echo.el/commit/371fbafdb14a293703ae7f7d2a94fbc79837f2b6))
- diff/atomic face - ([89d6e25](https://github.com/liuyinz/mini-echo.el/commit/89d6e25f3d9afbf8eec7fd9aa7a7e061f9fdef3b))
- [**breaking**] deprecated some options with new one - ([7ce9622](https://github.com/liuyinz/mini-echo.el/commit/7ce9622607ae668de263be24616d0cd799181a74))

## [0.12.1](https://github.com/liuyinz/mini-echo.el/compare/v0.12.0..v0.12.1) - 2024-08-27

### Bug Fixes

- hide mode lines in all buffers if some failed globally - ([f38cb0f](https://github.com/liuyinz/mini-echo.el/commit/f38cb0fff7b96322433731546aa2d33626a24422))

### Refactoring

- mini-echo-init-echo-area - ([0434313](https://github.com/liuyinz/mini-echo.el/commit/0434313f3149a557cc91c21605a499b6480cdf54))

## [0.12.0](https://github.com/liuyinz/mini-echo.el/compare/v0.11.1..v0.12.0) - 2024-08-16

### Bug Fixes

- kill temp buffer for calculating string-pixel-width - ([802321f](https://github.com/liuyinz/mini-echo.el/commit/802321f0658b364d4e9d90a4ac43959d46048015))

### Features

- use ellipsis when shrink-path is too long - ([f15119b](https://github.com/liuyinz/mini-echo.el/commit/f15119b90e46acc6ead73ea03cf2a6308799f72a))

## [0.11.1](https://github.com/liuyinz/mini-echo.el/compare/v0.11.0..v0.11.1) - 2024-06-03

### Bug Fixes

- **(shrink-path)** string-trim fails when buffer name contains * - ([bba194a](https://github.com/liuyinz/mini-echo.el/commit/bba194a1e4d3cf243917ed0a456de6a689f78115))
- **(shrink-path)** lose properties of current buffer status - ([ca7a7af](https://github.com/liuyinz/mini-echo.el/commit/ca7a7af6f0c6381c69d95b13bf25b0014c337d95))

### Refactoring

- remove segment vterm, add segment ide - ([8b54310](https://github.com/liuyinz/mini-echo.el/commit/8b543100c9695c32a51c40f5caf6f8b65a4e694f))
- update mini-echo-rules about segment "ide" - ([0d2b9ce](https://github.com/liuyinz/mini-echo.el/commit/0d2b9ce0afa6bec76a6ca31fd79be024fbfb0551))

## [0.11.0](https://github.com/liuyinz/mini-echo.el/compare/v0.10.0..v0.11.0) - 2024-06-02

### Bug Fixes

- **(buffer-name)** only update last part of buffer-name due to uniquify style - ([3c9ba73](https://github.com/liuyinz/mini-echo.el/commit/3c9ba7313f350120c66bef4429b55010b8c75b5a))
- **(shrink-path)** show duplicated dir due to uniquify - ([6fd22cf](https://github.com/liuyinz/mini-echo.el/commit/6fd22cf6bbfd7710e572a09130927c6df0af43ae))
- silent byte-comp warning - ([079015f](https://github.com/liuyinz/mini-echo.el/commit/079015fecb83f020c7c12c6868cbf3013db5f4ca))
- update mini-echo default options with shrink-path - ([029ce41](https://github.com/liuyinz/mini-echo.el/commit/029ce410aff7f8c15424c837853acfafc35b732f))
- use origin face for process segment - ([6d419f8](https://github.com/liuyinz/mini-echo.el/commit/6d419f8b118fa2858da3563e2dfeadcbc51e8902))

### Features

- add segment vterm - ([d5ec3be](https://github.com/liuyinz/mini-echo.el/commit/d5ec3beaad8a13fedbc34f8d6f5f670c45c47151))
- add segment dired - ([f7e9cea](https://github.com/liuyinz/mini-echo.el/commit/f7e9cea2af294422f48577c40da11b2478104eb1))

### Refactoring

- [**breaking**] update some segments name - ([e3b0d9e](https://github.com/liuyinz/mini-echo.el/commit/e3b0d9e5f65077cfb38ff6bb39a628cb2e4a6c5e))
- simplify rules with mini-echo--ruleset - ([a5f6f30](https://github.com/liuyinz/mini-echo.el/commit/a5f6f300bcd4a2cd63e4fcd860775adafdcdd2aa))

## [0.10.0](https://github.com/liuyinz/mini-echo.el/compare/v0.9.3..v0.10.0) - 2024-05-28

### Bug Fixes

- **(mise)** update mise/envrc status info - ([fb16e84](https://github.com/liuyinz/mini-echo.el/commit/fb16e8400af1634b2dd583beecbb801a8e8003c3))
- typos - ([3989ab9](https://github.com/liuyinz/mini-echo.el/commit/3989ab957ff8c1a7ae4c20ca6f977de964a4f420))

### Features

- **(mise)** add option mini-echo-mise-show-always - ([5b8ecb7](https://github.com/liuyinz/mini-echo.el/commit/5b8ecb7f0ead15f5ba5c0d05d145b3a4f9627961))

### Refactoring

- **(process)** handle ibuffer-mode specially - ([b79abee](https://github.com/liuyinz/mini-echo.el/commit/b79abeedde684f6e73e950856fead3ff748b19d7))

## [0.9.3](https://github.com/liuyinz/mini-echo.el/compare/v0.9.2..v0.9.3) - 2024-05-23

### Bug Fixes

- **(char-info)** only display info when character exist - ([4e29182](https://github.com/liuyinz/mini-echo.el/commit/4e2918225bea7fa7d232260bd0b2de48df68c6f8))
- calculate string length correctly on gui - ([9bcc56c](https://github.com/liuyinz/mini-echo.el/commit/9bcc56cb25951fcf6b317499e90335e9e127c4ef))

### Refactoring

- macro mini-echo-define-segment - ([c11620f](https://github.com/liuyinz/mini-echo.el/commit/c11620f0a7d083f740382f2c4e0af1a28a1c4e1d))
- use pcase instead cl-case - ([19689a1](https://github.com/liuyinz/mini-echo.el/commit/19689a14f465704a6a854da895eeb7b3288f7bf7))

## [0.9.2](https://github.com/liuyinz/mini-echo.el/compare/v0.9.1..v0.9.2) - 2024-05-14

### Miscellaneous Chores

- **(cliff)** fix changelog link error - ([76433e6](https://github.com/liuyinz/mini-echo.el/commit/76433e622657cfc4f991b886cc20bcaacb9df0d0))

## [0.9.1](https://github.com/liuyinz/mini-echo.el/compare/v0.9.0..v0.9.1) - 2024-05-14

### Bug Fixes

- type error in defcustom - ([cc1ffcc](https://github.com/liuyinz/mini-echo.el/commit/cc1ffccbc35ccfffbefa5c5bb34d658dbdfe0763))

### Refactoring

- use dash function instead - ([4cc246c](https://github.com/liuyinz/mini-echo.el/commit/4cc246cdaa025bcd46f0509a9cb468bea425f17a))

## [0.9.0](https://github.com/liuyinz/mini-echo.el/compare/v0.8.0..v0.9.0) - 2024-05-08

### Bug Fixes

- fontify minibuffer when activated too - ([9b3fd95](https://github.com/liuyinz/mini-echo.el/commit/9b3fd955e04ec168fa769a79cb9f480e84f90ffd))
- force hide all mode line when mini-echo-mode is enabled - ([8285a5f](https://github.com/liuyinz/mini-echo.el/commit/8285a5f9e87be869b1853e7c10024ca5231ab0d9))

### Documentation

- update dependencies section - ([b3e41b7](https://github.com/liuyinz/mini-echo.el/commit/b3e41b7997d02698b9cedbd0f2ceb8500bcce0c4))

### Features

- **(segment)** add segment of text-scale and mise - ([adeb402](https://github.com/liuyinz/mini-echo.el/commit/adeb402f5e508424960b6165d596b25fd6a6d756))

### Refactoring

- **(segment)** rewrite segment - buffer-position - ([aca80ea](https://github.com/liuyinz/mini-echo.el/commit/aca80ea92ee74f222ae82e52c146419484bf2abc))
- optimize hide-mode-line feature - ([e85294e](https://github.com/liuyinz/mini-echo.el/commit/e85294e06e9ce9824735aed0ab6d828661439c9a))

## [0.8.0](https://github.com/liuyinz/mini-echo.el/compare/v0.7.2..v0.8.0) - 2024-03-03

### Refactoring

- **(dependency)** [**breaking**] add new dependency of hide-mode-line - ([1fd8311](https://github.com/liuyinz/mini-echo.el/commit/1fd831111fc81aeb82156715af340dda15f346d7))

## [0.7.2](https://github.com/liuyinz/mini-echo.el/compare/v0.7.1..v0.7.2) - 2024-02-20

### Bug Fixes

- renmae segment elfeed-unread to elfeed - ([4a44c09](https://github.com/liuyinz/mini-echo.el/commit/4a44c098feb321cec4913380a36c0ea4e0463f86))
- make `update-func` more compatible - ([cd58b8d](https://github.com/liuyinz/mini-echo.el/commit/cd58b8ded9f8280d00e1a1052e22a141f4e13f7c))

### Features

- add segment elfeed - ([f547997](https://github.com/liuyinz/mini-echo.el/commit/f54799741131527ff6e200795b9f288ddc6d93c5))

### Miscellaneous Chores

- bump copyright years - ([3aed734](https://github.com/liuyinz/mini-echo.el/commit/3aed734872a8657c4cc443590b065a331e16ff3e))

### Refactoring

- **(macro)** use default info instead - ([54affbe](https://github.com/liuyinz/mini-echo.el/commit/54affbea08a7023ff373d8ec3e4a693b049b38ae))
- **(narrow)** use mode-line-format instead - ([b504ea9](https://github.com/liuyinz/mini-echo.el/commit/b504ea9ae180e85c335bc52e2db2857795ded4d5))
- provide new faces for segments - ([371d0fb](https://github.com/liuyinz/mini-echo.el/commit/371d0fb348a93bd18cf8b4933f1a3e3ffcddb66a))

## [0.7.1](https://github.com/liuyinz/mini-echo.el/compare/v0.7.0..v0.7.1) - 2023-12-27

### Bug Fixes

- only call segment related update-func when mini-echo-mode is enabled - ([b7e4c00](https://github.com/liuyinz/mini-echo.el/commit/b7e4c00160831778fe271137335449f64058bd0a))
- lack of information when some segments setup hooks or advices when first activated - ([b1bcb38](https://github.com/liuyinz/mini-echo.el/commit/b1bcb386a83c7f473620d0eef8d1739c36da8f3a))

### Documentation

- **(README)** update Customization part - ([7604aa2](https://github.com/liuyinz/mini-echo.el/commit/7604aa28a150d6e39b31cf529631e5adc0893f6d))

### Refactoring

- use plistp to check segment definition instead - ([7cf4d0a](https://github.com/liuyinz/mini-echo.el/commit/7cf4d0a59c4ec661683052bb000b2db11df06fa4))

## [0.7.0](https://github.com/liuyinz/mini-echo.el/compare/v0.6.2..v0.7.0) - 2023-12-26

### Bug Fixes

- remove segment display property too - ([b3d79a3](https://github.com/liuyinz/mini-echo.el/commit/b3d79a34fe75df13941d6729f5a6b6b785593f58))

### Documentation

- update todo list - ([9a9e673](https://github.com/liuyinz/mini-echo.el/commit/9a9e673a624e9c1c1586dca2ec3d1bb5002ce088))

### Features

- support parent mode rules if emacs version >= 30 - ([b9ce71d](https://github.com/liuyinz/mini-echo.el/commit/b9ce71dfbf7b7fde165cd33e067f2bb4ca46309d))

### Miscellaneous Chores

- **(dependency)** update emacs dependency to >= 29.1 - ([0559dd5](https://github.com/liuyinz/mini-echo.el/commit/0559dd59b8e122d15225a8763bdfcdc173129570))

### Refactoring

- segment "vcs" to adapted to vc-mode - ([226f5c0](https://github.com/liuyinz/mini-echo.el/commit/226f5c0cf493205114f230a1483fdeee57d03a06))
- fetch vcs info from vc-mode directly - ([39dacb4](https://github.com/liuyinz/mini-echo.el/commit/39dacb4734eb51c6cc6fb5573627190f86d0d49b))
- make segment reuse mode-line-format as much as possible - ([ec59632](https://github.com/liuyinz/mini-echo.el/commit/ec5963242f4ac57b3d6844d34a7304b0dfee90f9))

## [0.6.2](https://github.com/liuyinz/mini-echo.el/compare/v0.6.1..v0.6.2) - 2023-12-20

### Documentation

- **(todo)** update todo list - ([2e32cfd](https://github.com/liuyinz/mini-echo.el/commit/2e32cfdf1d25efcfbc1506e9fb230fb8cbd45f72))

### Features

- add segment repeat - ([228cd1c](https://github.com/liuyinz/mini-echo.el/commit/228cd1ca3433412347509ec2c6c065c5edfc2954))
- add segment buffer-point - ([3c60309](https://github.com/liuyinz/mini-echo.el/commit/3c6030944ca32c9d9144660e4e497c4f26520851))
- add segment char-info - ([b039eef](https://github.com/liuyinz/mini-echo.el/commit/b039eef2fa61005bc251d7e5ee3026760c83f9ef))
- support rules for derived major modes - ([edd6848](https://github.com/liuyinz/mini-echo.el/commit/edd68480f08eae7d517c9f540ec505fb3d6d5c38))

### Refactoring

- change envrc status str - ([3e0d2a4](https://github.com/liuyinz/mini-echo.el/commit/3e0d2a4b45bad543ad91264e9b136bb4ba760fac))

## [0.6.1](https://github.com/liuyinz/mini-echo.el/compare/v0.6.0..v0.6.1) - 2023-11-29

### Bug Fixes

- silent compiler warnings - ([6d16c28](https://github.com/liuyinz/mini-echo.el/commit/6d16c2808b325af30bc36b345cfd93fd7a1f14bb))

### Features

- add segment flycheck - ([0ff844d](https://github.com/liuyinz/mini-echo.el/commit/0ff844d43761166af6c6671599f777197b1852bc))
- setup rules for xwidget-webkit-mode by default - ([1b25b17](https://github.com/liuyinz/mini-echo.el/commit/1b25b17b4151c24fdd5e2650446aca7c20cfca0a))

### Refactoring

- change default value of mini-echo-rules - ([2cae921](https://github.com/liuyinz/mini-echo.el/commit/2cae921c978caff11e7be47fdd9245341bfa720c))

## [0.6.0](https://github.com/liuyinz/mini-echo.el/compare/v0.5.4..v0.6.0) - 2023-11-18

### Bug Fixes

- continue to show info even if minibuf is erased accidently - ([5dfd160](https://github.com/liuyinz/mini-echo.el/commit/5dfd1608d384be3176c4f06fef908933e2fd4a02))
- use default face for minibuffer-window by default - ([2793a21](https://github.com/liuyinz/mini-echo.el/commit/2793a21472375ed01ec97922ceb0fa6df7a40b33))
- remove trailing space of battery segment info - ([cf6c697](https://github.com/liuyinz/mini-echo.el/commit/cf6c6979c63b20692b1c0f34d6c2593b24d85a8b))
- avoid duplicated infos when load and switch new theme - ([5c43d10](https://github.com/liuyinz/mini-echo.el/commit/5c43d10138cb7911138a42d054dae0fc11bc7146))
- error in segment battery - ([d0c3021](https://github.com/liuyinz/mini-echo.el/commit/d0c3021cc7d0c3f1e8d4878f61c11c20bcc6af64))

### Documentation

- **(TODO)** add nerd-icons support in todo list - ([628df0c](https://github.com/liuyinz/mini-echo.el/commit/628df0c106d58fd55a3200646906ddfd690ca264))

### Features

- add segment battery - ([93ea4de](https://github.com/liuyinz/mini-echo.el/commit/93ea4de41f29864c8ef34f90c8b0492516ea8155))

### Refactoring

- deprecate face mini-echo-window-divider - ([dc51970](https://github.com/liuyinz/mini-echo.el/commit/dc51970a928ad4b0512f4cdfb5e483ccf6154e7f))
- remove useless function - ([9491020](https://github.com/liuyinz/mini-echo.el/commit/9491020bb7e3818ca8c94052c3590dafd53db7d5))

## [0.5.4](https://github.com/liuyinz/mini-echo.el/compare/v0.5.3..v0.5.4) - 2023-11-14

### Bug Fixes

- do not report error if declare-function missing file - ([dfea407](https://github.com/liuyinz/mini-echo.el/commit/dfea407a5d8a395a359b36899c30e2b5ae547227))
- use ellipsis if needed when buffer name is created by atomic-chrome - ([9d89e35](https://github.com/liuyinz/mini-echo.el/commit/9d89e352b73ff2222d62b43b8944206582723fef))
- update minibuf overlays show info always - ([5c6c655](https://github.com/liuyinz/mini-echo.el/commit/5c6c6554dd567e2b7b2b9f55d5f7c1d4fc048733))

### Features

- add segment envrc - ([333275e](https://github.com/liuyinz/mini-echo.el/commit/333275eb06385e2b133d34c4d395d173cbecd669))

### Refactoring

- do not show status when there is no .envrc file in dir - ([efb9a8f](https://github.com/liuyinz/mini-echo.el/commit/efb9a8f18788e7ceef4bdf49e42d91fb7520b21b))

## [0.5.3](https://github.com/liuyinz/mini-echo.el/compare/v0.5.2..v0.5.3) - 2023-11-03

### Documentation

- **(README)** update melpa status - ([0407d37](https://github.com/liuyinz/mini-echo.el/commit/0407d37c5f00095073a241b68108fadd2f41b21d))

### Features

- add option mini-echo-ellipsis - ([3cc5596](https://github.com/liuyinz/mini-echo.el/commit/3cc55960f4d835490cf35f9d2cbf80fbb0626225))

### Refactoring

- building new info after testing buffer-window is live - ([56871a5](https://github.com/liuyinz/mini-echo.el/commit/56871a564140b493d678627399942821d045a71d))

## [0.5.2](https://github.com/liuyinz/mini-echo.el/compare/v0.5.1..v0.5.2) - 2023-10-31

### Bug Fixes

- exclude temporarily created buffers to avoid flashing - ([48f2db8](https://github.com/liuyinz/mini-echo.el/commit/48f2db8f02342146623d07c0caefa732971f7a2b))

### Features

- add segment lsp-mode - ([bbbbde4](https://github.com/liuyinz/mini-echo.el/commit/bbbbde4c8f1e14a1917e6bdc25cadd858c724937))
- add segment lsp-bridge - ([860e806](https://github.com/liuyinz/mini-echo.el/commit/860e806d7a7bba8e70e685d51a79b74c409dde2a))
- add segment eglot - ([1a46223](https://github.com/liuyinz/mini-echo.el/commit/1a4622322bf3832bdb721d2c707dff1ff1906712))

### Refactoring

- rename mini-echo-short-segments-predicate to mini-echo-short-style-predicate - ([9822684](https://github.com/liuyinz/mini-echo.el/commit/9822684ad93e5c306d01fd0c3645082a8b64fd12))

## [0.5.1](https://github.com/liuyinz/mini-echo.el/compare/v0.5.0..v0.5.1) - 2023-10-28

### Bug Fixes

- wrong order of recovery if segment is toggled enabled again - ([049d989](https://github.com/liuyinz/mini-echo.el/commit/049d98965db74e9bd8e0c295bdffab2995259ff9))
- check mini-echo-major-mode-segments valid before usage - ([a18bbed](https://github.com/liuyinz/mini-echo.el/commit/a18bbed0fe3c75a38627663b20a1b78f922077bf))

### Documentation

- **(README)** fix typos - ([e1c8bcf](https://github.com/liuyinz/mini-echo.el/commit/e1c8bcf0fb3924c9fe12fadbb9c245fdeee1a311))

### Refactoring

- rename mini-echo-major-mode-segments to mini-echo-rules - ([f06b44c](https://github.com/liuyinz/mini-echo.el/commit/f06b44cb48d1565ec9be32af7fddbefeeb4dd1b2))

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
