# mini-echo.el

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg?style=flat)](LICENSE)
[![MELPA](https://melpa.org/packages/mini-echo-badge.svg)](https://melpa.org/#/mini-echo)

Echo buffer status in echo area, get rid of mode-line !

<!-- markdown-toc start -->

## Contents

- [mini-echo.el](#mini-echoel)
  - [Screenshot](#screenshot)
  - [Install](#install)
    - [dependencies](#dependencies)
    - [package](#package)
  - [Feature](#feature)
  - [Usage](#usage)
  - [Customization](#customization)
    - [Keywords](#keywords)
    - [Utils](#utils)
  - [Similar Package](#similar-package)
  - [Todo](#todo)
  - [FAQ](#faq)
  - [Donate](#donate)

<!-- markdown-toc end -->

## Screenshot

![example](example.gif)

## Install

### dependencies

- Emacs, version >= 29.1
- [hide-mode-line](https://github.com/hlissner/emacs-hide-mode-line)

### package

- Manually

Clone and add to `load-path`, require the package.

- Melpa

This package is available on [MELPA].
Install with `M-x package-install` `RET` `mini-echo` within Emacs.

## Feature

- light-weight, very simple structure and no many configs
- don't use mode-line at all, same experience in terminal
- port lots of segments from [doom-modeline](https://github.com/seagle0128/doom-modeline)
- easy to add new segment with macro

## Usage

```elisp
(require 'mini-echo)
(mini-echo-mode)
```

There are three ways to adjust mini-echo segments display:

1. `mini-echo-default-segments`: variable, plist of segments which are default to all major-modes

```elisp
;; set default segments of long/short style
(setq mini-echo-default-segments
  '(:long ("major-mode" "buffer-name" "vcs" "buffer-position"
           "buffer-size" "flymake" "process" "selection-info"
           "narrow" "macro" "profiler")
    :short ("buffer-name-short" "buffer-position" "process"
            "profiler" "selection-info" "narrow" "macro")))
```

2. `mini-echo-rules`: variable, list of rules applied for major modes.
   if your emacs version >= 30, then you can write a rule for parent mode which will take effect in all children modes. Otherwise, write rules for every specific major mode instead.

```elisp
;; Concell of (SEGMENT . POSITION) is required to adjust the appearence.
;; 0 means hide the segment in major mode if it's displayed by default.
;; non-zero number means the order of segment to be put, it's counted from 1.
(setq mini-echo-rules
      '((emacs-lisp-mode :both (("buffer-position" . 3))
                         :long (("evil" . 1) ("buffer-size" . 4))
                         :short (("vcs" . 0)))))
```

Explanation:
when `emacs-lisp-mode` is enabled, long-style shows "evil" segment in first place, shows "buffer-size" segment in fourth place (right-align). short-style hide "vcs" segment. And
both long/short style show "buffer-position" in third place.

3. `mini-echo-toggle`: command, show or hide some segment temporarily

Other options are here, see more info please check the file

```elisp
;; write your own predicate function to switch style
(setq mini-echo-short-style-predicate #'your-own-predicate)

;; set separator to concat information
(setq mini-echo-separator " ")

;; adjust window-divider-mode appearence
(setq mini-echo-window-divider-args '(t 1 1))

;;; adjust update interval as you wish
(setq mini-echo-update-interval 0.3)

;;; adjust the number to avoid truncation or wrap line of minibuffer window
(setq mini-echo-right-padding 1)
```

## Customization

Write a segment with `mini-echo-define-segment`, e.g.

```elisp

(mini-echo-define-segment "vcs"
  "Return vcs info of current buffer.
Segment appearence depends on var `vc-display-status' and faces like
`vc-state-base' and related `vc-**-state'."
  :fetch
  (when (bound-and-true-p vc-mode)
    (mini-echo-segment--print (mini-echo-segment--extract vc-mode)
                              nil mini-echo-vcs-max-length)))

(mini-echo-define-segment "time"
  "Return current time."
  :setup (display-time-mode 1)
  :fetch (mini-echo-segment--extract display-time-string))

(defvar mini-echo--repeat nil)
(mini-echo-define-segment "repeat"
  "Indicator of whether repeating transient map is active."
  :update-advice '((repeat-post-hook . :after))
  :fetch
  (when mini-echo--repeat
    (mini-echo-segment--print "REPEAT" 'mini-echo-repeat))
  :update
  (setq mini-echo--repeat (and repeat-mode repeat-in-progress)))

(mini-echo-define-segment "keycast"
  "Display keycast info."
  :update-hook '(post-command-hook)
  :setup (require 'keycast)
  :fetch (keycast--format keycast-mode-line-format)
  :update (keycast--update))
```

### Keywords

- `:fetch`: sexp, which runs when mini-echo update by interval.
- `:setup`: sexp, which runs when the segment is first activated , e.g. load library `keycast` when activate `keycast` segment.
- `:update`: sexp, which runs when `:update-hook` or `:update-advice` is triggered.
- `:update-hook`: list of hooks which run `:update` after it called, e.g. update "keycast" status after hook `post-command-hook`.
- `:update-advice`: alist of (symbol . how) which runs `:update` after it called, e.g. update "repeat" status after run function `(repeat-post-hook)`.

### Utils

- `mini-echo-segment--extract`: extract segment info from mode-line construct.
- `mini-echo-segment--print`: trim, truncate string with ellipsis if needed.

For more information, please see [mini-echo-segments.el](mini-echo-segments.el).

## Similar Package

- [feebleline](https://github.com/tautologyclub/feebleline)

- [mini-modeline](https://github.com/kiennq/emacs-mini-modeline)

- [awesome-tray](https://github.com/manateelazycat/awesome-tray)

## Todo

- [x] rewrite mini-echo-define-macro
- [x] add minibuffer background to distinguish in terminal
- [x] setup segments per buffer
- [ ] add environment support, such as python, node.js, asdf...
- [ ] add support to highlight current window
- [ ] add support for nerd-icons

## FAQ

- How to distinguish current window ?

  Highlight current window is still on development, you could set hl-line-mode, or use some dim other window package for now.
  e.g.

  ```elisp
  ;; or (global-hl-line-mode)
  (hl-line-mode)
  ;; Only highliht current buffer in current window
  (setq hl-line-sticky-flag nil)
  (setq global-hl-line-sticky-flag nil)
  ```

- How to show window border in terminal?

  In GUI, customize face `window-divider` to show window border, due to `window-divider-mode` is not available in terminal, you need to use other measures to identify windows.
  In terminal, `internal-border` is displayed, so only need to solve horizontal border problem. Enable packages like tabbar or topsy to help highlight horizontal border instead.

- How to distinguish minibuffer window in terminal?

  Customize face `mini-echo-minibuffer-window` to set different background color from
  default.

## Donate

If you think the it's helpful for you, please consider paying a cup of coffee
for me. Thank you! :smile:

<a href="https://paypal.me/liuyinz" target="_blank">
<img
src="https://www.paypalobjects.com/digitalassets/c/website/marketing/apac/C2/logos-buttons/optimize/44_Grey_PayPal_Pill_Button.png"
alt="PayPal" width="120" />
</a>
