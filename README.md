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
  - [Similar Package](#similar-package)
  - [Todo](#todo)
  - [FAQ](#faq)

<!-- markdown-toc end -->

## Screenshot

![example](example.gif)

## Install

### dependencies

- Emacs, version >= 28.1

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

2. `mini-echo-rules`: variable, list of rules which are only take effect in specific major mode, the format is as below:

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

keywords format:

- `:fetch`: sexp, which runs when mini-echo update by interval.
- `:setup`: sexp, which runs when the segment is first activated , e.g. load library `keycast` when activate `keycast` segment.
- `:update`: sexp, which runs when `:update-hook` or `:update-advice` is triggered.
- `:update-hook`: list of hooks which run `:update` after it called, e.g. update "vcs" status after run `find-file-hook`
- `:update-advice`: alist of (symbol . how) which runs `:update` after it called, e.g. update "vcs" status after run `vc-refresh-state`

```elisp
(mini-echo-define-segment "vcs"
  "Return vcs info of current buffer."
  :fetch mini-echo--vcs-status
  :update-hook '(find-file-hook after-save-hook after-revert-hook)
  :update-advice '((vc-refresh-state . :after))
  :update
  (setq mini-echo--vcs-status
        (when (and vc-mode buffer-file-name)
          (let* ((backend (vc-backend buffer-file-name))
                 (branch (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2)))
                 (limit mini-echo-vcs-max-length)
                 (face (cl-case (vc-state buffer-file-name backend)
                         (needs-update 'warning)
                         ((removed conflict unregistered) 'error)
                         (t 'success))))
            (propertize (concat "@" (if (> (length branch) limit)
                                        (concat (substring branch 0 (- limit 3))
                                                "..")
                                      branch))
                        'face `(:inherit (,face bold)))))))

(mini-echo-define-segment "time"
  "Return current time info."
  :setup (display-time-mode 1)
  :fetch (propertize display-time-string 'face 'mini-echo-time))

(mini-echo-define-segment "keycast"
  "Display keycast info."
  :fetch (keycast--format mini-echo-keycast-format)
  :setup (require 'keycast)
  :update (keycast--update)
  :update-hook '(post-command-hook))
```

For more information, please see [mini-echo-segments.el](mini-echo-segments.el).

## Similar Package

- [feebleline](https://github.com/tautologyclub/feebleline)

- [mini-modeline](https://github.com/kiennq/emacs-mini-modeline)

- [awesome-tray](https://github.com/manateelazycat/awesome-tray)

## Todo

- [x] rewrite mini-echo-define-macro
- [x] add minibuffer background to distinguish in terminal
- [x] setup segments per buffer
- [ ] provide rules for derived modes
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
