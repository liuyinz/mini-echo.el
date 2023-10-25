# mini-echo.el

Echo buffer status in minibuffer window instead of mode-line.
A lightweight replacement of [mini-modeline](https://github.com/kiennq/emacs-mini-modeline)

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

**Warning: This package isn't available in melpa yet.**

~~This package is available on [MELPA].~~
Install with `M-x package-install` `RET` `binky` within Emacs.

## Feature

- do not use mode-line at all, same experience in terminal
- use `window-divider-mode` in gui
- port lots of default segments from [doom-modeline](https://github.com/seagle0128/doom-modeline)
- configs are as simple as possible

## Usage

```elisp
(require 'mini-echo)
(mini-echo-mode)
```

- `options` as below, for more options please check source file.

```elisp
;; set different segments of default/short style according to window width
(setq mini-echo-default-segments '("flymake" "buffer-position" "vcs" "major-mode"))
(setq mini-echo-short-segments '("buffer-position" "major-mode"))

;; write your own predicate function to switch style
(setq mini-echo-short-segments-predicate #'your-own-predicate)

;; adjust window-divider-mode appearence
(setq mini-echo-window-divider-args '(t 1 1))

;;; adjust update interval as you wish
(setq mini-echo-update-interval 0.3)

;;; adjust the number to avoid truncation or wrap line of minibuffer window
(setq mini-echo-right-padding 1)
```

- `mini-echo-toggle` : to toggle some segment temporarily

## Customization

- Write a segment with `mini-echo-define-segment`, e.g.

  ```elisp
  (mini-echo-define-segment "vcs"
  "Return vcs info of current buffer."
  :fetch mini-echo--vcs-status
  :hook '(find-file-hook after-save-hook after-revert-hook)
  :advice '((vc-refresh-state . :after))
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
    :mode '(display-time-mode)
    :fetch
    (propertize display-time-string 'face 'mini-echo-time))

  (mini-echo-define-segment "keycast"
    "Display keycast info."
    :hook '(post-command-hook)
    :fetch
    (keycast--format mini-echo-keycast-format)
    :update
    (keycast--update)
    :setup '(:activate (require 'keycast)))
  ```

  - `:fetch`: sexp, which runs when mini-echo update by interval.
  - `:update`: sexp, which runs when `:hook` or `:advice` is triggered.
  - `:hook`: list of hooks which run `:update` after it called, e.g. update "vcs" status after run `find-file-hook`
  - `:advice`: alist of (symbol . how) which runs `:update` after it called, e.g. update "vcs" status after run `vc-refresh-state`
  - `:mode`: list of minor modes which should be enabled before running `:fetch`, e.g. turn on `display-time-mode` before fetch "time" info
  - `:setup`: sexp, which runs when the segment is activated for first time, e.g. load library `keycast` when fetch `keycast` info for first time.

  For more information, please see [mini-echo-segments.el](mini-echo-segments.el).

## Similar Package

- [feebleline](https://github.com/tautologyclub/feebleline)

- [mini-modeline](https://github.com/kiennq/emacs-mini-modeline)

- [awesome-tray](https://github.com/manateelazycat/awesome-tray)

## Todo

- [ ] add environment support, such as python, node.js, asdf...
- [x] rewrite mini-echo-define-macro
- [x] add minibuffer background to distinguish in terminal
