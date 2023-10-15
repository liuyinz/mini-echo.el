;;; mini-echo.el --- Echo buffer status in minibuffer window -*- lexical-binding: t -*-

;; Copyright (C) 2023 liuyinz

;; Author: liuyinz <liuyinz95@gmail.com>
;; Maintainer: liuyinz <liuyinz95@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: frames
;; Homepage: https://github.com/liuyinz/mini-echo.el

;; This file is not a part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; This file is not a part of GNU Emacs.

;;; Commentary:

;; Echo buffer status in minibuffer window

;;; Code:

(require 'mini-echo-segments)

(defgroup mini-echo nil
  "Echo buffer status in minibuffer window."
  :group 'mini-echo)

(defcustom mini-echo-default-segments
  '("macro" "narrow" "selection-info" "process" "flymake" "buffer-size"
    "buffer-position" "major-mode" "vcs" "buffer-name" "meow")
  "Segments displayed in mini-echo by default."
  :type '(repeat string)
  :group 'mini-echo)

(defcustom mini-echo-short-segments
  '("macro" "narrow" "selection-info" "process" "flymake"
    "buffer-position" "buffer-name-short" "meow")
  "Segments displayed in mini-echo in short style."
  :type '(repeat string)
  :group 'mini-echo)

(defcustom mini-echo-short-segments-predicate
  #'mini-echo-minibuffer-width-lessp
  "Predicate to use short style segments."
  :type '(choice
          (const :tag "" mini-echo-minibuffer-width-lessp)
          function)
  :group 'mini-echo)

(defcustom mini-echo-right-padding 0
  "Padding to append after mini echo info.
Set this to avoid truncation."
  :type 'number
  :group 'mini-echo)

(defcustom mini-echo-update-interval 0.5
  "Seconds between update mini echo segments."
  :type 'number
  :group 'mini-echo)

(defcustom mini-echo-window-divider-args '(t 1 1)
  "List of arguments to initialize `window-divider-mode'.
Format is a list of three argument:
  (`window-divider-default-places'
   `window-divider-default-right-width'
   `window-divider-default-bottom-width')."
  :type '(symbol number number)
  :group 'mini-echo)

(defcustom mini-echo-window-border-color "#5d6a76"
  "Color of window border."
  :type 'string
  :group 'mini-echo)

(defvar-local mini-echo--old-mdf nil)
(defvar mini-echo-old-window-border-color nil)
(defvar mini-echo-overlays nil)

(defun mini-echo-show-divider (&optional hide)
  "Enable `window-divider-mode' in mini echo.
If optional arg HIDE is non-nil, disable the mode instead."
  (if (null hide)
      (cl-destructuring-bind (window-divider-default-places
                              window-divider-default-right-width
                              window-divider-default-bottom-width)
          mini-echo-window-divider-args
        (setq mini-echo-old-window-border-color
              (cons (face-foreground 'window-divider nil 'default)
                    (face-background 'window-divider nil 'default)))
        (set-face-attribute 'window-divider nil
                            :foreground mini-echo-window-border-color
                            :background mini-echo-window-border-color)
        (window-divider-mode 1))
    (set-face-attribute 'window-divider nil
                        :foreground (car mini-echo-old-window-border-color)
                        :background (cdr mini-echo-old-window-border-color))
    (setq mini-echo-old-window-border-color nil)
    (window-divider-mode -1)))

(defun mini-echo-hide-modeline (&optional show)
  "Hide mode-line in mini echo.
If optional arg SHOW is non-nil, show the mode-line instead."
  (if (null show)
      (progn
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (setq mini-echo--old-mdf mode-line-format)
            (setq mode-line-format nil)))
        (setq-default mode-line-format nil))
    ;; FIXME new buffer under mini-echo recover has modeline face bug
    (let ((orig-value (get 'mode-line-format 'standard-value)))
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (setq mode-line-format (or mini-echo--old-mdf orig-value))
          (setq mini-echo--old-mdf nil)))
      (setq-default mode-line-format orig-value)))
  (when (called-interactively-p 'any)
    (redraw-display)))

(defun mini-echo-init-echo-area (&optional deinit)
  "Initialize echo area and minibuffer in mini echo.
If optional arg DEINIT is non-nil, remove all overlays."
  (if (null deinit)
      (dolist (buf '(" *Echo Area 0*" " *Echo Area 1*"))
        (with-current-buffer (get-buffer-create buf)
          (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
            (push ov mini-echo-overlays))))
    (mapc #'delete-overlay mini-echo-overlays)
    (setq mini-echo-overlays nil)
    (with-current-buffer " *Minibuf-0*"
      (delete-region (point-min) (point-max)))))

(defun mini-echo-minibuffer-width ()
  "Return current minibuffer window."
  (with-selected-frame (or (frame-parent (window-frame))
                           (window-frame))
    (window-width (minibuffer-window))))

(defun mini-echo-get-segment-string (segment)
  "Return string of SEGMENT info."
  (or (funcall (cdr (assoc segment mini-echo-segment-alist))) ""))

(defun mini-echo-minibuffer-width-lessp ()
  "Return non-nil if current minibuffer window width less than 120."
  (< (mini-echo-minibuffer-width) 120))

(defun mini-echo-build-info ()
  "Build mini-echo information."
  (condition-case nil
      (let* ((info (mapconcat
                    'identity
                    (seq-remove
                     #'string-empty-p
                     (mapcar #'mini-echo-get-segment-string
                             (if (funcall mini-echo-short-segments-predicate)
                                 mini-echo-short-segments
                               mini-echo-default-segments)))
                    " ")))
        (concat (propertize " "
                            'cursor 1
                            'display `(space :align-to
                                             (- right-fringe
                                                ,(+ mini-echo-right-padding
                                                    (string-width info)))))
                info))
    (format "mini-echo error happends")))

(defun mini-echo-update-minibuf ()
  "Update mini echo info in minibuf."
  (unless (active-minibuffer-window)
    ;; Display the text in Minibuf-0, every time Minibuf killed and recreate
    ;; overlays failed, so only can insert text
    (let ((info (mini-echo-build-info)))
      (with-current-buffer " *Minibuf-0*"
        (erase-buffer)
        (insert info)))))

(defun mini-echo-update-overlays (&optional msg)
  "Update mini echo info in overlays according to MSG.
If MSG is nil, then use `current-message' instead."
  (when-let* (((not (active-minibuffer-window)))
              (msg (or msg (current-message)))
              (info (mini-echo-build-info)))
    (dolist (ov mini-echo-overlays)
      (overlay-put ov 'after-string
                   (if (> (- (mini-echo-minibuffer-width)
                             (string-width info)
                             (string-width msg))
                          0)
                       info "")))))

;; update overlay before message print
(defun mini-echo-message (fn &rest args)
  "Update mini echo info in echo area before FN.
ARGS is optional."
  (mini-echo-update-overlays (and (car args) (apply #'format-message args)))
  (apply fn args))

(defun mini-echo-update-when-resized (&rest _)
  "Update mini echo info after resize frame size."
  (mini-echo-update-overlays))

;;;###autoload
(define-minor-mode mini-echo-mode
  "Minor mode to show buffer status in echo area."
  :group 'mini-echo
  :global t
  (if mini-echo-mode
      (progn
        (mini-echo-show-divider)
        (mini-echo-hide-modeline)
        (mini-echo-init-echo-area)
        ;; FIXME sometimes update twice when switch from echo to minibuf
        (run-with-timer 0 mini-echo-update-interval #'mini-echo-update-minibuf)
        (advice-add 'message :around #'mini-echo-message)
        (add-hook 'window-size-change-functions #'mini-echo-update-when-resized))
    (mini-echo-show-divider 'hide)
    (mini-echo-hide-modeline 'show)
    (mini-echo-init-echo-area 'deinit)
    (cancel-function-timers #'mini-echo-update-minibuf)
    (advice-remove 'message #'mini-echo-message)
    (remove-hook 'window-size-change-functions #'mini-echo-update-when-resized)))

(provide 'mini-echo)
;;; mini-echo.el ends here
