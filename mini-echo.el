;;; mini-echo.el --- Show buffer status in echo area -*- lexical-binding: t -*-

;; Copyright (C) 2023 liuyinz

;; Author: liuyinz <liuyinz95@gmail.com>
;; Maintainer: liuyinz <liuyinz95@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords:
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

;; Show buffer status in echo area instead of modeline.

;;; Code:

(require 'mini-echo-segments)

(defgroup mini-echo nil
  "docstring"
  :group 'mini-echo)

(defcustom mini-echo-full-segments
  '("macro-record" "selection-info" "process" "flymake" "buffer-size"
    "buffer-position" "major-mode" "meow")
  ""
  :type '(repeat string)
  :group 'mini-echo)

(defcustom mini-echo-short-segments
  '("macro-record" "selection-info" "process" "flymake" "buffer-position" "meow")
  ""
  :type '(repeat string)
  :group 'mini-echo)

(defvar mini-echo--old-mdf nil)
(defvar mini-echo-overlays nil)

(defun mini-echo-show-divider (&optional hide)
  "docstring"
  ;; TODO recover values if disable the mode
  (setq window-divider-default-places t)
  (setq window-divider-default-bottom-width 1)
  (setq window-divider-default-right-width 1)
  (if (null hide)
      (window-divider-mode 1)
    (window-divider-mode -1)))

(defun mini-echo-hide-modeline (&optional show)
  "docstring"
  (if (null show)
      (progn
        (setq mini-echo--old-mdf mode-line-format)
        (setq-default mode-line-format nil))
    (setq-default mode-line-format mini-echo--old-mdf)
    (setq-default mini-echo--old-mdf nil))
  (when (called-interactively-p 'any)
    (redraw-display)))

(defun mini-echo-init-overlays (&optional deinit)
  "docstring"
  (if (null deinit)
      (dolist (buf '(" *Echo Area 0*" " *Echo Area 1*"))
        (with-current-buffer (get-buffer-create buf)
          (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
            (push ov mini-echo-overlays))))
    (mapc #'delete-overlay mini-echo-overlays)
    (setq mini-echo-overlays nil)))

(defun mini-echo-set-text (text)
  "docstring"
  (unless (active-minibuffer-window)
    (let* ((width (+ 2 (string-width text)))
           (info (concat (propertize "  " 'cursor 1 'display
                                     `(space :align-to (- right-fringe ,width)))
                         text)))
      (dolist (ov mini-echo-overlays)
        (overlay-put ov 'after-string info))

      ;; Display the text in Minibuf-0
      (with-current-buffer " *Minibuf-0*"
        (delete-region (point-min) (point-max))
        (insert info)))))

(defun mini-echo-get-frame-width ()
  "docstring"
  (with-selected-frame (window-frame (minibuffer-window))
    (- (frame-width) left-margin-width right-margin-width)))

(defun mini-echo-get-segment-string (segment)
  "Return string of SEGMENT info."
  (or (funcall (cdr (assoc segment mini-echo-segment-alist))) ""))

(defun mini-echo-build-info ()
  "docstring"
  (condition-case nil
      (mapconcat 'identity (seq-remove #'string-empty-p
                                       (mapcar #'mini-echo-get-segment-string
                                               (if (> (mini-echo-get-frame-width) 120)
                                                   mini-echo-full-segments
                                                 mini-echo-short-segments)))
                 " ")
    (format "error happends")))

(defun mini-echo-update ()
  "docstring"
  (let* ((active-info (mini-echo-build-info))
         (echo-message (substring-no-properties (or (current-message) "")))
         (last-line (car (last (split-string echo-message "\n"))))
         (blank-width (- (mini-echo-get-frame-width)
                         (string-width active-info)
                         (string-width last-line))))
    (mini-echo-set-text (if (> blank-width 0) active-info ""))))

;;;###autoload
(define-minor-mode mini-echo-mode
  "Minor mode to show buffer status in echo area."
  :group 'mini-echo
  :global t
  :init-value nil
  (if mini-echo-mode
      (progn
        (mini-echo-show-divider)
        (mini-echo-hide-modeline)
        (mini-echo-init-overlays)
        ;; added overlays when switch to a new frame
        ;; (add-function :after after-focus-change-function
        ;;               #'mini-echo-update)
        (run-with-timer 0 0.2 #'mini-echo-update))
    (mini-echo-show-divider 'hide)
    (mini-echo-hide-modeline 'show)
    (mini-echo-init-overlays 'deinit)
    ;; (remove-function after-focus-change-function #'mini-echo-update)
    (cancel-function-timers #'mini-echo-update)))

(provide 'mini-echo)
;;; mini-echo.el ends here
