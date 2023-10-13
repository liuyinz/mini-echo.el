;;; mini-echo.el --- Show buffer status in echo area -*- lexical-binding: t -*-

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

;; Show buffer status in echo area instead of modeline.

;;; Code:

(require 'mini-echo-segments)

(defgroup mini-echo nil
  "Show buffer status in echo area."
  :group 'mini-echo)

(defcustom mini-echo-default-segments
  '("macro" "narrow" "selection-info" "process" "flymake" "buffer-size"
    "buffer-position" "buffer-status" "vcs" "major-mode" "meow")
  "Segments displayed in mini-echo by default."
  :type '(repeat string)
  :group 'mini-echo)

(defcustom mini-echo-short-segments
  '("macro" "narrow" "selection-info" "process" "flymake" "buffer-position"
    "buffer-status" "meow")
  "Segments displayed in mini-echo in short style."
  :type '(repeat string)
  :group 'mini-echo)

(defcustom mini-echo-short-segments-predicate
  #'mini-echo-frame-width-lessp
  "Predicate to use short style segments."
  :type '(choice
          (const :tag "" mini-echo-frame-width-lessp)
          function)
  :group 'mini-echo)

(defcustom mini-echo-right-padding 0
  "Number of characters appended after mini-echo information."
  :type 'number
  :group 'mini-echo)

(defcustom mini-echo-update-interval 0.5
  "Seconds between update mini echo segments."
  :type 'number
  :group 'mini-echo)

(defvar mini-echo--old-mdf nil)
(defvar mini-echo-overlays nil)

(defun mini-echo-show-divider (&optional hide)
  "Enable `window-divider-mode' in mini echo.
If optional arg HIDE is non-nil, disable the mode instead."
  ;; TODO recover values if disable the mode
  (setq window-divider-default-places t)
  (setq window-divider-default-bottom-width 1)
  (setq window-divider-default-right-width 1)
  (if (null hide)
      (window-divider-mode 1)
    (window-divider-mode -1)))

(defun mini-echo-hide-modeline (&optional show)
  "Hide mode-line in mini echo.
If optional arg SHOW is non-nil, show the mode-line instead."
  (if (null show)
      (progn
        (setq mini-echo--old-mdf mode-line-format)
        (setq-default mode-line-format nil))
    (setq-default mode-line-format mini-echo--old-mdf)
    (setq-default mini-echo--old-mdf nil))
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

(defun mini-echo-get-frame-width ()
  "Return current frame width for characters display."
  (with-selected-frame (or (frame-parent (window-frame))
                           (window-frame))
    (- (frame-width) left-margin-width right-margin-width)))

(defun mini-echo-get-segment-string (segment)
  "Return string of SEGMENT info."
  (or (funcall (cdr (assoc segment mini-echo-segment-alist))) ""))

(defun mini-echo-frame-width-lessp ()
  "Return non-nil if current frame width less than 120."
  (< (mini-echo-get-frame-width) 120))

(defun mini-echo-build-info ()
  "Build mini-echo information."
  (condition-case nil
      (mapconcat 'identity
                 (seq-remove #'string-empty-p
                             (mapcar #'mini-echo-get-segment-string
                                     (if (funcall mini-echo-short-segments-predicate)
                                         mini-echo-short-segments
                                       mini-echo-default-segments)))
                 " ")
    (format "mini-echo error happends")))

(defun mini-echo-update ()
  "Update mini-echo information."
  (let* ((last-message (car (last (split-string (substring-no-properties
                                                 (or (current-message) ""))
                                                "\n"))))
         (info (mini-echo-build-info))
         (info-length (+ mini-echo-right-padding (string-width info)))
         (align-info (concat (propertize " " 'cursor 1 'display
                                         `(space :align-to
                                                 (- right-fringe ,info-length)))
                             info))
         (overlay-info (if (> (- (mini-echo-get-frame-width)
                                 info-length
                                 (string-width last-message)) 0)
                           align-info "")))
    (unless (active-minibuffer-window)
      ;; Display overlays in echo area
      (dolist (ov mini-echo-overlays)
        (overlay-put ov 'after-string overlay-info))
      ;; Display the text in Minibuf-0, every time Minibuf killed and recreate
      ;; overlays failed, so only can insert text
      (with-current-buffer " *Minibuf-0*"
        (delete-region (point-min) (point-max))
        (insert align-info)))))

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
        (mini-echo-init-echo-area)
        (run-with-timer 0 mini-echo-update-interval #'mini-echo-update))
    (mini-echo-show-divider 'hide)
    (mini-echo-hide-modeline 'show)
    (mini-echo-init-echo-area 'deinit)
    (cancel-function-timers #'mini-echo-update)))

(provide 'mini-echo)
;;; mini-echo.el ends here
