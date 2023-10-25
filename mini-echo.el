;;; mini-echo.el --- Echo buffer status in minibuffer window -*- lexical-binding: t -*-

;; Copyright (C) 2023 liuyinz

;; Author: liuyinz <liuyinz95@gmail.com>
;; Maintainer: liuyinz <liuyinz95@gmail.com>
;; Version: 0.3.2
;; Package-Requires: ((emacs "28.1"))
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

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'face-remap)

(require 'mini-echo-segments)

(defgroup mini-echo nil
  "Echo buffer status in minibuffer window."
  :group 'mini-echo)

(defcustom mini-echo-default-segments
  '("profiler" "macro" "narrow" "selection-info" "process" "flymake"
    "buffer-size" "buffer-position" "vcs" "buffer-name" "major-mode")
  "Segments displayed in mini-echo by default."
  :type '(repeat string)
  :group 'mini-echo)

(defcustom mini-echo-short-segments
  '("profiler" "macro" "narrow" "selection-info" "process"
    "buffer-position" "buffer-name-short")
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

(defcustom mini-echo-update-interval 0.3
  "Seconds between update mini echo segments."
  :type 'number
  :group 'mini-echo)

(defcustom mini-echo-window-divider-args '(t 1 1)
  "List of arguments to initialize command `window-divider-mode'.
Format is a list of three argument:
  (`window-divider-default-places'
   `window-divider-default-right-width'
   `window-divider-default-bottom-width')."
  :type '(symbol number number)
  :group 'mini-echo)

(defface mini-echo-window-divider
  '((t (:foreground "#5d6a76")))
  "Face used to highlight the window divider.")

(defface mini-echo-minibuffer-window
  '((t (:background "#181f25")))
  "Face used to highlight the minibuffer window.")

(defconst mini-echo-area-buffers
  '(" *Echo Area 0*" " *Echo Area 1*" " *Minibuf-0*"))

(defvar mini-echo-toggle-segments nil)
(defvar mini-echo-overlays nil)

(defvar mini-echo--orig-colors nil)
(defvar-local mini-echo--orig-mdf nil)
(defvar-local mini-echo--remap-cookie nil)

;;; segments

(defun mini-echo-segment-valid-p (segment)
  "Return non-nil if SEGMENT is valid."
  (member segment (mapcar #'car mini-echo-segment-alist)))

;; TODO improve performace for recursion calling
(defun mini-echo-segments (style)
  "Return list of segments according to STYLE."
  (let* ((valid (mapcar #'car mini-echo-segment-alist))
         (default (seq-filter #'mini-echo-segment-valid-p
                              mini-echo-default-segments))
         (short (seq-filter #'mini-echo-segment-valid-p
                            mini-echo-short-segments))
         (shortp (funcall mini-echo-short-segments-predicate))
         (selected (if shortp short default))
         (unselected (if (not shortp) default short)))
    (cl-case style
      (valid valid)
      (default default)
      (short short)
      (selected selected)
      (unselected unselected)
      (current
       (let ((result selected))
         (dolist (filter mini-echo-toggle-segments result)
           (cl-destructuring-bind (segment . enable)
               filter
             (when (xor (member segment selected) enable)
               (if enable
                   (push segment result)
                 (delete segment result)))))))
      (no-current (seq-difference valid (mini-echo-segments 'current)))
      (activated (seq-union (mini-echo-segments 'current) unselected))
      (toggle (append (mini-echo-segments 'current)
                      (mini-echo-segments 'no-current))))))

(defun mini-echo-concat-segments ()
  "Return concatenated information of selected segments."
  (dolist (pair mini-echo-segment-alist)
    (cl-destructuring-bind (segment . struct)
        pair
      (let ((orig (mini-echo-segment-activate struct))
            (setup (mini-echo-segment-setup struct)))
        (when (xor orig (member segment (mini-echo-segments 'activated)))
          (setf (mini-echo-segment-activate struct) (not orig))
          (and (functionp setup)
               (funcall (mini-echo-segment-setup struct)))))))
  (cl-loop for segment in (mini-echo-segments 'current)
           when (funcall (mini-echo-segment-fetch
                          (cdr (assoc segment mini-echo-segment-alist))))
           collect it into result
           finally return
           (mapconcat 'identity (seq-remove #'string-empty-p result) " ")))

(defun mini-echo--toggle-completion ()
  "Return completion table for command mini echo toggle."
  (lambda (string pred action)
    (if (eq action 'metadata)
        `(metadata (display-sort-function . ,#'identity))
      (complete-with-action
       action
       (let ((current (mini-echo-segments 'current)))
         (mapcar (lambda (s)
                   (propertize s 'face (if (member s current) 'success 'error)))
                 (mini-echo-segments 'toggle)))
       string pred))))

;;; ui

(defun mini-echo-change-divider-color (&optional restore)
  "Change color of window divider when mini echo enable.
If optional arg RESTORE is non-nil, restore origin values."
  (dolist (face '(internal-border window-divider))
    (or restore (push (cons face (face-foreground face))
                      mini-echo--orig-colors))
    (set-face-attribute face nil
                        :foreground
                        (if restore (alist-get face mini-echo--orig-colors)
                          (face-foreground 'mini-echo-window-divider)))
    (and restore (setq mini-echo--orig-colors nil))))

(defun mini-echo-show-divider (&optional hide)
  "Show window divider when enable mini echo.
If optional arg HIDE is non-nil, disable the mode instead."
  (if (null hide)
      (cl-destructuring-bind (window-divider-default-places
                              window-divider-default-right-width
                              window-divider-default-bottom-width)
          mini-echo-window-divider-args
        (mini-echo-change-divider-color)
        (window-divider-mode 1))
    (window-divider-mode -1)
    (mini-echo-change-divider-color 'restore)))

(defun mini-echo-hide-modeline (&optional show)
  "Hide mode-line in mini echo.
If optional arg SHOW is non-nil, show the mode-line instead."
  (if (null show)
      (progn
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (setq mini-echo--orig-mdf mode-line-format)
            (setq mode-line-format nil)))
        (setq-default mode-line-format nil))
    ;; FIXME new buffer under mini-echo recover has mode line face bug
    (let ((orig-value (get 'mode-line-format 'standard-value)))
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (setq mode-line-format (or mini-echo--orig-mdf orig-value))
          (setq mini-echo--orig-mdf nil)))
      (setq-default mode-line-format orig-value)))
  (when (called-interactively-p 'any)
    (redraw-display)))

(defun mini-echo-fontify-minibuffer-window ()
  "Fontify whole window with user defined face attributes."
  (face-remap-add-relative 'default 'mini-echo-minibuffer-window))

(defun mini-echo-init-echo-area (&optional deinit)
  "Initialize echo area and minibuffer in mini echo.
If optional arg DEINIT is non-nil, remove all overlays."
  (if (null deinit)
      (dolist (buf mini-echo-area-buffers)
        (with-current-buffer (get-buffer-create buf)
          (when (minibufferp)
            ;; HACK echo area and minibuf buffer must not be empty if you want
            ;; to show it in minibuffer-window persistently. minibuf-0* is
            ;; empty by default, so insert a space instead.
            (and (= (buffer-size) 0) (insert " "))
            ;; NOTE every time activating minibuffer would reset face,
            ;; so re-fontify when entering inactive-minibuffer-mode
            (add-hook 'minibuffer-inactive-mode-hook
                      #'mini-echo-fontify-minibuffer-window))
          (push (make-overlay (point-min) (point-max) nil nil t)
                mini-echo-overlays)
          (setq-local mini-echo--remap-cookie
                      (mini-echo-fontify-minibuffer-window))))
    (dolist (buf mini-echo-area-buffers)
      (with-current-buffer (get-buffer-create buf)
        (when (minibufferp)
          (delete-minibuffer-contents)
          (remove-hook 'minibuffer-inactive-mode-hook
                       #'mini-echo-fontify-minibuffer-window))
        (face-remap-remove-relative mini-echo--remap-cookie)
        (setq-local mini-echo--remap-cookie nil)))
    (mapc #'delete-overlay mini-echo-overlays)
    (setq mini-echo-overlays nil)))

(defun mini-echo-minibuffer-width ()
  "Return width of minibuffer window in current non-child frame."
  (with-selected-frame (or (frame-parent (window-frame))
                           (window-frame))
    (window-width (minibuffer-window))))

(defun mini-echo-minibuffer-width-lessp ()
  "Return non-nil if current minibuffer window width less than 120."
  (< (mini-echo-minibuffer-width) 120))

(defun mini-echo-build-info ()
  "Build mini-echo information."
  (condition-case nil
      (let* ((combined (mini-echo-concat-segments))
             (padding (+ mini-echo-right-padding (string-width combined)))
             (prop `(space :align-to (- right-fringe ,padding))))
        (concat (propertize " " 'cursor 1 'display prop) combined))
    (format "mini-echo info building error")))

(defun mini-echo-update-overlays (&optional msg)
  "Update mini echo info in overlays according to MSG.
If MSG is nil, then use `current-message' instead."
  (when-let* (((not (active-minibuffer-window)))
              (msg (or msg (current-message) ""))
              (info (mini-echo-build-info)))
    (dolist (ov mini-echo-overlays)
      (overlay-put ov 'after-string
                   (if (> (- (mini-echo-minibuffer-width)
                             (string-width info)
                             (string-width msg))
                          0)
                       info "")))))

(defun mini-echo-update-overlays-before-message (&rest args)
  "Update mini echo info before print message.
ARGS is optional."
  (mini-echo-update-overlays (and (car args) (apply #'format-message args))))

(defun mini-echo-update-overlays-when-resized (&rest _)
  "Update mini echo info after resize frame size."
  (mini-echo-update-overlays))

(defun mini-echo-update ()
  "Update mini echo info in minibuf and echo area."
  (unless (active-minibuffer-window)
    ;; update echo area overlays after-string only if it's not empty
    (dolist (ov mini-echo-overlays)
      (unless (string-empty-p (overlay-get ov 'after-string))
        (overlay-put ov 'after-string (mini-echo-build-info))))))

;;;###autoload
(defun mini-echo-toggle (&optional reset)
  "Enable or disable selected segment temporarily.
If optional arg RESET is non-nil, clear all toggled segments."
  (interactive "P")
  (if (null reset)
      (when-let ((segment (completing-read
                           "Mini-echo toggle: "
                           (mini-echo--toggle-completion) nil t)))
        (setf (alist-get segment mini-echo-toggle-segments
                         nil nil #'string-equal)
              (if (member segment (mini-echo-segments 'current)) nil t)))
    (setq mini-echo-toggle-segments nil)
    (message "Mini-echo-toggle: reset.")))

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
        (run-with-timer 0 mini-echo-update-interval #'mini-echo-update)
        (advice-add 'message :before #'mini-echo-update-overlays-before-message)
        (add-hook 'window-size-change-functions
                  #'mini-echo-update-overlays-when-resized))
    (mini-echo-show-divider 'hide)
    (mini-echo-hide-modeline 'show)
    (mini-echo-init-echo-area 'deinit)
    (cancel-function-timers #'mini-echo-update)
    (advice-remove 'message #'mini-echo-update-overlays-before-message)
    (remove-hook 'window-size-change-functions
                 #'mini-echo-update-overlays-when-resized)))

(provide 'mini-echo)
;;; mini-echo.el ends here
