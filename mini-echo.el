;;; mini-echo.el --- Echo buffer status in minibuffer window -*- lexical-binding: t -*-

;; Copyright (C) 2023 liuyinz

;; Author: liuyinz <liuyinz95@gmail.com>
;; Maintainer: liuyinz <liuyinz95@gmail.com>
;; Version: 0.7.1
;; Package-Requires: ((emacs "29.1"))
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
  '(:long ("major-mode" "buffer-name" "vcs" "buffer-position"
           "buffer-size" "flymake" "process" "selection-info"
           "narrow" "macro" "profiler" "repeat")
    :short ("buffer-name-short" "buffer-position" "process"
            "profiler" "selection-info" "narrow" "macro" "repeat"))
  "Plist of segments which are default to all major modes."
  :type '(plist :key-type symbol
                :options '(:long :short)
                :value-type (repeat strings))
  :group 'mini-echo)

(defcustom mini-echo-rules
  '((special-mode :both (("buffer-size" . 0)))
    (dired-mode :both (("buffer-size" . 0)))
    (xwidget-webkit-mode :both (("buffer-size" . 0) ("buffer-position" . 0))))
  "List of rules which are only take effect in some major mode.
The format is like:
 (MAJOR-MODE :both  ((SEGMENT . POSITION) ...))
             :long  ((SEGMENT . POSITION) ...))
             :short ((SEGMENT . POSITION) ...)).
:both would setup for both long and short style, :long and :short have higher
priority over :both.
If Emacs version >= 30, write rule for a parent mode will take effect in every
children modes.  Otherwise, write rule for every specific major mode instead."
  :type '(alist :key-type symbol
                :value-type (plist :key-type symbol
                                   :options '(:both :long :short)
                                   :value-type (alist :key-type string
                                                      :value-type integer)))
  :package-version '(mini-echo . "0.6.3")
  :group 'mini-echo)

(defcustom mini-echo-short-style-predicate
  #'mini-echo-minibuffer-width-lessp
  "Predicate to select short style segments."
  :type '(choice
          (const :tag "" mini-echo-minibuffer-width-lessp)
          function)
  :package-version '(mini-echo . "0.5.1")
  :group 'mini-echo)

(defcustom mini-echo-separator " "
  "String separator for mini echo segments info."
  :type 'string
  :package-version '(mini-echo . "0.5.0")
  :group 'mini-echo)

(defcustom mini-echo-ellipsis ".."
  "String used to abbreviate text in segments info."
  :type 'string
  :package-version '(mini-echo . "0.5.2")
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

(defface mini-echo-minibuffer-window
  '((t :inherit default))
  "Face used to highlight the minibuffer window.")

(defconst mini-echo-managed-buffers
  '(" *Echo Area 0*" " *Echo Area 1*" " *Minibuf-0*"))

(defvar mini-echo-overlays nil)

(defvar mini-echo--orig-colors nil)
(defvar-local mini-echo--orig-mdf nil)
(defvar-local mini-echo--remap-cookie nil)

(defvar mini-echo--valid-segments nil)
(defvar mini-echo--default-segments nil)
(defvar mini-echo--toggled-segments nil)
(defvar mini-echo--rules nil)
(defvar mini-echo--info-last-build nil)

;;; segments

(defun mini-echo-segment-valid-p (segment)
  "Return non-nil if SEGMENT is valid."
  (member segment mini-echo--valid-segments))

(defun mini-echo-merge-segments (orig extra)
  "Merge EXTRA segments into ORIG list."
  (let* ((extra (seq-filter (lambda (x)
                              (mini-echo-segment-valid-p (car x)))
                            extra))
         (orig-uniq (seq-remove (lambda (x)
                                  (member x (mapcar #'car extra)))
                                orig))
         (extra-active (seq-remove (lambda (x) (= (cdr x) 0)) extra))
         (index 1)
         result)
    ;; TODO use length sum as boundary
    (while (consp extra-active)
      (if-let ((match (rassoc index extra-active)))
          (progn
            (push (car match) result)
            (setq extra-active (delete match extra-active)))
        (and-let* ((head (pop orig-uniq))) (push head result)))
      (setq index (1+ index)))
    (append (reverse result) orig-uniq)))

(defun mini-echo-ensure-segments ()
  "Ensure all predefined segments variable ready for mini echo."
  (setq mini-echo--valid-segments (mapcar #'car mini-echo-segment-alist))
  (cl-destructuring-bind (&key long short)
      mini-echo-default-segments
    (setq mini-echo--default-segments
          (list :long (seq-filter #'mini-echo-segment-valid-p long)
                :short (seq-filter #'mini-echo-segment-valid-p short))))
  (setq mini-echo--rules
        (cl-loop for rule in mini-echo-rules
                 collect
                 (cl-destructuring-bind (mode &key both long short)
                     rule
                   (list mode :long (mini-echo-merge-segments
                                     (plist-get mini-echo--default-segments
                                                :long)
                                     (cl-remove-duplicates
                                      (append both long)
                                      :key #'car :test #'equal))
                              :short (mini-echo-merge-segments
                                      (plist-get mini-echo--default-segments
                                                 :short)
                                      (cl-remove-duplicates
                                       (append both short)
                                       :key #'car :test #'equal)))))))

(defun mini-echo-get-segments (style)
  "Return list of segments according to STYLE."
  (cl-case style
    (valid mini-echo--valid-segments)
    (selected (plist-get
               ;; parent mode rules take effect in children modes if possible
               (or (and (fboundp #'derived-mode-all-parents)
                        (car (seq-keep
                              (lambda (x) (alist-get x mini-echo--rules))
                              (derived-mode-all-parents major-mode))))
                   (alist-get major-mode mini-echo--rules)
                   mini-echo--default-segments)
               (if (funcall mini-echo-short-style-predicate)
                   :short :long)))
    (current
     (let ((result (mini-echo-get-segments 'selected))
           extra)
       (dolist (filter mini-echo--toggled-segments)
         (cl-destructuring-bind (segment . enable)
             filter
           (if enable
               (unless (member segment result)
                 (push segment extra))
             (setq result (remove segment result)))))
       (append result extra)))
    (no-current (seq-difference (mini-echo-get-segments 'valid)
                                (mini-echo-get-segments 'current)))
    (toggle (cl-remove-duplicates
             (append (mapcar #'car mini-echo--toggled-segments)
                     (mini-echo-get-segments 'current)
                     (mini-echo-get-segments 'no-current))
             :test #'equal
             :from-end t))))

(defun mini-echo-concat-segments ()
  "Return concatenated information of selected segments."
  (let ((current-segments (mini-echo-get-segments 'current))
        result)
    (dolist (segment current-segments)
      (let* ((struct (alist-get segment mini-echo-segment-alist
                                nil nil #'equal))
             (status (mini-echo-segment-activate struct))
             (setup (mini-echo-segment-setup struct))
             (fetch (mini-echo-segment-fetch struct))
             (update (mini-echo-segment-update struct)))
        (unless status
          (setf (mini-echo-segment-activate struct) t)
          (and (functionp setup) (funcall setup))
          (and (functionp update) (funcall update)))
        (and-let* ((info (funcall fetch))) (push info result))))
    (mapconcat #'identity (seq-remove #'string-empty-p result)
               mini-echo-separator)))

(defun mini-echo--toggle-completion ()
  "Return completion table for command mini echo toggle."
  (lambda (string pred action)
    (if (eq action 'metadata)
        `(metadata (display-sort-function . ,#'identity))
      (complete-with-action
       action
       (let ((current (mini-echo-get-segments 'current)))
         (mapcar (lambda (s)
                   (propertize s 'face (if (member s current) 'success 'error)))
                 (mini-echo-get-segments 'toggle)))
       string pred))))

;;; ui

(defun mini-echo-show-divider (&optional hide)
  "Show window divider when enable mini echo.
If optional arg HIDE is non-nil, disable the mode instead."
  (if hide
      (window-divider-mode -1)
    (cl-destructuring-bind (window-divider-default-places
                            window-divider-default-right-width
                            window-divider-default-bottom-width)
        mini-echo-window-divider-args
      (window-divider-mode 1))))

(defun mini-echo-hide-modeline (&optional show)
  "Hide mode-line in mini echo.
If optional arg SHOW is non-nil, show the mode-line instead."
  (if show
      ;; FIXME new buffer created under mini-echo-mode has mode line face
      ;; bug after disable mini-echo-mode
      (let ((orig-value (get 'mode-line-format 'standard-value)))
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (setq mode-line-format (or mini-echo--orig-mdf orig-value))
            (setq mini-echo--orig-mdf nil)))
        (setq-default mode-line-format orig-value))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (setq mini-echo--orig-mdf mode-line-format)
        (setq mode-line-format nil)))
    (setq-default mode-line-format nil))
  (when (called-interactively-p 'any)
    (redraw-display)))

(defun mini-echo-fontify-minibuffer-window ()
  "Fontify whole window with user defined face attributes."
  (face-remap-add-relative 'default 'mini-echo-minibuffer-window))

(defun mini-echo-init-echo-area (&optional deinit)
  "Initialize echo area and minibuffer in mini echo.
If optional arg DEINIT is non-nil, remove all overlays."
  ;; delete old overlays by default
  (mapc #'delete-overlay mini-echo-overlays)
  (setq mini-echo-overlays nil)
  (if deinit
      (progn
        (dolist (buf mini-echo-managed-buffers)
          (with-current-buffer (get-buffer-create buf)
            (when (minibufferp) (delete-minibuffer-contents))
            (face-remap-remove-relative mini-echo--remap-cookie)
            (setq-local mini-echo--remap-cookie nil)))
        (remove-hook 'minibuffer-inactive-mode-hook
                     #'mini-echo-fontify-minibuffer-window))
    (dolist (buf mini-echo-managed-buffers)
      (with-current-buffer (get-buffer-create buf)
        (and (minibufferp) (= (buffer-size) 0) (insert " "))
        (push (make-overlay (point-min) (point-max) nil nil t)
              mini-echo-overlays)
        (setq-local mini-echo--remap-cookie
                    (mini-echo-fontify-minibuffer-window))))
    ;; NOTE every time activating minibuffer would reset face,
    ;; so re-fontify when entering inactive-minibuffer-mode
    (add-hook 'minibuffer-inactive-mode-hook
              #'mini-echo-fontify-minibuffer-window)))

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
      (if-let* ((win (get-buffer-window))
                ((window-live-p win)))
          (let* ((combined (mini-echo-concat-segments))
                 (padding (+ mini-echo-right-padding (string-width combined)))
                 (prop `(space :align-to (- right-fringe ,padding))))
            (setq mini-echo--info-last-build
                  (concat (propertize " " 'cursor 1 'display prop) combined)))
        mini-echo--info-last-build)
    (format "mini-echo info building error")))

(defun mini-echo-update-overlays (&optional msg)
  "Update mini echo info in overlays according to MSG.
If MSG is nil, then use `current-message' instead."
  (when-let* (((not (active-minibuffer-window)))
              (msg (or msg (current-message) ""))
              (info (mini-echo-build-info)))
    (dolist (ov mini-echo-overlays)
      (overlay-put ov 'after-string
                   (if (or (equal (buffer-name (overlay-buffer ov))
                                  " *Minibuf-0*")
                           (> (- (mini-echo-minibuffer-width)
                                 (string-width info)
                                 (string-width msg))
                              0))
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
  (if (bound-and-true-p mini-echo-mode)
      (if reset
          (progn
            (setq mini-echo--toggled-segments nil)
            (message "Mini-echo-toggle: reset."))
        (when-let ((segment (completing-read
                             "Mini-echo toggle: "
                             (mini-echo--toggle-completion) nil t)))
          (setf (alist-get segment mini-echo--toggled-segments
                           nil nil #'equal)
                (if (member segment (mini-echo-get-segments 'current)) nil t))))
    (user-error "Please enable mini-echo-mode first")))

;;;###autoload
(define-minor-mode mini-echo-mode
  "Minor mode to show buffer status in echo area."
  :group 'mini-echo
  :global t
  (if mini-echo-mode
      (progn
        (mini-echo-ensure-segments)
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
