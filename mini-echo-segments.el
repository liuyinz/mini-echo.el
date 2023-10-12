;;; mini-echo-segments.el --- summary -*- lexical-binding: t -*-

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

;;; Code:

(require 'cl-lib)

(defvar meow--indicator)
(defvar evil-state)
(defvar evil-visual-beginning)
(defvar evil-visual-end)
(defvar evil-this-macro)

(declare-function flymake--mode-line-counter "flymake")

(defcustom mini-echo-position-format "%l:%c,%p"
  "Format used to display lin, number and percentage in mini echo."
  :type 'string
  :group 'mini-echo)

;; faces
(defface mini-echo-major-mode
  '((t (:inherit bold)))
  "Face for mini-echo segment of major mode.")

(defface mini-echo-buffer-size
  '((t (:inherit default)))
  "Face for mini-echo segment of buffer size.")

(defface mini-echo-buffer-position
  '((t (:foreground "#FF6EB4")))
  "Face for mini-echo segment of buffer position.")

(defface mini-echo-remote-host
  '((t (:foreground "#E27E8D")))
  "Face for mini-echo segment of remote host.")

(defface mini-echo-process
  '((t (:foreground "#E27E8D")))
  "Face for mini-echo segment of process.")

(defface mini-echo-selection-info
  '((t (:foreground "#EBBF83" :bold t)))
  "Face for mini-echo segment of selection info.")

(defface mini-echo-macro-record
  '((t (:foreground "#8BD49C" :bold t)))
  "Face for mini-echo segment of macro record.")

(defvar mini-echo-segment-alist nil)

(defmacro mini-echo-define-segment (name &rest body)
  "Define a mini-echo segment NAME with DOCSTRING and BODY."
  (declare (indent defun) (doc-string 2))
  (let ((sym (intern (format "mini-echo-segment--%s" name)))
        (docstring (if (stringp (car body))
                       (pop body)
                     (format "Display %s in mini-echo" name))))
    (cond ((and (symbolp (car body))
                (not (cdr body)))
           `(add-to-list 'mini-echo-segment-alist
                         (cons ,name ',(car body))))
          (t
           `(progn
              (defun ,sym () ,docstring ,@body)
              (add-to-list 'mini-echo-segment-alist (cons ,name ',sym)))))))

(mini-echo-define-segment "major-mode"
  "Display the major mode in mini-echo."
  (when-let ((str (format-mode-line mode-name)))
    (propertize str 'face 'mini-echo-major-mode)))

(mini-echo-define-segment "buffer-position"
  "Display the cursor position of current buffer."
  (when-let ((str (string-replace "Bottom" "Bot"
                                  (format-mode-line mini-echo-position-format))))
    (propertize str 'face 'mini-echo-buffer-position)))

(mini-echo-define-segment "buffer-size"
  "Display the size of current buffer."
  (when-let ((str (format-mode-line "%I")))
    (propertize str 'face 'mini-echo-buffer-size)))

(mini-echo-define-segment "remote-host"
  "Display the hostname of remote buffer."
  (when default-directory
    (when-let ((host (file-remote-p default-directory 'host)))
      (propertize (concat "@" host) 'face 'mini-echo-remote-host))))

(mini-echo-define-segment "process"
  "Display process info."
  (when-let ((str (format-mode-line mode-line-process)))
    (propertize str 'face 'mini-echo-process)))

(mini-echo-define-segment "macro-record"
  "Display macro being recorded."
  (when (or defining-kbd-macro executing-kbd-macro)
    (let ((name (if (bound-and-true-p evil-this-macro)
                          (format "@%s" (char-to-string evil-this-macro))
                        "MACRO"))
          (status (if defining-kbd-macro "<<" ">>")))
      (propertize (concat name status) 'face 'miACROho-macro-record))))

(mini-echo-define-segment "meow"
  "Display the meow status of current buffer."
  (when (bound-and-true-p meow-mode)
    (string-trim meow--indicator)))

(mini-echo-define-segment "flymake"
  "Display flymake diagnostics of current buffer."
  (when (bound-and-true-p flymake-mode)
    (mapconcat
     (lambda (s)
       (let ((counter (cadr (flymake--mode-line-counter s t))))
         (propertize (or (plist-get counter :propertize) "0")
                     'face
                     (plist-get counter 'face))))
     '(:error :warning :note) "/")))

(defsubst mini-echo-column (pos)
  "Get the column of the position `POS'."
  (save-excursion (goto-char pos)
                  (current-column)))

(mini-echo-define-segment "selection-info"
  "Display current selection in current buffer."
  (when (or mark-active (and (bound-and-true-p evil-local-mode)
                             (eq evil-state 'visual)))
    (cl-destructuring-bind (beg . end)
        (if (and (bound-and-true-p evil-local-mode) (eq evil-state 'visual))
            (cons evil-visual-beginning evil-visual-end)
          (cons (region-beginning) (region-end)))
      (let ((lines (count-lines beg (min end (point-max)))))
        (propertize
         (cond ((or (bound-and-true-p rectangle-mark-mode)
                    (and (bound-and-true-p evil-visual-selection)
                         (eq 'block evil-visual-selection)))
                (let ((cols (abs (- (mini-echo-column end)
                                    (mini-echo-column beg)))))
                  (format "%dx%dB" lines cols)))
               ((and (bound-and-true-p evil-visual-selection)
                     (eq evil-visual-selection 'line))
                (format "%dL" lines))
               ((> lines 1)
                (format "%dC,%dL" (- end beg) lines))
               (t
                (format "%dC" (- end beg))))
         'face 'mini-echo-selection-info)))))

(provide 'mini-echo-segments)
;;; mini-echo-segments.el ends here
