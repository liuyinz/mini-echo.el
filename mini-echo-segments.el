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

;; Most segments are ported from other mode line package.
;; SEE https://github.com/seagle0128/doom-modeline/blob/master/doom-modeline-segments.el

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar meow--indicator)
(defvar evil-state)
(defvar evil-visual-beginning)
(defvar evil-visual-end)
(defvar evil-this-macro)
(defvar flymake--state)
(defvar magit-blob-mode)

(declare-function flymake--mode-line-counter "flymake")
(declare-function flymake-running-backends "flymake")
(declare-function flymake-disabled-backends "flymake")
(declare-function flymake-reporting-backends "flymake")
(declare-function projectile-project-root "projectile")
(declare-function ffip-project-root "ffip")

(defcustom mini-echo-position-format "%l:%c,%p"
  "Format used to display lin, number and percentage in mini echo."
  :type 'string
  :group 'mini-echo)

(defcustom mini-echo-buffer-status-style 'sign
  "Style used to display buffer status in mini echo."
  :type '(choice (const :tag "Change extra sign after buffer name" sign)
                 (const :tag "Change color of buffer name" color)
                 (const :tag "Change both color and sign" both))
  :group 'mini-echo)

(defcustom mini-echo-vcs-max-length 10
  "Max length limit of vcs segment string."
  :type 'number
  :group 'mini-echo)

(defcustom mini-echo-project-detection 'project
  "How to detect the project root in mini echo.
nil means to use `default-directory'.
`auto' means to detect the following options in order."
  :type '(choice (const :tag "Find File in Project" ffip)
                 (const :tag "Projectile" projectile)
                 (const :tag "Built-in Project" project)
                 function)
  :group 'mini-echo)

;; faces
(defface mini-echo-major-mode
  '((t (:inherit bold)))
  "Face for mini-echo segment of major mode."
  :group 'mini-echo)

(defface mini-echo-buffer-size
  '((t (:inherit default)))
  "Face for mini-echo segment of buffer size."
  :group 'mini-echo)

(defface mini-echo-buffer-position
  '((t (:foreground "violet")))
  "Face for mini-echo segment of buffer position."
  :group 'mini-echo)

(defface mini-echo-remote-host
  '((t (:foreground "#E27E8D")))
  "Face for mini-echo segment of remote host."
  :group 'mini-echo)

(defface mini-echo-selection-info
  '((t (:foreground "#EBBF83" :bold t)))
  "Face for mini-echo segment of selection info."
  :group 'mini-echo)

(defface mini-echo-project
  '((t (:foreground "#5EC4FF")))
  "Face for mini-echo segment of project directory."
  :group 'mini-echo)

(defface mini-echo-blob-revision
  '((t (:foreground "#E27E8D")))
  "Face for mini-echo segment of blob revision."
  :group 'mini-echo)

(defface mini-echo-macro
  '((t (:foreground "#8BD49C" :bold t)))
  "Face for mini-echo segment of macro status."
  :group 'mini-echo)

(defface mini-echo-process
  '((t (:foreground "#8BD49C" :bold t)))
  "Face for mini-echo segment of process."
  :group 'mini-echo)

(defface mini-echo-narrow
  '((t (:foreground "#8BD49C" :bold t)))
  "Face for mini-echo segment of narrow status."
  :group 'mini-echo)

(defface mini-echo-profiler
  '((t (:foreground "#8BD49C" :bold t)))
  "Face for mini-echo segment of profiler status."
  :group 'mini-echo)

(defvar mini-echo-segment-alist nil)

;; TODO refactor :var :func :hook :toggle to separate update frequency and hooks
(defmacro mini-echo-define-segment (name &rest body)
  "Define a mini-echo segment NAME with DOCSTRING and BODY."
  (declare (indent defun) (doc-string 2))
  (let ((sym (intern (format "mini-echo-segment--%s" name)))
        (docstring (if (stringp (car body))
                       (pop body)
                     (format "Display %s in mini-echo" name))))
    (cond ((and (functionp (car body))
                (not (cdr body)))
           `(add-to-list 'mini-echo-segment-alist
                         (cons ,name ',(car body))))
          (t
           `(progn
              (defun ,sym () ,docstring ,@body)
              (add-to-list 'mini-echo-segment-alist (cons ,name ',sym)))))))

(mini-echo-define-segment "major-mode"
  "Display the major mode in mini-echo."
  (when-let ((mode (format-mode-line mode-name)))
    (propertize mode 'face 'mini-echo-major-mode)))

(mini-echo-define-segment "buffer-position"
  "Display the cursor position of current buffer."
  (when-let ((pos (format-mode-line mini-echo-position-format)))
    (propertize (string-replace "Bottom" "Bot" pos)
                'face 'mini-echo-buffer-position)))

(mini-echo-define-segment "buffer-size"
  "Display the size of current buffer."
  (when-let ((size (format-mode-line "%I")))
    (propertize size 'face 'mini-echo-buffer-size)))

(mini-echo-define-segment "remote-host"
  "Display the hostname of remote buffer."
  (when default-directory
    (when-let ((host (file-remote-p default-directory 'host)))
      (propertize (concat "@" host) 'face 'mini-echo-remote-host))))

(mini-echo-define-segment "process"
  "Display process info."
  (when-let ((str (format-mode-line mode-line-process))
             ((not (string-empty-p str))))
    (concat ">>" (propertize str 'face 'mini-echo-process))))

(mini-echo-define-segment "profiler"
  "Display profiler status"
  (when (or (profiler-cpu-running-p)
            (profiler-memory-running-p))
    (propertize "Profiler" 'face 'mini-echo-profiler)))

(defun mini-echo-buffer-status ()
  "Display th status of current buffer."
  (cond
   ((bound-and-true-p magit-blob-mode) (cons "" nil))
   (buffer-read-only (cons "%" 'warning))
   ((and buffer-file-name (buffer-modified-p))
    (cons "*" 'success))
   ((and buffer-file-name
         (not (file-remote-p buffer-file-name))
         (not (file-exists-p buffer-file-name)))
    (cons "!" 'error))
   (t (cons " " nil))))

(defvar-local mini-echo-project-root nil)
(defun mini-echo-project-root ()
  "Get the path to the project root.
Return nil if no project was found."
  (or mini-echo-project-root
      (setq mini-echo-project-root
            (or (and (buffer-file-name)
                     (cl-case mini-echo-project-detection
                       (ffip (and (fboundp 'ffip-project-root)
                                  (let ((inhibit-message t))
                                    (ffip-project-root))))
                       (projectile (and (bound-and-true-p projectile-mode)
                                        (projectile-project-root)))
                       (project (when-let (((fboundp 'project-current))
                                           (project (project-current)))
                                  (expand-file-name
                                   (if (fboundp 'project-root)
                                       (project-root project)
                                     (car (with-no-warnings
                                            (project-roots project)))))))
                       (t (funcall mini-echo-project-detection))))
                ""))))

(defun mini-echo-buffer-name ()
  "Return current buffer name for mini echo."
  (cond
   (;; TODO need timemachine support
    (or (bound-and-true-p magit-blob-mode))
    (save-match-data
      (let ((str (buffer-name)))
        (when (string-match "\\(.+\\)\\.~\\(.+\\)~" str)
          (concat (file-name-nondirectory (match-string 1 str))
                  (propertize (concat "@" (substring (match-string 2 str) 0 7))
                              'face 'mini-echo-blob-revision))))))
   (t (let ((name (buffer-name)))
        (cl-destructuring-bind (sign . face)
            (mini-echo-buffer-status)
          (cl-case mini-echo-buffer-status-style
            (sign (concat name (propertize sign 'face face)))
            (color (propertize name 'face face))
            (both (propertize (concat name sign) 'face face))))))))

(mini-echo-define-segment "buffer-name"
  "Display file path of current buffer."
  (concat
   (if-let* ((filepath (buffer-file-name))
             (project (mini-echo-project-root))
             ((not (string-empty-p project)))
             ((string-prefix-p project filepath))
             (parts (split-string (string-trim filepath project) "/")))
       (mapconcat #'identity
                  `(,(propertize
                      (file-name-nondirectory (directory-file-name project))
                      'face 'mini-echo-project)
                    ,@(mapcar (lambda (x) (substring x 0 1)) (butlast parts))
                    nil)
                  "/"))
   (mini-echo-buffer-name)))

(mini-echo-define-segment "buffer-name-short"
  "Display file path of current buffer."
  (mini-echo-buffer-name))

(mini-echo-define-segment "macro"
  "Display macro being recorded."
  (when (or defining-kbd-macro executing-kbd-macro)
    (let ((status (if (bound-and-true-p evil-this-macro)
                      (format "@%s" (char-to-string evil-this-macro))
                    "MACRO")))
      (propertize status 'face 'mini-echo-macro))))

(mini-echo-define-segment "narrow"
  "Display narrow status of current buffer."
  (when (or (buffer-narrowed-p)
            (bound-and-true-p dired-narrow-mode))
    (propertize "NARROW" 'face 'mini-echo-narrow)))

(mini-echo-define-segment "flymake"
  "Display flymake diagnostics of current buffer."
  (when (bound-and-true-p flymake-mode)
    (let* ((no-known (zerop (hash-table-count flymake--state)))
           (running (flymake-running-backends))
           (disabled (flymake-disabled-backends))
           (reported (flymake-reporting-backends))
           (all-disabled (and disabled (null running)))
           (some-waiting (cl-set-difference running reported)))
      (concat (cond
               (no-known (propertize "?" 'face 'error))
               (some-waiting (propertize "*" 'face 'warning))
               (all-disabled (propertize "!" 'face 'error))
               (t  (propertize "-" 'face 'success)))
              (mapconcat
               (lambda (s)
                 (let ((counter (cadr (flymake--mode-line-counter s t))))
                   (propertize (or (plist-get counter :propertize) "0")
                               'face (or (plist-get counter 'face)
                                         'compilation-info))))
               '(:error :warning :note) "/")))))

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

(defvar-local mini-echo--vcs-status nil)
(defun mini-echo-update-vcs-status (&rest _)
  "Update vcs segment status."
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
                                        (concat (substring branch 0 (- limit 3)) "..")
                                      branch))
                        'face `(:inherit (,face bold)))))))

(add-hook 'find-file-hook #'mini-echo-update-vcs-status)
(add-hook 'after-save-hook #'mini-echo-update-vcs-status)
(advice-add #'vc-refresh-state :after #'mini-echo-update-vcs-status)

(mini-echo-define-segment "vcs"
  "Display current branch."
  (buffer-local-value 'mini-echo--vcs-status (current-buffer)))

(mini-echo-define-segment "meow"
  "Display the meow status of current buffer."
  (when (bound-and-true-p meow--indicator)
    (string-trim meow--indicator)))

;; TODO add more segments
;; (mini-echo-define-segment "evil")

;; (mini-echo-define-segment "keycast"
;;   "Display keycast info."
;;   (when (bound-and-true-p keycast-mode-line-format)
;;     (keycast--format keycast-mode-line-format)))

;; (mini-echo-define-segment "interaction-log"
;;   (when (bound-and-true-p interaction-log-mode)))

(provide 'mini-echo-segments)
;;; mini-echo-segments.el ends here
