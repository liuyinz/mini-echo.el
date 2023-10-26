;;; mini-echo-segments.el --- Collection of mini echo segments -*- lexical-binding: t -*-

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
(defvar flymake-suppress-zero-counters)
(defvar magit-blob-mode)
(defvar display-time-string)

(declare-function flymake--mode-line-counter "flymake")
(declare-function flymake-running-backends "flymake")
(declare-function flymake-disabled-backends "flymake")
(declare-function flymake-reporting-backends "flymake")
(declare-function projectile-project-root "projectile")
(declare-function ffip-project-root "ffip")
(declare-function keycast--format "keycast")
(declare-function keycast--update "keycast")
(declare-function evil-emacs-state-p "ext:evil-states" t t)
(declare-function evil-insert-state-p "ext:evil-states" t t)
(declare-function evil-motion-state-p "ext:evil-states" t t)
(declare-function evil-normal-state-p "ext:evil-states" t t)
(declare-function evil-operator-state-p "ext:evil-states" t t)
(declare-function evil-replace-state-p "ext:evil-states" t t)
(declare-function evil-visual-state-p "ext:evil-states" t t)
(declare-function evil-state-property "ext:evil-common")

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

(defcustom mini-echo-keycast-format "%10s%k%c%r"
  "The format spec used by keycast segment in mini echo."
  :type 'string
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

(defface mini-echo-time
  '((t (:foreground "#EBBF83")))
  "Face for mini-echo segment of time."
  :group 'mini-echo)

(defface mini-echo-evil-normal-state
  '((t (:foreground "#5EC4FF")))
  "Face for mini-echo segment of evil normal state."
  :group 'mini-echo)

(defface mini-echo-evil-insert-state
  '((t (:foreground "#8BD49C")))
  "Face for mini-echo segment of evil insert state."
  :group 'mini-echo)

(defface mini-echo-evil-visual-state
  '((t (:foreground "#EBBF83")))
  "Face for mini-echo segment of evil visual state."
  :group 'mini-echo)

(defface mini-echo-evil-emacs-state
  '((t (:foreground "violet")))
  "Face for mini-echo segment of evil Emacs state."
  :group 'mini-echo)

(defface mini-echo-evil-motion-state
  '((t (:foreground "#a0b3c5")))
  "Face for mini-echo segment of evil motion state."
  :group 'mini-echo)

(defface mini-echo-evil-operator-state
  '((t (:foreground "gold")))
  "Face for mini-echo segment of evil operator state."
  :group 'mini-echo)

(defface mini-echo-evil-replace-state
  '((t (:foreground "#E27E8D")))
  "Face for mini-echo segment of evil replace state."
  :group 'mini-echo)

(defvar mini-echo-segment-alist nil)

(cl-defstruct mini-echo-segment
  name &key fetch activate update hook advice setup)

;;;###autoload
(defmacro mini-echo-define-segment (name docstring &rest props)
  "Define a mini echo segment NAME with DOCSTRING and PROPS."
  (declare (indent defun) (doc-string 2))
  ;; plistp check
  (if-let* ((len (proper-list-p props))
            ((and (> len 0) (zerop (% len 2)))))
      (cl-destructuring-bind (&key fetch update hook advice mode setup)
          props
        (cl-destructuring-bind (fetch-func update-func setup-func)
            (mapcar (lambda (prop)
                      (intern (concat "mini-echo-segment-"
                                      (format "%s-%s" prop name))))
                    '("-fetch" "-update" "-setup"))
          `(progn
             (let ((segment (make-mini-echo-segment :name ,name)))
               (setf (alist-get ,name mini-echo-segment-alist nil nil #'equal)
                     segment)
               ;; fetch
               (defun ,fetch-func () ,docstring ,fetch)
               (setf (mini-echo-segment-fetch segment) ',fetch-func)
               ;; update
               (when (consp ',update)
                 (defun ,update-func () ,update)
                 (setf (mini-echo-segment-update segment) ',update-func)
                 (setf (mini-echo-segment-hook segment) ,hook)
                 (setf (mini-echo-segment-advice segment) ,advice))
               ;; setup
               (and (or ,mode ,hook ,advice ,setup)
                    (defun ,setup-func ()
                      (if (mini-echo-segment-activate segment)
                          (progn
                            (eval (plist-get ,setup :activate))
                            (mapc (lambda (x) (funcall x 1)) ,mode)
                            (mapc (lambda (x) (add-hook x ',update-func)) ,hook)
                            (mapc (lambda (x)
                                    (advice-add (car x) (cdr x) ',update-func))
                                  ,advice))
                        (eval (plist-get ,setup :deactivate))
                        ;; NOTE do not turn off modes even if deactivate segment
                        ;; to avoid interface other functionality
                        ;; (mapc (lambda (x) (funcall x -1)) ,mode)
                        (mapc (lambda (x) (remove-hook x ',update-func)) ,hook)
                        (mapc (lambda (x) (advice-remove (car x) ',update-func))
                              ,advice)))
                    (setf (mini-echo-segment-setup segment) ',setup-func))
               segment))))
    (message "mini-echo-define-segment: %s properties error" name)))

(defun mini-echo-segment-prop (segment prop)
  "Return PROP of SEGMENT."
  (funcall (intern (concat "mini-echo-segment-" (substring (symbol-name prop) 1)))
           (cdr (assoc segment mini-echo-segment-alist))))

;;; built-in

(mini-echo-define-segment "major-mode"
  "Return major mode info of current buffer."
  :fetch
  (when-let ((mode (format-mode-line mode-name)))
    (propertize mode 'face 'mini-echo-major-mode)))

(mini-echo-define-segment "buffer-position"
  "Return the cursor position of current buffer."
  :fetch
  (when-let ((pos (format-mode-line mini-echo-position-format)))
    (propertize (string-replace "Bottom" "Bot" pos)
                'face 'mini-echo-buffer-position)))

(mini-echo-define-segment "buffer-size"
  "Return the size of current buffer."
  :fetch
  (when-let ((size (format-mode-line "%I")))
    (propertize size 'face 'mini-echo-buffer-size)))

(defvar-local mini-echo--project-root nil)
(defun mini-echo-update-project-root ()
  "Update and return current project root path if exists."
  (setq mini-echo--project-root
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
            "")))

(mini-echo-define-segment "project"
  "Display the project name of current buffer."
  :advice '((vc-refresh-state . :after))
  :fetch
  (when-let ((project (or mini-echo--project-root
                          (mini-echo-update-project-root))))
    (propertize (file-name-nondirectory (directory-file-name project))
                'face 'mini-echo-project))
  :update (mini-echo-update-project-root))

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

(defun mini-echo-buffer-name-short ()
  "Return current buffer name."
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
  "Return file path of current buffer."
  :advice '((vc-refresh-state . :after))
  :fetch
  (concat
   (if-let* ((filepath (buffer-file-name))
             (project (or mini-echo--project-root
                          (mini-echo-update-project-root)))
             ((not (string-empty-p project)))
             ((string-prefix-p project filepath))
             (parts (split-string (string-trim filepath project) "/")))
       (mapconcat #'identity
                  `(,(propertize (file-name-nondirectory
                                  (directory-file-name project))
                                 'face 'mini-echo-project)
                    ,@(mapcar (lambda (x) (substring x 0 1)) (butlast parts))
                    nil)
                  "/"))
   (mini-echo-buffer-name-short))
  :update (mini-echo-update-project-root))

(mini-echo-define-segment "buffer-name-short"
  "Return name of current buffer."
  :fetch (mini-echo-buffer-name-short))

(mini-echo-define-segment "remote-host"
  "Return the hostname of remote buffer."
  :fetch
  (when default-directory
    (when-let ((host (file-remote-p default-directory 'host)))
      (propertize (concat "@" host) 'face 'mini-echo-remote-host))))

(mini-echo-define-segment "process"
  "Return current process info."
  :fetch
  (when-let ((str (format-mode-line mode-line-process))
             ((not (string-empty-p str))))
    (concat ">>" (propertize str 'face 'mini-echo-process))))

(mini-echo-define-segment "time"
  "Return current time."
  :mode '(display-time-mode)
  :fetch
  (propertize display-time-string 'face 'mini-echo-time))

(mini-echo-define-segment "profiler"
  "Return current profiler status"
  :fetch
  (when (or (profiler-cpu-running-p)
            (profiler-memory-running-p))
    (propertize "Profiler" 'face 'mini-echo-profiler)))

(mini-echo-define-segment "macro"
  "Indicator of macro being recorded or executed."
  :fetch
  (when (or defining-kbd-macro executing-kbd-macro)
    (let ((status (if (bound-and-true-p evil-this-macro)
                      (format "@%s" (char-to-string evil-this-macro))
                    "MACRO")))
      (propertize status 'face 'mini-echo-macro))))

(mini-echo-define-segment "narrow"
  "Indicator of narrow status of current buffer."
  :fetch
  (when (or (buffer-narrowed-p)
            (bound-and-true-p dired-narrow-mode))
    (propertize "NARROW" 'face 'mini-echo-narrow)))

(mini-echo-define-segment "flymake"
  "Return flymake diagnostics of current buffer."
  :fetch
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
                 (let* ((flymake-suppress-zero-counters nil)
                        (counter (cadr (flymake--mode-line-counter s))))
                   (propertize (plist-get counter :propertize)
                               'face (plist-get counter 'face))))
               '(:error :warning :note) "/")))))

(defsubst mini-echo-column (pos)
  "Get the column of the position `POS'."
  (save-excursion (goto-char pos)
                  (current-column)))

(mini-echo-define-segment "selection-info"
  "Return current selection in current buffer."
  :fetch
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

;;; third-party

(mini-echo-define-segment "meow"
  "Return the meow status of current buffer."
  :fetch
  (when (bound-and-true-p meow--indicator)
    (string-trim meow--indicator)))

(mini-echo-define-segment "keycast"
  "Display keycast info."
  :hook '(post-command-hook)
  :fetch
  (keycast--format mini-echo-keycast-format)
  :update
  (keycast--update)
  :setup '(:activate (require 'keycast)))

(mini-echo-define-segment "evil"
  "Display evil status of current buffer."
  :fetch
  (when (bound-and-true-p evil-local-mode)
    (propertize
     (let ((tag (evil-state-property evil-state :tag t)))
       (if (stringp tag) tag (funcall tag)))
     'face
     (cond
      ((evil-normal-state-p)   'mini-echo-evil-normal-state)
      ((evil-emacs-state-p)    'mini-echo-evil-emacs-state)
      ((evil-insert-state-p)   'mini-echo-evil-insert-state)
      ((evil-motion-state-p)   'mini-echo-evil-motion-state)
      ((evil-visual-state-p)   'mini-echo-evil-visual-state)
      ((evil-operator-state-p) 'mini-echo-evil-operator-state)
      ((evil-replace-state-p)  'mini-echo-evil-replace-state)
      (t 'mini-echo-evil-normal-state)))))

;; TODO add more segments

(provide 'mini-echo-segments)
;;; mini-echo-segments.el ends here.
