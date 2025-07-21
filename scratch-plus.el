;;; scratch-plus.el --- Better Scratch Buffer Behavior  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Samuel W. Flint

;; Author: Samuel W. Flint <swflint@flintfam.org>
;; Keywords: convenience
;; Homepage: https://git.sr.ht/~swflint/scratch-plus
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1"))

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

;;; Commentary:

;; This package provides four major features: unkillable scratch
;; buffers, persistent scratch buffers, per-major-mode scratch
;; buffers, and per-project scratch buffers.  Although a number of
;; packages provide these individually, I have found them difficult to
;; get to play well together.  This package is designed to simplify
;; their use instead.
;;
;; To enable the features, the major mode `scratch-plus-mode' must be
;; enabled.  I recommend enabling it as part of your
;; `after-init-hook', as follows:
;;
;;     (add-hook 'after-init-hook #'scratch-plus-mode)
;;
;; The different features must also be configured.
;;
;; To configure persistence, the `scratch-plus-save-directory'
;; variable must be set to a directory.  It will be created as-needed.
;; To allow persistence of per-project scratch buffers, the
;; `scratch-plus-project-subdir' variable must be set to a string,
;; which will be used to compute a directory local to the project; it
;; will also be created as needed.  Scratch buffers will have
;; `save-buffer' remapped, so that they will be stored appropriately
;; for restoration.  Additionally, scratch buffers can be saved with
;; an idle timer, this is configured using `scratch-plus-idle-save',
;; which is nil by default.
;;
;; Load-time restoration of scratch buffers is configured with two
;; variables, `scratch-plus-restore-type' and
;; `scratch-plus-force-restore'.  They behave as follows.  Consider
;; the pair made from (scratch-plus-restore-type
;; . scratch-plus-force-restore).
;;
;;  - (nil . any) : no restoration is completed
;;  - (demand . initial) : the initially-created scratch buffer is
;;    killed, and only it is restored
;;  - (t . initial) : The initially-created scratch buffer is killed;
;;    all are restored.
;;  - (demand . t) : All detected scratch buffers are killed, and only
;;    the initial buffer is restored.
;;  - (t . t) : All detected scratch buffers are killed, and all are
;;    restored.
;;
;; Further configuration of note is the variable
;; `scratch-plus-initial-message', which provides the message for
;; brand new (unrestored) scratch buffers.  This can be a string,
;; which will be used directly, or a function which takes the name of
;; the major mode and returns a string.  In either case, the value
;; will be filtered through `substitute-command-keys', and then turned
;; into a mode-specific comment.
;;
;; Finally, jumping to a scratch buffer is also configured by
;; `scratch-plus-display-action', which should be a valid action for
;; `display-buffer', or nil to use `display-buffer-action-alist'.
;;
;;;; Errors and Patches
;;
;; If you find an error, or have a patch to improve this package,
;; please send an email to ~swflint/emacs-utilities@lists.sr.ht.
;;; Code:

(require 'project)
(require 'cl-lib)


;;; Configuration

(defgroup scratch-plus ()
  "Better scratch buffer behavior."
  :group 'convenience)

(defcustom scratch-plus-restore-type 'demand
  "How should scratch buffers be restored?

Default behavior is to only restore scratch buffers when they are
opened (`demand').  Other possibilities include no restoration (nil),
and restoring all scratch buffers when `scratch-plus-mode' is
enabled (t)."
  :group 'scratch-plus
  :type '(choice
          (const :tag "No restoration." nil)
          (const :tag "On-demand restoration." demand)
          (const :tag "Restore all." t)))

(defcustom scratch-plus-force-restore 'initial
  "Should existing scratch buffers be deleted on restoration?

Default behavior is to force the restoration of only the scratch buffer
for `initial-major-mode' (value of `initial').  Other options include
not forcing restoration at all (nil), or forcing the restoration of all
scratch buffers (t)."
  :group 'scratch-plus
  :type '(choice (const :tag "Do not force restoration." nil)
                 (const :tag "Force restoration of only buffer for initial-major-mode" initial)
                 (const :tag "Force restoration of all scratch buffers." t)))

(defcustom scratch-plus-project-subdir nil
  "Where should project-specific scratch buffers be saved?

If nil, project-specific scratch buffers will not be saved.  If a
string, project-specific scratch buffers will be saved to a subdirectory
in the project root with this name.

This variable is safe as a buffer-local variable if it is nil or a
string."
  :group 'scratch-plus
  :type '(choice
          (string :tag "Save to project subdirectory.")
          (const :tag "Do not save project-specific scratch buffers." nil))
  :safe #'string-or-null-p)

(defcustom scratch-plus-save-directory nil
  "Where should scratch buffers be saved?

If nil, scratch buffers will not be saved.  Otherwise, if a directory,
scratch buffers will be saved there.  The directory will be created as
needed."
  :group 'scratch-plus
  :type '(choice
          (const :tag "Do not save scratch buffers." nil)
          (directory :tag "Save scratch buffers to directory.")))

(defcustom scratch-plus-idle-save nil
  "Should scratch buffers be saved when idle?

If nil, scratch buffers will not be saved when idle.  Otherwise, if an
integer, scratch buffers will be saved after Emacs has been idle for
this many seconds."
  :group 'scratch-plus
  :type '(choice
          (const :tag "Do not autosave scratch buffers." nil)
          (integer :tag "Save scratch buffers every n seconds idle time.")))

(defcustom scratch-plus-prevent-kill 'bury
  "Should `scratch-plus' prevent scratch buffers from being killed?

Default behavior is to bury scratch buffers instead of killing them.
Other options preventing the scratch buffer from being killed and doing
nothing (t) or allowing the scratch buffer to be killed (nil)."
  :group 'scratch-plus
  :type '(choice
          (const :tag "Do not prevent killing scratch buffers." nil)
          (const :tag "Bury the scratch buffer." bury)
          (const :tag "Prevent killing scratch buffers." t)))

(defcustom scratch-plus-initial-message initial-scratch-message
  "Initial message for scratch buffers.

Either a string or a function which takes the major mode and returns a
string.  The value will be run through `substitute-command-keys', and
then wrapped in a comment using `comment-region'.

Default is `initial-scratch-message'."
  :group 'scratch-plus
  :type '(choice (string :tag "Scratch message.")
                 (function :tag "Message generator function.")))

(defcustom scratch-plus-display-action nil
  "How scratch buffers should be displayed.

If nil, `display-buffer-action-alist' will be used.  Otherwise, this
value should be a valid ACTION for `display-buffer', which see."
  :group 'scratchh-plus
  :type `(choice (const :tag "Fall-back on display-buffer-alist" nil)
                 ,display-buffer--action-custom-type)
  :risky t)


;;; Utilities

(defun scratch-plus--format-scratch-buffer-name (mode &optional project)
  "Format scratch buffer name for MODE.

If PROJECT is non-nil, produce project-scratch style name."
  (cond
   (project (format "*project-scratch[%s/%s]*"
                    (scratch-plus--mode-to-mode-name mode)
                    (project-name project)))
   ((eq mode initial-major-mode) "*scratch*")
   (t (format "*scratch[%s]*" (scratch-plus--mode-to-mode-name mode)))))

(defun scratch-plus--buffer-scratch-p (buffer)
  "Is BUFFER a scratch buffer?

If buffer is a regular scratch buffer, return the symbol `scratch'.  If
buffer is a project-specific scratch buffer, return `project'.
Otherwise, return nil."
  (cond
   ((string-match-p (rx bol "*scratch") (buffer-name buffer)) 'scratch)
   ((string-match-p (rx bol "*project-scratch") (buffer-name buffer)) 'project)
   (t nil)))

(defun scratch-plus--directory (&optional project)
  "Get the directory to save files to.

If PROJECT is non-nil, get the directory to save project-local scratch
files in."
  (cond
   ((and project (stringp scratch-plus-project-subdir))
    (file-name-as-directory
     (file-name-concat (project-root (project-current))
                       scratch-plus-project-subdir)))
   ((stringp scratch-plus-save-directory)
    (file-name-as-directory scratch-plus-save-directory))))

(defun scratch-plus--save-name (mode &optional project)
  "Get the name of a save file for MODE.

If PROJECT is non-nil, generate it for PROJECT.

If the target directory does not exist, it will be created."
  (when-let* ((scratch-directory (scratch-plus--directory project)))
    (unless (file-directory-p scratch-directory)
      (make-directory scratch-directory t))
    (file-name-concat scratch-directory (format "scratch.%s" mode))))

(defun scratch-plus--mode-name-to-mode (the-mode-name)
  "Convert THE-MODE-NAME into the mode function."
  (let* ((mode-function-name (format "%s-mode" the-mode-name))
         (mode-symbol (intern mode-function-name)))
    (when (fboundp mode-symbol)
      mode-symbol)))

(defun scratch-plus--mode-to-mode-name (mode)
  "Convert MODE into a mode name."
  (let* ((mode-string (format "%s" mode))
         (hash-tick (string= "#'" (substring mode-string 0 2))))
    (substring mode-string (if hash-tick 2 0) (- (length mode-string) 5))))

(defun scratch-plus--known-modes-list (&optional project)
  "List known scratch modes.

If PROJECT is non-nil, use data for PROJECT."
  (let ((initial-match-regexp (if project
                                  (rx bol "*project-scratch")
                                (rx bol "*scratch")))
        (buffer-match-regexp (if project
                                 (rx-to-string `(and
                                                 bol
                                                 "*project-scratch["
                                                 (group-n 1 (* any))
                                                 ,(format "/%s]*" (project-name project))
                                                 eol))
                               (rx bol
                                   "*scratch["
                                   (group-n 1 (* any))
                                   "]*"
                                   eol)))
        (save-directory (scratch-plus--directory project)))
    (cl-remove-duplicates
     (append
      (mapcar (lambda (buffer)
                (save-match-data
                  (let ((name (buffer-name buffer)))
                    (if (string-match buffer-match-regexp name)
                        (match-string 1 name)
                      (scratch-plus--mode-to-mode-name initial-major-mode)))))
              (cl-remove-if-not (apply-partially #'string-match-p initial-match-regexp)
                                (buffer-list)
                                :key #'buffer-name))
      (when save-directory
        (delq nil
              (mapcar (lambda (name)
                        (when (string-match (rx bol "scratch." (group-n 1 (* any)) "-mode" eol) name)
                          (match-string 1 name)))
                      (directory-files save-directory)))))
     :test #'string=)))


;;; Prevent Killing

(defun scratch-plus-prevent-kill ()
  "Possibly prevent killing the current buffer.

The buffer will not be killed if it is a scratch buffer, and
`scratch-prevent-kill' is non-nil.

This function is intended to be placed on the irregular
`kill-buffer-query-functions' hook."
  (if (not (and scratch-plus-prevent-kill
                (scratch-plus--buffer-scratch-p (current-buffer))))
      t
    (scratch-plus-save-buffer (current-buffer))
    (when (eq scratch-plus-prevent-kill 'bury)
      (bury-buffer))
    nil))


;;; Save Scratch Buffers
(defun scratch-plus-save-buffer (&optional buffer)
  "If BUFFER is a scratch buffer, save based on configuration.

If BUFFER is nil, operate on the current buffer."
  (interactive)
  (when-let* ((buffer (or buffer (current-buffer)))
              (is-scratch-buffer (scratch-plus--buffer-scratch-p buffer))
              (save-name (scratch-plus--save-name (buffer-local-value 'major-mode buffer)
                                                  (when (eq is-scratch-buffer 'project-scratch)
                                                    (with-current-buffer buffer
                                                      (project-current))))))
    (write-region (point-min) (point-max) save-name nil nil)
    (set-buffer-modified-p nil)))

(defun scratch-plus-save-scratch-buffers ()
  "Save all scratch buffers based on current configuration."
  (dolist (buffer (buffer-list))
    (when (scratch-plus--buffer-scratch-p buffer)
      (scratch-plus-save-buffer buffer))))


;;; Open Scratch Buffers

(defun scratch-plus-buffer (mode &optional project)
  "Get or restore scratch buffer for MODE.

If PROJECT is non-nil, do so in project."
  (let* ((scratch-buffer-name (scratch-plus--format-scratch-buffer-name mode project))
         (buffer (get-buffer scratch-buffer-name)))
    (if buffer
        (progn
          (with-current-buffer buffer
            (scratch-plus-minor-mode)
            (when project
              (setq-local default-directory (project-root project)))
            (set-buffer-modified-p nil))
          buffer)
      (let ((new-buffer (get-buffer-create scratch-buffer-name))
            (save-file-name (and scratch-plus-restore-type
                                 (scratch-plus--save-name mode project))))
        (with-current-buffer new-buffer
          (when (and save-file-name
		     (file-exists-p save-file-name))
            (insert-file-contents save-file-name))
          (goto-char (point-min))
          (funcall mode)
          (scratch-plus-minor-mode)
          (when project
            (setq-local default-directory (project-root project)))
          (set-buffer-modified-p nil))
        new-buffer))))


;;; Restore scratch buffers

(defun scratch-plus-restore-scratches ()
  "Restore all global scratch buffers."
  (when (and (scratch-plus--directory)
             scratch-plus-restore-type)
    (cond
     ((and (eq scratch-plus-restore-type 'demand)
           (eq scratch-plus-force-restore 'initial))
      (when-let* ((scratch-buffer (get-buffer "*scratch*")))
        (kill-buffer scratch-buffer))
      (scratch-plus-buffer initial-major-mode))
     ((and (eq scratch-plus-restore-type 'demand)
           scratch-plus-force-restore)
      (mapc #'kill-buffer
            (mapcar #'scratch-plus--format-scratch-buffer-name
                    (mapcar #'scratch-plus--mode-name-to-mode
                            (scratch-plus--known-modes-list))))
      (scratch-plus-buffer initial-major-mode))
     ((eq scratch-plus-force-restore 'initial)
      (when-let* ((scratch-buffer (get-buffer "*scratch*")))
        (kill-buffer scratch-buffer))
      (mapc #'scratch-plus-buffer
            (mapcar #'scratch-plus--mode-name-to-mode
                    (scratch-plus--known-modes-list))))
     (t
      (mapc #'kill-buffer
            (mapcar #'scratch-plus--format-scratch-buffer-name
                    (mapcar #'scratch-plus--mode-name-to-mode
                            (scratch-plus--known-modes-list))))
      (mapc #'scratch-plus-buffer
            (mapcar #'scratch-plus--mode-name-to-mode
                    (scratch-plus--known-modes-list)))))))


;;; In-buffer minor mode

(defvar-keymap scratch-plus-minor-mode-map
  "<remap> <save-buffer>" #'scratch-plus-save-buffer)

(define-minor-mode scratch-plus-minor-mode
  "Minor mode for `scratch-plus'.

This minor mode does two things:

 - When enabled, if the buffer is empty, it populates the initial
   message using `scratch-plus-initial-message', adding a mode property
   line.

 - Remaps \\[save-buffer] to `scratch-plus-save-buffer' to store the
   buffer correctly."
  :lighter " S+"
  :keymap scratch-plus-minor-mode-map
  (when scratch-plus-minor-mode
    (when (= (buffer-size (current-buffer)) 0)
      (insert (substitute-command-keys
               (if (stringp scratch-plus-initial-message)
                   scratch-plus-initial-message
                 (funcall scratch-plus-initial-message major-mode))))
      (comment-region (point-min) (point-max))
      (goto-char (point-min))
      (insert "\n")
      (add-file-local-variable-prop-line 'mode major-mode)
      (goto-char (point-max))
      (insert "\n\n"))))


;;; Idle Timer for Save

(defvar scratch-plus--idle-save-timer nil
  "Idle timer for automatically saving scratch buffers.")

(defvar scratch-plus--idle-timer-initial-time nil
  "Idle time when timer was set.")

(defun scratch-plus-stop-idle-timer ()
  "Cancel the `scratch-plus' idle save timer."
  (when (timerp scratch-plus--idle-save-timer)
    (cancel-timer scratch-plus--idle-save-timer))
  (setf scratch-plus--idle-save-timer nil
        scratch-plus--idle-timer-initial-time nil))

(defun scratch-plus-start-idle-timer ()
  "Start the `scratch-plus' idle save timer."
  (scratch-plus-stop-idle-timer)
  (when scratch-plus-idle-save
    (setf scratch-plus--idle-timer-initial-time scratch-plus-idle-save
          scratch-plus--idle-save-timer (run-with-idle-timer scratch-plus-idle-save t #'scratch-plus-idle-timer-function))))

(defun scratch-plus-idle-timer-function ()
  "Save scratch buffers & possibly restart timer."
  (scratch-plus-save-scratch-buffers)
  (unless (eq scratch-plus-idle-save scratch-plus--idle-timer-initial-time)
    (scratch-plus-start-idle-timer)))



;;; User Interface

(defun scratch-plus-switch (arg &optional project)
  "Switch to scratch buffer.

ARG is used to modify behavior as follows:

 - nil
   switch to the scratch buffer for `initial-major-mode'.

 - \\[universal-argument]
   Switches to a scratch buffer for the current major mode.

 - \\[universal-argument] \\[universal-argument]
   Prompts for a major mode to switch to by name.  Existing saved
   scratch buffers and open scratch buffers are used to pre-populate the
   list, however any major mode can be selected by typing its name
   without \"-mode\".

If PROJECT is passed, a project-scratch will be used instead."
  (interactive "P")
  (when-let* ((buffer
               (pcase arg
                 (`(16)
                  (when-let* ((major-mode-name (completing-read "Major mode for scratch buffer: "
                                                                (scratch-plus--known-modes-list project)))
                              (new-mode (scratch-plus--mode-name-to-mode major-mode-name)))
                    (scratch-plus-buffer new-mode project)))
                 (`(4)
                  (scratch-plus-buffer major-mode project))
                 (_ (scratch-plus-buffer initial-major-mode project)))))
    (if scratch-plus-display-action
        (display-buffer buffer scratch-plus-display-action)
      (display-buffer buffer))))

(defun scratch-plus-switch-project (arg)
  "Switch to scratch buffer for current project.

ARG follows semantics for
`scratch-plus-switch' (\\[scratch-plus-switch]), which see."
  (interactive "P")
  (scratch-plus-switch arg (project-current)))

(defvar-keymap scratch-plus-mode-map
  :doc "Keymap for `scratch-plus-mode'."
  "C-x M-s" #'scratch-plus-switch
  "C-x p M-s" #'scratch-plus-switch-project
  "<remap> <scratch-buffer>" #'scratch-plus-switch)

;;;###autoload
(define-minor-mode scratch-plus-mode
  "Enable `scratch-plus' features."
  :global t
  :group 'scratch-plus
  :keymap scratch-plus-mode-map
  (if scratch-plus-mode
      (progn
        (add-hook 'kill-buffer-query-functions #'scratch-plus-prevent-kill)
        (add-hook 'kill-emacs-hook #'scratch-plus-save-scratch-buffers)
        (scratch-plus-start-idle-timer)
        (when scratch-plus-restore-type
          (scratch-plus-restore-scratches)))
    (scratch-plus-save-scratch-buffers)
    (scratch-plus-stop-idle-timer)
    (remove-hook 'kill-buffer-query-functions #'scratch-plus-prevent-kill)
    (remove-hook 'kill-emacs-hook #'scratch-plus-save-scratch-buffers)))

(provide 'scratch-plus)
;;; scratch-plus.el ends here
