;;; scratch-plus.el --- Better Scratch Buffer Behavior  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Samuel W. Flint

;; Author: Samuel W. Flint <swflint@flintfam.org>
;; Keywords: convenience
;; Homepage: https://git.sr.ht/~swflint/scratch-plus
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.1"))

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

;;

;;; Code:

(require 'project)


;;; Configuration

(defgroup scratch-plus ()
  "Better scratch buffer behavior."
  :group 'convenience)

(defcustom scratch-plus-restore-type 'demand
  "TODO"
  :group 'scratch-plus
  :type '(choice
          (const :tag "No restoration." nil)
          (const :tag "On-demand restoration." demand)
          (const :tag "Restore all." t)))

(defcustom scratch-plus-force-restore 'initial
  "TODO"
  :group 'scratch-plus
  :type '(choice (const :tag "Do not force restoration." nil)
                 (const :tag "Force restoration of only buffer for initial-major-mode" initial)
                 (const :tag "Force restoration of all scratch buffers." t)))

(defcustom scratch-plus-project-subdir nil
  "TODO"
  :group 'scratch-plus
  :type '(choice
          (string :tag "Save to project subdirectory.")
          (const :tag "Do not save project-specific scratch buffers." nil))
  :safe #'string-or-null-p)

(defcustom scratch-plus-save-directory nil
  "TODO"
  :group 'scratch-plus
  :type '(choice
          (const :tag "Do not save scratch buffers." nil)
          (directory :tag "Save scratch buffers to directory.")))

(defcustom scratch-plus-autosave nil
  "TODO"
  :group 'scratch-plus
  :type '(choice
          (const :tag "Do not autosave scratch buffers." nil)
          (const :tag "Autosave scratch buffers on buffer switch/focus change." t)
          (integer :tag "Save buffers every n seconds.")))

(defcustom scratch-plus-prevent-kill nil
  "TODO"
  :group 'scratch-plus
  :type '(choice
          (const :tag "Do not prevent killing scratch buffers." nil)
          (const :tag "Bury the scratch buffer." bury)
          (const :tag "Prevent killing scratch buffers." t)))


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
    (write-region (point-min) (point-max) save-name nil nil)))

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
              (setq-local default-directory (project-root project))))
          buffer)
      (let ((new-buffer (get-buffer-create scratch-buffer-name))
            (save-file-name (and scratch-plus-restore-type
                                 (scratch-plus--save-name mode project))))
        (with-current-buffer new-buffer
          (when save-file-name
            (insert-file-contents save-file-name))
          (goto-char (point-min))
          (funcall mode)
          (scratch-plus-minor-mode)
          (when project
            (setq-local default-directory (project-root project))))
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

(define-minor-mode scratch-plus-minor-mode
  "TODO"
  :lighter " S+")


;;; User Interface

(defun scratch-plus-switch (arg &optional project)
  "TODO"
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
    (display-buffer buffer)))

(defun scratch-plus-switch-project (arg)
  "TODO"
  (interactive "P")
  (scratch-plus-switch arg (project-current)))

(defvar-keymap scratch-plus-mode-map
  :doc "Keymap for `scratch-plus-mode'."
  "C-x M-s" #'scratch-plus-switch
  "C-x p M-s" #'scratch-plus-switch-project)

(define-minor-mode scratch-plus-mode
  "Enable scratch-plus mode. TODO"
  :global t
  :group 'scratch-plus
  :keymap 'scratch-plus-mode-map
  (if scratch-plus-mode
      (progn
        (add-hook 'kill-buffer-query-functions #'scratch-plus-prevent-kill)
        (add-hook 'kill-emacs-hook #'scratch-plus-save-scratch-buffers)
        (when scratch-plus-restore-type
          (scratch-plus-restore-scratches)))
    (scratch-plus-save-scratch-buffers)
    (remove-hook 'kill-buffer-query-functions #'scratch-plus-prevent-kill)
    (remove-hook 'kill-emacs-hook #'scratch-plus-save-scratch-buffers)))

(provide 'scratch-plus)
;;; scratch-plus.el ends here
