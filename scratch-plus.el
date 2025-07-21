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

(defcustom scratch-plus-restore-mode nil
  "TODO"
  :group 'scratch-plus
  :type '(choice
          (const :tag "No restoration." nil)
          (const :tag "On-demand restoration." demand)
          (const :tag "Always restore all." always)))

(defcustom scratch-plus-force-restore 'initial
  "TODO"
  :group 'scratch-plus
  :type '(choice (const :tag "Do not force restoration." nil)
                 (const :tag "Force restoration of only buffer for initial-major-mode" initial)
                 (const :tag "Force restoration of all scratch buffers." t)))

(defcustom scratch-plus-project-enable nil
  "TODO"
  :group 'scratch-plus
  :type '(choice
          (string :tag "Enable, save to project subdirectory.")
          (const :tag "Enable, do not save." t)
          (const :tag "Disable project-specific scratch buffers." nil)))

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
   ((and project (stringp scratch-plus-project-enable))
    (file-name-as-directory
     (file-name-concat (project-root (project-current))
                       scratch-plus-project-enable)))
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


;;; In-buffer minor mode

(define-minor-mode scratch-plus-minor-mode
  "TODO"
  :lighter " S+")


;;; Installation

(provide 'scratch-plus)
;;; scratch-plus.el ends here
