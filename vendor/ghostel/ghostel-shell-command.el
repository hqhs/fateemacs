;;; ghostel-shell-command.el --- async-shell-command in a ghostel terminal -*- lexical-binding: t; -*-

;; Author: Daniel Kraus <daniel@kraus.my>
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `ghostel-shell-command' is `async-shell-command' backed by a real TTY,
;; for programs that ignore TERM=dumb.  `ghostel-shell-command-global-mode'
;; routes every asynchronous `shell-command' through it, including Dired's
;; `&', `project-async-shell-command' and Lisp callers.
;;
;; To change only `async-shell-command', remap it instead of enabling the mode:
;;
;;   (keymap-global-set "<remap> <async-shell-command>" #'ghostel-shell-command)

;;; Code:

(require 'ghostel)
(require 'subr-x)

(declare-function dired-get-filename "dired" (&optional localp no-error-if-not-filep))

(defcustom ghostel-shell-command-buffer-name "*Ghostel Shell Command*"
  "Buffer name used by `ghostel-shell-command'."
  :type 'string
  :group 'ghostel)

;;;###autoload
(defun ghostel-shell-command (command &optional buffer-name)
  "Run shell COMMAND in a ghostel terminal, like `async-shell-command'.

COMMAND goes to `shell-file-name' via `shell-command-switch', the remote
host's for a TRAMP `default-directory'.  The buffer, BUFFER-NAME or
`ghostel-shell-command-buffer-name', is kept after COMMAND exits.
When it is still running a command, `async-shell-command-buffer' decides
whether to kill that one, use a new buffer, or rename the old one.
Returns the buffer."
  (interactive
   (list (read-shell-command
          (if shell-command-prompt-show-cwd
              (format-message "Ghostel shell command in `%s': "
                              (abbreviate-file-name default-directory))
            "Ghostel shell command: ")
          nil nil
          (let ((file (or buffer-file-name
                          (and (derived-mode-p 'dired-mode)
                               (dired-get-filename nil t)))))
            (and file (file-relative-name file))))))
  ;; A trailing & would background the job and lose its output.
  (setq command (string-trim-right command "[ \t]*&[ \t]*"))
  (when (string-blank-p command)
    (user-error "Empty shell command"))
  (pcase-let* ((name (or buffer-name ghostel-shell-command-buffer-name))
               (buffer (get-buffer-create name))
               (directory default-directory)
               (`(,shell ,switch)
                (with-connection-local-variables
                 (list shell-file-name shell-command-switch))))
    (when (or (get-buffer-process buffer)
              (process-live-p (buffer-local-value 'ghostel--process buffer)))
      (pcase-let ((`(,prompt . ,action)
                   (pcase async-shell-command-buffer
                     ('confirm-kill-process '("Kill it" . kill))
                     ('confirm-new-buffer '("Use a new buffer" . new))
                     ('new-buffer '(nil . new))
                     ('confirm-rename-buffer '("Rename it" . rename))
                     ('rename-buffer '(nil . rename)))))
        (when (and prompt
                   (not (yes-or-no-p (format "A command is running in %s.  %s? "
                                             name prompt))))
          (user-error "Shell command in progress"))
        (pcase action
          ;; Killing the buffer hangs up the child on both PTY backends.
          ('kill (let ((kill-buffer-query-functions nil)
                       (ghostel-query-before-killing nil))
                   (kill-buffer buffer))
                 (when (buffer-live-p buffer)
                   (user-error "Shell command in progress"))
                 (setq buffer (get-buffer-create name)))
          ('new (setq buffer (generate-new-buffer name)))
          ('rename (with-current-buffer buffer (rename-uniquely))
                   (setq buffer (get-buffer-create name))))))
    (with-current-buffer buffer
      (setq default-directory directory)
      ;; Before display, so `display-buffer-alist' rules can match the mode.
      (unless (derived-mode-p 'ghostel-mode)
        (ghostel-mode))
      (setq-local ghostel-kill-buffer-on-exit nil))
    (display-buffer buffer '(nil (allow-no-window . t)))
    (ghostel-exec buffer shell (list switch command)
                  `((kind . shell-command)
                    (command . (,shell ,switch ,command))))
    buffer))

(defun ghostel-shell-command--advice (orig command &optional output-buffer error-buffer)
  "Run a COMMAND ending in & through `ghostel-shell-command'.
ORIG handles synchronous commands and asynchronous ones whose
OUTPUT-BUFFER is the current buffer or not a buffer at all.
ERROR-BUFFER only reaches ORIG."
  (let ((name (cond ((bufferp output-buffer) (buffer-name output-buffer))
                    ((stringp output-buffer) output-buffer))))
    (if (and (string-match-p "&[ \t]*\\'" command)
             (or (null output-buffer) name)
             (not (equal name (buffer-name))))
        (ghostel-shell-command command name)
      (funcall orig command output-buffer error-buffer))))

;;;###autoload
(define-minor-mode ghostel-shell-command-global-mode
  "Run every asynchronous `shell-command' in a ghostel terminal.
Advises `shell-command', so \\[async-shell-command], Dired's `&',
`project-async-shell-command' and Lisp callers are all covered."
  :global t
  :group 'ghostel
  (if ghostel-shell-command-global-mode
      (advice-add 'shell-command :around #'ghostel-shell-command--advice)
    (advice-remove 'shell-command #'ghostel-shell-command--advice)))

(provide 'ghostel-shell-command)

;;; ghostel-shell-command.el ends here
