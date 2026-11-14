;;; inf-clojure.el --- Basic interaction with a Clojure REPL -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Phil Brown
;; Copyright © 2014-2026 Bozhidar Batsov
;; Copyright (C) 1988-2025 Free Software Foundation, Inc.

;; Authors:
;;   Phil Brown <pdbrown.git@gmail.com>
;;   Bozhidar Batsov <bozhidar@batsov.dev>
;;   Olin Shivers <shivers@cs.cmu.edu>
;; Maintainer: Phil Brown <pdbrown.git@gmail.com>

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Keywords: processes, comint, clojure
;; Version: 3.5.0
;; Package-Requires: ((emacs "28.1") (clojure-mode "5.11"))

;; This file is not part of GNU Emacs.

;; inf-clojure is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; inf-clojure is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with inf-clojure.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides interaction with a REPL hosted by a Clojure process.
;; It's based on ideas from the popular `inferior-lisp` package.
;; `inf-clojure` has two components - a Clojure REPL (`inf-clojure-mode`)
;; and a minor mode (`inf-clojure-minor-mode`), which extends `clojure-mode`
;; with commands to evaluate forms directly in the REPL.

;;; Code:

(require 'comint)
(require 'clojure-mode)
(require 'clojure-ts-mode nil :no-error)
(require 'thingatpt)
(require 'ansi-color)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'project)

(defvar inf-clojure-startup-forms '((lein . "lein repl")
                                    (boot . "boot repl")
                                    (clojure . "clojure")
                                    (cljs . "clojure -M -m cljs.main -r")
                                    (clojure-clr . "Clojure.Main")
                                    (planck . "planck -d")
                                    (babashka . "bb")
                                    (node-babashka . "nbb")
                                    (joker . "joker")))

(defvar inf-clojure-buffer nil
  "The current `inf-clojure' process buffer.

MULTIPLE PROCESS SUPPORT
===========================================================================
To run multiple Clojure processes, start each with a separate invocation
of \\[inf-clojure] or \\[inf-clojure-socket-repl]. REPL and socket
server buffers have unique names, which include the current project if
any.

Commands that send text from source buffers to Clojure processes --
like `inf-clojure-eval-defun' -- have to choose a
process to send to, when you have more than one Clojure process around.  This
is determined by the global variable `inf-clojure-buffer'.  Suppose you
have three inferior Clojures running:
    Buffer              Process
    foo                 inf-clojure
    bar                 inf-clojure<2>
    *inf-clojure*       inf-clojure<3>
If you do a \\[inf-clojure-eval-defun] command on some Clojure source code,
what process do you send it to?

- If you're in a process buffer (foo, bar, or *inf-clojure*),
  you send it to that process.
- If you're in some other buffer (e.g., a source file), you
  send it to the process attached to buffer `inf-clojure-buffer'.
This process selection is performed by function `inf-clojure-proc'.

Whenever \\[inf-clojure] fires up a new process, it resets
`inf-clojure-buffer' to be the new process's buffer.  If you only run
one process, this does the right thing.  If you run multiple
processes, you might need to change `inf-clojure-buffer' to
whichever process buffer you want to use.")

(defun inf-clojure-proc (&optional no-error)
  "Return the current inferior Clojure process.
When NO-ERROR is non-nil, don't throw an error when no process
has been found.  See also variable `inf-clojure-buffer'."
  (or (get-buffer-process (if (derived-mode-p 'inf-clojure-mode)
                              (current-buffer)
                            inf-clojure-buffer))
      (unless no-error
        (error "No Clojure subprocess; see variable `inf-clojure-buffer'"))))

(defun inf-clojure-select-repl-buffer ()
  "Find or select an ‘inf-clojure’ buffer to operate on.

Useful for commands that can invoked outside of an ‘inf-clojure’ buffer
\\(e.g. from a Clojure buffer\\)."
  ;; if we're in a inf-clojure buffer we simply return in
  (if (derived-mode-p 'inf-clojure-mode)
      (current-buffer)
    ;; otherwise we sift through all the inf-clojure buffers that are available
    (let ((repl-buffers (cl-remove-if-not (lambda (buf)
                                            (with-current-buffer buf
                                              (derived-mode-p 'inf-clojure-mode)))
                                          (buffer-list))))
      (cond
       ((null repl-buffers) (user-error "No inf-clojure REPL buffers found"))
       ((= (length repl-buffers) 1) (car repl-buffers))
       (t (get-buffer (completing-read "Select inf-clojure REPL: "
                                       (mapcar #'buffer-name repl-buffers))))))))

(defun inf-clojure-set-repl ()
  (interactive)
  "Set an `inf-clojure' buffer as the active (default) REPL."
  (setq inf-clojure-buffer (inf-clojure-select-repl-buffer))
  (message "Current inf-clojure REPL set to %s" chosen-repl-buffer))

(defgroup inf-clojure nil
  "Run an external Clojure process (REPL) in an Emacs buffer."
  :prefix "inf-clojure-"
  :group 'clojure
  :link '(url-link :tag "GitHub" "https://github.com/clojure-emacs/inf-clojure")
  :link '(emacs-commentary-link :tag "Commentary" "inf-clojure"))

(defconst inf-clojure-version
  (package-get-version)
  "The current version of `inf-clojure'.")

(defcustom inf-clojure-prompt-read-only t
  "If non-nil, the prompt will be read-only.

Also see the description of `comint-prompt-read-only'."
  :type 'boolean)

(defcustom inf-clojure-input-filter-regexp
  "\\`\\s *\\(:\\(\\w\\|\\s_\\)\\)?\\s *\\'"
  "What not to save on inferior Clojure's input history.
Input matching this regexp is not saved on the input history in Inferior Clojure
mode.  Default is whitespace followed by 0 or 1 single-letter colon-keyword
\(as in :a, :c, etc.)"
  :type 'regexp)

(defcustom inf-clojure-source-modes '(clojure-ts-mode clojure-mode)
  "Used to determine if a buffer contains Clojure source code.

If a buffer has one of these major modes, it's considered a Clojure
source file by all `inf-clojure' commands."
  :type '(repeat symbol)
  :safe (lambda (val) (and (listp val) (cl-every #'symbolp val))))

(defun inf-clojure--get-mode-line ()
  "Return modeline for `inf-clojure-minor-mode'."
  (if (and (bufferp inf-clojure-buffer)
           (buffer-live-p inf-clojure-buffer))
      (with-current-buffer inf-clojure-buffer
        (thread-last (buffer-name (current-buffer))
                     (replace-regexp-in-string "^\\*" "" )
                     (replace-regexp-in-string "\\*$" "" )))
    "inf-clojure[nil]"))

(defvar inf-clojure-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map comint-mode-map)
    (define-key map (kbd "C-x C-e") #'inf-clojure-eval-last-sexp)
    (define-key map (kbd "C-c M-o") #'inf-clojure-clear-repl-buffer)
    (define-key map (kbd "C-c C-t") #'inf-clojure-restart)
    (define-key map (kbd "C-c C-q") #'inf-clojure-quit)
    (define-key map (kbd "C-c M-i") #'inf-clojure-info)
    (define-key map (kbd "C-c C-z") #'inf-clojure-switch-to-recent-buffer)
    (easy-menu-define inf-clojure-mode-menu map
      "Inferior Clojure REPL Menu"
      '("Inf-Clojure REPL"
        ["Eval last sexp" inf-clojure-eval-last-sexp t]
        "--"
        ["Clear REPL" inf-clojure-clear-repl-buffer]
        ["Info" inf-clojure-info]
        ["Restart" inf-clojure-restart]
        ["Quit" inf-clojure-quit]
        "--"
        ["Version" inf-clojure-display-version]))
    map))

(defvar inf-clojure-insert-commands-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "d") #'inf-clojure-insert-defun)
    (define-key map (kbd "C-d") #'inf-clojure-insert-defun)
    (define-key map (kbd "e") #'inf-clojure-insert-last-sexp)
    (define-key map (kbd "C-e") #'inf-clojure-insert-last-sexp)
    map))

(defvar inf-clojure-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-M-x")  #'inf-clojure-eval-defun)     ; Gnu convention
    (define-key map (kbd "C-x C-e") #'inf-clojure-eval-last-sexp) ; Gnu convention
    (define-key map (kbd "C-c C-e") #'inf-clojure-eval-last-sexp)
    (define-key map (kbd "C-c C-c") #'inf-clojure-eval-defun)     ; SLIME/CIDER style
    (define-key map (kbd "C-c C-b") #'inf-clojure-eval-buffer)
    (define-key map (kbd "C-c C-r") #'inf-clojure-eval-region)
    (define-key map (kbd "C-c C-n") #'inf-clojure-eval-form-and-next)
    (define-key map (kbd "C-c C-j") inf-clojure-insert-commands-map)
    (define-key map (kbd "C-c C-z") #'inf-clojure-switch-to-repl)
    (define-key map (kbd "C-c C-t") #'inf-clojure-restart)
    (define-key map (kbd "C-c C-q") #'inf-clojure-quit)
    (define-key map (kbd "C-c M-i") #'inf-clojure-info)
    (define-key map (kbd "C-c M-c") #'inf-clojure-connect)
    (define-key map (kbd "C-c M-r") #'inf-clojure-set-repl)
    (easy-menu-define inf-clojure-minor-mode-menu map
      "Inferior Clojure Minor Mode Menu"
      '("Inf-Clojure"
        ["Eval top-level sexp at point" inf-clojure-eval-defun t]
        ["Eval last sexp" inf-clojure-eval-last-sexp t]
        ["Eval region" inf-clojure-eval-region t]
        ["Eval buffer" inf-clojure-eval-buffer t]
        "--"
        ["Switch to REPL" inf-clojure-switch-to-repl t]
        "--"
        ["Display REPL info" inf-clojure-info]
        ["Restart REPL" inf-clojure-restart]
        ["Quit REPL" inf-clojure-quit]))
    map))

;;;###autoload
(with-eval-after-load 'clojure-mode
  (define-key clojure-mode-map (kbd "C-c M-k") #'inf-clojure)
  (define-key clojure-mode-map (kbd "C-c M-n") #'inf-clojure-socket-repl)
  (define-key clojure-mode-map (kbd "C-c M-c") #'inf-clojure-connect))

;;;###autoload
(defcustom inf-clojure-mode-line
  '(:eval (format " %s" (inf-clojure--get-mode-line)))
  "Mode line lighter for `inf-clojure-minor-mode'.

The value of this variable is a mode line template as in
`mode-line-format'.  See Info Node `(elisp)Mode Line Format' for details
about mode line templates.

Customize this variable to change how `inf-clojure-minor-mode'
displays its status in the mode line.  The default value displays
the current REPL.  Set this variable to nil to disable the
mode line entirely."
  :type 'sexp
  :risky t)

;;;###autoload
(define-minor-mode inf-clojure-minor-mode
  "Minor mode for interacting with the inferior Clojure process buffer.

The following commands are available:

\\{inf-clojure-minor-mode-map}"
  :lighter inf-clojure-mode-line
  :keymap inf-clojure-minor-mode-map)

(defun inf-clojure--endpoint-p (x)
  "Return non-nil if and only if X is a valid endpoint.

A valid endpoint consists of a host and port
number (e.g. (\"localhost\" . 5555))."
  (and
   (listp x)
   (stringp (car x))
   (numberp (cdr x))))

(defcustom inf-clojure-custom-startup
   nil
   "Form to be used to start `inf-clojure'.
Can be a cons pair of (host . port) where host is a string and
port is an integer, or a string to startup an interpreter like
\"planck\"."
   :type '(choice string (cons string integer) (const nil)))

(defcustom inf-clojure-prompt-regexp "^[^=> \n]+=> *"
  "Regexp to recognize prompts printed by the clojure REPL.
Unused by default: the REPL buffer assumes prompts are the most recent
output that doesn't end with a newline. See `comint-use-prompt-regexp'."
  :type 'regexp)

(defcustom inf-clojure-socket-ready-regexp "^[^=> \n]+=> *"
  "Regexp to detect whether clojure socket REPL has started.
When you run `inf-clojure-socket-repl', inf-clojure starts the
clojure (server) process, waits for it to print a line that matches this
regexp, then attempts to connect to a socket repl.

If the clojure server process was started by the 'clojure' command with
no special args (other than a socket repl config), it will start a
console repl only after the socket repl has begun accepting connections,
and print a first prompt to its console output. By default,
`inf-clojure-socket-ready-regexp' matches such a prompt.

Note that the system property based
configuration system to start servers and socket repls, e.g.:
  -J-Dclojure.server.repl=\"{:port %d :accept clojure.core.server/repl}\"
doesn't run until after user.clj is loaded."
  :type 'regexp)

(defcustom inf-clojure-repl-use-same-window nil
  "Controls whether to display the REPL buffer in the current window or not."
  :type '(choice (const :tag "same" t)
                 (const :tag "different" nil))
  :safe #'booleanp
  :package-version '(inf-clojure . "2.0.0"))

(defcustom inf-clojure-auto-mode t
  "Automatically enable `inf-clojure-minor-mode'.
All buffers in `clojure-mode' will automatically be in
`inf-clojure-minor-mode' unless set to nil."
  :type 'boolean
  :safe #'booleanp
  :package-version '(inf-clojure . "3.1.0"))

(defcustom inf-clojure-process-connection-type 'pty
  "Control type of device used to communicate with subprocesses started
by inf-clojure. The symbol pty means means use a pseudo-terminal, nil
means use pipes.

Some clojure programs require a pty, and a pty enables job-control
features such as `comint-interrupt-subjob' for subprocess groups. Using
a pipe avoids the PTY canonical-mode input line limit, which can cause
`inf-clojure-mode' to hang when sending long forms to the REPL. See
`comint-max-line-length' for the line-length limits that apply to your
OS when using a pty.

If you need both a pty and to send long forms, you can use a socket
repl, see `inf-clojure-socket-repl', or disable the pty's canonical
mode, see \"Details and quirks\" section of `inf-clojure' docstring."
  ;; This defcustom was adapted from
  ;; https://github.com/p4v4n/inf-clojure/blob/1276d2d3337764a02e5589751e04a90f763c524d/inf-clojure.el
  :type '(choice (const :tag "Pipe (avoids PTY line limit)" nil)
                 (const :tag "PTY (job control, but line length limit)" 'pty))
  :package-version '(inf-clojure . "3.5.0"))


(defun inf-clojure--get-preferred-major-modes ()
  "Return list of preferred major modes that are actually available."
  (cl-remove-if-not (lambda (mode) (featurep mode))
                    inf-clojure-source-modes))

(defun inf-clojure--clojure-buffer-p ()
  "Return TRUE if the current buffer is a Clojure buffer."
  ;; TODO: Replace `apply' with a direct call once Emacs 30 is the
  ;; minimum version, as `derived-mode-p' accepts a list from 30+.
  (apply #'derived-mode-p (inf-clojure--get-preferred-major-modes)))

(defun inf-clojure--clojure-buffers ()
  "Return a list of all existing `clojure-mode' buffers."
  (cl-remove-if-not (lambda (buffer)
                      (with-current-buffer buffer
                        (inf-clojure--clojure-buffer-p)))
                    (buffer-list)))

(defun inf-clojure-enable-on-existing-clojure-buffers ()
  "Enable inf-clojure's minor mode on existing Clojure buffers.
See command `inf-clojure-minor-mode'."
  (interactive)
  (dolist (mode (inf-clojure--get-preferred-major-modes))
    (add-hook (intern (concat (symbol-name mode) "-hook")) #'inf-clojure-minor-mode))
  (dolist (buffer (inf-clojure--clojure-buffers))
    (with-current-buffer buffer
      (inf-clojure-minor-mode +1))))

(defun inf-clojure-disable-on-existing-clojure-buffers ()
  "Disable command `inf-clojure-minor-mode' on existing Clojure buffers."
  (interactive)
  (dolist (buffer (inf-clojure--clojure-buffers))
    (with-current-buffer buffer
      (inf-clojure-minor-mode -1))))

(define-derived-mode inf-clojure-mode comint-mode "Inferior Clojure"
  "Major mode for interacting with an inferior clojure process.
Mode `inf-clojure-mode' derives from `comint-mode' and implements a REPL
client connected to a local in-buffer clojure process or remote REPL
server over TCP. Use one of `inf-clojure', `inf-clojure-socket-repl', or
'inf-clojure-connect' to start and/or connect to a clojure REPL, see
below.

For information on running multiple processes in multiple buffers, see
documentation for variable `inf-clojure-buffer'.

\\{inf-clojure-mode-map}

Customization: Entry to this mode runs the hooks on `comint-mode-hook' and
`inf-clojure-mode-hook' (in that order).

You can send text to the inferior Clojure process from other buffers containing
Clojure source.
    `inf-clojure-switch-to-repl' switches the current buffer to the Clojure
    process buffer.
    `inf-clojure-eval-defun' sends the current defun to the Clojure process.
    `inf-clojure-eval-region' sends the current region to the Clojure process.

    Prefixing the inf-clojure-eval/defun/region commands with
    a \\[universal-argument] causes a switch to the Clojure process buffer after
    sending the text.


Commands:\\<inf-clojure-mode-map>
\\[comint-send-input] after the end of the process' output sends the text from
    the end of process to point.
\\[comint-send-input] before the end of the process' output copies the sexp
    ending at point to the end of the process' output, and sends it.
\\[comint-copy-old-input] copies the sexp ending at point to the end of the
    process' output,allowing you to edit it before sending it.
If `comint-use-prompt-regexp' is nil (the default), \\[comint-insert-input] on
   old input copies the entire old input to the end of the process' output,
   allowing you to edit it before sending it.  When not used on old input, or if
   `comint-use-prompt-regexp' is non-nil, \\[comint-insert-input] behaves
   according to its global binding.
\\[backward-delete-char-untabify] converts tabs to spaces as it moves back.
\\[clojure-indent-line] indents for Clojure; with argument, shifts rest
    of expression rigidly with the current line.
\\[indent-sexp] does \\[clojure-indent-line] on each line starting within
  following expression.  Paragraphs are separated only by blank lines.
  Semicolons start comments.  If you accidentally suspend your process,
  use \\[comint-continue-subjob] to continue it.


Entrypoints:

Run `inf-clojure' to start a REPL and process in the same
`inf-clojure-mode' buffer.

      +- *inf-clojure* -------+
      |                       |
      |  [Clojure process]    |
      |                       |
      |  user=>               |
      +-----------------------+

Run `inf-clojure-socket-repl' to start a process in a separate server
buffer, then connect to it from an `inf-clojure-mode' REPL buffer over
TCP.

      +- *inf-clojure* -------+      +- *inf-clojure-server* ---+
      |                       |      |                          |
      |  [Comint TCP client] <----------> Clojure process       |
      |                       |      |                          |
      |  user=>               |      +--------------------------+
      +-----------------------+

Run `inf-clojure-connect' to open an `inf-clojure-mode' REPL buffer and
connect it to a process serving a REPL over TCP.

      +- *inf-clojure* -------+
      |                       |
      |  [Comint TCP client] <----------> Clojure process
      |                       |
      |  user=>               |
      +-----------------------+"
  (setq-local comint-prompt-regexp inf-clojure-prompt-regexp)
  (setq-local mode-line-process '(":%s"))
  ;; Syntax highlighting for input at REPL prompt before it's sent to clojure.
  (setq-local comint-indirect-setup-function #'clojure-mode)
  (comint-fontify-input-mode)
  ;; Syntax highlighting for output from REPL.
  ;; NOTE: Using Tree-sitter based syntax highlighting in comint
  ;; buffer is currently not possible.
  (clojure-mode-variables)
  (clojure-font-lock-setup)
  (setq-local comint-get-old-input #'inf-clojure-get-old-input)
  (setq-local comint-input-filter #'inf-clojure-input-filter)
  (setq-local comint-prompt-read-only inf-clojure-prompt-read-only)
  (ansi-color-for-comint-mode-on)
  (when inf-clojure-auto-mode
    (inf-clojure-enable-on-existing-clojure-buffers)))

(defun inf-clojure-get-old-input ()
  "Return a string containing the sexp ending at point."
  (save-excursion
    (let ((end (point)))
      (backward-sexp)
      (buffer-substring (max (point) (comint-line-beginning-position)) end))))

(defun inf-clojure-input-filter (str)
  "Return t if STR does not match `inf-clojure-input-filter-regexp'."
  (not (string-match inf-clojure-input-filter-regexp str)))

(defun inf-clojure-clear-repl-buffer ()
  "Clear the REPL buffer."
  (interactive)
  (with-current-buffer (if (derived-mode-p 'inf-clojure-mode)
                           (current-buffer)
                         inf-clojure-buffer)
    (let ((comint-buffer-maximum-size 0))
      (comint-truncate-buffer))))

(defun inf-clojure-switch-to-repl (eob-p)
  "Switch to the inferior Clojure process buffer.
With prefix argument EOB-P, positions cursor at end of buffer."
  (interactive "P")
  (if (get-buffer-process inf-clojure-buffer)
      ;; Be willing to use another frame that already has the window in it.
      (pop-to-buffer inf-clojure-buffer '(display-buffer-reuse-window
                                          (reusable-frames . 0)))
    (call-interactively #'inf-clojure))
  (when eob-p
    (push-mark)
    (goto-char (point-max))))

(defun inf-clojure-switch-to-recent-buffer ()
  "Switch to the most recently used `inf-clojure-minor-mode' buffer."
  (interactive)
  (let ((recent-inf-clojure-minor-mode-buffer (seq-find (lambda (buf)
                                                          (with-current-buffer buf (bound-and-true-p inf-clojure-minor-mode)))
                                                        (buffer-list))))
    (if recent-inf-clojure-minor-mode-buffer
        (inf-clojure--swap-to-buffer-window recent-inf-clojure-minor-mode-buffer)
      (message "inf-clojure: No recent buffer known."))))

(defun inf-clojure-quit (&optional buffer)
  "Kill the REPL buffer and its underlying process.

You can pass the target BUFFER as an optional parameter
to suppress the usage of the target buffer discovery logic."
  (interactive)
  (let ((target-buffer (or buffer (inf-clojure-select-repl-buffer))))
    (when (get-buffer-process target-buffer)
      (delete-process target-buffer))
    (kill-buffer target-buffer)))

(defun inf-clojure-restart (&optional buffer)
  "Restart the REPL buffer and its underlying process.

You can pass the target BUFFER as an optional parameter
to suppress the usage of the target buffer discovery logic."
  (interactive)
  (let* ((target-buffer (or buffer (inf-clojure-select-repl-buffer)))
         (target-buffer-name (buffer-name target-buffer)))
    ;; TODO: Try to recycle the old buffer instead of killing and recreating it
    (inf-clojure-quit target-buffer)
    (call-interactively #'inf-clojure)
    (rename-buffer target-buffer-name)))

(defun inf-clojure-info (&optional buffer)
  "Display REPL command or connection details in the minibuffer.

You can pass the target BUFFER as an optional parameter
to suppress the usage of the target buffer discovery logic."
  (interactive)
  (with-current-buffer (or buffer (inf-clojure-select-repl-buffer))
    (message "%s" inf-clojure-repl-info)))

(defun inf-clojure--project-name (dir)
  "Extract a project name from a project DIR.
The name is simply the final segment of the path."
  (file-name-nondirectory (directory-file-name dir)))

(defun inf-clojure--project-dir ()
  "Return current Clojure project root, or nil if not in a project."
  (let ((project-vc-extra-root-markers '("project.clj"      ; Leiningen
                                         "build.gradle"     ; Gradle
                                         "build.gradle.kts" ; Gradle
                                         "deps.edn"         ; Clojure CLI (a.k.a. tools.deps)
                                         "shadow-cljs.edn"  ; shadow-cljs
                                         "bb.edn"           ; babashka
                                         "nbb.edn"          ; nbb
                                         "basilisp.edn"     ; Basilisp (Python)
                                         "squint.edn"       ; Squint
                                         )))
    (when-let ((project (project-current)))
      (project-root project))))

(defvar clojure-ts-mode-syntax-table)

(defun inf-clojure--generate-repl-name (tag &optional project-dir pid host port)
  (generate-new-buffer-name
   (format "*%s[%s%s%s%s]*"
           tag
           (if project-dir (inf-clojure--project-name project-dir) "")
           (if pid (format ".%d" pid) "")
           (if host (format "@%s" host) "")
           (if port (format ":%d" port) ""))))

(defun inf-clojure--generate-repl-name2 (tag &optional name)
  (generate-new-buffer-name
   (format "*%s%s*"
           tag
           (if name (format "[%s]" name) ""))))


;;;###autoload
(defun inf-clojure (cmd)
  "Start a clojure subprocess and open a new buffer connected to its REPL.

CMD is a string which serves as the startup command or a cons of
host and port.

Prompts user for REPL startup command and REPL type if not inferrable
from startup command. Uses `inf-clojure-custom-startup' if set. Use a
prefix to prevent using these when they are set.

 Runs the hooks from `inf-clojure-mode-hook' (after the
`comint-mode-hook' is run).  \(Type \\[describe-mode] in the
process buffer for a list of commands.)


Details and quirks:

`inf-clojure' creates a new buffer named like *inf-clojure*, starts a
clojure process in it, and activates `inf-clojure-mode', which is
derived from `comint-mode', to handle input and output to and from the
process and its REPL. By default, clojure starts 'clojure.main/repl'
which reads from stdin and writes to stdout and stderr. Those file
descriptors point to either a pty device (the default) or pipes set up
by comint depending on the value of
`inf-clojure-process-connection-type'. Pty devices buffer input by line
which imposes a maximum line length, see `comint-max-line-length', and
comint (and thus your REPL) will hang forever if it tries to send a line
longer than the limit. Work around this using pipes, either by setting
`inf-clojure-process-connection-type', or also by using a socket or
remote repl, see `inf-clojure-socket-repl' and `inf-clojure-connect'.
Or, you can disable the pty's line buffering feature. On Linux and
Macos, turn off line buffering by disabling the pty's \"canonical
mode\".

  (clojure.java.process/exec {:in :inherit} \"stty\" \"-icanon\")
  ;; Note clojure.java.process requires clojure version 1.12

See also https://unix.stackexchange.com/questions/643777/is-there-any-limit-on-line-length-when-pasting-to-a-terminal-in-linux"
  (interactive (list (or (unless current-prefix-arg
                           inf-clojure-custom-startup)
                         (completing-read "Select Clojure REPL startup command: "
                                          (mapcar #'cdr inf-clojure-startup-forms)
                                          nil
                                          'confirm-after-completion))))
  (let (prog args host port)
    (if (consp cmd)
        (progn
          (setq host (car cmd))
          (setq port (cdr cmd))
          (message "Connecting to Clojure REPL at `%s:%d'" host port))
      (let ((cmdlist (split-string-shell-command cmd)))
        (setq prog (car cmdlist))
        (setq args (cdr cmdlist)))
      (message "Starting Clojure REPL with `%s'" cmd))
    (let* (
           (project-dir (inf-clojure--project-dir))
           (default-directory (or project-dir default-directory))
           (repl-name (inf-clojure--generate-repl-name2 "inf-clojure" project-dir))
           ;; (repl-name "*inf-clojure*")

           )
      (setq default-directory (or (inf-clojure--project-dir) default-directory))
      (let ((process-connection-type inf-clojure-process-connection-type)
            (repl-name "*inf-clojure*"))

        (with-current-buffer

            (apply #'make-comint-in-buffer
                                    repl-name
                                    repl-name
                                    (or prog
                                        (cons host port))
                                    nil ; startfile
                                    args)

          (inf-clojure-mode)
          (hack-dir-local-variables-non-file-buffer)

          (setq-local inf-clojure-repl-info
                      (if prog
                          `((cmd . ,(cons prog args))
                            (pid . ,(process-id (get-buffer-process (current-buffer)))))
                        `((host . ,host)
                          (port . ,port))))
          (setq inf-clojure-buffer (current-buffer))
          (set-syntax-table (pcase (car (inf-clojure--get-preferred-major-modes))
                              ('clojure-ts-mode clojure-ts-mode-syntax-table)
                              (_ clojure-mode-syntax-table)))
          (if inf-clojure-repl-use-same-window
              (pop-to-buffer-same-window (current-buffer))
            (pop-to-buffer (current-buffer)))
          (current-buffer))))))



;;;###autoload
(defun inf-clojure-connect (host port)
  "Connect to a REPL server listening at HOST on tcp PORT.

Create a new REPL buffer named like *inf-clojure* and activate `inf-clojure-mode'."
  (interactive "shost: \nnport: ")
  (inf-clojure (cons host port)))

(defvar-local inf-clojure-socket-callback nil
  "Used to transfer state between the socket process buffer & REPL buffer.")

(defvar-local inf-clojure-socket-buffer nil
  "Used to kill the associated socket buffer when its REPL buffer is killed.")

(defun inf-clojure-socket-filter (process output)
  "A filter that gets triggered each time the socket receives new OUTPUT.
This function prints out the output received but also watches for a
prompt or other output using the `inf-clojure-socket-ready-regexp', once
this happens a callback is triggered if available. The callback is
intended to be used to trigger a `inf-clojure-connect' once we can
determine that a socket REPL is ready to receive a connection.

PROCESS is the process object that is being filtered.

OUTPUT is the latest data received from the process"
  (message "RUN FILTER!!!!")
  (let ((server-buffer (process-buffer process)))
    (when (buffer-live-p server-buffer)
      (with-current-buffer server-buffer
        (insert output)))
    (when (string-match inf-clojure-socket-ready-regexp output)
      (with-current-buffer server-buffer
	(when inf-clojure-socket-callback
	  (funcall inf-clojure-socket-callback))))))

(defun inf-clojure-socket-repl-sentinel (process _event)
  "Ensures socket REPLs are cleaned up when the REPL buffer is closed.

PROCESS is the process object that is connected to a socket REPL.

EVENT is the event that triggered this function to be called."
  (when (not (process-live-p process))
    (let ((repl-buffer (process-buffer process)))
      (with-current-buffer repl-buffer
        (when inf-clojure-socket-buffer
          (kill-buffer inf-clojure-socket-buffer))))))

(defvar inf-clojure-socket-repl-startup-forms
  '((lein . "JVM_OPTS='-Dclojure.server.repl={:port %d :accept clojure.core.server/repl}' lein repl")
    (boot . "export BOOT_JVM_OPTIONS='-Dclojure.server.repl=\"{:port %d :accept clojure.core.server/repl}\"' boot repl")
    (clojure . "clojure -J-Dclojure.server.repl=\"{:port %d :accept clojure.core.server/repl}\"")
    (cljs . "clojure -J-Dclojure.server.repl=\"{:port %d :accept cljs.server.browser/repl}\"")
    (planck . "planck -n %d")
    (babashka . "bb socket-repl %d")))

(defcustom inf-clojure-socket-repl-port
  nil
  "TCP port number rendered into the chosen
`inf-clojure-socket-repl-startup-forms' template, used by
`inf-clojure-socket-repl'. If left as nil, a random port will be selected
between 15000-25000."
  :type '(choice integer (const nil))
  :package-version '(inf-clojure . "3.3.0"))

;;;###autoload
(defun inf-clojure-socket-repl (cmd)
  "Start a clojure subprocess and open a new buffer connected to its REPL via TCP.

CMD is the command line instruction used to start the socket REPL. If it
contains a %d format pattern, that will be replaced by the specified or
randomly generated `inf-clojure-socket-repl-port' number. Set
`inf-clojure-custom-startup' or choose from the defaults provided in
`inf-clojure-socket-repl-startup-forms'.


Details and quirks:

`inf-clojure-socket-repl' creates a new buffer named like
*inf-clojure-server*, starts a clojure server process in it, and then
waits for the process to begin serving a REPL over TCP. The standard way
to make clojure start this \"socket REPL\" is to pass an additional
system property to the JVM. The command is as follows and available as
one of the choices presented by `inf-clojure-socket-repl':

  clojure -J-Dclojure.server.repl=\"{:port %d :accept clojure.core.server/repl}\"

`inf-clojure-socket-repl' waits for the server to begin accepting
connections by assuming it'll print a string to its stdout matching
`inf-clojure-socket-ready-regexp' and waiting for such a match. Then, it
uses `inf-clojure-connect' to open a REPL buffer, named like
*inf-clojure* and connects it to the REPL at localhost on the specified
`inf-clojure-socket-repl-port'. The *inf-clojure* buffer runs
`inf-clojure-mode', which is derived from `comint-mode', and uses
comint's networking feature by which a comint session interacts with
remote I/O streams over TCP. REPL input is sent directly to the
server-side TCP socket without passing through a pty device, so this
socket REPL implementation is not affected by pty line buffering and
line length issues like the buffer-local REPL of `inf-clojure' can be.

The clojure server process actually runs 2 REPLs by default. It starts
the socket REPL first, then loads user.clj, and finally starts the
standard, console-local `clojure.main/repl' connected to
stdin/stdout/stderr. These file descriptors are connected to the
*inf-clojure-server* buffer, but interaction is primitive since that
buffer is in Fundamental mode. You can view output sent to stdout/stderr
there. If you need to send input to stdin, say because your clojure
program is blocked on (.read System/in) or trying to read a password
from the pty, you can switch to the *inf-clojure-server* buffer and then
use M-x eval-expression to run something like:

  (process-send-string (get-buffer-process (current-buffer)) \":hello-there\\n\")

The first prompt printed to stdout by the `clojure.main/repl' is in fact
what the default `inf-clojure-socket-ready-regexp' looks for to indicate
startup is complete and that the `clojure.server.repl' is up. If your
clojure program starts in a different way, such that it does not start
the `clojure.main/repl' printing to stdout, you may need to make it
print something else once a socket repl is being served, and adjust
`inf-clojure-socket-ready-regexp' accordingly.

Note that when you interact with the socket REPL over the TCP connection
from the *inf-clojure* buffer, your REPL environment has bound clojure
vars *in*, *out*, and *err* to the TCP connection on the server. These
are _not_ the same as stdin/stdout/stderr . That's why a (println \"in
the socket repl\") sends output right back to you in the *inf-clojure*
buffer, but a (.println System/out \"goes to\") disappears to the
*inf-clojure-server* buffer."
  (interactive (list (or (unless current-prefix-arg
                           inf-clojure-custom-startup)
                         (completing-read "Select Clojure socket REPL startup command: "
                                          (mapcar #'cdr inf-clojure-socket-repl-startup-forms)
                                          nil
                                          'confirm-after-completion))))
  (let* ((host "localhost")
         (port (or inf-clojure-socket-repl-port (+ 15000 (random 10000))))
         (project-dir (inf-clojure--project-dir))
         (server-name (inf-clojure--generate-repl-name "inf-clojure-server"
                                                       project-dir
                                                       nil
                                                       host
                                                       port))
         (server-buffer (generate-new-buffer " inf-clojure-tmp"))
         (server-cmd (format cmd port))
         (server (let ((default-directory (or project-dir default-directory))
                       (process-connection-type inf-clojure-process-connection-type))
                   (start-file-process-shell-command server-name server-buffer server-cmd))))
    (with-current-buffer server-buffer
      (rename-buffer (format "*%s*"
                             (inf-clojure--generate-repl-name
                              "inf-clojure-server"
                              project-dir
                              (process-id (get-buffer-process (current-buffer)))
                              host
                              port)))
      (setq-local
       inf-clojure-socket-callback
       (lambda ()
         (let* ((created-repl-buffer (inf-clojure-connect host port)))
           (with-current-buffer (get-buffer created-repl-buffer)
             (setq-local inf-clojure-socket-buffer server-buffer)
             (set-process-sentinel
              (get-buffer-process (current-buffer))
              #'inf-clojure-socket-repl-sentinel))))))
    (set-process-filter server #'inf-clojure-socket-filter)
    (message "Starting clojure socket REPL server at %s:%d with %s" host port server-cmd)
    (if inf-clojure-repl-use-same-window
        (pop-to-buffer-same-window server-buffer)
      (pop-to-buffer server-buffer))))

(defun inf-clojure-eval-region (start end &optional and-go)
  "Send the current region to the inferior Clojure process.
Sends substring between START and END.  Prefix argument AND-GO
means switch to the Clojure buffer afterwards.
When `inf-clojure-eval-ns-aware' is non-nil, the code is evaluated
in the context of the buffer's namespace."
  (interactive "r\nP")
  (let* ((str (buffer-substring-no-properties start end)))
    (comint-simple-send (inf-clojure-proc) str))
  (when and-go (inf-clojure-switch-to-repl t)))

(defun inf-clojure-eval-string (code)
  "Send the string CODE to the inferior Clojure process to be executed."
  (comint-simple-send (inf-clojure-proc) code))

(defun inf-clojure--defun-at-point (&optional bounds)
  "Return text or range of defun at point.
If BOUNDS is truthy return a dotted pair of beginning and end of
current defun else return the string."
  (save-excursion
    (end-of-defun)
    (let ((end (point))
          (case-fold-search t)
          (func (if bounds #'cons #'buffer-substring-no-properties)))
      (beginning-of-defun-raw)
      (funcall func (point) end))))

(defun inf-clojure-eval-defun (&optional and-go)
  "Send the current defun to the inferior Clojure process.
Prefix argument AND-GO means switch to the Clojure buffer afterwards."
  (interactive "P")
  (save-excursion
    (let ((bounds (inf-clojure--defun-at-point t)))
     (inf-clojure-eval-region (car bounds) (cdr bounds) and-go))))

(defun inf-clojure-eval-buffer (&optional and-go)
  "Send the current buffer to the inferior Clojure process.
Prefix argument AND-GO means switch to the Clojure buffer afterwards."
  (interactive "P")
  (save-excursion
    (widen)
    (let ((case-fold-search t))
      (inf-clojure-eval-region (point-min) (point-max) and-go))))

(defun inf-clojure-eval-last-sexp (&optional and-go)
  "Send the previous sexp to the inferior Clojure process.
Prefix argument AND-GO means switch to the Clojure buffer afterwards."
  (interactive "P")
  (inf-clojure-eval-region (save-excursion (backward-sexp) (point)) (point) and-go))

(defun inf-clojure-eval-form-and-next ()
  "Send the previous sexp to the inferior Clojure process and move to the next one."
  (interactive)
  (while (not (zerop (car (syntax-ppss))))
    (up-list))
  (inf-clojure-eval-last-sexp)
  (forward-sexp))

(defun inf-clojure-insert-and-eval (form)
  "Insert FORM into process and evaluate.
Indent FORM.  FORM is expected to have been trimmed."
  (let ((clojure-process (inf-clojure-proc)))
    ;; ensure the repl buffer scrolls. See similar fix in CIDER:
    ;; https://github.com/clojure-emacs/cider/pull/2590
    (with-selected-window (or (get-buffer-window inf-clojure-buffer)
                              (selected-window))
      (with-current-buffer (process-buffer clojure-process)
        (comint-goto-process-mark)
        (let ((beginning (point)))
          (insert form)
          (let ((end (point)))
            (goto-char beginning)
            (indent-sexp end)
            ;; font-lock the inserted code
            (font-lock-ensure beginning end)
            (goto-char end)))
        (comint-send-input t t)))))

(defun inf-clojure-insert-defun ()
  "Send current defun to process."
  (interactive)
  (inf-clojure-insert-and-eval (string-trim (inf-clojure--defun-at-point))))

(defun inf-clojure-insert-last-sexp ()
  "Send last sexp to process."
  (interactive)
  (inf-clojure-insert-and-eval
   (buffer-substring-no-properties (save-excursion (backward-sexp) (point))
                                   (point))))




;;;; Misc
;;;; ========

(defun inf-clojure-display-version ()
  "Display the current `inf-clojure' version in the minibuffer."
  (interactive)
  (message "inf-clojure (version %s)" inf-clojure-version))

(provide 'inf-clojure)

;; Local variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; inf-clojure.el ends here
