;;; ediprolog.el --- Emacs Does Interactive Prolog

;; Copyright (C) 2006, 2007, 2008, 2009, 2012, 2013, 2016, 2017  Markus Triska

;; Author: Markus Triska <triska@metalevel.at>
;; Keywords: languages, processes
;; Homepage: https://www.metalevel.at/ediprolog/

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; These definitions let you interact with SWI-Prolog in all buffers.
;; You can consult Prolog programs and evaluate embedded queries.

;; Installation
;; ============
;;
;; Copy ediprolog.el to your load-path and add to your .emacs:
;;
;;     (require 'ediprolog)
;;     (global-set-key [f10] 'ediprolog-dwim)
;;
;; Restart Emacs and customize ediprolog with
;;
;;     M-x customize-group RET ediprolog RET
;;

;; Usage
;; =====
;;
;; The central function is `ediprolog-dwim' (Do What I Mean), which is
;; bound to F10 by the snippet above. Depending on the content at
;; point, `ediprolog-dwim' does the "appropriate" thing: If point is
;; on a query, F10 sends the query to a Prolog process, and you
;; interact with the process in the current buffer as on a terminal.
;; Queries start with "?-" or ":-", possibly preceded by "%" and
;; whitespace. An example of a query is (without leading ";;"):
;;
;;   %?- member(X, [a,b,c]).
;;
;; If you press F10 when point is on that query, you get:
;;
;;   %?- member(X, [a,b,c]).
;;   %@ X = a ;
;;   %@ X = b ;
;;   %@ X = c ;
;;   %@ false.
;;
;; When waiting for output of the Prolog process, you can press C-g to
;; unblock Emacs and continue with other work. To resume interaction
;; with the Prolog process, use M-x ediprolog-toplevel RET.

;; If you press F10 when point is *not* on a query, the buffer content
;; is consulted in the Prolog process, and point is moved to the first
;; error (if any). In transient mark mode, if the region is active,
;; only the text in the region is consulted.

;; For convenience, the most recent interactions with the Prolog
;; process are logged in the buffer "*ediprolog-history*".

;; Use M-x ediprolog-localize RET to make any Prolog process started
;; in the current buffer buffer-local. This way, you can run distinct
;; processes simultaneously. Revert with M-x ediprolog-unlocalize RET.

;; `ediprolog-dwim' with prefix arguments has special meanings:
;;
;;   C-0 F10       kill Prolog process
;;   C-1 F10       always consult buffer (even when point is on a query)
;;   C-2 F10       always consult buffer, using a new process
;;   C-7 F10       equivalent to `ediprolog-toplevel'
;;   C-u F10       first consult buffer, then evaluate query (if any)
;;   C-u C-u F10   like C-u F10, with a new process

;; Tested with SWI-Prolog 7.3.21 + Emacs 22.1, 23.4, 24.5, 25.1 and 26.0

;;; Code:

(defconst ediprolog-version "1.2")

(defgroup ediprolog nil
  "Transparent interaction with SWI-Prolog."
  :group 'languages
  :group 'processes)

(defcustom ediprolog-program
  (or (executable-find "swipl") (executable-find "pl") "swipl")
  "Program name of the Prolog executable."
  :group 'ediprolog
  :type 'string)

(defcustom ediprolog-program-switches nil
  "List of switches passed to the Prolog process. Example:
'(\"-G128M\" \"-O\")"
  :group 'ediprolog
  :type '(repeat string))

(defcustom ediprolog-prefix "%@ "
  "String to prepend when inserting output from the Prolog
process into the buffer."
  :group 'ediprolog
  :type 'string)

(defcustom ediprolog-max-history 80000
  "Maximal size of history buffers storing recent interactions, or
nil to never truncate the history."
  :group 'ediprolog
  :type 'sexp)

(defvar ediprolog-process               nil "A Prolog process.")

(defvar ediprolog-temp-buffer           nil
  "Buffer that temporarily saves process output ")

(defvar ediprolog-seen-prompt           nil
  "Whether a prompt was (recently) emitted by the Prolog process.")

(defvar ediprolog-read-term             nil
  "Whether the Prolog process waits for the user to enter a term.")

(defvar ediprolog-indent-prefix         ""
  "Any whitespace occurring before the most recently executed query.")

(defvar ediprolog-temp-file             nil
  "File name of a temporary file used for consulting the buffer.")

(defvar ediprolog-prompt "?ediprolog- "
  "Prompt used in the Prolog session. It must differ from the
default Prolog prompt.")

(defvar ediprolog-consult-buffer "*ediprolog-consult*"
  "Buffer used to display consult output.")

(defvar ediprolog-consult-window        nil
  "Window used to show consult output.")

(defvar ediprolog-history-buffer        nil
  "Buffer that stores recent interactions.")

(defvar ediprolog-interrupted           nil
  "True iff waiting for the previous query was interrupted with C-g.")

(defmacro ediprolog-wait-for-prompt-after (&rest forms)
  "Evaluate FORMS and wait for prompt."
  `(progn
     (setq ediprolog-seen-prompt nil)
     (ediprolog-ensure-buffer "temp")
     (with-current-buffer ediprolog-temp-buffer
       (let (buffer-read-only)
         (erase-buffer)))
     ;; execute forms with default-directory etc. from invocation buffer
     ,@forms
     (while (not ediprolog-seen-prompt)
       ;; Wait for output/sentinel and update consult window, if any.
       ;; As `accept-process-output' does not run the sentinel in
       ;; Emacs <= 23.1, we use `sit-for' to do both. However,
       ;; `sit-for' returns immediately if keyboard input is
       ;; available, so we must discard input.
       (discard-input)
       (sit-for 0.1))))

(defmacro ediprolog-remember-interruption (form)
  "Set `ediprolog-interrupted' if evaluation of FORM was interrupted."
  `(condition-case nil
       ,form
     (quit (setq ediprolog-interrupted t))))

;; Only the sentinel can reliably detect if no more output follows -
;; even if process-status is 'exit, further output can still follow.
(defun ediprolog-sentinel (proc str)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((status (with-temp-buffer
                      (insert str)
                      (while (search-backward "\n" nil t)
                        (replace-match ""))
                      (buffer-string))))
        (ediprolog-log
         (format "%s: %s.\n"
                 (substring (current-time-string) 4 -5) status) "green" t))
      (when (string-match "^\\(?:finished\n\\|exited abnormally\\|killed\n\\)"
                          str)
        (setq ediprolog-seen-prompt t)))))

(defun ediprolog-ensure-buffer (name)
  (let ((str (format "*ediprolog-%s*" name))
        (var (intern (format "ediprolog-%s-buffer" name))))
    (unless (buffer-live-p (symbol-value var))
      (set var (generate-new-buffer str))
      (with-current-buffer (symbol-value var)
        (buffer-disable-undo)
        (setq buffer-read-only t)))))

(defun ediprolog-log (str &optional col nl)
  (ediprolog-ensure-buffer "history")
  (with-current-buffer ediprolog-history-buffer
    (let (buffer-read-only)
      (goto-char (point-max))
      (let ((s (format "%s%s" (if (and nl (not (bolp))) "\n" "") str)))
        (insert (if col (propertize s 'face `(:background ,col)) s)))
      (let ((size (- (point-max) (point-min))))
        (when (and ediprolog-max-history
                   (> size ediprolog-max-history))
          ;; delete older half of the (possibly narrowed) history
          (delete-region (point-min) (+ (point-min) (/ size 2))))))))

(defun ediprolog-run-prolog ()
  "Start a Prolog process."
  (let ((args (cons ediprolog-program ediprolog-program-switches)))
    (ediprolog-log (format "%s: starting: %S\n"
                           (substring (current-time-string) 4 -5) args)
                   "green" t)
    (condition-case nil
        (ediprolog-wait-for-prompt-after
         (setq ediprolog-process
               (apply #'start-process "ediprolog" (current-buffer) args))
         (set-process-sentinel ediprolog-process 'ediprolog-sentinel)
         (set-process-filter ediprolog-process
                             'ediprolog-wait-for-prompt-filter)
         (ediprolog-send-string
          (format "set_prolog_flag(color_term, false),\
                  '$set_prompt'('%s').\n" ediprolog-prompt)))
      ((error quit)
       (ediprolog-log "No prompt found." "red" t)
       (error "No prompt from: %s" ediprolog-program)))))

(defun ediprolog-kill-prolog ()
  "Kill the Prolog process and run the process sentinel."
  (when (ediprolog-running)
    (delete-process ediprolog-process)))

(defun ediprolog-show-consult-output (str)
  (with-current-buffer (get-buffer-create ediprolog-consult-buffer)
    (setq buffer-read-only t)
    (let (buffer-read-only)
      (erase-buffer)
      (insert str)
      (goto-char (point-min))
      ;; remove normal consult status lines, which start with "%" 
      (while (re-search-forward "^[\t ]*%.*\n" nil t)
        (delete-region (match-beginning 0) (match-end 0))))
    (setq str (buffer-string)))
  ;; show consult output in a separate window unless it is a prefix of
  ;; success (i.e., consulted without errors), or still an incomplete
  ;; line that starts with a comment character
  (unless (or (string-match "^[\t ]*\\(?:%.*\\)?\\'" str)
              (let ((success "true."))
                (and (<= (length str) (length success))
                     (string= str (substring success 0 (length str))))))
    (setq ediprolog-consult-window (display-buffer ediprolog-consult-buffer))
    (set-window-dedicated-p ediprolog-consult-window t)
    (fit-window-to-buffer ediprolog-consult-window (/ (frame-height) 2))))

(defun ediprolog-consult-filter (proc str)
  "Filter used when consulting a file, showing consult output."
  (with-current-buffer (ediprolog-temp-buffer proc)
    (goto-char (point-max))
    (let (buffer-read-only)
      (insert str))
    (with-current-buffer (process-buffer proc)
      (ediprolog-log str))
    (when (re-search-backward
           (format "^%s" (regexp-quote ediprolog-prompt)) nil t)
      (with-current-buffer (process-buffer proc)
        (setq ediprolog-seen-prompt t)))
    (skip-chars-backward "\n")
    (ediprolog-show-consult-output (buffer-substring (point-min) (point)))))

(defun ediprolog-wait-for-prompt-filter (proc str)
  "Filter that only waits until prompt appears."
  (with-current-buffer (ediprolog-temp-buffer proc)
    (goto-char (point-max))
    (let (buffer-read-only)
      (insert str))
    (with-current-buffer (process-buffer proc)
      (ediprolog-log str))
    (when (re-search-backward
           (format "^%s" (regexp-quote ediprolog-prompt)) nil t)
      (with-current-buffer (process-buffer proc)
        (setq ediprolog-seen-prompt t)))))


;;;###autoload
(defun ediprolog-dwim (&optional arg)
  "Load current buffer into Prolog or post query (Do What I Mean).
If invoked on a line starting with `:-' or `?-', possibly
preceded by `%' and whitespace, call `ediprolog-interact' with
the query as argument. Otherwise, call `ediprolog-consult'.

With prefix argument 0, kill the Prolog process. With prefix 1,
equivalent to `ediprolog-consult'. With prefix 2, equivalent to
`ediprolog-consult' with a new Prolog process. With prefix 7,
equivalent to `ediprolog-toplevel'. With just C-u, first call
`ediprolog-consult' and then, if point is on a query, call
`ediprolog-interact' with it as argument. Analogously, C-u C-u
for `ediprolog-consult' with a new process. With other prefix
arguments, equivalent to `ediprolog-remove-interactions'."
  (interactive "P")
  (cond ((eq arg 0)
         (unless (ediprolog-running)
           (error "No Prolog process running"))
         (ediprolog-kill-prolog)
         (message "Prolog process killed."))
        ((eq arg 1) (ediprolog-consult))
        ((eq arg 2) (ediprolog-consult t))
        ((eq arg 7)
         (unless (ediprolog-more-solutions)
           (error "No query in progress"))
         (ediprolog-toplevel))
        ((equal arg '(4)) (ediprolog-consult) (ediprolog-query))
        ((equal arg '(16)) (ediprolog-consult t) (ediprolog-query))
        ((null arg) (unless (ediprolog-query) (ediprolog-consult)))
        (t (ediprolog-remove-interactions))))

(defun ediprolog-process-ready ()
  "Error if the previous query is still in progress."
  (when (and ediprolog-interrupted
             (ediprolog-running)
             (ediprolog-more-solutions))
    (error "Previous query still in progress, see `ediprolog-toplevel'"))
  (setq ediprolog-interrupted nil))

(defun ediprolog-query ()
  "If point is on a query, send it to the process and start interaction."
  (ediprolog-process-ready)
  (when (and (not (and transient-mark-mode mark-active))
             (save-excursion
               (beginning-of-line)
               (looking-at "\\([\t ]*\\)%*[\t ]*[:?]-")))
    ;; whitespace preceding the query is the indentation level
    (setq ediprolog-indent-prefix (match-string 1))
    (let* ((from (goto-char (match-end 0)))
           (to (if (re-search-forward "\\.[\t ]*\\(?:%.*\\)?$" nil t)
                   ;; omit trailing whitespace
                   (+ (point) (skip-chars-backward "\t "))
                 (error "Missing `.' at the end of this query")))
           (query (buffer-substring-no-properties from to))
           (handle (and (fboundp 'prepare-change-group)
                        (fboundp 'undo-amalgamate-change-group)
                        (cons t (prepare-change-group)))))
      (end-of-line)
      (insert "\n" ediprolog-indent-prefix ediprolog-prefix)
      (ediprolog-interact
       (format "%s\n" (mapconcat #'identity
                                 ;; `%' can precede each query line
                                 (split-string query "\n[ \t%]*") " ")))
      (when handle
        (undo-amalgamate-change-group (cdr handle))))
    t))

;;;###autoload
(defun ediprolog-interact (query)
  "Send QUERY to Prolog process and interact as on a terminal.

You can use \\[keyboard-quit] to unblock Emacs in the case of
longer-running queries. When the query completes and the toplevel
asks for input, use \\[ediprolog-toplevel] to resume interaction
with the Prolog process."
  (unless (ediprolog-running)
    (ediprolog-run-prolog))
  (set-marker (process-mark ediprolog-process) (point))
  (set-process-buffer ediprolog-process (current-buffer))
  (set-process-filter ediprolog-process 'ediprolog-interact-filter)
  (ediprolog-ensure-buffer "temp")
  (with-current-buffer ediprolog-temp-buffer
    (let (buffer-read-only)
      (erase-buffer)))
  (setq ediprolog-seen-prompt nil
        ediprolog-read-term nil)
  (ediprolog-send-string query)
  (ediprolog-toplevel))

(defun ediprolog-send-string (str)
  "Send string to Prolog process and log it."
  (ediprolog-log str "cyan")
  (process-send-string ediprolog-process str))

(defun ediprolog-toplevel ()
  "Start or resume Prolog toplevel interaction in the buffer.

You can use this function if you have previously quit (with
\\[keyboard-quit]) waiting for a longer-running query and now
want to resume interaction with the toplevel."
  (interactive)
  (when ediprolog-process
    (select-window (display-buffer (process-buffer ediprolog-process))))
  (ediprolog-remember-interruption
   (while (ediprolog-more-solutions)
     (let (str
           char)
       ;; poll for user input; meanwhile, process output can arrive
       (while (and (ediprolog-more-solutions) (null str))
         (goto-char (process-mark ediprolog-process))
         (if ediprolog-read-term
             (progn
               (setq str (concat (read-string "Input: ") "\n"))
               (ediprolog-insert-at-marker
                str ediprolog-indent-prefix ediprolog-prefix)
               (setq ediprolog-read-term nil))
           (condition-case nil
               (when (setq char (if (>= emacs-major-version 22)
                                    (read-char nil nil 0.1)
                                  (with-timeout (0.1 nil)
                                    (read-char))))
                 ;; char-to-string might still yield an error (C-0 etc.)
                 (setq str (char-to-string char)))
             (error
              (message "Non-character key")
              ;; non-character keys must not remain in the input
              ;; buffer, lest `read-char' return immediately
              (discard-input)))))
       (when (ediprolog-more-solutions)
         (if (eq char ?\C-c)            ; char can be nil too
             ;; sending C-c directly yields strange SWI buffering
             (interrupt-process ediprolog-process)
           (ediprolog-send-string str)))))))

;;;###autoload
(defun ediprolog-remove-interactions ()
  "Remove all lines starting with `ediprolog-prefix' from buffer.

In transient mark mode, if the region is active, the function
operates on the region."
  (interactive)
  (save-excursion
    (save-restriction
      (when (and transient-mark-mode mark-active)
        (narrow-to-region (region-beginning) (region-end)))
      (goto-char (point-min))
      (flush-lines (concat "^[\t ]*" (regexp-quote ediprolog-prefix)))))
  (message "Interactions removed."))


;;;###autoload
(defun ediprolog-consult (&optional new-process)
  "Buffer is loaded into a Prolog process. If NEW-PROCESS is
non-nil, start a new process. Otherwise use the existing process,
if any. In case of errors, point is moved to the position of the
first error, and the mark is left at the previous position.

In transient mark mode, if the region is active, the function
operates on the region."
  (interactive)
  (when (string= (buffer-name) ediprolog-consult-buffer)
    (error "Cannot consult the consult buffer"))
  (when (window-live-p ediprolog-consult-window)
    (condition-case nil
        ;; deleting the window can still raise an error, if the window
        ;; was the only window in the frame and the consult buffer was
        ;; killed (and it thus displays a different buffer now)
        (delete-window ediprolog-consult-window)
      (error nil)))
  (when (buffer-live-p ediprolog-consult-buffer)
    (bury-buffer ediprolog-consult-buffer))
  (when new-process
    (ediprolog-kill-prolog))
  (unless (ediprolog-running)
    (ediprolog-run-prolog))
  (ediprolog-process-ready)
  (set-process-buffer ediprolog-process (current-buffer))
  (unless ediprolog-temp-file
    (setq ediprolog-temp-file (make-temp-file "ediprolog")))
  (let ((start (if (and transient-mark-mode mark-active)
                   (region-beginning) (point-min)))
        (end (if (and transient-mark-mode mark-active)
                 (region-end) (point-max))))
    (write-region start end ediprolog-temp-file nil 'silent))
  (set-process-filter ediprolog-process 'ediprolog-consult-filter)
  (ediprolog-remember-interruption
   (ediprolog-wait-for-prompt-after
    (ediprolog-send-string (format "['%s'].\n" ediprolog-temp-file))))
  (message "%s consulted." (if (and transient-mark-mode mark-active)
                               "Region" "Buffer"))
  ;; go to line of the first error, if any
  (let ((line (with-current-buffer ediprolog-temp-buffer
                (when (save-excursion
                        (goto-char (point-min))
                        (re-search-forward "^ERROR.*?:\\([0-9]+\\)" nil t))
                  (string-to-number (match-string 1))))))
    (when line
      (if (and transient-mark-mode mark-active)
          (when (fboundp 'line-number-at-pos)
            (goto-line (+ (line-number-at-pos (region-beginning)) line -1)))
        (goto-line line)))))

(defun ediprolog-running ()
  "True iff `ediprolog-process' is a running process."
  (and (processp ediprolog-process)
       (eq (process-status ediprolog-process) 'run)))

(defun ediprolog-more-solutions ()
  "True iff there could be more solutions from the process."
  (not ediprolog-seen-prompt))

(defun ediprolog-interact-filter (proc string)
  "Insert output from the process and update the state."
  (when (and (buffer-live-p (ediprolog-temp-buffer proc))
             (buffer-live-p (process-buffer proc)))
    (let (str)
      (with-current-buffer (ediprolog-temp-buffer proc)
        (goto-char (point-max))
        (let (buffer-read-only)
          (insert string))
        (with-current-buffer (process-buffer proc)
          (ediprolog-log string))
        ;; read a term from the user?
        (when (re-search-backward "^|: $" nil t)
          (with-current-buffer (process-buffer proc)
            (setq ediprolog-read-term t))
          (setq str (buffer-string))
          (let (buffer-read-only)
            (erase-buffer)))
        ;; check for prompt
        (goto-char (point-max))
        (when (re-search-backward
               (format "^%s" (regexp-quote ediprolog-prompt)) nil t)
          (with-current-buffer (process-buffer proc)
            (setq ediprolog-seen-prompt t))
          ;; ignore further output due to accidental user input (C-j,
          ;; C-m, etc.) while the query was running
          (set-process-filter proc 'ediprolog-ignore-filter)
          (skip-chars-backward "\n")
          (setq str (buffer-substring (point-min) (point))))
        (unless str
          (goto-char (point-max))
          ;; delay final line if it can still be completed to prompt
          (let ((l (buffer-substring (line-beginning-position) (point))))
            (when (and (<= (length l) (length ediprolog-prompt))
                       (string= l (substring ediprolog-prompt 0 (length l))))
              (goto-char (line-beginning-position))))
          ;; delay emitting newlines until we are sure no prompt
          ;; follows; one or two newlines can precede a prompt
          (let ((d (abs (skip-chars-backward "\n"))))
            (when (> d 2)
              (forward-char (- d 2))))
          (setq str (buffer-substring (point-min) (point)))
          (let (buffer-read-only)
            (delete-region (point-min) (point))))
        (when str
          (with-temp-buffer
            ;; precede each line with ediprolog prefices
            (insert str)
            (goto-char (point-min))
            (while (search-forward "\n" nil t)
              (replace-match
               (format "\n%s%s" (with-current-buffer (process-buffer proc)
                                  ediprolog-indent-prefix) ediprolog-prefix)))
            (setq str (buffer-string)))
          (with-current-buffer (process-buffer proc)
            (let ((near (<= (abs (- (point) (process-mark proc))) 1)))
              (ediprolog-insert-at-marker str)
              (when near
                ;; catch up with output if point was reasonably close
                (goto-char (process-mark proc))))))))))


(defun ediprolog-insert-at-marker (&rest args)
  "Insert strings ARGS at marker and update the marker."
  (save-excursion
    (goto-char (process-mark ediprolog-process))
    (end-of-line)
    (apply #'insert args)
    (set-marker (process-mark ediprolog-process) (point))))

(defun ediprolog-ignore-filter (proc str)
  "Log and then ignore all process output."
  (with-current-buffer (process-buffer proc)
    (ediprolog-log str "gray")))

(defun ediprolog-temp-buffer (proc)
  (with-current-buffer (process-buffer proc)
    ;; temp buffer can be buffer local
    ediprolog-temp-buffer))

(defun ediprolog-map-variables (func)
  "Call FUNC with all ediprolog variables that can become buffer-local."
  (mapc func '(ediprolog-process
               ediprolog-program
               ediprolog-program-switches
               ediprolog-temp-buffer
               ediprolog-history-buffer
               ediprolog-seen-prompt
               ediprolog-interrupted
               ediprolog-read-term
               ediprolog-indent-prefix
               ediprolog-temp-file)))

;;;###autoload
(defun ediprolog-localize ()
  "After `ediprolog-localize', any Prolog process started from
this buffer becomes buffer-local."
  (interactive)
  (unless (local-variable-p 'ediprolog-process)
    (ediprolog-map-variables #'make-local-variable)
    (setq ediprolog-temp-file nil
          ediprolog-process nil
          ediprolog-history-buffer nil
          ediprolog-temp-buffer nil)))

(defun ediprolog-unlocalize ()
  "Revert the effect of `ediprolog-localize'."
  (interactive)
  (when (local-variable-p 'ediprolog-process)
    (ediprolog-kill-prolog)
    (ediprolog-map-variables #'kill-local-variable)))

(provide 'ediprolog)

;;; ediprolog.el ends here
