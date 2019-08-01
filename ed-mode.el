;;; ed-mode.el --- ed for Emacs.

;; Copyright © 2015 the ed-mode contributors

;; This file is not part of GNU Emacs.

;; This file is free software.
;; Permission to use, copy, modify, and distribute this software for
;; any purpose with or without fee is hereby granted, provided that
;; the above copyright notice and this permission notice appear in all
;; copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
;; WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
;; AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR
;; CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
;; OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
;; NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
;; CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

;; Author: Antti Nykänen <aon@iki.fi>
;; Maintainer: Ryan Prior <ryanprior@gmail.com>
;; URL: https://github.com/ryanprior/ed-mode
;; ORIGIN: http://aon.iki.fi/files/emacsed/emacsed.el
;; Keywords: emulation

;;; Commentary:

;; ed-mode lets you interact with Emacs buffers like you would do with
;; the ed editor.

;; Make sure that this file is somewhere along your load-path, then
;; add (require 'ed) to your init file.

;; Use ed by opening a file like you'd normally do, and then do:
;; M-x ed RET

;;; Code:

(defgroup ed nil
  "ed for Emacs."
  :prefix "ed-"
  :group 'emulations
  :link '(url-link :tag "Github" "https://github.com/ryanprior/ed-mode"))

(defvar ed-last-error-message ""
  "The most recent error message.")

(defvar ed-verbose-errors-p nil
  "Whether or not verbose errors should be shown by default.")

(defvar ed-quit-already-attempted-p nil
  "Whether or not a quit has already been attempted.")

(defvar ed-mode-hook nil
  "Hook run when entering ed mode.")

(defvar ed-mode-map (make-sparse-keymap)
  "Keymap for ed mode.")

(define-key ed-mode-map "\r" 'ed-newline)

(defconst ed-cmd-alist
  '(("=" "ed-cmd-linenum")
    ("!" "ed-cmd-exec")
    ("a" "ed-cmd-append")
    ("c" "ed-cmd-change")
    ("d" "ed-cmd-delete")
    ;;("e" "ed-cmd-edit-file")
    ;;("E" "ed-cmd-edit-file-unconditionally")
    ("f" "ed-cmd-file")
    ;;("g" "ed-cmd-global")
    ;;("G" "ed-cmd-global-interactive")
    ("h" "ed-cmd-print-last-error")
    ("H" "ed-cmd-verbose-errors")
    ("i" "ed-cmd-insert")
    ("j" "ed-cmd-join")
    ("k" "ed-cmd-mark")
    ("l" "ed-cmd-unambiguous-print")
    ("m" "ed-cmd-move")
    ("n" "ed-cmd-numbered-print")
    ("p" "ed-cmd-print")
    ("P" "ed-cmd-prompt")
    ("q" "ed-cmd-quit")
    ("Q" "ed-cmd-quit-unconditionally")
    ("r" "ed-cmd-read")
    ("s" "ed-cmd-replace")
    ("t" "ed-cmd-transfer")
    ("u" "ed-cmd-undo")
    ;;("v" "ed-cmd-global-inverse")
    ;;("V" "ed-cmd-global-interactive-inverse")
    ("w" "ed-cmd-write")
    ;;("wq" "ed-cmd-write-and-quit")
    ;;("W" "ed-cmd-write-append")
    ;;("x" "ed-cmd-paste")
    ;;("y" "ed-cmd-copy")
    ;;("z" "ed-cmd-scroll")
    ("#" "ed-cmd-comment")
    )
  "Associated list for ed commands and their respective
functions.")

(defun ed-line-number-at-pos (&optional pos)
  "Return (narrowed) buffer line number at position POS.
If POS is nil, use current buffer location. Provided for
compatibility with Emacs 21."
  (let ((opoint (or pos (point))) start)
    (save-excursion
      (goto-char (point-min))
      (setq start (point))
      (goto-char opoint)
      (forward-line 0)
      (1+ (count-lines start (point))))))

(defun ed-get-line ()
  "Returns the string on the current line."
  (copy-region-as-kill
   (+ (if (and (not ed-is-inserting)
               ed-display-prompt)
          (length ed-prompt)
        0)
      (point-at-bol))
   (point-at-eol))
  (pop kill-ring))

(defun ed-newline ()
  "Insert a newline, executing the command on this line if in
command mode."
  (interactive)
  (ed-exec-line)
  (when (ed-get-ed-buffer)
    (insert "\n")
    (if (and (not ed-is-inserting) ed-display-prompt)
        (insert ed-prompt))))

(defun ed-strtonum (num)
  "Return the line number indicated by the ed address notation in num."
  (with-current-buffer ed-associated-buffer
    (cond
     ((or (equal num "") (equal num "."))
      (ed-line-number-at-pos))
     ((string-match "^/\\(.*\\)/$" num)
      (save-excursion
        (end-of-line)
        (ed-line-number-at-pos (search-forward-regexp
                                (match-string 1 num) nil t))))
     ((string-match "^\\?\\(.*\\)\\?$" num)
      (save-excursion
        (beginning-of-line)
        (ed-line-number-at-pos (search-backward-regexp
                                (match-string 1 num) nil t))))
     ((equal num "$")
      (if (> (ed-line-number-at-pos (point-max)) 1)
          (1+ (count-lines 1 (buffer-size)))
        1))
     ((and (equal (string-to-char num) ?') (= (length num) 2))
      (cdr (assoc (substring num 1) (with-current-buffer (ed-get-ed-buffer)
                                      ed-mark-alist))))
     ((or (equal (string-to-char num) ?+)
          (equal (string-to-char num) ?-))
      (cond
       ((or (equal num "-") (equal num "^"))
        (setq num "-1"))
       ((equal num "+")
        (setq num "+1")))
      (+ (ed-line-number-at-pos) (string-to-number num)))
     (t (string-to-number num)))))

(defun ed-num-extract (string)
  "Return a list with the addresses and the rest of the command
in separate strings."
  (let ((state 0)
        (first-num "")
        (second-num "")
        (rest "")
        (is-searchstr nil)
        (is-mark nil))
    (dolist (chr (string-to-list string))
      (let ((schr (char-to-string chr)))
        (cond
         ((and (< state 2) (= chr ?%))
          (setq first-num "1")
          (setq second-num (number-to-string (ed-strtonum "$")))
          (setq state 2))
         ((and (< state 2) (= chr ?\;))
          (setq first-num (number-to-string (ed-strtonum ".")))
          (setq second-num (number-to-string (ed-strtonum "$")))
          (setq state 2))
         ((and (< state 2) (or (= chr ?/) (= chr ??)))
          (setq is-searchstr (not is-searchstr))
          (if (= state 0)
              (setq first-num (concat first-num schr))
            (setq second-num (concat second-num schr))))
         ((and (< state 2) (equal chr ?'))
          (if (= state 0)
              (setq first-num schr)
            (setq second-num schr))
          (setq is-mark t))
         (is-mark
          (if (= state 0)
              (setq first-num (concat first-num schr))
            (setq second-num (concat second-num schr)))
          (setq is-mark nil))
         ((and (< state 2) (or (> (ed-strtonum schr) 0)
                               (= chr ?0) is-searchstr))
          (if (= state 0)
              (setq first-num (concat first-num schr))
            (setq second-num (concat second-num schr))))
         ((and (= chr ?,) (= state 0))
          (if (equal first-num "")
              (progn (setq first-num "1")
                     (setq second-num (number-to-string (ed-strtonum "$")))
                     (setq state 2))
            (setq state (1+ state))))
         (t (or (= state 2) (setq state 2))
            (setq rest (concat rest schr))))))
    (unless is-searchstr
      (when (and (equal second-num "") (or
                                        (equal (string-to-char first-num) ??)
                                        (equal (string-to-char first-num) ?/)))
        (setq first-num (number-to-string (ed-strtonum first-num)))
        (setq second-num first-num))
      (list first-num second-num rest))))

(defun ed-exec-line ()
  "Run the ed command on this line."
  (let ((line (ed-get-line)))
    (if ed-is-inserting
        (if (equal "." line)
            (progn
              (setq ed-is-inserting nil)
              (with-current-buffer ed-associated-buffer (previous-line 1)))
          (with-current-buffer ed-associated-buffer
            (insert line "\n")))
      (if (< (length line) 1)
          (progn
            (ed-goto-line-print (+ (ed-strtonum ".") 1)))
        (let* ((extracted (ed-num-extract line))
               (start (car extracted))
               (end (nth 1 extracted))
               (command (if (> (length (nth 2 extracted)) 0)
                            (substring (nth 2 extracted) 0 1) ""))
               (args (if (> (length (nth 2 extracted)) 1)
                         (substring (nth 2 extracted) 1) ""))
               (function (intern-soft (cadr (assoc command ed-cmd-alist)))))
          (cond
           ((or (> (ed-strtonum start) (ed-strtonum "$"))
                (> (ed-strtonum end) (ed-strtonum "$"))
                (< (ed-strtonum start) 1)
                (< (ed-strtonum end) 1)
                (and (not (equal end ""))
                     (< (ed-strtonum end) (ed-strtonum start))))
            (ed-cmd-error "Undefined error."))
           (function
            (funcall function args start end))
           ((not (equal end ""))
            (ed-goto-line-print (ed-strtonum end)))
           (start
            (ed-goto-line-print (ed-strtonum start)))
           (t (ed-cmd-error "Undefined error."))))))))

(defun ed-get-ed-buffer ()
  "Gets the buffer of this ed instance."
  (get-buffer (concat "ed: " (buffer-name ed-associated-buffer))))

(defun ed-goto-line-print (line)
  "Go to a line and output it."
  (with-current-buffer ed-associated-buffer
    (goto-line line))
  (ed-cmd-print "" "." ""))

(defun ed-quit-buffer (&optional force-quit-p)
  "Quit an ed buffer."
  (if (or force-quit-p
          ed-quit-already-attempted-p
          (not (buffer-modified-p ed-associated-buffer)))
      (let ((ed-buffer (ed-get-ed-buffer)))
        (if (> (length (window-list)) 1)
            (delete-window (get-buffer-window ed-buffer)))
        (kill-buffer ed-buffer)
        (setq ed-quit-already-attempted-p nil))
    (setq ed-quit-already-attempted-p t)
    (ed-cmd-error "Warning: buffer modified.")))

(defun ed-cmd-append (args start end)
  "Start appending text."
  (setq ed-is-inserting t)
  (let ((start (ed-strtonum start)))
    (with-current-buffer ed-associated-buffer
      (goto-line (+ 1 start)))))

(defun ed-cmd-change (args start end)
  "Delete the text between start and end and insert some new to
replace it."
  (setq ed-is-inserting t)
  (ed-cmd-delete args start end))

(defun ed-cmd-comment (&rest unused)
  "Add a comment without executing any commands.")

(defun ed-cmd-delete (args start end)
  "Delete the text between start and end."
  (setq start (ed-strtonum start))
  (if (equal "" end)
      (setq end start)
    (setq end (ed-strtonum end)))
  (with-current-buffer ed-associated-buffer
    (let* ((current-line (ed-line-number-at-pos))
           (start-pos (point-at-bol (- start (- current-line 1))))
           (end-pos (point-at-eol (- end (- current-line 1)))))
      (delete-region (- start-pos
                        (if (equal (char-after (1- start-pos)) ?\n) 1 0))
                     end-pos))))

(defun ed-cmd-error (message &optional verbose-p &rest unused)
  "Throw the error message."
  (setq ed-last-error-message message)
  (if (or ed-verbose-errors-p verbose-p)
      (insert "\n" ed-last-error-message)
    (insert "\n?")))

(defun ed-cmd-exec (args &rest unused)
  (if (or (not (equal end ""))
          (< (length args) 1))
      (ed-cmd-error "Undefined error."))
  (insert "\n" (shell-command-to-string args) "!"))

(defun ed-cmd-file (&rest unused)
  "Return the name of the file in the associated buffer."
  (insert "\n" (buffer-file-name ed-associated-buffer)))

(defun ed-cmd-insert (args start end)
  "Start inserting text."
  (setq ed-is-inserting t)
  (let ((start (ed-strtonum start)))
    (with-current-buffer ed-associated-buffer
      (goto-line start))))

(defun ed-cmd-join (args start end)
  (let* ((start (ed-strtonum start))
         (end (ed-strtonum end)))
    (with-current-buffer ed-associated-buffer
      (goto-line start)
      (while (<= start end)
        (join-line 1)
        (setq start (1+ start))))))

(defun ed-cmd-linenum (args start end)
  "Print a line number."
  (insert "\n" (number-to-string (ed-strtonum start))))

(defun ed-cmd-mark (args start end)
  "Insert a mark."
  (if (and (= (length args) 1))
      (progn
        (setq ed-mark-alist (remove (assoc args ed-mark-alist) ed-mark-alist))
        (push (cons args (ed-strtonum start)) ed-mark-alist))
    (ed-cmd-error "Undefined error.")))

(defun ed-cmd-move (args start end)
  "Move lines."
  (let* ((start (ed-strtonum start))
         (end (+ 1 (ed-strtonum end)))
         (to (ed-strtonum args)))
    (with-current-buffer ed-associated-buffer
      (let ((current-line (ed-line-number-at-pos)))
        (kill-region (point-at-bol (- start (- current-line 1)))
                     (+ (if (= (point-at-eol) (point-max)) 0 1)
                        (point-at-eol (- end current-line))))
        (goto-line to)
        (insert (pop kill-ring))))))

(defun ed-cmd-numbered-print (args start end)
  "Print lines with their line numbers."
  (let* ((start (ed-strtonum start))
         (end (if (equal end "") start (ed-strtonum end))))
    (with-current-buffer ed-associated-buffer
      (let ((current-line (ed-line-number-at-pos)))
        (while (<= start end)
          (copy-region-as-kill
           (point-at-bol (- start (- current-line 1)))
           (point-at-eol (- start (- current-line 1))))
          (with-current-buffer (ed-get-ed-buffer)
            (insert "\n" (number-to-string start)
                    "\t" (pop kill-ring)))
          (setq start (1+ start)))))))

(defun ed-cmd-print (args start end)
  "The ed print command."
  (setq start (ed-strtonum start))
  (if (equal end "")
      (setq end start)
    (setq end (ed-strtonum end)))
  (with-current-buffer ed-associated-buffer
    (let ((current-line (ed-line-number-at-pos)))
      (copy-region-as-kill
       (point-at-bol (- start (- current-line 1)))
       (point-at-eol (- end (- current-line 1)))))
    (goto-line end))
  (insert "\n" (pop kill-ring)))

(defun ed-cmd-print-last-error (&optional verbose-p &rest unused)
  "Print most recent error."
  (if (or ed-verbose-errors-p verbose-p)
      (insert "\n" ed-last-error-message)
    (insert "\n?")))

(defun ed-cmd-prompt (&rest unused)
  "Toggle the prompt."
  (setq ed-display-prompt (not ed-display-prompt)))

(defun ed-cmd-quit (&optional force-quit-p &rest unused)
  "Quit ed."
  (ed-quit-buffer))

(defun ed-cmd-quit-unconditionally (&rest unused)
  "Quit ed unconditionally, regardless of modified buffer."
  (ed-quit-buffer t))

(defun ed-cmd-read (args start end)
  "Insert a file to the buffer."
  (setq start (ed-strtonum start))
  (if (or (not (equal end ""))
          (< (length args) 2))
      (ed-cmd-error "Undefined error.")
    (progn
      (with-current-buffer ed-associated-buffer
        (goto-line (1+ start))
        (goto-line
         (ed-line-number-at-pos
          (+ (point)
             (cadr (insert-file-contents (substring args 1))))))))))

(defun ed-cmd-replace (args start end)
  "The ed replace command."
  (if (equal args "")
      (ed-cmd-replace ed-last-replace start end)
    (progn
      (if (or (equal start "") (equal end ""))
          (setq start (setq end (with-current-buffer ed-associated-buffer
                                  (ed-line-number-at-pos))))
        (progn (setq start (ed-strtonum start))
               (setq end (ed-strtonum end))))
      (setq split-regexp (split-string args "/"))
      (if (equal (car split-regexp) "")
          (setq split-regexp (cdr split-regexp)))
      (if (not (or (= (length split-regexp) 2)
                   (= (length split-regexp) 3)))
          (ed-cmd-error "Undefined error.")
        (let ((from (car split-regexp))
              (to (nth 1 split-regexp))
              (arg (if (= (length split-regexp) 2) "" (nth 2 split-regexp))))
          (setq ed-last-replace args)
          (with-current-buffer ed-associated-buffer
            (let* ((current-line (ed-line-number-at-pos))
                   (begin-point (point-at-bol (- start (1- current-line))))
                   (end-point (point-at-eol (- end (1- current-line)))))
              (goto-char begin-point)
              (cond
               ((equal arg "g")
                (while (re-search-forward from end-point t)
                  (replace-match to t t)))
               (t (re-search-forward from end-point t)
                  (replace-match to t t)))))))
      (ed-goto-line-print end))))

(defun ed-cmd-transfer (args start end)
  "Transfer lines."
  (let* ((start (ed-strtonum start))
         (end (+ 1 (ed-strtonum end)))
         (to (ed-strtonum args)))
    (with-current-buffer ed-associated-buffer
      (let ((current-line (ed-line-number-at-pos)))
        (copy-region-as-kill (point-at-bol (- start (- current-line 1)))
                             (+ (if (= (point-at-eol) (point-max)) 0 1)
                                (point-at-eol (- end current-line))))
        (goto-line to)
        (insert (pop kill-ring))))))

(defun ed-cmd-unambiguous-print (args start end)
  "Print lines unambiguously."
  (let* ((start (ed-strtonum start))
         (end (if (equal end "") start (ed-strtonum end))))
    (with-current-buffer ed-associated-buffer
      (let ((current-line (ed-line-number-at-pos)))
        (copy-region-as-kill
         (point-at-bol (- start (- current-line 1)))
         (point-at-eol (- end (- current-line 1))))))
    (let ((contents
           (replace-regexp-in-string
            "\t" "\\\\t"
            (replace-regexp-in-string
             "$" "\$" (pop kill-ring)))))
      (insert "\n" contents))))

(defun ed-cmd-undo (&rest unused)
  "Undo, currently doesn't probably do what it should."
  (with-current-buffer ed-associated-buffer
    (undo)))

(defun ed-cmd-verbose-errors (&rest unused)
  "Toggle verbose error messages."
  (setq ed-verbose-errors-p (not ed-verbose-errors-p))
  (ed-cmd-print-last-error))

(defun ed-cmd-write (&rest unused)
  "Write changes to the buffer being edited."
  (with-current-buffer ed-associated-buffer
    (save-buffer))
  (insert "\n" (int-to-string (buffer-size ed-associated-buffer))))

(defvar ed-associated-buffer nil
  "The buffer associated with this ed session.")

(defvar ed-is-inserting nil
  "t if ed is in accepting text input instead of commands.")

(defvar ed-prompt "*"
  "The ed prompt.")

(defvar ed-display-prompt nil
  "t if ed should display the prompt.")

(defvar ed-last-replace nil
  "Last replaced regexp.")

(defvar ed-mark-alist nil
  "Stored ed marks in this buffer.")

(defun ed-mode ()
  "Major mode for ed buffers."
  (kill-all-local-variables)
  (use-local-map ed-mode-map)
  (setq mode-name "ed")
  (setq major-mode 'ed-mode)
  (make-local-variable 'ed-associated-buffer)
  (make-local-variable 'ed-display-prompt)
  (make-local-variable 'ed-is-inserting)
  (make-local-variable 'ed-last-replace)
  (make-local-variable 'ed-mark-alist)
  (run-hooks 'ed-mode-hook))

(defun ed ()
  "Launch an ed session associated with the current buffer."
  (interactive)
  (let ((edit-buffer (buffer-name)))
    (pop-to-buffer (concat "ed: " edit-buffer))
    (ed-mode)
    (setq ed-associated-buffer (get-buffer edit-buffer))))

(provide 'ed)

;;; ed-mode.el ends here.
