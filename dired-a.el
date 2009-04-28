;;; dired-a Various dired extensions.
;; Copyright (c) 1996 Inge Frick

;; Author: Inge Frick (inge@nada.kth.se)
;; Created: August 1996
;; Version: 1.00
;; Keywords: dired
;; Last edited: Tue Nov 19 14:30:32 1996 by Inge Frick <inge@nada.kth.se>

;; This file is not part of GNU Emacs (yet).
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  Neither the author nor any distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; this file, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;;; Commentary:
;;  This file contains a number of extensions to dired.
;;  1. The delete command can delete a non-empty directory by deleting
;;     all its files and subdirectories.
;;  2. The copy command can copy a directory by copying a directory and
;;     all its files and subdirectories.
;;  3. Handle archives (tar, arc, lharc, zoo etc.). This is not a replacement
;;     for tar-mode or archive-mode but it complements them. There are commands
;;     unpacking an entire archive, extracting specified files (it might be
;;     better to use tar-mode etc. for this) and get a content listing.
;;     Furthermore the copy command is extended to copy marked files to a
;;     target that is an archive.
;;  4. View files with a method depending on file name.
;;  5. Print files with a method depending on file name.
;;  6. Remove carriage return at end of lines for marked files.
;;  7. Toggle case for file base name (excluding the extension).
;;
;;  If you only want some of these extensions, it is fairly easy to lift out
;;  the code you want. The first page contains some support functions, the
;;  other pages are all independent of each other. ;;;; is used to delimit
;;  pages instead of page-marks to avoid possible problems in mail.

;;; Code:

;;;; This page only contains some support functions.

(autoload 'dired-map-over-marks-check "dired-aux")

(defun dired-arcs (alist op &optional buf)
  "Operate on current file depending on file name.
ALIST is an alist where each element has the form (REGEXP . CMD).
  If the regular expression REGEXP matches file name then CMD is the operation
  to perform. CMD is one of:
  1. A symbol (a function of two arguments the absolute file name and the
     relative file name).
  2. A string (a format string with the relative file name as argument).
  3. A list of strings (the command and its flags).
OP is the name (to tell the user) of the operation to perform.
The optional argument BUF is:
  1. 0 (meaning don't wait for the operation to complete).
  2. Not present or nil (meaning wait, but don't display result of operation).
  3. A buffer in which the result is displayed."
 (let* ((file (dired-get-filename))
         (name (dired-make-relative file))
         (fn (dired-match-alist name alist)))
    (cond
     ((null fn)
      (if buf (error "Don't know how to %s %s" op name)
        (message "Don't know how to %s %s" op name)
        (dired-log "Don't know how to %s %s" op name)
        name))
     ((not (or (listp fn) (stringp fn))) (funcall fn file name))
     (t (dired-arcs-process fn op name nil buf)))))

(defun dired-arcs-process (fn op name &optional fn-list buf)
  ;; FN is a command to be performed.
  ;; OP is the name (to tell the user) of the command.
  ;; NAME is the file to operate on, or the destination if FN-LIST is not nil.
  ;; FN-LIST is either nil (meaning the operation is performed on NAME) or
  ;; a list of source-files (with NAME as the destination).
  ;; BUF is either 0 (meaning don't wait for command FN to complete),
  ;;     nil (meaning wait for completion but don't display any result) or
  ;;     a buffer where the result is displayed.
  (setq fn (cond
            ((listp fn) (append fn (cons name fn-list)))
            (fn-list (format fn (mapconcat 'identity fn-list " ") name))
            (t (format fn name))))
  (let* ((wait (not (eq buf 0)))
         (pcmd (format "%s command: %s%s"
                       op
                       (if (listp fn) (mapconcat 'identity fn " ") fn)
                       (if wait " ..." "")))
         (prg (if (listp fn) (car fn) shell-file-name))
         (display (and buf wait))
         (args (if (listp fn) (cdr fn) (list shell-command-switch fn)))
         (buffer (if wait (get-buffer-create (or buf "*Dired Cmd*")) 0)))
    (message pcmd)
    (if wait
        (save-excursion
          (set-buffer buffer)
          (setq buffer-read-only nil)
          (erase-buffer)
          (if display (display-buffer buffer))))
    (setq prg (apply 'call-process prg nil buffer display args))
    (and wait
         (if (eq prg 0) (progn (message "%s done" pcmd) nil)
           (if (not (stringp prg)) (setq prg (format "Return code: %s" prg)))
           (if buf (error prg)
             (dired-log buffer)
             (dired-log prg)
             (message "%s error" pcmd)
             name)))))

(defun dired-match-alist (name alist)
  "Search for match for NAME in ALIST and return the corresponding value."
  (let ((fn nil))
    ;; remove backup suffixes from file name
    (let ((jka-compr-enabled nil))              ; We want to see .Z
      (setq name (file-name-sans-versions name)))
    ;; find first matching alist entry
    (while (and (not fn) alist)
      (if (string-match (car (car alist)) name)
          (setq fn (cdr (car alist)))
        (setq alist (cdr alist))))
    fn))

;;;; Extend delete command in dired to allow recursive delete.

;;  Use dired-delete-directory instead of delete-directory.
;;  Note that dired-delete-directory contains Unix specific code. It
;;  has to be changed to fit other operating systems.

;; dired must be loaded as we redefine dired-internal-do-deletions.
(require 'dired)

;; Redefine dired-internal-do-deletions to use dired-delete-directory
;; instead of delete-directory.
(defun dired-internal-do-deletions (l arg)
  ;; L is an alist of files to delete, with their buffer positions.
  ;; ARG is the prefix arg.
  ;; Filenames are absolute (VMS needs this for logical search paths).
  ;; (car L) *must* be the *last* (bottommost) file in the dired buffer.
  ;; That way as changes are made in the buffer they do not shift the
  ;; lines still to be changed, so the (point) values in L stay valid.
  ;; Also, for subdirs in natural order, a subdir's files are deleted
  ;; before the subdir itself - the other way around would not work.
  (let ((files (mapcar (function car) l))
        (count (length l))
        (succ 0))
    ;; canonicalize file list for pop up
    (setq files (nreverse (mapcar (function dired-make-relative) files)))
    (if (dired-mark-pop-up
         " *Deletions*" 'delete files dired-deletion-confirmer
         (format "Delete %s " (dired-mark-prompt arg files)))
        (save-excursion
          (let (failures);; files better be in reverse order for this loop!
            (while l
              (goto-char (cdr (car l)))
              (let (buffer-read-only)
                (condition-case err
                    (let ((fn (car (car l))))
                      ;; This test is equivalent to
                      ;; (and (file-directory-p fn) (not (file-symlink-p fn)))
                      ;; but more efficient
                      (if (eq t (car (file-attributes fn)))
                          (dired-delete-directory fn)
                        (delete-file fn))
                      ;; if we get here, removing worked
                      (setq succ (1+ succ))
                      (message "%s of %s deletions" succ count)
                      (delete-region (progn (beginning-of-line) (point))
                                     (progn (forward-line 1) (point)))
                      (dired-clean-up-after-deletion fn))
                  (error;; catch errors from failed deletions
                   (dired-log "%s\n" err)
                   (setq failures (cons (car (car l)) failures)))))
              (setq l (cdr l)))
            (if (not failures)
                (message "%d deletion%s done" count (dired-plural-s count))
              (dired-log-summary
               (format "%d of %d deletion%s failed"
                       (length failures) count
                       (dired-plural-s count))
               failures))))
      (message "(No deletions performed)")))
  (dired-move-to-filename))

(defvar dired-recursive-deletes `top
  "*Decide whether recursive deletes are allowed.
Nil means no recursive deletes.
always means delete recursively without asking.
top means ask for each directory at top level.
Anything else means ask for each directory.")

;; Possibly delete a directory and all its files.
;; Unix specific. Modify for other operating systems.
(defun dired-delete-directory (dir)
  (let ((files (cdr (cdr (directory-files dir t))))) ; Skip . and ..
    (if (and dired-recursive-deletes
             files                      ; directory not empty
             (or (eq dired-recursive-deletes 'always)
                 (yes-or-no-p (format "Recursive delete of %s " dir))))
        (let ((dired-recursive-deletes
               (if (eq dired-recursive-deletes 'top) 'always
                 dired-recursive-deletes)))
          (if (eq dired-recursive-deletes 'always) ; Handle separately as Unix
              (call-process "rm" nil nil nil "-r" dir) ; has recursive delete
            (while files                ; Recursively delete (possibly asking).
              (if (eq t (car (file-attributes (car files))))
                  (dired-delete-directory (car files))
                (delete-file (car files)))
              (setq files (cdr files)))
            (delete-directory dir)))
      (delete-directory dir))))

;;;; Extend copy command in dired. Recursive copy. Copy to archive.

;;  This modifies the copy command in dired in two ways:
;;  1. Allow copying of a directory and all its files and subdirectories.
;;  2. Allow copying to a generalized directory (an archive).

;;  Note that this code contains some Unix specific parts.

;;; Code:

;; First load dired-aux as we redefine dired-do-create-files and
;; dired-copy-file.
(require 'dired-aux)

;; We use dired-arcs-process.

;; Change this to suit your installation.
(defvar dired-to-archive-copy-alist
  '(("\\.sh\\(ar\\|[0-9]\\)*$" nil "shar %s > %s")
    ("\\.tar$" ("tar" "uvf") ("tar" "cvf"))
    ("\\.tgz$\\|\\.tar\\.g?[zZ]$"
     "tar uvf - %s | gzip > %s" "tar cvf - %s | gzip > %s")
    ("\\.arc$" ("arc" "a") nil)
    ("\\.zoo$" ("zoo" "aP") nil))
  "*Alist with information how to add files to an archive.
Each element has the form (REGEXP ADD-CMD NEW-CMD). If REGEXP matches
the file name of a target, that target is an archive and ADD-CMD is a command
that adds to an existing archive and NEW-CMD is a command that makes a new
archive (overwriting an old one if it exists). ADD-CMD and NEW-CMD are:
1. Nil (meaning we cannot do this for this type of archive) (one of
   ADD-CMD and NEW-CMD must be non-nil).
2. A symbol that must be a function e.g. dired-do-archive-op.
3. A format string with two arguments, the source files concatenated into
   a space separated string and the target archive.
4. A list of strings, the command and its flags, to which the target and
   the source-files are concatenated.")

;; These are fluid variables in dired-archive-p and dired-do-archive-op.
(defvar dired-archive-op nil)           ; Define here to shut up
(defvar dired-archive-rm nil)           ; byte compiler.

;; Redefine dired-do-create-files to support generalized directories.
(defun dired-do-create-files (op-symbol file-creator operation arg
                                             &optional marker-char op1
                                             how-to)
  ;; Create a new file for each marked file.
  ;; Prompts user for target, which is a directory in which to create
  ;;   the new files.  Target may be a plain file if only one marked
  ;;   file exists.
  ;; OP-SYMBOL is the symbol for the operation.  Function `dired-mark-pop-up'
  ;;   will determine whether pop-ups are appropriate for this OP-SYMBOL.
  ;; FILE-CREATOR and OPERATION as in dired-create-files.
  ;; ARG as in dired-get-marked-files.
  ;; Optional arg OP1 is an alternate form for OPERATION if there is
  ;;   only one file.
  ;; Optional arg MARKER-CHAR as in dired-create-files.
  ;; Optional arg HOW-TO determines how to treat target:
  ;;   If HOW-TO is not given (or nil), and target is a directory, the
  ;;     file(s) are created inside the target directory.  If target
  ;;     is not a directory, there must be exactly one marked file,
  ;;     else error.
  ;;   If HOW-TO is t, then target is not modified.  There must be
  ;;     exactly one marked file, else error.
  ;; Else HOW-TO is assumed to be a function of one argument, target,
  ;;     that looks at target and returns a value for the into-dir
  ;;     variable.  The function dired-into-dir-with-symlinks is provided
  ;;     for the case (common when creating symlinks) that symbolic
  ;;     links to directories are not to be considered as directories
  ;;     (as file-directory-p would if HOW-TO had been nil).
  (or op1 (setq op1 operation))
  (let* ((fn-list (dired-get-marked-files nil arg))
         (rfn-list (mapcar (function dired-make-relative) fn-list))
         (fn-count (length fn-list))
         (target (expand-file-name ; fluid variable inside dired-create-files
                   (dired-mark-read-file-name
                    (concat (if (= 1 fn-count) op1 operation) " %s to: ")
                    (dired-dwim-target-directory)
                    op-symbol arg
                    rfn-list)))
         dired-archive-op               ; fluid variables in dired-archive-p
         dired-archive-rm               ; and dired-do-archive-op
         (into-dir (cond ((null how-to) (file-directory-p target))
                         ((eq how-to t) nil)
                         (t (funcall how-to target)))))
    (if (and into-dir (not (eq into-dir t)))
        (funcall into-dir operation rfn-list fn-list target)
      (if (and (> fn-count 1)
               (not into-dir))
          (error "Marked %s: target must be a directory: %s" operation target))
      ;; rename-file bombs when moving directories unless we do this:
      (or into-dir (setq target (directory-file-name target)))
      (dired-create-files
       file-creator operation fn-list
       (if into-dir                     ; target is a directory
           ;; This function uses fluid variable target when called
           ;; inside dired-create-files:
           (function
            (lambda (from)
              (expand-file-name (file-name-nondirectory from) target)))
         (function (lambda (from) target)))
       marker-char))))

;; Redefine dired-do-copy to support generalized directories.
(defun dired-do-copy (&optional arg)
  "Copy all marked (or next ARG) files, or copy the current file.
This normally preserves the last-modified date when copying.
When operating on just the current file, you specify the new name.
When operating on multiple or marked files, you specify a directory,
and new copies of these files are made in that directory
with the same names that the files currently have."
  (interactive "P")
  (dired-do-create-files 'copy (function dired-copy-file)
                           (if dired-copy-preserve-time "Copy [-p]" "Copy")
                           arg dired-keep-marker-copy
                           nil (function dired-archive-p)))

(defun dired-archive-p (target)
  ;; If TARGET is a generalized directory (an archive) then
  ;; return commands to either add to or make new archive.
  ;; dired-archive-p is used as a how-to argument to dired-do-create-files.
  (let ((fn (dired-match-alist target dired-to-archive-copy-alist))
        exist)
    (if (null fn)                       ; not generalized directory,
        (file-directory-p target)       ; is it ordinary directory instead?
      ;; is a generalized directory.
      (setq fn (cond
                ((or (not (setq exist (file-exists-p target))) ; new target
                     (null (car fn)))   ; can't add to a target
                 (and (y-or-n-p (format "%s archive %s "
                                        (if exist "Overwrite" "Make")
                                        target))
                      (car (cdr fn)))) ; Make new archive.
                ((y-or-n-p (format "Add to archive %s " target)) (car fn))
                ((not (y-or-n-p "Overwrite archive ")) nil)
                ((car (cdr fn)))        ; Can overwrite archive,
                (t (setq dired-archive-rm t) ; can't, do it by first removing
                   (car fn))))          ; target and then adding to it.
      (if (symbolp fn) (symbol-function fn)
        (setq dired-archive-op fn)
        (function dired-do-archive-op)))))

(defun dired-do-archive-op (operation rfn-list fn-list target &optional buf)
  (if dired-archive-rm (delete-file target))
  (dired-arcs-process dired-archive-op operation target rfn-list buf))

(defvar dired-recursive-copies `top
  "*Decide whether recursive copies are allowed.
Nil means no recursive copies.
always means copy recursively without asking.
top means ask for each directory at top level.
Anything else means ask for each directory.")

;; Redefine dired-copy-file so that it can copy directories (recursively).
(defun dired-copy-file (from to ok-flag)
  (if (and dired-recursive-copies
           (eq t (car (file-attributes from)))
           (or (eq dired-recursive-copies 'always)
               (yes-or-no-p (format "Recursive copies of %s " from))))
      (let ((files (cdr (directory-files from t))) ; Skip .
            (dired-recursive-copies
             (if (eq dired-recursive-copies 'top) 'always
               dired-recursive-copies)))
        (if (not (file-exists-p to))
            (make-directory to)
          (dired-handle-overwrite to))
        (while (setq files (cdr files)) ; Skip ..
          (dired-copy-file (car files)
                           (concat (file-name-as-directory to)
                                   (file-name-nondirectory (car files)))
                           ok-flag)))
    (dired-handle-overwrite to)
    (copy-file from to ok-flag dired-copy-preserve-time)))

;;;; Unpack archive depending on type.
;; Uses dired-arcs and dired-map-over-marks-check.

(defvar dired-unpack-alist
 '(
   ("\\.u\\(ue\\|aa\\)$" . dired-uud)
   ("\\.sh\\(ar\\|[0-9]\\)*$" "unshar")
   ("\\.tar$" "tar" "xvf")
   ("\\.tgz$\\|\\.tar\\.g?[zZ]$" . "gunzip -c %s | tar xvf -")
   ("\\.arc$" "arc" "x")
   ("\\.zip$" "unzip" "-Uox")
   ("\\.zoo$" "zoo" "x.//")
   ("\\.lzh$" "lha" "x")
   ("\\.g?[zZ]$" . dired-uncompressing)
   )
 "*Alist with information how to unpack files. See function dired-arcs.")

(defun dired-do-unpack (&optional arg)
"Unpack marked (or next ARG) files. Method used depends on file name."
  (interactive "P")
  (dired-map-over-marks-check (function dired-unpack) arg 'unpack t))

(defun dired-unpack ()
  (dired-arcs dired-unpack-alist "Unpack"))

(defun dired-uncompressing (from-file name)
  (dired-compress))     ; Do it this way to allow ange-ftp to redefine

(defun dired-gunziping (from-file name)
  ;; Unzip current file.  Return nil for success, offending filename else.
  (let* (buffer-read-only
         (to-file (substring from-file 0 -2)))
    (if (dired-check-process (concat "Unzipping " from-file)
                             "gunzip" from-file)
        name
      (dired-update-file-line to-file)
      nil)))

(defun dired-uud (from-file name)
  ;; Uudecode current file.  Return nil for success, offending filename else.
  (let ((buffer-read-only nil)
        (to-file (concat (file-name-directory from-file)
                         (dired-uud-out-file-name from-file))))
    (if (dired-check-process (concat "Uudecoding " name)
                             "uud" from-file)
        name
      (dired-add-entry to-file)
      nil)))

(defun dired-uud-out-file-name (file) ""
  (let ((buf (get-buffer-create " Temp"))
        (end nil))
   (save-excursion
     (set-buffer buf)
     (erase-buffer))
   (call-process "grep" nil buf nil "^begin " file)
   (save-excursion
     (set-buffer buf)
     (goto-char (point-min))
     (if (not (looking-at "begin")) nil
       (end-of-line)
       (skip-chars-backward "[ \t]")
       (setq end (point))
       (skip-chars-backward "^ \t")
       (buffer-substring (point) end) ))))

(define-key dired-mode-map "U" 'dired-do-unpack)
(define-key dired-mode-map [menu-bar operate unpack]
  '("Unpack" . dired-do-unpack))

;;;; Extract from archive depending on type.
;; Uses dired-uud and dired-uncompressing.

(defvar dired-extract-alist
 '(
   ("\\.u\\(ue\\|aa\\)$" . dired-uud) ; There is only one file
   ("\\.tar$" . "tar xvf %s ")
   ("\\.tgz$\\|\\.tar\\.g?[zZ]$" . "gunzip -c %s | tar xvf - ")
   ("\\.arc$" . "arc x %s ")
   ("\\.zip$" . "unzip -Ux %s ")
   ("\\.zoo$" . "zoo x. %s ")
   ("\\.lzh$" . "lha x %s ")
   ("\\.g?[zZ]$" . dired-uncompressing)       ; There is only one file
   )
 "*Alist with information how to extract from files. See function dired-arcs.")

(defun dired-extract ()
  "Extract from file, using method depending on file name."
  (interactive)
  (let* ((file (dired-get-filename))
         (name (dired-make-relative file))
         (fn (dired-match-alist name dired-extract-alist)))
    (cond
     ((null fn) (error "Don't know how to extract from %s" name))
     ((stringp fn) (shell-command (read-string "" (format fn name))))
     (t (funcall fn file name)) ) ) )

(define-key dired-mode-map "y" 'dired-extract)
(define-key dired-mode-map [menu-bar immediate extract]
  '("Extract from archive..." . dired-extract))

;;;; List content of archive depending on type.
;; Uses dired-arcs and dired-uud-out-file-name.

(defvar dired-arc-dir-alist
  '(
    ("\\.u\\(ue\\|aa\\)$" . dired-uud-file-name)
    ("\\.tar$" "tar" "tvf")
    ("\\.tgz$\\|\\.tar\\.g?[zZ]$" . "gunzip -c %s | tar tvf - ")
    ("\\.arc$" "arc" "v")
    ("\\.zip$" "unzip" "-v")
    ("\\.zoo$" "zoo" "l")
    ("\\.lzh$" "lha" "l")
    ("\\.g?[zZ]$" . dired-uncompress-name)
    )
  "*Alist with information how to list directory of archive.
See function dired-arcs.")

(defun dired-arc-dir (&optional buf)
  "List content of archive, using archiver depending on file name extension.
Put listing in optional buffer BUF (default *Archive List*)."
  (interactive)
  (dired-arcs dired-arc-dir-alist "Directory list" (or buf "*Archive List*")))

(defun dired-uncompress-name (file name) ""
  (message (substring name 0 -2)))

(defun dired-uud-file-name (file name) ""
  (if (setq file (dired-uud-out-file-name file)) (message file)
    (message "Can't find the name in %s" name)))

(define-key dired-mode-map "a" 'dired-arc-dir)
(define-key dired-mode-map [menu-bar immediate listarc]
  '("List archive content" . dired-arc-dir))

;;;; View file depending on type.
;; Uses dired-arcs.

(defvar dired-view-alist
  '(("\\.dvi$" "xdvi" "-expert" "-s" "4")
    ("\\.ps$" "ghostview")
    ("\\.\\(gif\\|jpeg\\|tiff\\)$" "xv")
    ("\\.*" . dired-call-view-file)   ; Default. Must be last in alist.
    )
  "*Alist with commands to view depending on extension.
See function dired-arcs.")

(defun dired-call-view-file (dummy1 dummy2)
  (dired-view-file))

(defun dired-do-view ()
"View marked (or next ARG) files. Method used depends on file name."
  (interactive)
  (dired-arcs dired-view-alist "View"
              0)) ; Return immediately after displaying file.

(define-key dired-mode-map "r" 'dired-view-file) ; Use emacs view regardless of
                                        ; file type.
(define-key dired-mode-map "v" 'dired-do-view) ; View according to file type.

;;;; Print files depending on type.
;; Uses dired-arcs and dired-map-over-marks-check.

;; First load dired-aux as we redefine dired-do-print.
(require 'dired-aux)

(defvar dired-print-alist
  '(("\\.dvi$" "dvips")
    ("\\.ps$"  "lpr")
    ("\\.ps\\.g?[zZ]$" . "gunzip -c %s | lpr")
    (".*\\.g?[zZ]$" . "gunzip -c %s | lpp -4 -n -i")
    (".*" "lpp" "-4" "-n" "-i")               ; Default. Must be last in alist.
    )
  "*Alist with commands to print depending on extension.
See function dired-arcs.")

(defun dired-do-print (&optional arg)
  "Print marked (or next ARG) files. Method used depends on file name."
  (interactive "P")
  (dired-map-over-marks-check (function dired-printer) arg 'print t))

(define-key dired-mode-map "P" 'dired-do-print)

(defun dired-printer ()
  (dired-arcs dired-print-alist "Print"))

(define-key dired-mode-map "P" 'dired-do-print)

;;;; Print files compactly depending on type.
;; Uses dired-arcs and dired-map-over-marks-check.

(defvar dired-compact-print-alist
  '(("\\.dvi$" "dvips")
    (".*\\.g?[zZ]$" . "gunzip -c %s | lpp -5 -n")
    (".*" "lpp"  "-5" "-n")             ; Default. Must be last in alist.
    )
  "*Alist with commands to print compactly depending on file name.
See function dired-arcs.")

(defun dired-do-compact-print (&optional arg) "\
Print compactly marked (or next ARG) files. Method used depends on file name."
  (interactive "P")
  (dired-map-over-marks-check (function dired-compact-printer) arg
                              'compact-print t))

(defun dired-compact-printer ()
  (dired-arcs dired-compact-print-alist "Print compactly"))

(define-key dired-mode-map "W" 'dired-do-compact-print)
(define-key dired-mode-map [menu-bar operate compact-print]
  '("Print compactly" . dired-do-compact-print))

;;;; Toggle case of name (not extension) of file.

(defun dired-toggle-name-case ()
  "Toggle case of name (not extension) of this file"
  (interactive)
  (let* ((buffer-read-only nil)
         (from-file (dired-get-filename))
         (from-name (file-name-nondirectory from-file))
         (to-name nil)
         (end nil)
         (start nil) )
    (end-of-line)
    (search-backward from-name)
    (setq start (point))
    (if (search-forward "." end 1) (backward-char 1))
    (setq end (point))
    (setq from-name (buffer-substring start end))
    (if (string= (setq to-name (downcase from-name)) from-name)
        (setq to-name (upcase from-name)) )
    (delete-region start end)
    (insert to-name)
    (beginning-of-line)
    (rename-file from-file (dired-get-filename) 1)
    (dired-next-line 1)))

(define-key dired-mode-map "j" 'dired-toggle-name-case)

;;;; Remove <cr> at end of lines in marked (or next ARG) files."

;; The following has already been done.
;;(autoload 'dired-map-over-marks-check "dired-aux")

(defun dired-do-rmcr (&optional arg)
  "Remove <cr> at end of lines in marked (or next ARG) files."
  (interactive "P")
  (dired-map-over-marks-check (function dired-rmcr) arg 'rmcr t))

(defun dired-rmcr ()
  (let* ((from-file (dired-get-filename))
         (tmp-buf (find-file-noselect from-file))
         (mod (buffer-modified-p tmp-buf)))
    (save-excursion
          (message "Removing <cr>...")
          (set-buffer tmp-buf)
          (if mod (revert-buffer))
          (goto-char (point-min))
          (replace-regexp "\r$" "")
          (if (buffer-modified-p)
              (progn (write-file from-file) (message "Removing <cr>...done"))
            (message "Removing <cr>...none found"))
          (kill-buffer tmp-buf))
    nil))

 (define-key dired-mode-map "K" 'dired-do-rmcr)
 (define-key dired-mode-map [menu-bar operate removeCR]
   '("Remove CR at line-end" . dired-do-rmcr))

;;;;

(provide 'dired-a)

;;; dired-a ends here.
