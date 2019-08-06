;;; psessionm.el --- Persistent save of elisp objects. -*- lexical-binding: t -*-

;; Author: Thierry Volpiatto <thierry.volpiatto@gmail.com>
;; Copyright (C) 2010~2014 Thierry Volpiatto, all rights reserved.
;; X-URL: https://github.com/thierryvolpiatto/psessionm

;; Compatibility: GNU Emacs 24.1+
;; Package-Requires: ((emacs "24") (cl-lib "0.5") (async "1.9.3"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'async)

(defvar dired-buffers)


(defgroup psessionm nil
  "Persistent sessions."
  :group 'frames)

(defcustom psessionm-elisp-objects-default-directory
  (locate-user-emacs-file "elisp-objects/")
  "The directory where lisp objects will be stored."
  :group 'psessionm
  :type 'string)

(defcustom psessionm-object-to-save-alist
  '((extended-command-history . "extended-command-history.el")
    (helm-external-command-history . "helm-external-command-history.el")
    (helm-surfraw-engines-history . "helm-surfraw-engines-history.el")
    (psessionm--save-buffers-alist . "psessionm-save-buffers-alist.el")
    (helm-ff-history . "helm-ff-history.el")
    (regexp-search-ring . "regexp-search-ring.el")
    (search-ring . "search-ring.el")
    (file-name-history . "file-name-history.el")
    (kill-ring . "kill-ring.el")
    (kill-ring-yank-pointer . "kill-ring-yank-pointer.el")
    (register-alist . "register-alist.el")
    (psessionm--winconf-alist . "psessionm-winconf-alist.el"))
  "Alist of vars to save persistently.
It is composed of (var_name . \"var_name.el\").
Where \"var_name.el\" is the file where to save value of 'var_name.

These variables are saved when `psessionm-mode' is enabled, you don't
have to add here the `minibuffer-history' variables, instead enable
`psessionm-savehist-mode' as a replacement of `savehist-mode'."
  :group 'psessionm
  :type '(alist :key-type symbol :value-type string))

(defcustom psessionm-save-buffers-unwanted-buffers-regexp ".*[.]org$\\|diary$\\|[.]newsticker-cache$"
  "Regexp matching buffers you don't want to save."
  :group 'psessionm
  :type 'string)

(defcustom psessionm-savehist-ignored-variables nil
  "List of `minibuffer-history' variables to not save."
  :group 'psessionm
  :type '(repeat symbol))

;;; The main function to save objects to byte compiled file.
;;
;; Each object have its own compiled file.
(defun psessionm--dump-object-to-file (obj file)
  "Save symbol object OBJ to the byte compiled version of FILE.
OBJ can be any lisp object, list, hash-table, etc...
Windows configurations and markers are not supported.
FILE must be an elisp file with ext \"*.el\" (NOT \"*.elc\").
Loading the *.elc file will restitute object.
That may not work with Emacs versions <=23.1 for hash tables."
  (require 'cl-lib) ; Be sure we use the CL version of `eval-when-compile'.
  (cl-assert (not (file-exists-p file)) nil
             (format "dump-object-to-file: File `%s' already exists, please remove it." file))
  (unwind-protect
       (let ((print-length           nil)
             (print-level            nil)
             (print-circle           t)
             (print-escape-nonascii  t)
             (print-escape-multibyte t))
         (with-temp-file file
           (prin1 `(setq ,obj (eval-when-compile ,obj)) (current-buffer)))
         (byte-compile-file file)
         (message "`%s' dumped to %sc" obj file))
    (delete-file file)))

;;; Objects (variables to save)
;;
;;
(defun psessionm--dump-object-to-file-save-alist (&optional skip-props)
  (when psessionm-object-to-save-alist
    (cl-loop for (o . f) in psessionm-object-to-save-alist
             for abs = (expand-file-name f psessionm-chosen-session-directory)
             ;; Registers and kill-ring are treated specially.
             do
             (cond ((and (eq o 'register-alist)
                         (symbol-value o))
                    (psessionm--dump-object-save-register-alist f skip-props))
                   ((and (boundp o) (symbol-value o))
                    (psessionm--dump-object-no-properties o abs skip-props))))))

(cl-defun psessionm--restore-objects-from-directory
    (&optional (dir psessionm-chosen-session-directory))
  (let ((file-list (directory-files dir t directory-files-no-dot-files-regexp)))
    (cl-loop for file in file-list do (and file (load file)))))

(defun psessionm--purecopy (object)
  (cond ((stringp object)
         (substring-no-properties object))
        ((consp object)
         (cl-loop for elm in object
                  ;; A string.
                  if (stringp elm)
                  collect (substring-no-properties elm)
                  else
                  ;; Proper lists.
                  if (and (consp elm) (null (cdr (last elm))))
                  collect (psessionm--purecopy elm)
                  else
                  ;; Dotted lists.
                  ;; We handle here only dotted list where car and cdr
                  ;; are atoms i.e. (x . y) and not (x . (x . y)) or
                  ;; (x . (x y)) which should fit most cases.
                  if (and (consp elm) (cdr (last elm)))
                  collect (let ((key (car elm))
                                (val (cdr elm)))
                            (cons (if (stringp key)
                                      (substring-no-properties key)
                                    key)
                                  (if (stringp val)
                                      (substring-no-properties val)
                                    val)))
                  else
                  collect elm))
        (t object)))

(defun psessionm--dump-object-no-properties (object file &optional skip-props)
  ;; Force not checking properties with SKIP-PROPS.
  (let ((value (symbol-value object)))
    (unless skip-props
      (set object (psessionm--purecopy value)))
    (psessionm--dump-object-to-file object file)))

(cl-defun psessionm--dump-object-save-register-alist (&optional (file "register-alist.el") skip-props)
  "Save `register-alist' but only supported objects."
  (let ((register-alist (cl-loop for (char . val) in register-alist
                                 unless (or (markerp val)
                                            (vectorp val)
                                            (and (consp val) (window-configuration-p (car val))))
                                 collect (cons char (if (stringp val)
                                                        (substring-no-properties val)
                                                      val))))
        (def-file (expand-file-name file psessionm-chosen-session-directory)))
    (psessionm--dump-object-no-properties 'register-alist def-file skip-props)))

;;; Persistents window configs
;;
;;
(defconst psessionm--last-winconf "last_session5247")
(defvar psessionm--winconf-alist nil)
(defun psessionm--window-name ()
  (let (result)
    (walk-windows (lambda (w) (cl-pushnew (buffer-name (window-buffer w)) result)))
    (mapconcat 'identity result " | ")))

;;;###autoload
(defun psessionm-save-winconf (place)
  "Save persistently current window config to PLACE.
Arg PLACE is the key of an entry in `psessionm--winconf-alist'."
  (interactive (list (let ((name (psessionm--window-name)))
                       (read-string (format "Place (%s) : " name) nil nil name))))
  (let ((assoc (assoc place psessionm--winconf-alist))
        (new-conf (list (cons place (window-state-get nil 'writable)))))
    (if assoc
        (setq psessionm--winconf-alist (append new-conf
                                             (delete assoc psessionm--winconf-alist)))
        (setq psessionm--winconf-alist (append new-conf psessionm--winconf-alist)))))

(defun psessionm--restore-winconf-1 (conf &optional window ignore)
  (let ((winconf (assoc conf psessionm--winconf-alist)))
    (if winconf
        (with-selected-frame (last-nonminibuffer-frame)
          (delete-other-windows)
          (window-state-put (cdr (assoc conf psessionm--winconf-alist)) window ignore))
      (user-error "Psessionm: Invalid window configuration `%s'" conf))))

;;;###autoload
(defun psessionm-restore-winconf (conf)
  "Restore window config CONF.
Arg CONF is an entry in `psessionm--winconf-alist'."
  (interactive (list (completing-read
                      "WinConfig: "
                      (sort (mapcar 'car psessionm--winconf-alist) #'string-lessp))))
  (psessionm--restore-winconf-1 conf))

;;;###autoload
(defun psessionm-delete-winconf (conf)
  "Delete window config CONF from `psessionm--winconf-alist'."
  (interactive (list (completing-read
                      "WinConfig: "
                      (sort (mapcar 'car psessionm--winconf-alist) #'string-lessp))))
  (let ((assoc (assoc conf psessionm--winconf-alist)))
    (setq psessionm--winconf-alist (delete assoc psessionm--winconf-alist))))

(defun psessionm-save-last-winconf ()
  (unless (and (boundp 'helm-alive-p) helm-alive-p)
    (psessionm-save-winconf psessionm--last-winconf)))

(defun psessionm-restore-last-winconf ()
  (run-with-idle-timer
   0.01 nil (lambda ()
             (psessionm--restore-winconf-1 psessionm--last-winconf nil 'safe))))

;;; Persistents-buffer 
;;
;;
(defun psessionm--save-some-buffers ()
  (require 'dired)
  (cl-loop with dired-blist = (cl-loop for (_f . b) in dired-buffers
                                       when (buffer-name b)
                                       collect b)
           with blist = (append (buffer-list) dired-blist)
           for b in blist
           for buf-fname = (or (buffer-file-name b) (car (rassoc b dired-buffers)))
           for place = (with-current-buffer b (point))
           when (and buf-fname
                     (not (or (file-remote-p buf-fname)
                              (and (fboundp 'tramp-archive-file-name-p)
                                   (tramp-archive-file-name-p buf-fname))))
                     (not (string-match  psessionm-save-buffers-unwanted-buffers-regexp
                                         buf-fname))
                     (file-exists-p buf-fname))
           collect (cons buf-fname place)))

(defvar psessionm--save-buffers-alist nil)
(defun psessionm--dump-some-buffers-to-list ()
  (setq psessionm--save-buffers-alist (psessionm--save-some-buffers)))

(defun psessionm--restore-some-buffers ()
  (when psessionm--save-buffers-alist
    (let* ((max (length psessionm--save-buffers-alist))
           (progress-reporter (make-progress-reporter "Restoring buffers..." 0 max)))
      (cl-loop for (f . p) in psessionm--save-buffers-alist
               for count from 0
               do
               (with-current-buffer (find-file-noselect f 'nowarn)
                 (goto-char p)
                 (push-mark p 'nomsg)
                 (progress-reporter-update progress-reporter count)))
      (progress-reporter-done progress-reporter))))

(defun psessionm-savehist-hook ()
  (unless (or (eq minibuffer-history-variable t)
              (memq minibuffer-history-variable psessionm-savehist-ignored-variables))
    (cl-pushnew (cons minibuffer-history-variable
                      (concat (symbol-name minibuffer-history-variable) ".el"))
                psessionm-object-to-save-alist
                :test 'equal)))

;;;###autoload
(define-minor-mode psessionm-savehist-mode
    "Save minibuffer-history variables persistently."
  :global t
  (if psessionm-savehist-mode
      (add-hook 'minibuffer-setup-hook 'psessionm-savehist-hook)
    (remove-hook 'minibuffer-setup-hook 'psessionm-savehist-hook)))

;;; Auto saving psessionm
;;
(defun psessionm--get-variables-regexp ()
  (regexp-opt (cl-loop for (k . _v) in psessionm-object-to-save-alist
                       collect (symbol-name k))))

(defun psessionm-save-all-async ()
  "Save current emacs session asynchronously."
  (message "Psessionm: auto saving session...")
  (psessionm-save-last-winconf)
  (psessionm--dump-some-buffers-to-list)
  (async-start
   `(lambda ()
      (add-to-list 'load-path
                   ,(file-name-directory (locate-library "psessionm")))
      (require 'psessionm)
      ;; Inject variables without properties.
      ,(async-inject-variables (format "\\`%s" (psessionm--get-variables-regexp))
                               nil nil 'noprops)
      ;; No need to treat properties here it is already done.
      (psessionm--dump-object-to-file-save-alist 'skip-props))
   (lambda (_result)
     (message "Psessionm: auto saving session done"))))


(defvar psessionm-chosen-session-directory nil)

(defun psessionm-save-session (directory)
  "Save current emacs session to directory"
  (interactive (list (read-directory-name "Save session to " 
                                          psessionm-elisp-objects-default-directory)))
  (unless (file-directory-p directory)
    (make-directory directory))

  (setq psessionm-chosen-session-directory directory)

  (psessionm-save-last-winconf)
  (psessionm--dump-some-buffers-to-list)
  (psessionm--dump-object-to-file-save-alist)
  )

(defun psessionm-restore-session (directory)
  "Restore emacs session from directory"
  (interactive (list (read-directory-name "Restore session from " 
                                          psessionm-elisp-objects-default-directory)))

  (setq psessionm-chosen-session-directory directory)

  (psessionm--restore-objects-from-directory)
  (psessionm--restore-some-buffers)
  (psessionm-restore-last-winconf)
  )

;;;###autoload
(define-minor-mode psessionm-mode
    "Persistent emacs sessions."
  :global t
  (if psessionm-mode
      (progn
        (unless (file-directory-p psessionm-elisp-objects-default-directory)
          (make-directory psessionm-elisp-objects-default-directory t))
        )
    ))


(provide 'psessionm)

;;; psessionm.el ends here
