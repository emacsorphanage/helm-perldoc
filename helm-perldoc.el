;;; helm-perldoc.el --- perldoc with helm interface

;; Copyright (C) 2012 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-helm-perldoc
;; Version: 0.01
;; Package-Requires: ((helm "1.0"))

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

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'helm)

(defgroup helm-perldoc nil
  "perldoc with helm interface"
  :group 'helm)

(defcustom helm-perldoc:ignore-modules
  '("strict" "warnings" "base" "parent" "lib")
  "Ignore imported modules"
  :type 'list
  :group 'helm-perldoc)

(defvar helm-perldoc:modules nil
  "List of all installed modules")

(defun helm-perldoc:collect-perl-paths ()
  (with-temp-buffer
    (let ((paths nil)
          (ret (call-process "perl" nil t nil "-le" "print for @INC")))
      (unless (zerop ret)
        (error "Failed collect Perl modules"))
      (goto-char (point-min))
      (while (not (eobp))
        (let ((path (buffer-substring-no-properties
                     (point) (line-end-position))))
          (unless (string-match "^\\.$" path)
            (push (file-name-as-directory path) paths)))
        (forward-line))
      (reverse (sort paths #'string<)))))

(defun helm-perldoc:transform-path (path root)
  (replace-regexp-in-string
   "\\.\\(?:pm\\|pod\\)$" ""
   (replace-regexp-in-string
    "/" "::"
    (replace-regexp-in-string (format "^%s" root) "" path))))

(defun helm-perldoc:directory-p (file dir)
  (and (not (string-match "^\\." file))
       (file-directory-p (file-name-as-directory (concat dir file)))))

(defvar helm-perldoc:searched-path (make-hash-table :test #'equal))

(defun helm-perldoc:collect-modules (dir root)
  (when (not (gethash dir helm-perldoc:searched-path))
    (puthash dir t helm-perldoc:searched-path)
    (loop with modules = nil
          for file in (directory-files dir)
          for abspath = (concat dir file)
          do
          (cond ((string-match "\\.\\(pm\\|pod\\)$" file)
                 (let ((mod (helm-perldoc:transform-path abspath root)))
                   (push mod modules)))
                ((helm-perldoc:directory-p file dir)
                 (let* ((moddir (file-name-as-directory abspath))
                        (mods (helm-perldoc:collect-modules moddir root)))
                   (when mods
                     (setq modules (append mods modules))))))
          finally
          return (remove-duplicates modules :test #'string=))))

;;;###autoload
(defun helm-perldoc:setup ()
  (interactive)
  (when (or current-prefix-arg (null helm-perldoc:modules))
    (setq helm-perldoc:searched-path (make-hash-table :test #'equal))
    (let ((perl-paths (helm-perldoc:collect-perl-paths)))
      (setq helm-perldoc:modules
            (loop for path in perl-paths
                  append (helm-perldoc:collect-modules path path))))))

(defvar helm-perldoc:buffer "*perldoc*")

(defun helm-perldoc:exec (cmd &optional mode-func)
  (with-current-buffer (get-buffer-create helm-perldoc:buffer)
    (setq buffer-read-only nil)
    (erase-buffer)
    (let ((ret (call-process-shell-command cmd nil t)))
      (unless (zerop ret)
        (error (format "Failed '%s'" cmd)))
      (goto-char (point-min))
      (when mode-func
        (funcall mode-func))
      (setq buffer-read-only t)
      (pop-to-buffer (current-buffer)))))

(defun helm-perldoc:action-view-document (candidate)
  (helm-perldoc:exec (format "perldoc %s" candidate)))

(defun helm-perldoc:action-view-source (candidate)
  (helm-perldoc:exec (format "perldoc -m %s" candidate) #'cperl-mode))

(defun helm-perldoc:action-check-corelist (candidate)
  (unless (executable-find "corelist")
    (error "Please install 'Module::CoreList'"))
  (message "%s" (shell-command-to-string
                 (format "corelist %s" candidate))))

(defun helm-perldoc:search-import-statement ()
  (save-excursion
    (when (re-search-backward "^\\s-*\\(use\\)\\s-+" nil t)
      (let ((column (helm-perldoc:point-to-column (match-beginning 1))))
        (forward-line)
        (list :point (point) :column column)))))

(defun helm-perldoc:search-package-statement ()
  (save-excursion
    (when (re-search-backward "^package^\\s+*" nil t)
      (let ((column (helm-perldoc:point-to-column (match-beginning 0))))
        (forward-line)
        (list :point (point) :column column)))))

(defun helm-perldoc:search-insertion-point ()
  (helm-aif (or (helm-perldoc:search-import-statement)
                (helm-perldoc:search-package-statement))
      it
    (progn
      (save-excursion
        (goto-char (point-min))
        (loop while (string-match "^#" (thing-at-point 'line))
              do
              (forward-line))
        (list :point (point) :column 0)))))

(defun helm-perldoc:construct-import-statement (column modules)
  (let ((spaces (loop for i from 1 to column
                      collect " " into lst
                      finally
                      return (apply #'concat lst))))
    (mapconcat (lambda (mod)
                 (format "%suse %s;\n" spaces mod)) modules "")))

(defun helm-perldoc:point-to-column (p)
  (save-excursion
    (goto-char p)
    (current-column)))

(defun helm-perldoc:action-insert-modules (candidate)
  (let* ((insertion-plist (helm-perldoc:search-insertion-point))
         (statement (helm-perldoc:construct-import-statement
                     (plist-get insertion-plist :column)
                     (helm-marked-candidates))))
    (save-excursion
      (goto-char (plist-get insertion-plist :point))
      (insert statement))))

(define-helm-type-attribute 'perldoc
  '((action
     ("View Document" . helm-perldoc:action-view-document)
     ("View Source Code" . helm-perldoc:action-view-source)
     ("Import Modules" . helm-perldoc:action-insert-modules)
     ("Check by corelist" . helm-perldoc:action-check-corelist))
    "Perldoc helm attribute"))

(defun helm-perldoc:filter-imported-modules (modules)
  (loop for module in modules
        when (and (not (string-match "^[[:digit:]]" module))
                  (not (member module helm-perldoc:ignore-modules)))
        collect module))

(defun helm-perldoc:imported-init ()
  (with-helm-current-buffer
    (save-excursion
      (goto-char (point-min))
      (loop with regexp = "^\\s-*\\(?:use\\|require\\)\\s-+\\([^ \t;]+\\)"
            while (re-search-forward regexp nil t)
            collect (match-string-no-properties 1) into modules
            finally
            return
            (let ((filterd (helm-perldoc:filter-imported-modules modules)))
              (remove-duplicates (sort filterd #'string<) :test #'equal))))))

(defun helm-perldoc:other-init ()
  (unless helm-perldoc:modules
    (error "Please exec 'M-x helm-perldoc:setup'"))
  (sort (copy-list helm-perldoc:modules) #'string<))

(defvar helm-perldoc:imported-source
  '((name . "Imported Modules")
    (candidates . helm-perldoc:imported-init)
    (type . perldoc)
    (candidate-number-limit . 9999)))

(defvar helm-perldoc:other-source
  '((name . "Other Modules")
    (candidates . helm-perldoc:other-init)
    (type . perldoc)
    (candidate-number-limit . 9999)))

;;;###autoload
(defun helm-perldoc ()
  (interactive)
  (helm :sources '(helm-perldoc:imported-source helm-perldoc:other-source)
        :buffer (get-buffer-create "*helm-perldoc*")))

(provide 'helm-perldoc)

;;; helm-perldoc.el ends here
