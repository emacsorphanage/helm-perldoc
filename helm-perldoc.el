;;; helm-perldoc.el --- perldoc with helm interface

;; Copyright (C) 2014 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-helm-perldoc
;; Version: 0.02
;; Package-Requires: ((helm "1.0") (deferred "0.3.1") (cl-lib "0.4"))

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

(require 'cl-lib)

(require 'helm)
(require 'deferred)

(defgroup helm-perldoc nil
  "perldoc with helm interface"
  :group 'helm)

(defcustom helm-perldoc:ignore-modules
  '("strict" "warnings" "base" "parent" "lib")
  "Ignore imported modules"
  :type 'list
  :group 'helm-perldoc)

(defcustom helm-perldoc:perl5lib nil
  "PERL5LIB environment variable"
  :type '(choice (string :tag "Set this value as PERL5LIB")
                 (boolean :tag "Not use PERL5LIB environment variable" nil))
  :group 'helm-perldoc)

(defvar helm-perldoc:modules nil
  "List of all installed modules")

(defvar helm-perldoc:buffer "*perldoc*")
(defvar helm-perldoc:run-setup-task-flag nil)
(defvar helm-perldoc:module-history nil)

(defvar helm-perldoc:search-command
  (concat (if load-file-name
              (file-name-directory load-file-name)
            default-directory) "helm-perldoc-collect-modules.pl"))

(defmacro with-perl5lib (&rest body)
  (declare (indent 0) (debug t))
  `(let ((process-environment process-environment))
     (when helm-perldoc:perl5lib
       (push (concat "PERL5LIB=" helm-perldoc:perl5lib) process-environment))
     ,@body))

(defun helm-perldoc:collect-installed-modules ()
  (setq helm-perldoc:run-setup-task-flag t)
  (deferred:$
    (deferred:process-buffer
      "perl" helm-perldoc:search-command)
    (deferred:nextc it
      (lambda (buf)
        (with-current-buffer buf
          (goto-char (point-min))
          (setq helm-perldoc:modules
                (cl-loop with modules = nil
                         while (not (eobp))
                         collect
                         (prog1
                             (buffer-substring-no-properties
                              (line-beginning-position) (line-end-position))
                           (forward-line 1))))
          (kill-buffer (current-buffer)))))))

;;;###autoload
(defun helm-perldoc:setup ()
  (interactive)
  (when (or current-prefix-arg (null helm-perldoc:modules))
    (helm-perldoc:collect-installed-modules)))

(defface helm-perldoc:header-module-name
  '((((background dark))
     :foreground "white" :weight bold)
    (((background light))
     :foreground "black" :weight bold))
  "Module name in header"
  :group 'helm-perldoc)

(defun helm-perldoc:exec (cmd &optional mode-func)
  (with-current-buffer (get-buffer-create helm-perldoc:buffer)
    (fundamental-mode) ;; clear old mode
    (view-mode -1)
    (erase-buffer)
    (unless (zerop (with-perl5lib (call-process-shell-command cmd nil t)))
      (error (format "Failed '%s'" cmd)))
    (goto-char (point-min))
    (when mode-func
      (funcall mode-func))
    (view-mode +1)
    (pop-to-buffer (current-buffer))))

(defun helm-perldoc:show-header-line (module type)
  (let ((header-msg (format "\"%s\" %s"
                            module
                            (or (and (eq type :document) "Document")
                                "Source Code"))))
    (with-current-buffer (get-buffer helm-perldoc:buffer)
      (setq header-line-format
            (propertize header-msg 'face 'helm-perldoc:header-module-name)))))

(defvar helm-perldoc:history-source
  '((name . "Perldoc History")
    (candidates . helm-perldoc:module-history)
    (volatile)
    (type . perldoc)))

;;;###autoload
(defun helm-perldoc:history ()
  (interactive)
  (helm :sources '(helm-perldoc:history-source)
        :buffer (get-buffer-create "*helm-perldoc*")))

(defsubst helm-perldoc:register-history (module)
  (add-to-list 'helm-perldoc:module-history module nil 'string=))

(defun helm-perldoc:action-view-document (module)
  (helm-perldoc:register-history module)
  (helm-perldoc:exec (format "perldoc %s" module))
  (helm-perldoc:show-header-line module :document))

(defun helm-perldoc:module-file-path (module)
  (let ((cmd (concat "perldoc -lm " module)))
    (with-temp-buffer
      (unless (zerop (with-perl5lib (call-process-shell-command cmd nil t)))
        (error "Failed: %s" cmd))
      (goto-char (point-min))
      (buffer-substring-no-properties (point) (line-end-position)))))

(defun helm-perldoc:action-view-source (module)
  (helm-perldoc:register-history module)
  (let ((module-file (helm-perldoc:module-file-path module)))
    (find-file-read-only-other-window module-file)))

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
    (when (re-search-backward "^package^\\s-+*" nil t)
      (let ((column (helm-perldoc:point-to-column (match-beginning 0))))
        (forward-line)
        (list :point (point) :column column)))))

(defun helm-perldoc:search-insertion-point ()
  (helm-aif (or (helm-perldoc:search-import-statement)
                (helm-perldoc:search-package-statement))
      it
    (save-excursion
      (goto-char (point-min))
      (cl-loop while (string-match "^#" (thing-at-point 'line))
               do
               (forward-line))
      (list :point (point) :column 0))))

(defun helm-perldoc:construct-import-statement (column modules)
  (let ((spaces (cl-loop for i from 1 to column
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

(defun helm-perldoc:filter-modules (modules)
  (cl-loop for module in modules
           when (and (not (string-match "^[[:digit:]]" module))
                     (not (member module helm-perldoc:ignore-modules)))
           collect module into filtered-modules
           finally
           return (cl-remove-duplicates
                   (sort filtered-modules #'string<) :test #'equal)))

(defun helm-perldoc:search-endline ()
  (with-helm-current-buffer
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "^__\\(?:DATA\\|END\\)__" nil t))))

(defun helm-perldoc:extracted-modules (str)
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (cl-loop while (re-search-forward "\\<\\([a-zA-Z0-9_:]+\\)\\>" nil t)
             collect (match-string-no-properties 1))))

(defun helm-perldoc:superclass-init ()
  (with-helm-current-buffer
    (save-excursion
      (goto-char (point-min))
      (cl-loop with bound = (helm-perldoc:search-endline)
               with regexp = "^\\s-*use\\s-+\\(?:parent\\|base\\)\\s-+\\(?:qw\\)?\\(.+?\\)$"
               while (re-search-forward regexp bound t)
               appending (helm-perldoc:extracted-modules
                          (match-string-no-properties 1)) into supers
               finally
               return (helm-perldoc:filter-modules supers)))))

(defun helm-perldoc:transform-module-path (module)
  (if (string-match-p "/" module)
      (replace-regexp-in-string
       "\\.p[ml]\\'" ""
       (replace-regexp-in-string "/" "::" module))
    module))

(defun helm-perldoc:imported-init ()
  (with-helm-current-buffer
    (save-excursion
      (goto-char (point-min))
      (cl-loop with bound = (helm-perldoc:search-endline)
               with regexp = "^\\s-*\\(?:use\\|require\\)\\s-+\\(['\"]\\)?\\([^'\" \t;]+\\)\\1?"
               while (re-search-forward regexp bound t)
               collect (helm-perldoc:transform-module-path (match-string-no-properties 2)) into modules
               finally
               return (helm-perldoc:filter-modules modules)))))

(defun helm-perldoc:other-init ()
  (unless helm-perldoc:modules
    (if helm-perldoc:run-setup-task-flag
        (error "Please wait. Setup asynchronous task does not complete yet")
      (error "Please exec 'M-x helm-perldoc:setup'")))
  (sort (cl-copy-list helm-perldoc:modules) 'string<))

(defvar helm-perldoc:imported-source
  '((name . "Imported Modules")
    (candidates . helm-perldoc:imported-init)
    (type . perldoc)
    (candidate-number-limit . 9999)))

(defvar helm-perldoc:superclass-source
  '((name . "SuperClass")
    (candidates . helm-perldoc:superclass-init)
    (type . perldoc)
    (candidate-number-limit . 9999)))

(defvar helm-perldoc:other-source
  '((name . "Installed Modules")
    (candidates . helm-perldoc:other-init)
    (type . perldoc)
    (candidate-number-limit . 9999)))

(defun helm-perldoc:check-buffer ()
  (let ((buf (get-buffer helm-perldoc:buffer)))
   (when buf
     (with-current-buffer buf
       (unless (file-directory-p default-directory)
         (kill-buffer))))))

;;;###autoload
(defun helm-perldoc ()
  (interactive)
  (helm-perldoc:check-buffer)
  (helm :sources '(helm-perldoc:imported-source
                   helm-perldoc:superclass-source
                   helm-perldoc:other-source)
        :buffer "*helm-perldoc*"))

(provide 'helm-perldoc)

;;; helm-perldoc.el ends here
