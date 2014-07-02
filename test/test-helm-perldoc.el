;;; test-helm-perldoc.el --- tests for helm-perldoc

;; Copyright (C) 2014 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; Version: 0.01

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

(require 'ert)
(require 'helm-perldoc)

(ert-deftest filter-modules ()
  "filtering modules"
  (let ((modules '("123" "strict" "LWP::UserAgent" "Furl" "B::Tap" "Furl")))
    (let ((got (helm-perldoc:filter-modules modules))
          (expected '("B::Tap" "Furl" "LWP::UserAgent")))
      (should (equal got expected)))))

(ert-deftest construct-library-path-without-optional-paths ()
  "construct library path without optional path(carton and PERL5LIB)"
  (let ((got (helm-perldoc:construct-perl5lib)))
    (should (string= got ""))))

(ert-deftest construct-library-path-with-perl5lib ()
  "construct library path with perl5lib"
  (let ((helm-perldoc:perl5lib "/home/perl/lib/perl5"))
    (let ((got (helm-perldoc:construct-perl5lib)))
      (should (string= got "/home/perl/lib/perl5")))))

(ert-deftest construct-library-path-with-cpan-path ()
  "construct library path with cpan path"
  (let ((helm-perldoc:carton-paths '("/lib1/perl5" "/lib2/perl5")))
    (let ((got (helm-perldoc:construct-perl5lib)))
      (should (string= "/lib1/perl5:/lib2/perl5" got)))))

(ert-deftest construct-library-path-both ()
  "construct library path with perl5lib and cpan path"
  (let ((helm-perldoc:perl5lib "/lib3/perl5")
        (helm-perldoc:carton-paths '("/lib1/perl5" "/lib2/perl5")))
    (let ((got (helm-perldoc:construct-perl5lib)))
      (should (string= "/lib1/perl5:/lib2/perl5:/lib3/perl5" got)))))

(ert-deftest construct-import-statement ()
  "construct module import statements"
  (let ((got (helm-perldoc:construct-import-statement 4 '("strict" "Amon2"))))
    (should (string= "    use strict;\n    use Amon2;\n" got))))

(provide 'test-helm-perldoc)

;;; test-helm-perldoc.el ends here
