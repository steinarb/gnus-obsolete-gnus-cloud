;;; gnustest-gnus-util.el --- Selectived tests only.
;; Copyright (C) 2015 Free Software Foundation, Inc.

;; Author: Jens Lechtenbörger <jens.lechtenboerger@fsfe.org>

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The tests here are restricted to three functions:
;; gnus-test-list, gnus-subsetp, gnus-setdiff
;;
;; Run as follows:
;; emacs -Q -batch -L .. -l gnustest-gnus-util.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'gnus-util)

(ert-deftest test-list ()
  ;; False for non-lists.
  (should-not (gnus-test-list 1 'listp))
  (should-not (gnus-test-list "42" 'listp))

  ;; False for empty lists.
  (should-not (gnus-test-list '() 'listp))
  (should-not (gnus-test-list '() 'nlistp))

  ;; Real tests for other lists.
  (should (gnus-test-list '(()) 'listp))
  (should (gnus-test-list '(() ()) 'listp))
  (should-not (gnus-test-list '(1) 'listp))
  (should-not (gnus-test-list '(() 1) 'listp))
  (should-not (gnus-test-list '(1 ()) 'listp))
  (should-not (gnus-test-list '(() 1 ()) 'listp))
  )

(ert-deftest subsetp ()
  ;; False for non-lists.
  (should-not (gnus-subsetp "1" "1"))
  (should-not (gnus-subsetp "1" '("1")))
  (should-not (gnus-subsetp '("1") "1"))

  ;; Real tests.
  (should (gnus-subsetp '() '()))
  (should (gnus-subsetp '() '("1")))
  (should (gnus-subsetp '("1") '("1")))
  (should (gnus-subsetp '(42) '("1" 42)))
  (should (gnus-subsetp '(42) '(42 "1")))
  (should (gnus-subsetp '(42) '("1" 42 2)))
  (should-not (gnus-subsetp '("1") '()))
  (should-not (gnus-subsetp '("1") '(2)))
  (should-not (gnus-subsetp '("1" 2) '(2)))
  (should-not (gnus-subsetp '(2 "1") '(2)))
  (should-not (gnus-subsetp '("1" 2) '(2 3)))

  ;; Duplicates don't matter for sets.
  (should (gnus-subsetp '("1" "1") '("1")))
  (should (gnus-subsetp '("1" 2 "1") '(2 "1")))
  (should (gnus-subsetp '("1" 2 "1") '(2 "1" "1" 2)))
  (should-not (gnus-subsetp '("1" 2 "1" 3) '(2 "1" "1" 2))))

(ert-deftest setdiff ()
  ;; False for non-lists.
  (should-not (gnus-setdiff "1" "1"))
  (should-not (gnus-setdiff "1" '()))
  (should-not (gnus-setdiff '() "1"))

  ;; Real tests.
  (should-not (gnus-setdiff '() '()))
  (should-not (gnus-setdiff '() '("1")))
  (should-not (gnus-setdiff '("1") '("1")))
  (should (equal '("1") (gnus-setdiff '("1") '())))
  (should (equal '("1") (gnus-setdiff '("1") '(2))))
  (should (equal '("1") (gnus-setdiff '("1" 2) '(2))))
  (should (equal '("1") (gnus-setdiff '("1" 2 3) '(3 2))))
  (should (equal '("1") (gnus-setdiff '(2 "1" 3) '(3 2))))
  (should (equal '("1") (gnus-setdiff '(2 3 "1") '(3 2))))
  (should (equal '(2 "1") (gnus-setdiff '(2 3 "1") '(3))))

  ;; Duplicates aren't touched for sets if they are not removed.
  (should-not (gnus-setdiff '("1" "1") '("1")))
  (should (equal '("1") (gnus-setdiff '(2 "1" 2) '(2))))
  (should (equal '("1" "1") (gnus-setdiff '(2 "1" 2 "1") '(2)))))

;;; gnustest-gnus-util.el ends here
