;;; gnus-cloud.el --- storing and retrieving data via IMAP

;; Copyright (C) 2014 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: mail

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl))

(defgroup gnus-cloud nil
  "Syncing Gnus data via IMAP."
  :group 'gnus)

(defcustom gnus-cloud-synced-files '("~/\\.authinfo"
				     "~/\\.authinfo\\.gpg"
				     "~/\\.gnus\\.el"
				     "~/News/.*.SCORE")
  "List of file regexps that should be kept up-to-date via the cloud."
  :group 'gnus-cloud
  :type '(repeat regexp))

(defvar gnus-cloud-version "0.1")

(defun gnus-cloud-make-chunk (elems)
  (with-temp-buffer
    (insert (format "Version %s\n" gnus-cloud-version))
    (insert (gnus-cloud-insert-data elems))))

(defun gnus-cloud-insert-data (elems)
  (mm-with-unibyte-buffer
    (dolist (elem elems)
      (cond
       ((eq (car elem) :file)
	(let (length data)
	  (mm-with-unibyte-buffer
	    (insert-file-contents-literally (cadr elem))
	    (setq length (buffer-size)
		  data (buffer-string)))
	  (insert (format "file %S %s %d\n"
			  (cadr elem)
			  (format-time-string
			   "%FT%T%z" (nth 5 (file-attributes (cadr elem))))
			  length))
	  (insert data)
	  (insert "\n")))
       ((eq (car elem) :buffer)
	(insert (format "data %S %d\n" (cadr elem)
			(with-current-buffer (caddr elem)
			  (buffer-size))))
	(insert-buffer-substring (caddr elem))
	(insert "\n"))))
    (gnus-cloud-encode-data)
    (buffer-string)))

(defun gnus-cloud-encode-data ()
  (call-process-region (point-min) (point-max) "gzip"
		       t (current-buffer) nil
		       "-c")
  (base64-encode-region (point-min) (point-max)))

(defun gnus-cloud-decode-data ()
  (base64-decode-region (point-min) (point-max))
  (call-process-region (point-min) (point-max) "gunzip"
		       t (current-buffer) nil
		       "-c"))

(defun gnus-cloud-parse-chunk ()
  )

(provide 'gnus-cloud)

;;; gnus-cloud.el ends here
