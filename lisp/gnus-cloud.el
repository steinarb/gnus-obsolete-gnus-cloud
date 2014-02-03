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

(defvar gnus-cloud-version 1)

(defvar gnus-cloud-method nil
  "The IMAP select method used to store the cloud data.")

(defun gnus-cloud-make-chunk (elems)
  (with-temp-buffer
    (insert (format "Version %s\n" gnus-cloud-version))
    (insert (gnus-cloud-insert-data elems))
    (buffer-string)))

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
	  (insert (format "(:file %S %S %d)\n"
			  (cadr elem)
			  (format-time-string
			   "%FT%T%z" (nth 5 (file-attributes (cadr elem))))
			  length))
	  (insert data)
	  (insert "\n")))
       ((eq (car elem) :buffer)
	(insert (format "(:data %S %d)\n" (cadr elem)
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
  (save-excursion
    (goto-char (point-min))
    (unless (looking-at "Version \\([0-9]+\\)")
      (error "Not a valid Cloud chunk in the current buffer"))
    (forward-line 1)
    (let ((version (string-to-number (match-string 1)))
	  (data (buffer-substring (point) (point-max))))
      (mm-with-unibyte-buffer
	(insert data)
	(cond
	 ((= version 1)
	  (gnus-cloud-decode-data)
	  (gnus-cloud-parse-version-1))
	 (t
	  (error "Unsupported Cloud chunk version %s" version)))))))

(defun gnus-cloud-parse-version-1 ()
  (let ((elems nil))
    (while (not (eobp))
      (while (and (not (eobp))
		  (not (looking-at "(:file\\|(:data")))
	(forward-line 1))
      (unless (eobp)
	(let ((spec (ignore-errors (read (current-buffer))))
	      length)
	  (when (and (consp spec)
		     (or (eq (car spec) :file)
			 (eq (car spec) :data)))
	    (setq length (car (last spec)))
	    (push (append (butlast spec)
			  (list
			   (buffer-substring (1+ (point))
					     (+ (point) 1 length))))
		  elems)
	    (goto-char (+ (point) 1 length))))))
    (nreverse elems)))

(defun gnus-cloud-update-data (elems)
  (dolist (elem elems)
    (cond
     ((eq (car elem) :data)
      )
     ((eq (car elem) :file)
      (unless (= (length elem) 4)
	(error "Invalid length of a file spec: %s" (length elem)))
      (gnus-cloud-update-file (cdr elem)))
     (t
      (error "Unknown type %s" (car elem))))))

(defun gnus-cloud-update-file (elem)
  (let ((file-name (pop elem))
	(date (pop elem))
	(contents (pop elem)))
    (unless (gnus-cloud-file-covered-p file-name)
      (message "%s isn't covered by the cloud; ignoring" file-name))
    (when (or (not (file-exists-p file-name))
	      (and (file-exists-p file-name)
		   (mm-with-unibyte-buffer
		     (insert-file-contents-literally file-name)
		     (not
		      (equal (buffer-string (point-min) (point-max))
			     contents)))))
      (gnus-cloud-replace-file file-name date contents))))

(defun gnus-cloud-replace-file (file-name date new-contents)
  (mm-with-unibyte-buffer
    (insert new-contents)
    (when (file-exists-p file-name)
      (let ((backup (car (find-backup-file-name file-name))))
	(rename-file file-name backup)))
    (write-region (point-min) (point-max) file-name)))

(defun gnus-cloud-file-covered-p (file-name)
  (let ((matched nil))
    (dolist (regexp gnus-cloud-synced-files)
      (when (string-match regexp file-name)
	(setq matched t)))
    matched))

(provide 'gnus-cloud)

;;; gnus-cloud.el ends here
