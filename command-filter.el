;;; command-filter.el --- simple utilty macro for making text filter by unix command  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  nilninull

;; Author: nilninull <nilninull@gmail.com>
;; Keywords: convenience

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

;;

;;; Code:

(defmacro define-command-filter (name &rest cmd-series)
  "
  (define-command-filter count-numbers
    (\"sed\" \"s/[^0-9]\\+/ /g\")
    (\"wc\" \"-w\"))
"
  (when (stringp (car cmd-series))
    (setq cmd-series (list cmd-series)))
  `(defun ,(intern (format "command-filter-%s" name)) ()
     "This function was made by `define-command-filter'"
     (interactive)
     (let ((temp-files (cl-loop repeat ,(1+ (length cmd-series))
                                collect (make-temp-file "command-filter-")))
           (cmd-series ',cmd-series)
           beg end)

       (if (region-active-p)
           (setq beg (region-beginning)
                 end (region-end))
         (setq beg (point-min)
               end (point-max)))

       (unwind-protect
           (progn
             (write-region beg end (car temp-files))
             (delete-region beg end)

             (insert-file-contents
              (cl-labels ((do-process (in out)
                                      (cl-destructuring-bind (cmd . args) (pop cmd-series)
                                        (apply #'call-process cmd in (list :file out) nil args))
                                      out))
                (cl-reduce #'do-process temp-files)))

             ;; (cl-mapcar (lambda (cmd in out)
             ;;              (cl-destructuring-bind (cmd . args) cmd
             ;;                (apply #'call-process cmd in (list :file out) nil args)))
             ;;            cmd-series temp-files (cdr temp-files))
             ;; (insert-file-contents (car (last temp-files)))
             )
         (dolist (file temp-files)
           (delete-file file))))))

(defmacro define-sed-filter (name &rest options)
  ""
  `(define-command-filter ,name ("sed" ,@options)))

(defmacro define-awk-filter (name &rest options)
  ""
  `(define-command-filter ,name ("awk" ,@options)))

(provide 'command-filter)
;;; command-filter.el ends here
