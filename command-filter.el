;;; command-filter.el --- simple utilty macro for making text filter by unix command  -*- lexical-binding: t; -*-

;; Copyright (C) 2015, 2019  nilninull

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
;; Sometimes, I would like to apply unix commands to Emacs buffers and regions.
;;
;; Emacs has `shell-command-on-region', but using a long command over
;; and over is troublesome.
;;
;; That's why I wrote this program.
;;
;; This filter program affects the region when the region is active,
;; and the entire buffer when the region is inactive.
;;
;;; Examples:
;; ;; Define filter commands like these.
;;
;; (define-command-filter test-filter-1-1 ("sed" "s/.*/\\U&/"))
;;
;; ;; You can specify multiple commands like shell pipes behavior.
;;
;; (define-command-filter test-filter-multi-1
;;   ("awk" "-F	" "{print $3}")
;;   ("sed" "s/.*/\\u&/"))
;;
;; ;; You can omit parentheses, if you like.
;;
;; (define-command-filter test-filter-2-1 "awk" "{print toupper($0)}")
;;
;; ;; And you can specify multiple commands split by pipe *symbol*.
;;
;; (define-command-filter test-filter-multi-2
;;   "awk" "-F	" "{print $3}" | "sed" "s/.*/\\u&/")
;;
;; ;; You can use multiline arguments.
;;
;; (define-command-filter test-filter-3-1 "python" "-c" "
;; import sys
;; for line in sys.stdin:
;;     print(line.upper(), end='')
;; ")
;;
;; ;; And some other language examples.
;;
;; (define-command-filter test-filter-4-1 ("ruby" "-p" "-e" "$_.upcase!"))
;;
;; (define-command-filter test-filter-5-1 "ghc" "-e" "interact (map Data.Char.toUpper)")
;;
;; (define-command-filter test-filter-6-1 ("perl" "-pe" "$_ = uc $_"))
;;
;; (define-command-filter test-filter-7-1 "lua" "-e" "for line in io.lines() do print(line:upper()) end")
;;
;; ;; There are special macros for some commands.
;;
;; (define-sed-filter test-filter-1-2 "s/.*/\\L&/")
;;
;; (define-awk-filter test-filter-2-2 "{print tolower($0)}")
;;
;; (define-python-filter test-filter-3-2 "-c" "
;; import sys
;; for line in sys.stdin:
;;     print(line.lower(), end='')
;; ")
;;
;; (define-ruby-filter test-filter-4-2 "-p" "-e" "$_.downcase!")
;;
;; (define-ghc-filter test-filter-5-2 "-e" "interact (map Data.Char.toLower)")
;;
;; (define-perl-filter test-filter-6-2 "-pe" "$_ = lc $_")
;; 
;; (define-lua-filter test-filter-7-2 "-e" "for line in io.lines() do print(line:lower()) end")
;;
;;; Appendix:
;; ;; This macro defines a function that takes a string as an argument
;; ;; and returns a string processed by the command.
;;
;; (define-pipe-filter test-filter-8 "awk" "-F," "$2~/TARGET/{print $3}" | "sort" | "uniq" "-c")
;;
;; ;; If :chomp keyword specified, remove newline charactors from end
;; ;; of output.
;;
;; (define-pipe-filter test-filter-9 :chomp "awk" "-F," "$2~/TARGET/{print $3}" | "sort" | "uniq" | "wc" "-l")
;;
;; ;; The function made by this macro can work without a input string
;; ;; depending on the command defined.
;;
;; (define-pipe-filter test-filter-10 :chomp "date")

;;; Code:
(eval-when-compile (require 'subr-x))

(defmacro define-command-filter (name &rest cmd-series)
  "Define new command-filter.

NAME used by filter name like command-filter--NAME.
CMD-SERIES is a list of shell command lists or shell command lists.

If you want to see an example, please read the comment of the program."

  (when (stringp (car cmd-series))
    (setq cmd-series (cl-labels ((split-by-pipe (cmds)
                                                (if-let ((i (cl-position '| cmds)))
                                                    (cons (seq-take cmds i) (split-by-pipe (seq-drop cmds (1+ i))))
                                                  (list cmds))))
                       (split-by-pipe cmd-series))))
  `(defun ,(intern (format "command-filter--%s" name)) ()
     "This filter program affects the region when the region is active,
and the entire buffer when the region is inactive.

This function was made by `define-command-filter' macro."
     (interactive)
     (let ((temp-files (cl-loop repeat ,(1+ (length cmd-series))
                                collect (make-temp-file ".command-filter-")))
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
  "Define sed filter command.

NAME used by filter name like command-filter--NAME
OPTIONS are passed to the program."
  `(define-command-filter ,name "sed" ,@options))

(defmacro define-awk-filter (name &rest options)
  "Define awk filter command.

NAME used by filter name like command-filter--NAME
OPTIONS are passed to the program."
  `(define-command-filter ,name "awk" ,@options))

(defmacro define-python-filter (name &rest options)
  "Define python filter command.

NAME used by filter name like command-filter--NAME
OPTIONS are passed to the program."
  `(define-command-filter ,name "python" ,@options))

(defmacro define-ruby-filter (name &rest options)
  "Define ruby filter command.

NAME used by filter name like command-filter--NAME
OPTIONS are passed to the program."
  `(define-command-filter ,name "ruby" ,@options))

(defmacro define-ghc-filter (name &rest options)
  "Define ghc filter command.

NAME used by filter name like command-filter--NAME
OPTIONS are passed to the program."
  `(define-command-filter ,name "ghc" ,@options))

(defmacro define-perl-filter (name &rest options)
  "Define perl filter command.

NAME used by filter name like command-filter--NAME
OPTIONS are passed to the program."
  `(define-command-filter ,name "perl" ,@options))

(defmacro define-lua-filter (name &rest options)
  "Define lua filter command.

NAME used by filter name like command-filter--NAME
OPTIONS are passed to the program."
  `(define-command-filter ,name "lua" ,@options))

;;; Appendix
;; This `define-pipe-filter' macro may not be used.  However, I
;; made it because it can be created as it is with the mechanism of
;; `define-command-filter' macro.

(defmacro define-pipe-filter (name &rest cmd-series)
  "Define new pipe-filter.

pipe-filter pass the argument strings to the CMD-SERIES
processes, and return process's output strings.

NAME used by filter name like pipe-filter--NAME.
CMD-SERIES is a list of shell command lists or shell command lists.

If :chomp keyword appeared in CMD-SERIES, return strings remove
newline charactors from end of string.

If you want to see an example, please read the comment of the program."

  (let ((chomp (when (memq :chomp cmd-series)
                 (setq cmd-series (delq :chomp cmd-series))
                 t)))
    (when (stringp (car cmd-series))
      (setq cmd-series (cl-labels ((split-by-pipe (cmds)
                                                  (if-let ((i (cl-position '| cmds)))
                                                      (cons (seq-take cmds i) (split-by-pipe (seq-drop cmds (1+ i))))
                                                    (list cmds))))
                         (split-by-pipe cmd-series))))
    `(defun ,(intern (format "pipe-filter--%s" name)) (&optional str)
       "This function processes STR with commands.

Depending on the command defined, this function will work without an input string.

This function was made by `define-pipe-filter' macro."
       (let ((temp-files (cl-loop repeat ,(1+ (length cmd-series))
                                  collect (make-temp-file ".pipe-filter-")))
             (cmd-series ',cmd-series))
         (unwind-protect
             (with-temp-buffer
               (when str
                 (insert str)
                 (write-region (point-min) (point-max) (car temp-files))
                 (delete-region (point-min) (point-max)))

               (insert-file-contents
                (cl-labels ((do-process (in out)
                                        (cl-destructuring-bind (cmd . args) (pop cmd-series)
                                          (apply #'call-process cmd in (list :file out) nil args))
                                        out))
                  (cl-reduce #'do-process temp-files)))
               ,(when chomp
                  `(progn
                     (goto-char (point-max))
                     (while (= (preceding-char) ?\n) (delete-char -1))))
               (buffer-string))
           (dolist (file temp-files)
             (delete-file file)))))))

(provide 'command-filter)
;;; command-filter.el ends here
