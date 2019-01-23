# command-filter.el
Emacs utilty macros for making text filter by unix commands.

Emacs has `shell-command-on-region', but using a long command over
and over is troublesome.

That's why I wrote this program.

This filter program affects the region when the region is active,
and the entire buffer when the region is inactive.

## Examples
Define filter commands like these.
```emacs-lisp
(define-command-filter test-filter-1-1 ("sed" "s/.*/\\U&/"))
```
You can specify multiple commands like shell pipes behavior.
```emacs-lisp
(define-command-filter test-filter-multi
  ("awk" "-F	" "{print $3}")
  ("sed" "s/.*/\\u&/"))
```
If you use only one command, you can omit parentheses.
```emacs-lisp
(define-command-filter test-filter-2-1 "awk" "{print toupper($0)}")
```
You can use multiline arguments.

```emacs-lisp
(define-command-filter test-filter-3-1 "python" "-c" "
import sys;
for line in sys.stdin:
    print(line.upper(), end='')
")
```
And some other language examples.
```emacs-lisp
(define-command-filter test-filter-4-1 ("ruby" "-p" "-e" "$_.upcase!"))

(define-command-filter test-filter-5-1 ("ghc" "-e" "interact (map Data.Char.toUpper)"))
```
There are special macros for some commands.
```emacs-lisp
(define-sed-filter test-filter-1-2 "s/.*/\\L&/")

(define-awk-filter test-filter-2-2 "{print tolower($0)}")

(define-python-filter test-filter-3-2 "-c" "
import sys;
for line in sys.stdin:
    print(line.lower(), end='')
")

(define-ruby-filter test-filter-4-2 "-p" "-e" "$_.downcase!")

(define-ghc-filter test-filter-5-2 "-e" "interact (map Data.Char.toLower)")
```
