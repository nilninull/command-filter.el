# command-filter.el
Emacs utilty macros for making text filter by unix commands.

Emacs has `shell-command-on-region`, but using a long command over
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
(define-command-filter test-filter-multi-1
  ("awk" "-F	" "{print $3}")
  ("sed" "s/.*/\\u&/"))
```
You can omit parentheses, if you like.
```emacs-lisp
(define-command-filter test-filter-2-1 "awk" "{print toupper($0)}")
```
And you can specify multiple commands split by pipe **symbol**.
```emacs-lisp
(define-command-filter test-filter-multi-2
  "awk" "-F	" "{print $3}" | "sed" "s/.*/\\u&/")
```
You can use multiline arguments.

```emacs-lisp
(define-command-filter test-filter-3-1 "python" "-c" "
import sys
for line in sys.stdin:
    print(line.upper(), end='')
")
```
And some other language examples.
```emacs-lisp
(define-command-filter test-filter-4-1 ("ruby" "-p" "-e" "$_.upcase!"))

(define-command-filter test-filter-5-1 "ghc" "-e" "interact (map Data.Char.toUpper)")

(define-command-filter test-filter-6-1 ("perl" "-pe" "$_ = uc $_"))

(define-command-filter test-filter-7-1 "lua" "-e" "for line in io.lines() do print(line:upper()) end")
```
There are special macros for some commands.
```emacs-lisp
(define-sed-filter test-filter-1-2 "s/.*/\\L&/")

(define-awk-filter test-filter-2-2 "{print tolower($0)}")

(define-python-filter test-filter-3-2 "-c" "
import sys
for line in sys.stdin:
    print(line.lower(), end='')
")

(define-ruby-filter test-filter-4-2 "-p" "-e" "$_.downcase!")

(define-ghc-filter test-filter-5-2 "-e" "interact (map Data.Char.toLower)")

(define-perl-filter test-filter-6-2 "-pe" "$_ = lc $_")

(define-lua-filter test-filter-7-2 "-e" "for line in io.lines() do print(line:lower()) end")
```

## Appendix
This macro defines a function that takes a string as an argument
and returns a string processed by the command.

This `define-pipe-filter` macro may not be used.  However, I made
it because it can be created as it is with the mechanism of
`define-command-filter` macro.

```emacs-lisp
(define-pipe-filter test-filter-8 "awk" "-F," "$1~/TARGET/{print $2}" | "sort" | "uniq" "-c")
```

If `:chomp` keyword specified, remove newline charactors from end of output.

```emacs-lisp
(define-pipe-filter test-filter-9 :chomp "awk" "-F," "$1~/TARGET/{print $2}" | "sort" "-u" | "wc" "-l")
```
The function made by this macro can work without a input string depending on the command defined.

```emacs-lisp
(define-pipe-filter test-filter-10 :chomp "date")
```
