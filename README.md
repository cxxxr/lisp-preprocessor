# Lisp Preprocessor

Common Lisp embedded template engine

## Usage

### input
```common-lisp
#include <stdio.h>

int main(void) {
    #{ :indent
    (loop :for i :from 0 :below 10
          :do (format t "printf(\"Loop: %d\", ~D);~%" i)) #}
    return 0;
}
```

### compile
`(lisp-preprocessor:run-template-into-file #p"input" #p"output")`

### output
```common-lisp
#include <stdio.h>

int main(void) {
    printf("Loop: %d", 0);
    printf("Loop: %d", 1);
    printf("Loop: %d", 2);
    printf("Loop: %d", 3);
    printf("Loop: %d", 4);
    printf("Loop: %d", 5);
    printf("Loop: %d", 6);
    printf("Loop: %d", 7);
    printf("Loop: %d", 8);
    printf("Loop: %d", 9);
    return 0;
}
```

## API

### (run-template-into-stream compiland stream &rest arguments)
### (run-template-into-string compiland &rest arguments)
### (run-template-into-file compiland file &rest arguments)
```common-lisp
(let ((compiland (compile-template "#{ (princ (string-capitalize $arg)) #}" :arguments '($arg))))
  (run-template-into-string compiland "test"))
; => "Test"
```
compiland is a string or pathname, or a compiled template function.

### (compile-template text &key template-begin template-begin arguments)
```common-lisp
* (compile-template "foo #{ (princ \"bar\") #} baz") ; => #<FUNCTION (LAMBDA (#:STREAM)) {...}>
* (run-template-into-stream * *standard-output*)
foo bar baz

* (compile-template "foo {{{ (princ \"bar\" }}} baz" :template-begin "{{{" :template-end "}}}")
* (run-template-into-stream * *standard-output*)
foo bar baz

* (run-template-into-stream (compile-template "#{ (princ (list ? ??)) #}" :arguments '(? ??)) *standard-output* "foo" "bar")
(foo bar)
```

## Syntax

```
#{ template #}
```

### indent
Keep indentation of start position

```
#{ :indent [offset]
...
#}
```

```c
// input
int main(void) {
    #{ :indent
    (loop :for i :from 0 :below 10
          :do (format t "printf(\"Loop: %d\", ~D);~%" i)) #}
    return 0;
}

// output
int main(void) {
    printf("Loop: %d", 0);
    printf("Loop: %d", 1);
    printf("Loop: %d", 2);
    printf("Loop: %d", 3);
    printf("Loop: %d", 4);
    printf("Loop: %d", 5);
    printf("Loop: %d", 6);
    printf("Loop: %d", 7);
    printf("Loop: %d", 8);
    printf("Loop: %d", 9);
    return 0;
}
```

### chop
Chop whitespace before and after the template

```
#{ :chop #}
```

```common-lisp
;;; input
(defun foo (#{ (format t "~{~(~A~)~^ ~}" (loop :for symbol :in '(a b c) :collect symbol)) #}
            #{ :chop #}
            #{ (format t " ~{~(~A~)~^ ~}" (loop :for symbol :in '(x y z) :collect symbol)) #})
  (values a b c x y z))

;;; output
(defun foo (a b c x y z)
  (values a b c x y z))
```

### License
MIT
