# Lisp Preprocessor

Common Lisp embedded template engine

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
