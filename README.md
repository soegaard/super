# super

## Introduction

The `super` language is a meta-language that adds features
to an existing racket language.

Use it as:

    #lang super <lang>

where <lang> is the name of an language like racket, racket or similar.



## Field and Method Access, Method Calls

In a super language identifiers with dots can be used for field access
and for method calls.

1. o.f                       access field f of object o

2. o.f1.f2                   access field f2 of object o.f1

3. (o .m a ...)              invoke method m on object o wth arguments a...

4. (o .m1 a1 ... .m2 a2 ...) same as ((o .m1 a1 ...) .m2 a2 ...)

5. (o.m a ...)               invoke method m on object o wth arguments a... 

6. (o.m a ... .m1 a1 ...)    invoke method m1 on resultof object (o.m a ...) with arguments a2 ... 


## Indexing with square brackets

In a super language the reader reads an expressions of the form

    id[expr ...]      (no space between the identifier and the bracket)

as 

    (#%ref id expr ...).
   
A default binding for `#%ref` is provide that allows indexing for
vectors, lists, strings and bytes.

The default bindings is defined like this:

    (define-syntax (#%ref stx)
      (syntax-parse stx 
        [(#%ref x:id index:expr)
         (syntax/loc stx
           (let ([i index])
             (cond
               [(vector? x) (vector-ref x i)]
               [(list?   x) (list-ref   x i)]
               [(string? x) (string-ref x i)]
               [(bytes?  x) (bytes-ref  x i)]
               [else
                (error '#%ref (~a "expected a vector, list, string or byte string, got: " x))])))]))
