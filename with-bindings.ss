(module with-bindings mzscheme
  (require (lib "servlet.ss" "web-server"))
  (provide (all-defined))
  
  (define-syntax thunk
    (syntax-rules ()
      [(_ body ...)
       (lambda () body ...)]))
  (define (ident val)
    (thunk val))
  
  (define-syntax with-binding
    (syntax-rules ()
      [(_ bindings (single-name ...) body ...)
       (with-bindings bindings
                      ()
                      (single-name ...)
                      ()
                      body ...)]))  
  (define-syntax with-bindings
    (syntax-rules ()
      [(_ bindings (bool-name ...) (single-name ...) (multiple-name ...) body ...)
       (with-bindings/default-values bindings
                               (bool-name ...)
                               ((single-name #f) ...)
                               ((multiple-name (list)) ...)
                               body ...)]))
  (define-syntax with-bindings/default-values
    (syntax-rules ()
      [(_ bindings (bool-name ...) ((single-name single-default) ...) ((multiple-name multiple-default) ...) body ...)
       (with-bindings/defaults bindings 
                               (bool-name ...) 
                               ((single-name (ident single-default)) ...) 
                               ((multiple-name (ident multiple-default)) ...) 
                               body ...)]))
  (define-syntax with-bindings/defaults
    (syntax-rules ()
      [(_ bindings (bool-name ...) ((single-name single-default-thunk) ...) ((multiple-name multiple-default-thunk) ...) body ...)
       (let ([the-bindings bindings])
         (let ([bool-name (exists-binding? 'bool-name the-bindings)]
               ...
               [single-name (if (exists-binding? 'single-name the-bindings)
                                (extract-binding/single 'single-name the-bindings)
                                (single-default-thunk))]
               ...
               [multiple-name (if (exists-binding? 'multiple-name the-bindings)
                                  (extract-bindings 'multiple-name the-bindings)
                                  (multiple-default-thunk))]
               ...)
           body
           ...))])))