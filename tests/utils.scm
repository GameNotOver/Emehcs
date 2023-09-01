;; 定义 λ 为 lambda 语法
(define-syntax λ
  (syntax-rules ()
    [(_ arg* bd bd* ...)
      (lambda arg* bd bd* ...)]))

;; 定义 𝑓 为 define 语法
(define-syntax 𝒅𝒆𝒇
  (syntax-rules ()
    [(_ name bd)
      (define name bd)]))

(define-syntax pmatch
  (syntax-rules (else guard)
    [(_ (rator rand ...) cs ...)
      (let ([v (rator rand ...)])
        (pmatch v cs ...))]
    [(_ v) (errorf 'pmatch "failed: ~s" v)]
    [(_ v (else e0 e ...))
      (begin e0 e ...)]
    [(_ v (pat (guard g ...) e0 e ...) cs ...)
      (let ([fk (lambda () (pmatch v cs ...))])
        (ppat v pat 
          (if (and g ...)
              (begin e0 e ...)
              (fk))
          (fk)))]
    [(_ v (pat e0 e ...) cs ...)
      (let ([fk (lambda () (pmatch v cs ...))])
        (ppat v pat (begin e0 e ...) (fk)))]))

(define-syntax ppat
  (syntax-rules (quote unquote)
    [(_ v () kt kf)
      (if (null? v) kt kf)]
    [(_ v (quote lit) kt kf)
      (if (equal? v (quote lit)) kt kf)]
    [(_ v (unquote var) kt kf)
      (let ([var v]) kt)]
    [(_ v (x . y) kt kf)
      (if (pair? v)
          (let ([vx (car v)]
                [vy (cdr v)])
            (ppat vx x (ppat vy y kt kf) kf))
          kf)]
    [(_ v lit kt kf)
      (if (equal? v (quote lit)) kt kf)]))

(𝒅𝒆𝒇 void?
  (λ (x)
    (if (eq? x 'void) #t #f)))

(define-syntax test-runner
  (syntax-rules (>)
    [(_) 'done]
    [(_ > test result more ...)
      (let ([t test])
        (assert (equal? t 'result))
        (test-runner more ...))]))