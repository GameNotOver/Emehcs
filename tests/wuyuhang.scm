;; 工具函数
;; 不支持 set
(load "utils.scm")
(define apply-closure
  (λ (n₁ n₂)
    (n₁ n₂)))
;; 使用 box 装箱值
(define make-closure
  (λ (x body env)
    (λ (y)
      (val-of-cbv body (extend-env x y env)))))
(define extend-env
  (λ (x v env)
    (λ (y) (if (eqv? y x) v (apply-env env y)))))
(define apply-env
  (λ (env y)
    (env y)))
(define empty-env
  (λ ()
    (λ (y)
      (errorf 'env "unbound variable: ~s" y))))
;;
;; call-by-value 解释器
(define val-of-cbv
  (λ (exp env)
    (pmatch exp
      [,y (guard (symbol? y)) (unbox (apply-env env y))]
      [,b (guard (boolean? b)) b]
      [,n (guard (number? n)) n]
      [(zero? ,n) (zero? (val-of-cbv n env))]
      [(+ ,n₁ ,n₂) (+ (val-of-cbv n₁ env) (val-of-cbv n₂ env))]
      [(if ,test ,conseq ,alt) (if (val-of-cbv test env)
                                   (val-of-cbv conseq env)
                                   (val-of-cbv alt env))]
      [(begin2 ,e₁ ,e₂) (begin (val-of-cbv e₁ env) (val-of-cbv e₂ env))]
      [(set! ,x ,rhs) (set-box! (apply-env env x) (val-of-cbv rhs env))]
      [(λ (,x) ,body) (make-closure x body env)]
      [(,rator ,rand) (apply-closure (val-of-cbv rator env)
                                     (box (val-of-cbv rand env)))])))