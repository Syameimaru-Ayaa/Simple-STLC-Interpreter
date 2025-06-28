#lang racket

;; =============================================================================
;; 类型定义+底层设计
;; =============================================================================

; 类型数据结构
(define-values (struct:int-type make-int-type int-type?
                int-type-field-count set-int-type-field!)
  (make-struct-type 'int-type #f 0 0 'prefab))

(define-values (struct:bool-type make-bool-type bool-type?
                bool-type-field-count set-bool-type-field!)
  (make-struct-type 'bool-type #f 0 0 'prefab))

(define-values (struct:fun-type make-fun-type fun-type?
                fun-type-field-count set-fun-type-field!)
  (make-struct-type 'fun-type #f 2 0 'prefab))

(define fun-type-param (make-struct-field-accessor fun-type-field-count 0))
(define fun-type-return (make-struct-field-accessor fun-type-field-count 1))

; 表达式数据结构
(define-values (struct:var-expr make-var-expr var-expr?
                var-expr-field-count set-var-expr-field!)
  (make-struct-type 'var-expr #f 1 0 'prefab))
(define var-expr-name (make-struct-field-accessor var-expr-field-count 0))

(define-values (struct:int-expr make-int-expr int-expr?
                int-expr-field-count set-int-expr-field!)
  (make-struct-type 'int-expr #f 1 0 'prefab))
(define int-expr-value (make-struct-field-accessor int-expr-field-count 0))

(define-values (struct:bool-expr make-bool-expr bool-expr?
                bool-expr-field-count set-bool-expr-field!)
  (make-struct-type 'bool-expr #f 1 0 'prefab))
(define bool-expr-value (make-struct-field-accessor bool-expr-field-count 0))

(define-values (struct:lambda-expr make-lambda-expr lambda-expr?
                lambda-expr-field-count set-lambda-expr-field!)
  (make-struct-type 'lambda-expr #f 3 0 'prefab))
(define lambda-expr-param (make-struct-field-accessor lambda-expr-field-count 0))
(define lambda-expr-param-type (make-struct-field-accessor lambda-expr-field-count 1))
(define lambda-expr-body (make-struct-field-accessor lambda-expr-field-count 2))

(define-values (struct:app-expr make-app-expr app-expr?
                app-expr-field-count set-app-expr-field!)
  (make-struct-type 'app-expr #f 2 0 'prefab))
(define app-expr-fun (make-struct-field-accessor app-expr-field-count 0))
(define app-expr-arg (make-struct-field-accessor app-expr-field-count 1))

(define-values (struct:if-expr make-if-expr if-expr?
                if-expr-field-count set-if-expr-field!)
  (make-struct-type 'if-expr #f 3 0 'prefab))
(define if-expr-cond (make-struct-field-accessor if-expr-field-count 0))
(define if-expr-then (make-struct-field-accessor if-expr-field-count 1))
(define if-expr-else (make-struct-field-accessor if-expr-field-count 2))

(define-values (struct:binop-expr make-binop-expr binop-expr?
                binop-expr-field-count set-binop-expr-field!)
  (make-struct-type 'binop-expr #f 3 0 'prefab))
(define binop-expr-op (make-struct-field-accessor binop-expr-field-count 0))
(define binop-expr-left (make-struct-field-accessor binop-expr-field-count 1))
(define binop-expr-right (make-struct-field-accessor binop-expr-field-count 2))

; 值数据结构
(define-values (struct:int-val make-int-val int-val?
                int-val-field-count set-int-val-field!)
  (make-struct-type 'int-val #f 1 0 'prefab))
(define int-val-value (make-struct-field-accessor int-val-field-count 0))

(define-values (struct:bool-val make-bool-val bool-val?
                bool-val-field-count set-bool-val-field!)
  (make-struct-type 'bool-val #f 1 0 'prefab))
(define bool-val-value (make-struct-field-accessor bool-val-field-count 0))

(define-values (struct:closure-val make-closure-val closure-val?
                closure-val-field-count set-closure-val-field!)
  (make-struct-type 'closure-val #f 3 0 'prefab))
(define closure-val-param (make-struct-field-accessor closure-val-field-count 0))
(define closure-val-body (make-struct-field-accessor closure-val-field-count 1))
(define closure-val-env (make-struct-field-accessor closure-val-field-count 2))

;; =============================================================================
;; 环境管理
;; =============================================================================

(define (empty-env)
  (hash))

(define (env-extend env var val)
  (hash-set env var val))

(define (env-lookup env var)
  (hash-ref env var
            (lambda () (error "Unbound variable: ~a" var))))

(define (env-has? env var)
  (hash-has-key? env var))

;; =============================================================================
;; 解析器 (Parser)
;; =============================================================================

; 解析类型
(define (parse-type type-expr)
  (cond
    [(equal? type-expr 'Int) (make-int-type)]
    [(equal? type-expr 'Bool) (make-bool-type)]
    [(and (list? type-expr) 
          (= (length type-expr) 3)
          (equal? (list-ref type-expr 1) '->))
     (let ([param-type (parse-type (list-ref type-expr 0))]
           [return-type (parse-type (list-ref type-expr 2))])
       (make-fun-type param-type return-type))]
    [else (error "Invalid type: ~a" type-expr)]))

; 验证格式 (x : Int)
(define (valid-lambda-param? param-list)
  (and (list? param-list)
       (= (length param-list) 3)
       (symbol? (list-ref param-list 0))        ; 参数名
       (equal? (list-ref param-list 1) ':)      ; (x : Int)的冒号，这种魔法数字有时间包装一下
       ; 第三个元素可以是符号或列表比如说(x : (Int -> Int))
       (or (symbol? (list-ref param-list 2))
           (list? (list-ref param-list 2)))))

; 本体
(define (parse expr)
  (cond
    ; 字面量
    [(integer? expr) (make-int-expr expr)]
    [(equal? expr #t) (make-bool-expr #t)]
    [(equal? expr #f) (make-bool-expr #f)]
    [(equal? expr 'true) (make-bool-expr #t)]
    [(equal? expr 'false) (make-bool-expr #f)]
    
    ; 变量
    [(symbol? expr) (make-var-expr expr)]
    
    ; Lambda表达式: (lambda (x : Type) body) - 改进版本
    [(and (list? expr) 
          (>= (length expr) 3)
          (equal? (car expr) 'lambda))
     (let ([param-list (cadr expr)])
       (if (valid-lambda-param? param-list)
           (let ([body (caddr expr)])
             (let ([param-name (list-ref param-list 0)]
                   [param-type-expr (list-ref param-list 2)])
               ; 尝试解析参数类型
               (with-handlers ([exn:fail? (lambda (e) 
                                           (error "Invalid parameter type in lambda: ~a" param-type-expr))])
                 (let ([param-type (parse-type param-type-expr)])
                   (make-lambda-expr param-name
                                     param-type
                                     (parse body))))))
           (error "Invalid lambda parameter format: expected (name : type), got ~a" param-list)))]
    
    ; 条件表达式: (if cond then else)
    [(and (list? expr) 
          (= (length expr) 4)
          (equal? (car expr) 'if))
     (make-if-expr (parse (cadr expr))
                   (parse (caddr expr))
                   (parse (cadddr expr)))]
    
    ; 二元运算: (+, -, *, =, <, >)
    [(and (list? expr) 
          (= (length expr) 3)
          (member (car expr) '(+ - * = < >)))
     (make-binop-expr (car expr)
                      (parse (cadr expr))
                      (parse (caddr expr)))]
    
    ; 函数应用: (f arg) - 必须放在最后作为默认情况
    [(and (list? expr) (= (length expr) 2))
     (make-app-expr (parse (car expr)) (parse (cadr expr)))]
    
    [else (error "Invalid expression: ~a" expr)]))

;; =============================================================================
;; 类型系统
;; =============================================================================

(define (type-equal? t1 t2)
  (cond
    [(and (int-type? t1) (int-type? t2)) #t]
    [(and (bool-type? t1) (bool-type? t2)) #t]
    [(and (fun-type? t1) (fun-type? t2))
     (and (type-equal? (fun-type-param t1) (fun-type-param t2))
          (type-equal? (fun-type-return t1) (fun-type-return t2)))]
    [else #f]))

(define (typecheck env expr)
  (cond
    [(int-expr? expr) (make-int-type)]
    [(bool-expr? expr) (make-bool-type)]
    [(var-expr? expr) 
     (env-lookup env (var-expr-name expr))]
    [(lambda-expr? expr)
     (let* ([param (lambda-expr-param expr)]
            [param-type (lambda-expr-param-type expr)]
            [body (lambda-expr-body expr)]
            [new-env (env-extend env param param-type)]
            [body-type (typecheck new-env body)])
       (make-fun-type param-type body-type))]
    [(app-expr? expr)
     (let ([fun-type (typecheck env (app-expr-fun expr))]
           [arg-type (typecheck env (app-expr-arg expr))])
       (if (fun-type? fun-type)
           (if (type-equal? (fun-type-param fun-type) arg-type)
               (fun-type-return fun-type)
               (error "Type mismatch in application: expected ~a, got ~a" 
                      (type-to-string (fun-type-param fun-type))
                      (type-to-string arg-type)))
           (error "Cannot apply non-function type: ~a" 
                  (type-to-string fun-type))))]
    [(if-expr? expr)
     (let ([cond-type (typecheck env (if-expr-cond expr))]
           [then-type (typecheck env (if-expr-then expr))]
           [else-type (typecheck env (if-expr-else expr))])
       (unless (bool-type? cond-type)
         (error "Condition must be Bool, got: ~a" (type-to-string cond-type)))
       (unless (type-equal? then-type else-type)
         (error "If branches must have same type: ~a vs ~a" 
                (type-to-string then-type) (type-to-string else-type)))
       then-type)]
    [(binop-expr? expr)
     (let ([left-type (typecheck env (binop-expr-left expr))]
           [right-type (typecheck env (binop-expr-right expr))]
           [op (binop-expr-op expr)])
       (case op
         [(+ - *)
          (unless (and (int-type? left-type) (int-type? right-type))
            (error "Arithmetic operations require Int operands"))
          (make-int-type)]
         [(= < >)
          (unless (and (int-type? left-type) (int-type? right-type))
            (error "Comparison operations require Int operands"))
          (make-bool-type)]
         [else (error "Unknown operator: ~a" op)]))]
    [else (error "Unknown expression type in typecheck")]))

;; =============================================================================
;; 求值器
;; =============================================================================

(define (evaluate env expr)
  (cond
    [(int-expr? expr) (make-int-val (int-expr-value expr))]
    [(bool-expr? expr) (make-bool-val (bool-expr-value expr))]
    [(var-expr? expr) 
     (env-lookup env (var-expr-name expr))]
    [(lambda-expr? expr)
     (make-closure-val (lambda-expr-param expr)
                       (lambda-expr-body expr)
                       env)]
    [(app-expr? expr)
     (let ([fun-val (evaluate env (app-expr-fun expr))]
           [arg-val (evaluate env (app-expr-arg expr))])
       (if (closure-val? fun-val)
           (let* ([param (closure-val-param fun-val)]
                  [body (closure-val-body fun-val)]
                  [closure-env (closure-val-env fun-val)]
                  [new-env (env-extend closure-env param arg-val)])
             (evaluate new-env body))
           (error "Cannot apply non-function value: ~a" fun-val)))]
    [(if-expr? expr)
     (let ([cond-val (evaluate env (if-expr-cond expr))])
       (if (bool-val? cond-val)
           (if (bool-val-value cond-val)
               (evaluate env (if-expr-then expr))
               (evaluate env (if-expr-else expr)))
           (error "Condition must be Bool value: ~a" cond-val)))]
    [(binop-expr? expr)
     (let ([left-val (evaluate env (binop-expr-left expr))]
           [right-val (evaluate env (binop-expr-right expr))]
           [op (binop-expr-op expr)])
       (if (and (int-val? left-val) (int-val? right-val))
           (let ([l (int-val-value left-val)]
                 [r (int-val-value right-val)])
             (case op
               [(+) (make-int-val (+ l r))]
               [(-) (make-int-val (- l r))]
               [(*) (make-int-val (* l r))]
               [(=) (make-bool-val (= l r))]
               [(<) (make-bool-val (< l r))]
               [(>) (make-bool-val (> l r))]
               [else (error "Unknown operator: ~a" op)]))
           (error "Invalid operands for ~a: ~a, ~a" op left-val right-val)))]
    [else (error "Unknown expression type in evaluate")]))

;; =============================================================================
;; 辅助函数
;; =============================================================================

(define (type-to-string type)
  (cond
    [(int-type? type) "Int"]
    [(bool-type? type) "Bool"]
    [(fun-type? type)
     (format "(~a -> ~a)" 
             (type-to-string (fun-type-param type))
             (type-to-string (fun-type-return type)))]
    [else "Unknown"]))

(define (value-to-string val)
  (cond
    [(int-val? val) (number->string (int-val-value val))]
    [(bool-val? val) (if (bool-val-value val) "true" "false")]
    [(closure-val? val) "<function>"]
    [else "Unknown"]))

;; =============================================================================
;; 解释器入口
;; =============================================================================

(define (interpret expr-string)
  (let* ([expr (parse expr-string)]
         [type (typecheck (empty-env) expr)]
         [result (evaluate (empty-env) expr)])
    (values result type)))

(define (safe-interpret expr-string)
  (with-handlers ([exn:fail? (lambda (e) 
                              (values #f (exn-message e)))])
    (let-values ([(result type) (interpret expr-string)])
      (values (format "~a : ~a" 
                      (value-to-string result)
                      (type-to-string type))
              #f))))

;; =============================================================================
;; 调试工具
;; =============================================================================

; 详细的解析调试工具
(define (debug-parse-detailed expr)
  (displayln (format "=== 详细解析调试: ~a ===" expr))
  (match expr
    [(list 'lambda param-list body)
     (displayln "这是一个 lambda 表达式")
     (displayln (format "参数列表: ~a" param-list))
     (displayln (format "函数体: ~a" body))
     (displayln (format "参数列表是否有效: ~a" (valid-lambda-param? param-list)))
     (when (valid-lambda-param? param-list)
       (let ([param-name (list-ref param-list 0)]
             [param-type-expr (list-ref param-list 2)])
         (displayln (format "参数名: ~a" param-name))
         (displayln (format "参数类型表达式: ~a" param-type-expr))
         (displayln (format "参数类型表达式是列表: ~a" (list? param-type-expr)))
         (with-handlers ([exn:fail? (lambda (e) 
                                    (displayln (format "类型解析失败: ~a" (exn-message e))))])
           (let ([parsed-type (parse-type param-type-expr)])
             (displayln (format "解析后的类型: ~a" (type-to-string parsed-type)))))))]
    [else (displayln "不是 lambda 表达式")])
  (displayln ""))

;; =============================================================================
;; 测试用例
;; =============================================================================

(define (run-comprehensive-tests)
  (displayln "=== 测试开始 ===")
  
  ; 测试1: 基本字面量
  (displayln "测试1: 基本类型")
  (let-values ([(result error) (safe-interpret 42)])
    (if error
        (displayln (format "错误: ~a" error))
        (displayln (format "42 => ~a" result))))
  
  (let-values ([(result error) (safe-interpret 'true)])
    (if error
        (displayln (format "错误: ~a" error))
        (displayln (format "true => ~a" result))))
  
  ; 测试2: 简单算术
  (displayln "\n测试2: 简单算术")
  (let-values ([(result error) (safe-interpret '(+ 3 4))])
    (if error
        (displayln (format "错误: ~a" error))
        (displayln (format "(+ 3 4) => ~a" result))))
  
  ; 测试3: 恒等函数
  (displayln "\n测试3: 恒等函数")
  (let-values ([(result error) (safe-interpret '(lambda (x : Int) x))])
    (if error
        (displayln (format "错误: ~a" error))
        (displayln (format "(lambda (x : Int) x) => ~a" result))))
  
  ; 测试4: 函数应用
  (displayln "\n测试4: 函数应用")
  (let-values ([(result error) (safe-interpret '((lambda (x : Int) (+ x 1)) 5))])
    (if error
        (displayln (format "错误: ~a" error))
        (displayln (format "((lambda (x : Int) (+ x 1)) 5) => ~a" result))))
  
  ; 测试5: 条件表达式
  (displayln "\n测试5: 条件表达式")
  (let-values ([(result error) (safe-interpret '(if (< 3 5) 10 20))])
    (if error
        (displayln (format "错误: ~a" error))
        (displayln (format "(if (< 3 5) 10 20) => ~a" result))))
  
  ; 测试5.5: 调试高阶函数解析
  (displayln "\n测试5.5: 调试高阶函数解析")
  (debug-parse-detailed '(lambda (f : (Int -> Int)) (lambda (x : Int) (f x))))
  
  ; 测试6: 高阶函数
  (displayln "\n测试6: 高阶函数")
  (let-values ([(result error) 
                (safe-interpret '((lambda (f : (Int -> Int)) 
                                    (lambda (x : Int) (f x)))
                                  (lambda (y : Int) (+ y 1))))])
    (if error
        (displayln (format "错误: ~a" error))
        (displayln (format "高阶函数 => ~a" result))))
  
  ; 测试7: 类型错误检测
  (displayln "\n测试7: 类型错误检测")
  (let-values ([(result error) (safe-interpret '((lambda (x : Int) x) true))])
    (if error
        (displayln (format "正确检测到类型错误: ~a" error))
        (displayln (format "意外成功: ~a" result))))
  
  (displayln "=== 测试完成 ==="))

;; 运行测试
(run-comprehensive-tests)
