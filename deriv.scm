(use util.match)

(define (%normalize->binary+ args)
  (match args
    [ () 0]
    [ (x) x ]
    [ (x . xs) `(+ ,x ,(%normalize->binary+ xs)) ] ) )

(define (%normalize->binary* args)
  (match args
    [ () 1]
    [ (x) x ]
    [ (x . xs) `(* ,x ,(%normalize->binary* xs)) ] ) )

(define (normalize->binary expr)
  (match expr
    [ (? symbol? expr) expr ]
    [ (? number? expr) expr ]
    [ ('+ . args) (%normalize->binary+ (map normalize->binary args)) ]
    [ ('* . args) (%normalize->binary* (map normalize->binary args)) ]
    [ (op . args) (cons op (map normalize->binary args)) ]
    ))

(define (simplify1 expr)
  (match expr
    [ `(- ,x ,y)
      `(+ ,(simplify1 x) (* -1 ,(simplify1 y))) ]
    [ `(+ 0 ,y) y ]
    [ '(+ ,x 0) x ]
    [ `(+ ,(? number? x) ,(? number? y))
      (+ x y) ]
    [ `(+ ,(? number? x) (+ ,(? number? y) ,z))
      `(+ ,(+ x y) ,z) ]

    [ `(* 0 ,y) 0 ]
    [ `(* ,x 0) 0 ]
    [ `(* 1 ,y) y ]
    [ `(* ,x 1) x ]
    [ `(* ,(? number? x) ,(? number? y))
      (* x y) ]
    [ `(* ,(? number? x) (* ,(? number? y) ,z))
      `(* ,(* x y) ,z) ]

    [ `(/ ,x 1) x ]
    [ `(/ ,(? number? x) ,(? number? y))
      (/ x y) ]

    [ `(,unop ,x)
      `(,unop ,(simplify1 x)) ]
    [ `(,binop ,x ,y)
      `(,binop ,(simplify1 x) ,(simplify1 y)) ]

    [else expr] ) )

(define non-structured? (any-pred number? symbol?))
(define structured? (complement non-structured?))

(define (simplify2 expr)
  (match expr
    [ `(+ (+ ,x ,y) ,z)
      `(+ ,(simplify2 x)
          (+ ,(simplify2 y)
             ,(simplify2 z))) ]
    [ `(+ ,(? structured? x) (+ ,(? non-structured? y) ,z))
      `(+ ,y (+ ,(simplify2 x) ,(simplify2 z))) ]
    [ `(+ ,(? symbol? x) (+ ,(? number? y) ,z))
      `(+ ,y (+ ,(simplify2 x) ,(simplify2 z))) ]
    [ `(+ ,(? structured? x) ,(? non-structured? y))
      `(+ ,y ,(simplify2 x)) ]
    [ `(+ ,(? symbol? x) ,(? number? y))
      `(+ ,y ,x) ]

    [ `(* (* ,x ,y) ,z)
      `(* ,(simplify2 x)
          (* ,(simplify2 y)
             ,(simplify2 z))) ]
    [ `(* ,(? structured? x) (* ,(? non-structured? y) ,z))
      `(* ,y (* ,(simplify2 x) ,(simplify2 z))) ]
    [ `(* ,(? symbol? x) (* ,(? number? y) ,z))
      `(* ,y (* ,(simplify2 x) ,(simplify2 z))) ]
    [ `(* ,(? structured? x) ,(? non-structured? y))
      `(* ,y ,(simplify2 x)) ]
    [ `(* ,(? symbol? x) ,(? number? y))
      `(* ,y ,x) ]

    [ `(* (+ ,x ,y) ,z)
      `(+ (* ,(simplify2 x) ,(simplify2 z))
          (* ,(simplify2 y) ,(simplify2 z))) ]
    [ `(* ,x (+ ,y ,z))
      `(+ (* ,(simplify2 x) ,(simplify2 y))
          (* ,(simplify2 x) ,(simplify2 z))) ]

    [ `(/ ,x (/ ,y ,z))
      `(/ (* ,(simplify2 x)
             ,(simplify2 z))
          ,(simplify2 y))
      ]

    [ `(exp (log ,x)) (simplify2 x) ]

    [ `(log (exp ,x)) (simplify2 x) ]

    [ `(,unop ,x)
      `(,unop ,(simplify2 x)) ]
    [ `(,binop ,x ,y)
      `(,binop ,(simplify2 x) ,(simplify2 y)) ]

    [else expr] ) )

(define (simplify3 expr)
  (match expr
    [ `(+ (* ,(? number? a) ,x)
          (* ,(? number? b) ,y)
          )
      (=> next)
      (or (equal? x y) (next))
      `(* ,(+ a b) ,x) ]
    [ `(+ (* ,(? number? a) ,x) ,y)
      (=> next)
      (or (equal? x y) (next))
      `(* ,(+ 1 a) ,x) ]
    [ `(+ ,x (* ,(? number? a) ,y))
      (=> next)
      (or (equal? x y) (next))
      `(* ,(+ 1 a) ,x) ]
    [ `(+ ,x ,y)
      (=> next)
      (or (equal? x y) (next))
      `(* 2 ,x) ]
    [ `(+ ,x ,y)
      `(+ ,(simplify3 x)
          ,(simplify3 y) ) ]

    [ `(,unop ,x)
      `(,unop ,(simplify3 x)) ]
    [ `(,binop ,x ,y)
      `(,binop ,(simplify3 x) ,(simplify3 y)) ]

    [else expr] ) )

(define ((fix*$ conv) expr)
  (let loop [[expr expr]]
    (let1 converted (conv expr)
      (if (equal? converted expr)
        converted
        (loop converted)))))

(define simplify*
  (fix*$ (.$ (fix*$ simplify1)
             (fix*$ simplify2)
             (fix*$ simplify3)
             )))

(define (expand-var expr var expand-to)
  (match expr
    [ (? (pa$ eq? var) expr) expand-to ]
    [ (? number?) expr ]
    [ (op . args) (cons op (map (cut expand-var <> var expand-to) args)) ]
    [ () '() ]
    ))

(define (%d/d-composed var op arg)
  (let1 newvar (gensym)
    `(* ,(d/d var arg)
        ,(expand-var (d/d newvar `(,op ,newvar))
                     newvar arg) ) ) )

(define (%d/d-inverted invop y)
  (let1 newvar (gensym)
    `(/ 1
        ,(expand-var (d/d newvar `(,invop ,newvar))
                     newvar y) ) ) )

(define (d/d var expr)
  (match expr
    [ (? (pa$ eq? var) expr) 1 ]
    [ (? symbol? expr) 0 ]
    [ (? number? expr) 0 ]
    [ ('+ lhs rhs)
     `(+ ,(d/d var lhs)
         ,(d/d var rhs) ) ]
    [ ('- lhs rhs)
     `(- ,(d/d var lhs)
         ,(d/d var rhs) ) ]
    [ ('* lhs rhs)
     `(+ (* ,(d/d var lhs) ,rhs)
         (* ,lhs ,(d/d var rhs)) ) ]
    [ ('/ lhs rhs)
     `(/ (- (* ,(d/d var lhs) ,rhs)
            (* ,lhs ,(d/d var rhs)) )
         (* ,rhs ,rhs)) ]
    [ ('exp arg)
     (match arg
       [ (? (pa$ eq? var)) expr ]
       [ else (%d/d-composed var 'exp arg) ] ) ]
    [ ('log arg)
     (%d/d-inverted 'exp expr)]
    [ ('sin arg)
     (match arg
       [ (? (pa$ eq? var)) `(cos ,arg) ]
       [ else (%d/d-composed var 'sin arg) ] ) ]
    [ ('cos arg)
     (match arg
       [ (? (pa$ eq? var)) `(* -1 (sin ,arg)) ]
       [ else (%d/d-composed var 'cos arg) ] ) ]
    [ ('tan arg)
     (d/d var `(/ (sin ,arg) (cos ,arg))) ]
    [ ('asin arg)
     (%d/d-inverted 'sin expr) ]
    [ ('acos arg)
     (%d/d-inverted 'cos expr) ]
    [ ('atan arg)
     (%d/d-inverted 'tan expr) ]
    ))

;; TODO: 和・積の微分は2項で 正規化は多項で
;; TODO: 三角関数の定数畳み込み
