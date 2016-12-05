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

(define (expand-var expr var expand-to)
  (match expr
    [ (? (pa$ eq? var) expr) expand-to ]
    [ (op . args) (cons op (map (cut expand-var <> var expand-to) args)) ]
    [ () '() ]
    ))

(define (%d/d-composed var op arg)
  (let1 newvar (gensym)
    `(* ,(d/d var arg)
        ,(expand-var (d/d newvar `(,op ,newvar))
                     newvar arg) ) ) )

(define (d/d var expr)
  (match expr
    [ (? (pa$ eq? var) expr) 1 ]
    [ (? symbol? expr) 0 ]
    [ (? number? expr) 0 ]
    [ ('+ lhs rhs)
     `(+ ,(d/d var lhs)
         ,(d/d var rhs)
         )
     ]
    [ ('* lhs rhs)
     `(+ (* ,(d/d var lhs) ,rhs)
         (* ,lhs ,(d/d var rhs))
         )
     ]
    [ ('exp arg)
     (match arg
       [ (? (pa$ eq? var)) expr ]
       [ else
         (%d/d-composed var 'exp arg) ] ) ]
    [ ('log arg)
     (match arg
       [ (? (pa$ eq? var)) `(/ 1 ,arg) ]
       [ else
         (%d/d-composed var 'log arg) ] ) ]
    [ ('sin arg)
     (match arg
       [ (? (pa$ eq? var)) `(cos ,arg) ]
       [ else
         (%d/d-composed var 'sin arg) ] ) ]
    [ ('cos arg)
     (match arg
       [ (? (pa$ eq? var)) `(* -1 (sin ,arg)) ]
       [ else
         (%d/d-composed var 'cos arg) ] ) ]
    ))

;; TODO: 定数たたみこみ
;; TODO: 和・積の引数順序の正規化
;; TODO: 同類項をまとめる

(define e1 '(* 1 (+ 2 3 4) 5 6 (+ 3 (* 7 8 9) 10 11) 12)
  )
