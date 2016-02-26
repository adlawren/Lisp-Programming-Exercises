(load "fl-interpreter.lisp")

; Basic Tests
(assert (equal nil (fl-interp nil nil)))
(assert (equal '(RANDOM-UNIMPLEMENTED-COMMAND) (fl-interp '(RANDOM-UNIMPLEMENTED-COMMAND) nil)))

; Primitive Addition Tests
(assert (equal 3 (fl-interp '(+ 1 2) nil)))
(assert (equal 6 (fl-interp '(+ 1 (+ 2 3)) nil)))
(assert (equal 10 (fl-interp '(+ (+ 1 2) (+ 3 4)) nil)))
(assert (equal 28 (fl-interp '(+ (+ 1 (+ 2 3)) (+ 4 (+ 5 (+ 6 7)))) nil)))

; Primitive Subtraction Tests
(assert (equal -1 (fl-interp '(- 1 2) nil)))
(assert (equal 2 (fl-interp '(- 1 (- 2 3)) nil)))
(assert (equal 0 (fl-interp '(- (- 1 2) (- 3 4)) nil)))
(assert (equal 4 (fl-interp '(- (- 1 (- 2 3)) (- 4 (- 5 (- 6 7)))) nil)))

; Primitive Multiplication Tests
(assert (equal 2 (fl-interp '(* 1 2) nil)))
(assert (equal 6 (fl-interp '(* 1 (* 2 3)) nil)))
(assert (equal 24 (fl-interp '(* (* 1 2) (* 3 4)) nil)))
(assert (equal 5040 (fl-interp '(* (* 1 (* 2 3)) (* 4 (* 5 (* 6 7)))) nil)))

; Composite Primitive Arithmetic Tests
(assert (equal -5 (fl-interp '(- 1 (* 2 3)) nil)))
(assert (equal -9 (fl-interp '(- (+ 1 2) (* 3 4)) nil)))
(assert (equal -37 (fl-interp '(+ (- 1 (* 2 3)) (* 4 (- 5 (+ 6 7)))) nil)))

; Primitive Greater-Than Tests
(assert (equal nil (fl-interp '(> 1 2) nil)))
(assert (equal T (fl-interp '(> 2 1) nil)))

; Primitive Less-Than Tests
(assert (equal T (fl-interp '(< 1 2) nil)))
(assert (equal nil (fl-interp '(< 2 1) nil)))

; Primitive Equals Tests
(assert (equal T (fl-interp '(= 1 1) nil)))
(assert (equal nil (fl-interp '(= 1 2) nil)))

; Primitive And Tests
(assert (equal nil (fl-interp '(and nil nil) nil)))
(assert (equal nil (fl-interp '(and nil T) nil)))
(assert (equal nil (fl-interp '(and T nil) nil)))
(assert (equal T (fl-interp '(and T T) nil)))

; Primitive Or Tests
(assert (equal nil (fl-interp '(or nil nil) nil)))
(assert (equal T (fl-interp '(or nil T) nil)))
(assert (equal T (fl-interp '(or T nil) nil)))
(assert (equal T (fl-interp '(or T T) nil)))

; Primitive Not Tests
(assert (equal T (fl-interp '(not nil) nil)))
(assert (equal nil (fl-interp '(not T) nil)))

; Primitive If Tests
(assert (equal 1 (fl-interp '(if (= 1 1) 1 2) nil)))
(assert (equal 2 (fl-interp '(if (= 1 2) 1 2) nil)))

; Primitive Null Tests
(assert (equal T (fl-interp '(null nil) nil)))
(assert (equal nil (fl-interp '(null T) nil)))
(assert (equal nil (fl-interp '(null 1) nil)))
(assert (equal nil (fl-interp '(null (1)) nil)))
(assert (equal T (fl-interp '(null (and nil nil)) nil)))

; Primitive Atom Tests
(assert (equal T (fl-interp '(atom nil) nil)))
(assert (equal T (fl-interp '(atom T) nil)))
(assert (equal T (fl-interp '(atom 1) nil)))
(assert (equal nil (fl-interp '(atom (1)) nil)))

; Primitive Eq Tests
(assert (equal T (fl-interp '(eq 1 1) nil)))
(assert (equal nil (fl-interp '(eq 1 2) nil)))
(assert (equal nil (fl-interp '(eq (1) (1)) nil)))
(assert (equal nil (fl-interp '(eq (1) (2)) nil)))
(assert (equal nil (fl-interp '(eq (1 2) (1 2)) nil)))
(assert (equal nil (fl-interp '(eq (1 2) (3 4)) nil)))

; Primitive First Tests
(assert (equal nil (fl-interp '(first nil) nil)))
(assert (equal 1 (fl-interp '(first (1)) nil)))
(assert (equal 1 (fl-interp '(first (1 2 3)) nil)))

; Primitive Rest Tests
(assert (equal nil (fl-interp '(rest nil) nil)))
(assert (equal nil (fl-interp '(rest (1)) nil)))
(assert (equal '(2 3) (fl-interp '(rest (1 2 3)) nil)))

; Primitive Cons Tests
(assert (equal '(nil) (fl-interp '(cons nil nil) nil)))
(assert (equal '(1) (fl-interp '(cons 1 nil) nil)))
(assert (equal '(1 2 3) (fl-interp '(cons 1 (2 3)) nil)))

; Primitive Equal Tests
(assert (equal T (fl-interp '(equal 1 1) nil)))
(assert (equal nil (fl-interp '(equal 1 2) nil)))
(assert (equal nil (fl-interp '(equal (1) 1) nil)))
(assert (equal T (fl-interp '(equal (1) (1)) nil)))
(assert (equal nil (fl-interp '(equal (1 2) (1)) nil)))
(assert (equal T (fl-interp '(equal (1 2) (1 2)) nil)))

; Primitive Number Tests
(assert (equal nil (fl-interp '(number nil) nil)))
(assert (equal nil (fl-interp '(number T) nil)))
(assert (equal T (fl-interp '(number 1) nil)))
(assert (equal nil (fl-interp '(number (1)) nil)))
(assert (equal T (fl-interp '(number (+ 1 2)) nil)))

; Miscellaneous Composite Tests
(assert (equal 12 (fl-interp '(if (and (or (- 1 0) (number nil)) (null (atom (1 2)))) (* 3 4) (> 9 0)) nil)))
(assert (equal T (fl-interp '(if (and nil (null (atom (1 2)))) (* 3 4) (> 9 0)) nil)))

; User-Defined Function Header Requisition Tests
(assert (equal '(test-fn X Y) (fl-get-function-header '(test-fn X Y = (+ X Y)))))

; User-Defined Function Body Requisition Tests
(assert (equal '(+ X Y) (fl-get-function-body '(test-fn X Y = (+ X Y)))))

; User-Defined Function Argument Acquisition Tests
(assert (equal 1 (fl-get-arg-value 'X '(X Y Z) '(1 2 3))))
(assert (equal 2 (fl-get-arg-value 'Y '(X Y Z) '(1 2 3))))
(assert (equal 3 (fl-get-arg-value 'Z '(X Y Z) '(1 2 3))))

; User-Defined Function Expansion Requisition Tests

(assert (equal nil (fl-get-function-application nil nil nil)))

(assert
  (equal
    '(if (null (1 2 3)) 0 (+ 1 (count (rest (1 2 3)))))
    (fl-get-function-application
      '(X)
      '((1 2 3))
      '(if (null X) 0 (+ 1 (count (rest X))))
    )
  )
)

; User-Defined Function Parse Tests
(assert
  (equal
    3
    (fl-parse-user-defined-function
      '(test-fn 1 2)
      '(
        (test-fn X Y = (+ X Y))
       )
    )
  )
)

; User-Defined Function Evaluation Tests

(assert
  (equal
    3
    (fl-interp
      '(test-fn 1 2)
      '(
        (test-fn X Y = (+ X Y))
      )
    )
  )
)

(assert
  (equal
    7
    (fl-interp
      '(test-fn 1 2 3)
      '(
        (test-fn X Y Z = (+ X (+ Y (+ 1 Z))))
      )
    )
  )
)

(assert
  (equal
    1
    (fl-interp
      '(count (1))
      '((count X = (if (null X) 0 (+ 1 (count (rest X))))))
    )
  )
)

(assert
  (equal
    2
    (fl-interp
      '(count (1 2))
      '((count X = (if (null X) 0 (+ 1 (count (rest X))))))
    )
  )
)

(assert
  (equal
    3
    (fl-interp
      '(count (1 2 3))
      '((count X = (if (null X) 0 (+ 1 (count (rest X))))))
    )
  )
)

(assert
  (equal
    10
    (fl-interp
      '(count (1 2 3 4 5 6 7 8 9 10))
      '((count X = (if (null X) 0 (+ 1 (count (rest X))))))
    )
  )
)

(assert
  (equal
    '(1 2)
    (fl-interp
      '(append (1) (2))
      '(
        (append X Y = (if (null X)
                            Y
                            (cons (first X) (append (rest X) Y)))
        )
      )
    )
  )
)

(assert
  (equal
    '(1)
    (fl-interp
      '(reverse (1))
      '(
        (reverse X =  (if (null X)
                           nil
                           (append (reverse (rest X)) (cons (first X) nil))))
        (append X Y = (if (null X)
                            Y
                            (cons (first X) (append (rest X) Y)))
        )
      )
    )
  )
)

(assert
  (equal
    '(2 1)
    (fl-interp
      '(reverse (1 2))
      '(
        (reverse X =  (if (null X)
                           nil
                           (append (reverse (rest X)) (cons (first X) nil))))
        (append X Y = (if (null X)
                            Y
                            (cons (first X) (append (rest X) Y)))
        )
      )
    )
  )
)

(assert
  (equal
    '(10 9 8 7 6 5 4 3 2 1)
    (fl-interp
      '(reverse (1 2 3 4 5 6 7 8 9 10))
      '(
        (reverse X =  (if (null X)
                        nil
      					        (append
                          (reverse (rest X))
                          (cons (first X) nil)
                        )
                      )
        )
        (append X Y = (if (null X)
                        Y
      				          (cons
                          (first X)
                          (append (rest X) Y)
                        )
                      )
        )
      )
    )
  )
)
