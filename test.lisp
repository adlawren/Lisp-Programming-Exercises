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
(assert (equal nil (fl-interp '(first (+ 1 2)) nil)))

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
