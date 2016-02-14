; An interpreter for a simple functional language.
;
; Test Cases:
;
; ...
(defun fl-interp (appl progr)
  (let
    (
      (first-item (car appl))
    )
    (if (null appl)
      ()
      (cond
        (
	  (equal
            first-item
            '+
          )
          (fl-interp-add appl progr)
	)
	(
	  (equal
            first-item
            '-
	  )
	  (fl-interp-sub appl progr)
	)
	(
	  (equal
            first-item
            '*
          )
	  (fl-interp-mul appl progr)
	)	
	(
	  (equal
            first-item
            '>
          )
	  (fl-interp-greater-than appl progr)
	)
	(
	  (equal
            first-item
            '<
          )
	  (fl-interp-less-than appl progr)
	)
	(
	  (equal
            first-item
            '=
          )
	  (fl-interp-equals appl progr)
	)
	(
	  (equal
            first-item
            'and
          )
	  (fl-interp-and appl progr)
	)
	(
	  (equal
            first-item
            'or
          )
	  (fl-interp-or appl progr)
	)
	(
	  (equal
            first-item
            'not
          )
	  (fl-interp-not appl progr)
	)
        (T
          '(TODO - IMPLEMENT NON-PRIMITIVE CASE)
        )
      )
    )
  )
)

; Helper function which computes the primitive addition function.
;
; Test Cases:
;
; ...
(defun fl-interp-add (appl progr)
  (+ (if (atom (cadr appl))
       (cadr appl)
       (fl-interp (cadr appl) progr)
     )
     (if (atom (caddr appl))
       (caddr appl)
       (fl-interp (caddr appl) progr)
     )
  )
)

; Helper function which computes the primitive subtraction function.
;
; Test Cases:
;
; ...
(defun fl-interp-sub (appl progr)
  (- (if (atom (cadr appl))
       (cadr appl)
       (fl-interp (cadr appl) progr)
     )
     (if (atom (caddr appl))
       (caddr appl)
       (fl-interp (caddr appl) progr)
     )
  )
)

; Helper function which computes the primitive multiplication function.
;
; Test Cases:
; ...
(defun fl-interp-mul (appl progr)
  (* (if (atom (cadr appl))
       (cadr appl)
       (fl-interp (cadr appl) progr)
     )
     (if (atom (caddr appl))
       (caddr appl)
       (fl-interp (caddr appl) progr)
     )
  )
)

; Helper function which computes the primitive greater-than function.
;
; Test Cases:
;
; ...
(defun fl-interp-greater-than (appl progr)
  (> (if (atom (cadr appl))
       (cadr appl)
       (fl-interp (cadr appl) progr)
     )
     (if (atom (caddr appl))
       (caddr appl)
       (fl-interp (caddr appl) progr)
     )
  )
)

; Helper function which computes the primitive less-than function.
;
; Test Cases:
;
; ...
(defun fl-interp-less-than (appl progr)
  (< (if (atom (cadr appl))
       (cadr appl)
       (fl-interp (cadr appl) progr)
     )
     (if (atom (caddr appl))
       (caddr appl)
       (fl-interp (caddr appl) progr)
     )
  )
)

; Helper function which computes the primitive equivalence function.
;
; Test Cases:
; ...
(defun fl-interp-equals (appl progr)
  (= (if (atom (cadr appl))
       (cadr appl)
       (fl-interp (cadr appl) progr)
     )
     (if (atom (caddr appl))
       (caddr appl)
       (fl-interp (caddr appl) progr)
     )
  )
)

; Helper function which computes the primitive and function.
;
; Test Cases:
;
; ...
(defun fl-interp-and (appl progr)
  (and (if (atom (cadr appl))
         (cadr appl)
         (fl-interp (cadr appl) progr)
       )
       (if (atom (caddr appl))
         (caddr appl)
         (fl-interp (caddr appl) progr)
       )
  )
)

; Helper function which computes the primitive or function.
;
; Test Cases:
;
; ...
(defun fl-interp-or (appl progr)
  (or (if (atom (cadr appl))
        (cadr appl)
        (fl-interp (cadr appl) progr)
      )
      (if (atom (caddr appl))
        (caddr appl)
        (fl-interp (caddr appl) progr)
      )
  )
)

; Helper function which computes the primitive not function.
;
; Test Cases:
;
; ...
(defun fl-interp-not (appl progr)
  (not (if (atom (cadr appl))
       (cadr appl)
       (fl-interp (cadr appl) progr)
     )
  )
)

; Basic Tests
(print (fl-interp nil nil))
(print (fl-interp '(UNKNOWN) nil))

; Primitive Addition Tests
(print (fl-interp '(+ 1 2) nil))
(print (fl-interp '(+ 1 (+ 2 3)) nil))
(print (fl-interp '(+ (+ 1 2) (+ 3 4)) nil))
(print (fl-interp '(+ (+ 1 (+ 2 3)) (+ 4 (+ 5 (+ 6 7)))) nil))

; Primitive Subtraction Tests
(print (fl-interp '(- 1 2) nil))
(print (fl-interp '(- 1 (- 2 3)) nil))
(print (fl-interp '(- (- 1 2) (- 3 4)) nil))
(print (fl-interp '(- (- 1 (- 2 3)) (- 4 (- 5 (- 6 7)))) nil))

; Primitive Multiplication Tests
(print (fl-interp '(* 1 2) nil))
(print (fl-interp '(* 1 (* 2 3)) nil))
(print (fl-interp '(* (* 1 2) (* 3 4)) nil))
(print (fl-interp '(* (* 1 (* 2 3)) (* 4 (* 5 (* 6 7)))) nil))

; Primitive Greater-Than Tests
(print (fl-interp '(> 1 2) nil))
(print (fl-interp '(> 2 1) nil))

; Primitive Less-Than Tests
(print (fl-interp '(< 1 2) nil))
(print (fl-interp '(< 2 1) nil))

; Primitive Equals Tests
(print (fl-interp '(= 1 1) nil))
(print (fl-interp '(= 1 2) nil))

; Primitive And Tests
(print (fl-interp '(and nil nil) nil))
(print (fl-interp '(and nil T) nil))
(print (fl-interp '(and T nil) nil))
(print (fl-interp '(and T T) nil))

; Primitive Or Tests
(print (fl-interp '(or nil nil) nil))
(print (fl-interp '(or nil T) nil))
(print (fl-interp '(or T nil) nil))
(print (fl-interp '(or T T) nil))

; Primitive Not Tests
(print (fl-interp '(not nil) nil))
(print (fl-interp '(not T) nil))

; Composite Primitive Arithmetic Tests
(print (fl-interp '(- 1 (* 2 3)) nil))
(print (fl-interp '(- (+ 1 2) (* 3 4)) nil))
(print (fl-interp '(+ (- 1 (* 2 3)) (* 4 (- 5 (+ 6 7)))) nil))

