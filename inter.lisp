; An interpreter for a simple functional language.
;
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
        (T
          '(TODO - IMPLEMENT NON-PRIMITIVE CASE)
        )
      )
    )
  )
  #|
    (
      (let
        (
          (first-item (car appl))
        )
        (cond
	  #|
          (
            (equal
              first-item
              '+
            )
            (+ (if (atom (cadr appl))
	         (cadr appl)
	         (fl-interp (cadr appl))
	       )
	       (if (atom (caddr appl))
	         (caddr appl)
	         (fl-interp (caddr appl))
	       )
      	    )
	  )
	  |#
	  #|
	  (
	    (equal
              first-item
              '-
            )
            (- (cadr appl) (caddr appl))
          )
          (
            (equal
              first-item
              '*
            )
            (* (cadr appl) (caddr appl))
          )
	  |#
          (T
            '(TODO - IMPLEMENT NON-PRIMITIVE CASE)
          )
        )
      ) 
    )
  )
  |#
)

; Test
(print '(HELLO WORLD))
(print (fl-interp '(UNKNOWN) nil))

; Primitive Addition Tests
(print (fl-interp '(+ 1 2) nil))
(print (fl-interp '(+ 1 (+ 2 3)) nil))
(print (fl-interp '(+ (+ 1 2) (+ 3 4)) nil))
(print (fl-interp '(+ (+ 1 (+ 2 3)) (+ 4 (+ 5 (+ 6 7)))) nil))

; Primitive Subtraction Tests
(print (fl-interp '(- 2 1) nil))

; Primitive Multiplication Tests
(print (fl-interp '(* 2 1) nil))

; Composite Primitive Arithmetic Tests
; ...

