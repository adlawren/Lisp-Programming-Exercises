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
	(
	  (equal
            first-item
            '-
          )
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
	(
	  (equal
            first-item
            '*
          )
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
	(
	  (equal
            first-item
            '>
          )
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
	(
	  (equal
            first-item
            '<
          )
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
	(
	  (equal
            first-item
            '=
          )
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
	(
	  (equal
            first-item
            'and
          )
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
	(
	  (equal
            first-item
            'or
          )
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
	(
	  (equal
            first-item
            'not
          )
          (not (if (atom (cadr appl))
	         (cadr appl)
	         (fl-interp (cadr appl) progr)
	       )
      	  )
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

; Basic Tests
(print (fl-interp nil nil))
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

