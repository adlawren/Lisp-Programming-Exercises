;(print (+ 5 (* 3 2)))

;(print (rest nil))

;(print (eq nil '()))

;(print (eq '(a) '(a . nil)))

;(print (equal '(a) '(a . nil)))

(defun f (L)
  (if (null L) nil
    (cons (f (car L))
	  (f (cdr L))
    )
  )
)

(print (f nil))
(print (f '((a) b)))
