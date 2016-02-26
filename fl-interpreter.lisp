; An interpreter for a simple functional language.
;
; Test Cases:
;
; (fl-interp '(+ (- 1 (* 2 3)) (* 4 (- 5 (+ 6 7)))) nil) => -37
; (fl-interp '(if (and (or (- 1 0) (number nil)) (null (atom (1 2)))) (* 3 4) (> 9 0)) nil) => 12
; (fl-interp
;   '(count (1 2 3 4 5 6 7 8 9 10))
;   '((count X = (if (null X) 0 (+ 1 (count (rest X))))))
; ) => 10
; (fl-interp
;   '(reverse (1 2 3 4 5 6 7 8 9 10))
;   '((reverse X =  (if (null X) nil (append (reverse (rest X)) (cons (first X) nil))))
;     (append X Y = (if (null X) Y (cons (first X) (append (rest X) Y))))
;    )
; ) => (10 9 8 7 6 5 4 3 2 1)
(defun fl-interp (E P)
  (if (atom E) E
    (let (
           (first-item (car E))
           (args (cdr E))
         )
      (cond
        (
          (equal
            first-item
            'if
          )
          (if (equal T (fl-interp (car args) P))
              (fl-interp (cadr args) P)
              (fl-interp (caddr args) P)
          )
        )
        (
          (equal
            first-item
            'null
          )
          (null (fl-interp (car args) P))
        )
        (
          (equal
            first-item
            'atom
          )
          (atom (fl-interp (car args) P))
        )
        (
          (equal
            first-item
            'eq
          )
          (eq
            (fl-interp (car args) P)
            (fl-interp (cadr args) P)
          )
        )
        (
          (equal
            first-item
            'first
          )
          (car
            (fl-interp (car args) P)
          )
        )
        (
          (equal
            first-item
            'rest
          )
          (cdr
            (fl-interp (car args) P)
          )
        )
        (
          (equal
            first-item
            'cons
          )
          (cons (fl-interp (car args) P) (fl-interp (cadr args) P))
        )
        (
          (equal
            first-item
            'equal
          )
          (equal
            (fl-interp (car args) P)
            (fl-interp (cadr args) P)
          )
        )
        (
          (equal
            first-item
            'number
          )
          (numberp (fl-interp (car args) P))
        )
        (
          (equal
            first-item
            '+
          )
          (+
            (fl-interp (car args) P)
            (fl-interp (cadr args) P)
          )
        )
        (
          (equal
            first-item
            '-
          )
          (-
            (fl-interp (car args) P)
            (fl-interp (cadr args) P)
          )
        )
        (
          (equal
            first-item
            '*
          )
          (*
            (fl-interp (car args) P)
            (fl-interp (cadr args) P)
          )
        )
        (
          (equal
            first-item
            '>
          )
          (>
            (fl-interp (car args) P)
            (fl-interp (cadr args) P)
          )
        )
        (
          (equal
            first-item
            '<
          )
          (<
            (fl-interp (car args) P)
            (fl-interp (cadr args) P)
          )
        )
        (
          (equal
            first-item
            '=
          )
          (=
            (fl-interp (car args) P)
            (fl-interp (cadr args) P)
          )
        )
        (
          (equal
            first-item
            'and
          )
          (and
            (fl-interp (car args) P)
            (fl-interp (cadr args) P)
          )
        )
        (
          (equal
            first-item
            'or
          )
          (or
            (fl-interp (car args) P)
            (fl-interp (cadr args) P)
          )
        )
        (
          (equal
            first-item
            'not
          )
          (not
            (fl-interp (car args) P)
          )
        )
        (T
          (fl-parse-user-defined-function (cons (fl-interp first-item P) (fl-interp args P)) P)
        )
      )
    )
  )
)

; Helper function which parses a non-primitive function given to the interpreter.
;
; Test Cases:
;
; (fl-parse-user-defined-function '(test-fn 1 2) '((test-fn X Y = (+ X Y)))) => 3
(defun fl-parse-user-defined-function (F P)
  (if (null P)
    F
    (if (equal (car F) (caar P))
      (fl-interp
        (fl-get-function-application
          (cdr (fl-get-function-header (car P)))
          (cdr F)
          (fl-get-function-body (car P))
        )
        P
      )
      (fl-parse-user-defined-function F (cdr P))
    )
  )
)

; Helper function which retrieves the function application associated with a function header, argmument values and body.
;
; Test Cases:
;
; (fl-get-function-application '(X) '((1 2 3)) '(if (null X) 0 (+ 1 (count (rest X))))) => (if (null (1 2 3)) 0 (+ 1 (count (rest (1 2 3)))))
(defun fl-get-function-application (H A B)
  (if (null B)
    nil
    (let ((first-item (car B)))
      (cond
        (
          (fl-is-argument first-item)
          (cons (fl-get-arg-value
                  first-item
                  H
                  A
                )
                (fl-get-function-application H A (cdr B))
          )
        )
        (
          (atom first-item)
          (cons first-item (fl-get-function-application H A (cdr B)))
        )
        (T
          (cons
            (cons (car first-item) (fl-get-function-application H A (cdr first-item)))
            (fl-get-function-application H A (cdr B))
          )
        )
      )
    )
  )
)

; Helper function which retrieves the value associated with the given parameter name and function argument list.
;
; Test Cases:
;
; (fl-get-arg-value 'X '(X Y Z) '(1 2 3)) => 1
; (fl-get-arg-value 'Y '(X Y Z) '(1 2 3)) => 2
; (fl-get-arg-value 'Z '(X Y Z) '(1 2 3)) => 3
(defun fl-get-arg-value (N H A)
  (cond
    (
      (null H)
      nil
    )
    (
      (equal (car H) N)
      (car A)
    )
    (T (fl-get-arg-value N (cdr H) (cdr A)))
  )
)

; Helper function which retrieves the header associated with a user-defined function.
;
; Test Cases:
;
; (fl-get-function-header '(test-fn X Y = (+ X Y))) => (test-fn X Y)
(defun fl-get-function-header (P)
  (if (equal '= (car P))
    nil
    (cons (car P) (fl-get-function-header (cdr P)))
  )
)

; Helper function which retrieves the body associated with a user-defined function.
;
; Test Cases:
;
; (fl-get-function-body '(test-fn X Y = (+ X Y))) => (+ X Y)
(defun fl-get-function-body (P)
  (cond
    (
      (null P)
      nil
    )
    (
      (equal '= (car P))
      (cadr P)
    )
    (T
      (fl-get-function-body (cdr P))
    )
  )
)

; Helper function which determines whether or not the given argument is a valid argument name.
;
; Test Cases:
;
; (fl-is-argument 'X) => T
; (fl-is-argument 'L) => T
; (fl-is-argument 'B) => F
(defun fl-is-argument (A)
  (or
    (equal 'X A)
    (equal 'Y A)
    (equal 'Z A)
    (equal 'M A)
    (equal 'L A)
  )
)
