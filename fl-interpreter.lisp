; An interpreter for a simple functional language.
;
; Test Cases:
;
; ...
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
          (parse-user-defined-function (cons (fl-interp first-item P) (fl-interp args P)) P)
        )
      )
    )
  )
)


; Helper function which parses a non-primitive function given to the interpreter.
;
; Test Cases:
;
; ...
(defun parse-user-defined-function (F P)
  (if P
    (if (equal (car F) (caar P))
      (fl-interp
        (fl-get-function-application
          (cdr (fl-get-function-header (car P)))
          (cdr F)
          (fl-get-function-body (car P))
        )
        P
      )
      (parse-user-defined-function F (cdr P))
    )
    F
  )
)

; Helper function which retrieves the function application associated with a function header, argmument values and body.
;
; Test Cases:
;
; ...
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
; ...
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
; ...
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
; ...
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
; ...
(defun fl-is-argument (A)
  (or
    (equal 'X A)
    (equal 'Y A)
    (equal 'Z A)
    (equal 'M A)
    (equal 'L A)
  )
)
