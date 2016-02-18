; An interpreter for a simple functional language.
;
; Test Cases:
;
; ...
(defun fl-interp (E P)
  (if (atom E) E
    (let ((first-item (car E)))
      (cond
        (
          (equal
            first-item
            'if
          )
          (if (equal T (fl-interp (cadr E) P))
              (fl-interp (caddr E) P)
              (fl-interp (cadddr E) P)
          )
        )
        (
          (equal
            first-item
            'null
          )
          (null (fl-interp (cadr E) P))
        )
        (
          (equal
            first-item
            'atom
          )
          (atom (fl-interp (cadr E) P))
        )
        (
          (equal
            first-item
            'eq
          )
          (eq
            (fl-interp (cadr E) P)
            (fl-interp (caddr E) P)
          )
        )
        (
          (equal
            first-item
            'first
          )
          (car
            (fl-interp (cadr E) P)
          )
        )
        (
          (equal
            first-item
            'rest
          )
          (cdr
            (fl-interp (cadr E) P)
          )
        )
        (
          (equal
            first-item
            'cons
          )
          (cons (fl-interp (cadr E) P) (fl-interp (caddr E) P))
        )
        (
          (equal
            first-item
            'equal
          )
          (equal
            (fl-interp (cadr E) P)
            (fl-interp (caddr E) P)
          )
        )
        (
          (equal
            first-item
            'number
          )
          (numberp (fl-interp (cadr E) P))
        )
        (
          (equal
            first-item
            '+
          )
          (+
            (fl-interp (cadr E) P)
            (fl-interp (caddr E) P)
          )
        )
        (
          (equal
            first-item
            '-
          )
          (-
            (fl-interp (cadr E) P)
            (fl-interp (caddr E) P)
          )
        )
        (
          (equal
            first-item
            '*
          )
          (*
            (fl-interp (cadr E) P)
            (fl-interp (caddr E) P)
          )
        )
        (
          (equal
            first-item
            '>
          )
          (>
            (fl-interp (cadr E) P)
            (fl-interp (caddr E) P)
          )
        )
        (
          (equal
            first-item
            '<
          )
          (<
            (fl-interp (cadr E) P)
            (fl-interp (caddr E) P)
          )
        )
        (
          (equal
            first-item
            '=
          )
          (=
            (fl-interp (cadr E) P)
            (fl-interp (caddr E) P)
          )
        )
        (
          (equal
            first-item
            'and
          )
          (and
            (fl-interp (cadr E) P)
            (fl-interp (caddr E) P)
          )
        )
        (
          (equal
            first-item
            'or
          )
          (or
            (fl-interp (cadr E) P)
            (fl-interp (caddr E) P)
          )
        )
        (
          (equal
            first-item
            'not
          )
          (not
            (fl-interp (cadr E) P)
          )
        )
        (T
          ;(fl-parse-user-defined-function E P (fl-get-function-definition first-item P))
          (let
            (
              (function-def (fl-get-function-definition first-item P))
            )
            (if (null function-def)
              E
              (fl-interp
                (fl-get-function-application
                  (cdr (fl-get-function-header function-def))
                  (fl-eval-args (cdr E) P)
                  (fl-get-function-body function-def)
                )
                P
              )
            )
          )
        )
      )
    )
  )
)

(defun fl-eval-args (A P)
  (if (null A)
    nil
    (cons (fl-interp (car A) P) (fl-eval-args (cdr A) P))
  )
)

(defun fl-get-function-application (H A B)
  (if (null B)
    nil
    (let ((first-item (car B)))
      (cond
        (
          (fl-is-parameter first-item)
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

; Helper function which determines whether or not the given argument is a user defined function.
;
; Test Cases:
;
; ...
(defun fl-get-function-definition (N P)
  (cond
    (
      (null P)
      nil
    )
    (
      (equal N (caar P))
      (car P)
    )
    (T (fl-get-function-definition N (cdr P)))
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

; Helper function which retrieves the value of a parameter from an application of a user-defined function.
;
; Test Cases:
;
; ...
(defun fl-get-function-parameter-value (E H V)
  (cond
    (
      (or
        (null E)
        (null H)
      )
      nil
    )
    (
      (equal (car H) V)
      (car E)
    )
    (T (fl-get-function-parameter-value (cdr E) (cdr H) V))
  )
)

; Helper function which computes the expanded functional application of a given user-defined function, given the original application of the function and the header & body of the function.
;
; Test Cases:
;
; ...
(defun fl-get-program-application (E H B P)
  (if (null B)
    nil
    (let ((first-item (car B)))
      (cond
        (
          (fl-is-parameter first-item)
          (cons (fl-get-function-parameter-value
                  E
                  H
                  first-item
                )
                (fl-get-program-application E H (cdr B) P)
          )
        )
        (
          (atom first-item)
          (cons first-item (fl-get-program-application E H (cdr B) P))
        )
        (T
          (cons
            (cons (car first-item) (fl-get-program-application E H (cdr first-item) P))
            (fl-get-program-application E H (cdr B) P)
          )
        )
      )
    )
  )
)

(defun fl-is-parameter (P)
  (or
    (equal 'X P)
    (equal 'Y P)
    (equal 'Z P)
    (equal 'M P)
    (equal 'L P)
  )
)

; Helper function which determines whether or not the given argument is a user defined function.
;
; Test Cases:
;
; ...
(defun fl-parse-user-defined-function (E P D)
  (if (null D)
    E
    (fl-interp
      (fl-get-program-application
        E
        (fl-get-function-header D)
        (fl-get-function-body D)
        P
      )
      P
    )
  )
)
