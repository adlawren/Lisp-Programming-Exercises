; An interpreter for a simple functional language.
;
; Test Cases:
;
; ...
(defun fl-interp (E P)
  (cond
    (
      (atom E)
      E
    )
    (
      (equal
        (car E)
        'if
      )
      (fl-interp-if E P)
    )
    (
      (equal
        (car E)
        'null
      )
      (fl-interp-null E P)
    )
    (
      (equal
        (car E)
        'atom
      )
      (fl-interp-atom E P)
    )
    (
      (equal
        (car E)
        'eq
      )
      (fl-interp-eq E P)
    )
    (
      (equal
        (car E)
        'first
      )
      (fl-interp-first E P)
    )
    (
      (equal
        (car E)
        'rest
      )
      (fl-interp-rest E P)
    )
    (
      (equal
        (car E)
        'cons
      )
      (fl-interp-cons E P)
    )
    (
      (equal
        (car E)
        'equal
      )
      (fl-interp-equal E P)
    )
    (
      (equal
        (car E)
        'number
      )
      (fl-interp-number E P)
    )
    (
      (equal
        (car E)
        '+
      )
      (fl-interp-add E P)
    )
    (
      (equal
        (car E)
        '-
      )
      (fl-interp-sub E P)
    )
    (
      (equal
        (car E)
        '*
      )
      (fl-interp-mul E P)
    )
    (
      (equal
        (car E)
        '>
      )
      (fl-interp-greater-than E P)
    )
    (
      (equal
        (car E)
        '<
      )
      (fl-interp-less-than E P)
    )
    (
      (equal
        (car E)
        '=
      )
      (fl-interp-equals E P)
    )
    (
      (equal
        (car E)
        'and
      )
      (fl-interp-and E P)
    )
    (
      (equal
        (car E)
        'or
      )
      (fl-interp-or E P)
    )
    (
      (equal
        (car E)
        'not
      )
      (fl-interp-not E P)
    )
    (T
      (fl-parse-user-defined-function E P (fl-get-function-definition E P))
    )
  )
)

; ...

; Helper function which computes the primitive if function.
;
; Test Cases:
;
; ...
(defun fl-interp-if (E P)
  (if (equal T (fl-interp (cadr E) P))
      (fl-interp (caddr E) P)
      (fl-interp (cadddr E) P)
  )
)

; Helper function which computes the primitive null function.
;
; Test Cases:
;
; ...
(defun fl-interp-null (E P)
  (if (atom (cadr E))
    (null (cadr E))
    (null (fl-interp (cadr E) P))
  )
)

; Helper function which computes the primitive atom function.
;
; Test Cases:
;
; ...
(defun fl-interp-atom (E P)
  (if (atom (cadr E))
    T
    (cond
      (
        (or
          (null (caadr E))
          (numberp (caadr E))
        )
        nil
      )
      (T (atom (fl-interp (cadr E) P)))
    )
  )
)

; Helper function which computes the primitive eq function.
;
; Test Cases:
;
; ...
(defun fl-interp-eq (E P)
  (eq
    (fl-interp (cadr E) P)
    (fl-interp (caddr E) P)
  )
)

; Helper function which computes the primitive first function.
;
; Test Cases:
;
; ...
(defun fl-interp-first (E P)
  (if (atom E)
    nil
    (cond
      (
        (or
          (null (caadr E))
          (numberp (caadr E))
        )
        (caadr E)
      )
      (T (fl-interp-first (fl-interp (cadr E) P) P))
    )
  )
)

; Helper function which computes the primitive rest function.
;
; Test Cases:
;
; ...
(defun fl-interp-rest (E P)
  (if (atom E)
    nil
    (cond
      (
        (or
          (null (caadr E))
          (numberp (caadr E))
        )
        (cdadr E)
      )
      (T (fl-interp-rest (fl-interp (cadr E) P) P))
    )
  )
)

; Helper function which computes the primitive cons function.
;
; Test Cases:
;
; ...
(defun fl-interp-cons (E P)
  (cons
    (if (atom (cadr E))
      (cadr E)
      (fl-interp (cadr E) P)
    )
    (cond
      (
        (or
          (null (caaddr E))
          (numberp (caaddr E))
        )
        (caddr E)
      )
      (T (fl-interp (caddr E) P))
    )
  )
)

; Helper function which computes the primitive equal function.
;
; Test Cases:
;
; ...
(defun fl-interp-equal (E P)
  (equal
    (cond
      (
        (or
          (null (cadr E))
          (numberp (cadr E))
          (null (caadr E))
          (numberp (caadr E))
        )
        (cadr E)
      )
      (T (fl-interp (cadr E) P))
    )
    (cond
      (
        (or
          (null (caddr E))
          (numberp (caddr E))
          (null (caaddr E))
          (numberp (caaddr E))
        )
        (caddr E)
      )
      (T (fl-interp (caddr E) P))
    )
  )
)

; Helper function which computes the primitive number function.
;
; Test Cases:
;
; ...
(defun fl-interp-number (E P)
  (numberp
    (cond
      (
        (atom (cadr E))
        (cadr E)
      )
      (
        (or
          (null (caadr E))
          (numberp (caadr E))
        )
        (cadr E)
      )
      (T (fl-interp (cadr E) P))
    )
  )
)

; Helper function which computes the primitive addition function.
;
; Test Cases:
;
; ...
(defun fl-interp-add (E P)
  (+ (if (atom (cadr E))
       (cadr E)
       (fl-interp (cadr E) P)
     )
     (if (atom (caddr E))
       (caddr E)
       (fl-interp (caddr E) P)
     )
  )
)

; Helper function which computes the primitive subtraction function.
;
; Test Cases:
;
; ...
(defun fl-interp-sub (E P)
  (- (if (atom (cadr E))
       (cadr E)
       (fl-interp (cadr E) P)
     )
     (if (atom (caddr E))
       (caddr E)
       (fl-interp (caddr E) P)
     )
  )
)

; Helper function which computes the primitive multiplication function.
;
; Test Cases:
; ...
(defun fl-interp-mul (E P)
  (* (if (atom (cadr E))
       (cadr E)
       (fl-interp (cadr E) P)
     )
     (if (atom (caddr E))
       (caddr E)
       (fl-interp (caddr E) P)
     )
  )
)

; Helper function which computes the primitive greater-than function.
;
; Test Cases:
;
; ...
(defun fl-interp-greater-than (E P)
  (> (if (atom (cadr E))
       (cadr E)
       (fl-interp (cadr E) P)
     )
     (if (atom (caddr E))
       (caddr E)
       (fl-interp (caddr E) P)
     )
  )
)

; Helper function which computes the primitive less-than function.
;
; Test Cases:
;
; ...
(defun fl-interp-less-than (E P)
  (< (if (atom (cadr E))
       (cadr E)
       (fl-interp (cadr E) P)
     )
     (if (atom (caddr E))
       (caddr E)
       (fl-interp (caddr E) P)
     )
  )
)

; Helper function which computes the primitive equivalence function.
;
; Test Cases:
; ...
(defun fl-interp-equals (E P)
  (= (if (atom (cadr E))
       (cadr E)
       (fl-interp (cadr E) P)
     )
     (if (atom (caddr E))
       (caddr E)
       (fl-interp (caddr E) P)
     )
  )
)

; Helper function which computes the primitive and function.
;
; Test Cases:
;
; ...
(defun fl-interp-and (E P)
  (and (if (atom (cadr E))
         (cadr E)
         (fl-interp (cadr E) P)
       )
       (if (atom (caddr E))
         (caddr E)
         (fl-interp (caddr E) P)
       )
  )
)

; Helper function which computes the primitive or function.
;
; Test Cases:
;
; ...
(defun fl-interp-or (E P)
  (or (if (atom (cadr E))
        (cadr E)
        (fl-interp (cadr E) P)
      )
      (if (atom (caddr E))
        (caddr E)
        (fl-interp (caddr E) P)
      )
  )
)

; Helper function which computes the primitive not function.
;
; Test Cases:
;
; ...
(defun fl-interp-not (E P)
  (not (if (atom (cadr E))
       (cadr E)
       (fl-interp (cadr E) P)
     )
  )
)

; Helper function which determines whether or not the given argument is a user defined function.
;
; Test Cases:
;
; ...
(defun fl-get-function-definition (E P)
  (cond
    (
      (null P)
      nil
    )
    (
      (equal (car E) (caar P))
      (car P)
    )
    (T (fl-get-command E (cdr P)))
  )
)

; Helper function which retrieves the header associated with a user-defined function.
;
; Test Cases:
;
; ...
(defun fl-get-function-header (P H)
  (cond
    (
      (or
        (null P)
        (equal '= (car P))
      )
      H
    )
    (T
      (fl-get-function-header (cdr P) (append H (list (car P))))
    )
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
        (atom E)
        (atom H)
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
(defun fl-get-program-application (E H B A)
  (if (null B)
    A
    (cond
      (
        (atom (car B))
        (if (fl-get-function-parameter-value E H (car B))
          (fl-get-program-application E H (cdr B) (append A (list (fl-get-function-parameter-value E H (car B)))))
          (fl-get-program-application E H (cdr B) (append A (list (car B))))
        )
      )
      (T
        (append
          A
          (list (fl-get-program-application E H (car B) nil))
          (fl-get-program-application E H (cdr B) nil)
        )
      )
    )
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
        (fl-get-function-header D nil)
        (fl-get-function-body D)
        nil
      )
      P
    )
  )
)
