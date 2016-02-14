; An interpreter for a simple functional language.
;
; Test Cases:
;
; ...
(defun fl-interp (appl progr)
  (let
    (
      (first-item (if (atom appl)
		                appl
		                (car appl)
		              )
      )
    )
    (cond
      (
        (or
          (null first-item)
          (equal T first-item)
          (numberp first-item)
        )
        first-item
      )
      (
        (equal
          first-item
          'if
        )
        (fl-interp-if appl progr)
      )
      (
        (equal
          first-item
          'null
        )
        (fl-interp-null appl progr)
      )
      (
        (equal
          first-item
          'atom
        )
        (fl-interp-atom appl progr)
      )
      (
        (equal
          first-item
          'eq
        )
        (fl-interp-eq appl progr)
      )
      (
        (equal
          first-item
          'first
        )
        (fl-interp-first appl progr)
      )
      (
        (equal
          first-item
          'rest
        )
        (fl-interp-rest appl progr)
      )
      (
        (equal
          first-item
          'cons
        )
        (fl-interp-cons appl progr)
      )
      (
        (equal
          first-item
          'equal
        )
        (fl-interp-equal appl progr)
      )
      (
        (equal
          first-item
          'number
        )
        (fl-interp-number appl progr)
      )
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
        '(UNKNOWN COMMAND)
      )
    )
  )
)

; Helper function which computes the primitive if function.
;
; Test Cases:
;
; ...
(defun fl-interp-if (appl progr)
  (if (equal T (fl-interp (cadr appl) progr))
      (fl-interp (caddr appl) progr)
      (fl-interp (cadddr appl) progr)
  )
)

; Helper function which computes the primitive null function.
;
; Test Cases:
;
; ...
(defun fl-interp-null (appl progr)
  (if (atom (cadr appl))
    (null (cadr appl))
    (null (fl-interp (cadr appl) progr))
  )
)

; Helper function which computes the primitive atom function.
;
; Test Cases:
;
; ...
(defun fl-interp-atom (appl progr)
  (if (atom (cadr appl))
    T
    (cond
      (
        (or
          (null (caadr appl))
          (numberp (caadr appl))
        )
        nil
      )
      (T (atom (fl-interp (cadr appl) progr)))
    )
  )
)

; Helper function which computes the primitive eq function.
;
; Test Cases:
;
; ...
(defun fl-interp-eq (appl progr)
  (eq (if (atom (cadr appl))
       (cadr appl)
       (fl-interp (cadr appl) progr)
     )
     (if (atom (caddr appl))
       (caddr appl)
       (fl-interp (caddr appl) progr)
     )
  )
)

; Helper function which computes the primitive first function.
;
; Test Cases:
;
; ...
(defun fl-interp-first (appl progr)
  (if (atom appl)
    nil
    (cond
      (
        (or
          (null (caadr appl))
          (numberp (caadr appl))
        )
        (caadr appl)
      )
      (T (fl-interp-first (fl-interp (cadr appl) progr) progr))
    )
  )
)

; Helper function which computes the primitive rest function.
;
; Test Cases:
;
; ...
(defun fl-interp-rest (appl progr)
  (if (atom appl)
    nil
    (cond
      (
        (or
          (null (caadr appl))
          (numberp (caadr appl))
        )
        (cdadr appl)
      )
      (T (fl-interp-rest (fl-interp (cadr appl) progr) progr))
    )
  )
)

; Helper function which computes the primitive cons function.
;
; Test Cases:
;
; ...
(defun fl-interp-cons (appl progr)
  (cons
    (if (atom (cadr appl))
      (cadr appl)
      (fl-interp (cadr appl) progr)
    )
    (cond
      (
        (or
          (null (caaddr appl))
          (numberp (caaddr appl))
        )
        (caddr appl)
      )
      (T (fl-interp (caddr appl) progr))
    )
  )
)

; Helper function which computes the primitive equal function.
;
; Test Cases:
;
; ...
(defun fl-interp-equal (appl progr)
  (equal
    (cond
      (
        (or
          (null (cadr appl))
          (numberp (cadr appl))
          (null (caadr appl))
          (numberp (caadr appl))
        )
        (cadr appl)
      )
      (T (fl-interp (cadr appl) progr))
    )
    (cond
      (
        (or
          (null (caddr appl))
          (numberp (caddr appl))
          (null (caaddr appl))
          (numberp (caaddr appl))
        )
        (caddr appl)
      )
      (T (fl-interp (caddr appl) progr))
    )
  )
)

; Helper function which computes the primitive number function.
;
; Test Cases:
;
; ...
(defun fl-interp-number (appl progr)
  (numberp
    (cond
      (
        (atom (cadr appl))
        (cadr appl)
      )
      (
        (or
          (null (caadr appl))
          (numberp (caadr appl))
        )
        (cadr appl)
      )
      (T (fl-interp (cadr appl) progr))
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

; Helper function which determines whether or not the given argument is a function application.
;
; Test Cases:
;
; ...
(defun is-command (X)
  (if (or
        (null (car X))
        (numberp (car X))
      )
    nil
    T
  )
)
