; An interpreter for a simple functional language.
;
; Test Cases:
;
; ...
(defun fl-interp (appl progr)
  (cond
    (
      (atom appl)
      appl
    )
    (
      (equal
        (car appl)
        'if
      )
      (fl-interp-if appl progr)
    )
    (
      (equal
        (car appl)
        'null
      )
      (fl-interp-null appl progr)
    )
    (
      (equal
        (car appl)
        'atom
      )
      (fl-interp-atom appl progr)
    )
    (
      (equal
        (car appl)
        'eq
      )
      (fl-interp-eq appl progr)
    )
    (
      (equal
        (car appl)
        'first
      )
      (fl-interp-first appl progr)
    )
    (
      (equal
        (car appl)
        'rest
      )
      (fl-interp-rest appl progr)
    )
    (
      (equal
        (car appl)
        'cons
      )
      (fl-interp-cons appl progr)
    )
    (
      (equal
        (car appl)
        'equal
      )
      (fl-interp-equal appl progr)
    )
    (
      (equal
        (car appl)
        'number
      )
      (fl-interp-number appl progr)
    )
    (
      (equal
        (car appl)
        '+
      )
      (fl-interp-add appl progr)
    )
    (
      (equal
        (car appl)
        '-
      )
      (fl-interp-sub appl progr)
    )
    (
      (equal
        (car appl)
        '*
      )
      (fl-interp-mul appl progr)
    )
    (
      (equal
        (car appl)
        '>
      )
      (fl-interp-greater-than appl progr)
    )
    (
      (equal
        (car appl)
        '<
      )
      (fl-interp-less-than appl progr)
    )
    (
      (equal
        (car appl)
        '=
      )
      (fl-interp-equals appl progr)
    )
    (
      (equal
        (car appl)
        'and
      )
      (fl-interp-and appl progr)
    )
    (
      (equal
        (car appl)
        'or
      )
      (fl-interp-or appl progr)
    )
    (
      (equal
        (car appl)
        'not
      )
      (fl-interp-not appl progr)
    )
    (T
      (fl-parse-command appl (fl-get-command appl progr))
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
  (eq
    (fl-interp (cadr appl) progr)
    (fl-interp (caddr appl) progr)
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

; Helper function which determines whether or not the given argument is a user defined function.
;
; Test Cases:
;
; ...
(defun fl-parse-command (appl def)
  (if (null def)
    appl
    nil ;TODO: implement this
  )
)

; Helper function which determines whether or not the given argument is a user defined function.
;
; Test Cases:
;
; ...
(defun fl-get-command (appl progr)
  (cond
    (
      (null progr)
      nil
    )
    (
      (equal (car appl) (caar progr))
      (car progr)
    )
    (T (fl-get-command appl (cdr progr)))
  )
)

#| TODO: remove; test
(trace fl-get-command)
(fl-get-command nil nil)
(fl-get-command '(1 2 3) nil)
(fl-get-command '(test-fn 1) '((test-fn X = (+ 1 X))))
(fl-get-command '(other-test-fn 1) '((test-fn X = (+ 1 X)) (other-test-fn X = (+ 2 X))))
(untrace fl-get-command)
|#
