; Returns T if the list Y contains X, which may be either an atom or a list.
;
; Test Cases:
; (xmember 'A ()) => nil
; (xmember 'A '(A)) => T
; (xmember 'A '(B A)) => T
; (xmember 'A '(B A C)) => T
; (xmember 'A '(B C D)) = > nil
; (xmember '(A) '()) => nil
; (xmember '(A) '(B)) => nil
; (xmember '(A) '(B (C) D)) => nil
; (xmember '(A) '(B (C D) E (A))) => T
(defun xmember (X Y)
    (if (car Y)
        (if (equal X (car Y))
            T
            (xmember X (cdr Y)))
         nil)
)

; Test Case, what should happen here??
;(xmember '() '(()))


; Returns a single list containing all atoms, nested or otherwise, contained in the list X in the orginal order.
;
; Test Cases:
; (flatten '()) => ()
; (flatten '(A)) => (A)
; (flatten '(A B C)) => (A B C)
; (flatten '(A (B C) D)) => (A B C D)
; (flatten '(A (B (C D E) F (G H) I) J)) => (A B C D E F G H I J)
(defun flatten (X)
    (if (car X)
        (if (atom (car X))
            (append (list (car X)) (flatten (cdr X)))
            (append (flatten (car X)) (flatten (cdr X)))
        )
        ()
    )
)


; Returns a single list that mixes the elements of the two list parameters together.
; If the lists are not the same length, the elements at the end of the longer list are appended to the end of the resutant list.
;
; Test Cases:
; (mix '() '()) => ()
; (mix '(A) '(B)) => (A B)
; (mix '(A C E) '(B D F)) => (A B C D E F)
; (mix '(A C E) '(B D F G H I J)) => (A B C D E F G H I J)
(defun mix (L1 L2)
    (cond
        ((and (car L1) (car L2))
            (append
                (list
                    (car L1) (car L2)
                )
                (mix (cdr L1) (cdr L2))
            )
        )
        ((car L1) L1)
        ((car L2) L2)
        (T ())
    )
)


; Returns a pair of lists (L1, L2) which each consist of elements from list L alternatingly.
;
; Test Cases:
; (split '()) => '(() ())
; (split '(A)) => '((A) ())
; (split '(A B)) => '((A) (B))
; (split '(A B C)) => '((A C) (B))
; (split '(A B C D)) => '((A C) (B D))
; (split '(A B C D E F G)) => '((A C E G) (B D F))
; (split '(A B C D E F G H)) => '((A C E G) (B D F H))
(defun split (L)
    (if (car L)
        (if (cadr L)
            (list
                (append (list
                        (car L))
                        (car (split (cddr L)))
                )
                (append (list
                        (cadr L))
                        (cadr (split (cddr L)))
                )
            )
            (list
                (list (car L))
                '()
            )
        )
        (list '() '())
    )
)


; Returns the number of top-level items in the given list.
;
; Test Cases:
; (listlength '()) => 0
; (listlength '(A)) => 1
; (listlength '(A B C D)) => 4
; (listlength '(A (B C) D)) => 3
(defun listlength (L)
    (if (car L)
        (+ 1 (length (cdr L)))
        0
    )
)

; ...
;
; Test Cases:
; (subsetsum '() '(() 0) '() 0) => ()
; (subsetsum '(1) '(() 0) '() 1) => (1)
; (subsetsum '(1) '(() 0) '() 2) => ()
; (subsetsum '(1 2 3) '(() 0) '() 5) => (2 3)
; (subsetsum '(1 2 3) '(() 0) '() 4) => (1 3)
; (subsetsum '(1 2 3) '(() 0) '() 7) => ()
; (subsetsum '(1 2 3 4 5 6 7 8 9) '(() 0) '() 13) => (1 3 4 5)
;(defun subsetsum (L S R X)
;    (if (null L)
;        R
;        (subsetsum
;            (cdr L)
;            (allsets S (car L))
;            (matchingsum
;                (allsets S (car L))
;                X
;            )
;            X
;        )
;    )
;)

(defun computesubsetsum (L S X)
    (matchingsum (getallsubsets L S (+ X 1)) X)
)

(defun subsetsum (L X)
    (computesubsetsum L '((() 0)) X)
)

(print '(SUBSETSUM TESTS))
(trace subsetsum)

;(subsetsum '() 0)
;(subsetsum '(1) 1)
;(subsetsum '(1) 2)
;(subsetsum '(1 2 3) 5)
;(subsetsum '(1 2 3) 4)
;(subsetsum '(1 2 3) 7)
;(subsetsum '(1 2 3 4 5 6 7 8 9) 13)

; Easy
;(subsetsum '(1 2 3) 5)
;(subsetsum '(1 5 3) 2)
;(subsetsum '(1 16 2 8 4) 29)
;(subsetsum '(1 1 5 6 8) 10)
;(subsetsum '(1 10 100 1000 10000) 5)

; Hard
; ...

(untrace subsetsum)

(defun getallsubsets (L S M)
    (if (null L)
        S
        (getallsubsets (cdr L) (allsets S (car L) M) M)
    )
)

;(print '(GETALLSUBSETS TESTS))
;(trace getallsubsets)
;(getallsubsets '() '((() 0)))
;(getallsubsets '(1) '((() 0)))
;(getallsubsets '(1 2 3) '((() 0)))
;(untrace getallsubsets)

; ...
;
; Test Cases:
; (matchingsum '() 0) => ()
; (matchingsum '(((1) 1)) 1) => (1)
; (matchingsum '(((1 1) 2)) 2) => (1 1)
; (matchingsum '(((1 1) 2) ((1 3) 4)) 5) => ()
; (matchingsum '(((1 1) 2) ((1 3) 4) ((2 3) 5)) 5) => (2 3)
(defun matchingsum (S X)
  (if (null S)
    ()
    (if (equal
            (cadar S)
            X
        )
        (caar S)
        (matchingsum
            (cdr S)
            X
        )
    )
  )
)

;(print '(MATCHINGSUM TESTS))
;(trace matchingsum)
;(matchingsum '() 0)
;(matchingsum '(((1) 1)) 1)
;(matchingsum '(((1 1) 2)) 2)
;(matchingsum '(((1 1) 2) ((1 3) 4)) 5)
;(matchingsum '(((1 1) 2) ((1 3) 4) ((2 3) 5)) 5)
;(untrace matchingsum)

; Returns the sum of the items in the list.
;
; Test Cases:
; (sum '()) => 0
; (sum '(1)) => 1
; (sum '(1 2 3 4 5)) => 15
(defun sum (L)
    (if (null L)
        0
        (+
            (car L)
            (sum (cdr L))
        )
    )
)

; ...
;
; Test Cases:
; (allsets '() X) => ()
; (allsets '((() 0)) 1) => ((() 0) ((1) 1))
; (allsets '((() 0) ((1) 1) ((1 2) 3)) 3) => ((() 0) ((1) 1) ((1 2) 3) ((3) 3) ((1 3) 4) ((1 2 3) 6))
; (allsets '(((1 2) 3) ((2 2) 4) ((1 4 5) 10)) 12) => (((1 2) 3) ((2 2) 4) ((1 4 5) 10) ((1 2 12) 15) ((2 2 12) 16) ((1 4 5 12) 22))
(defun allsets (S X M)
    (if (null S)
        ()
        (append
            S
            (appendall S X M)
        )
    )
)

; ...
;
; Test Cases:
; (appendall '() 1) => ()
; (appendall '((() 0)) 1) => (((1) 1))
; (appendall '(((1) 1) ((2) 2)) 3) => (((1 3) 4) ((2 3) 5))
; (appendall '(((1 2) 3) ((4 5 6 7) 8)) 9) => (((1 2 9) 12) ((4 5 6 7 9) 17))
(defun appendall (L X M)
    (if (null L)
        ()
        (append
            (if (< (+ (cadar L) X) M)
                (list
                    (list
                        (append
                            (caar L)
                            (list X)
                        )
                        (+ (cadar L) X)
                    )
                )
                ()
            )
            (appendall (cdr L) X M)
        )
    )
)

;(print '(APPENDALL TESTS))
;(trace appendall)
;(appendall '() 1 100)
;(appendall '((() 0)) 1 111)
;(appendall '(((1) 1) ((2) 2)) 3 100)
;(appendall '(((1 2) 3) ((4 5 6 7) 8)) 9 15)
;(untrace appendall)
