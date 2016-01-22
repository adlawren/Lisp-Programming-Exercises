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

; Returns the subset of the vaues in given list which sum to the provided value.
; Returns an empty list if no such subset can be found.
;
; Test Cases:
; (subsetsum '() '() '() 0) => ()
; (subsetsum '(1) '() '() 1) => (1)
; (subsetsum '(1) '() '() 2) => ()
; (subsetsum '(1 2 3) '() '() 5) => (2 3)
; (subsetsum '(1 2 3) '() '() 4) => (1 3)
; (subsetsum '(1 2 3) '() '() 7) => ()
; (subsetsum '(1 2 3 4 5 6 7 8 9) '() '() 13) => (1 3 4 5)
(defun subsetsum (L S R X)
    (if (null L)
        R
        (subsetsum
            (cdr L)
            (allsets S (car L))
            (matchingsum
                (allsets S (car L))
                X
            )
            X
        )
    )
)

; Returns a list in the provided list of lists, the elements of which sum to the given value.
; Returns an empty list if no such list can be found.
;
; Test Cases:
; (matchingsum '() 0) => ()
; (matchingsum '(()) 0) => ()
; (matchingsum '((1)) 0) => ()
; (matchingsum '((1)) 1) => (1)
; (matchingsum '((1 2) (3 4)) 7) => (3 4)
; (matchingsum '((1 2 3) (4 5) (6 7 8)) 5) => ()
; (matchingsum '((1 2 3) (4 5) (6 7 8)) 21) => (6 7 8)
(defun matchingsum (S X)
    (if (null S)
        ()
        (if
            (equal
                (sum (car S))
                X
            )
            (append
                (car S)
            )
            (matchingsum (cdr S) X)
        )

    )
)

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

; Returns all of the possible lists which may be formed using the additional element and the given list of lists.
;
; Test Cases:
; (allsets nil 0) => (() (0))
; (allsets '((1)) 0) => ((1) (1 0))
; (allsets '(() (1) (1 2)) 3) => (() (1) (1 2) (3) (1 3) (1 2 3))
(defun allsets (S X)
    (if (null S)
        (list
            '()
            (list X)
        )
        (append
            S
            (appendall S X)
        )
    )
)

; Returns a list of the lists formed by appending the given element to each list in the given list of lists.
;
; Test Cases:
; (appendall nil 0)
; (appendall '((0)) 1)
; (appendall '((0 1) (2 3)) 4)
; (appendall '((0) (1 2 3) (4 5 6 7 8)) 9)
(defun appendall (L X)
    (if (null L)
        ()
        (append
            (list
                (append (car L) (list X))
            )
            (appendall (cdr L) X)
        )
    )
)
