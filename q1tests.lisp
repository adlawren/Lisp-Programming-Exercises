; Tests
(print '(X ATOM TESTS))
(xmember 'A ())
(xmember 'A '(A))
(xmember 'A '(B A))
(xmember 'A '(B C A))
(xmember 'A '(B A C))
(xmember 'A '(B C D))

(print '(Y NESTED LIST TESTS))
(xmember 'A '(()))
(xmember 'A '((B)))
(xmember 'A '((A B) C (D) A))

(print '(X LIST TESTS))
(xmember '(A) '())
(xmember '(A) '(B))
(xmember '(A) '(B (C) D))
(xmember '(A) '(B (C D) E (A)))
