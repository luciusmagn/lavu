# Occurrence typing stress corpus. Lines starting with "#" are comments.
# Each non-comment line is a full query body inferred in a fresh session.
# --- baseline sanity ---
(lambda (x) (if (string? x) (string-length x) (+ x 1)))
(lambda (x) (if (number? x) (+ x 1) 0))
# --- not / nested not ---
(lambda (x) (if (not (string? x)) (+ x 1) (string-length x)))
(lambda (x) (if (not (not (string? x))) (string-length x) (+ x 1)))
# --- and / or conditions ---
(lambda (x y) (if (and (string? x) (number? y)) (+ (string-length x) y) 0))
(lambda (x) (if (or (string? x) (symbol? x)) (symbol? x) (not x)))
(lambda (x) (if (or (number? x) (string? x)) x #f))
# --- else-branch refinement of unions ---
(lambda (x) (if (string? x) x "fallback"))
# --- cond chains ---
(lambda (x) (cond ((number? x) (+ x 1)) ((string? x) (string-length x)) (else 0)))
(lambda (x) (cond ((number? x) x) ((string? x) (string->number x)) (else #f)))
# --- case ---
(lambda (x) (case x ((1 2 3) 'small) ((10) 'ten) (else 'other)))
# --- nested ifs refine progressively ---
(lambda (x) (if (pair? x) (if (number? (car x)) (+ (car x) 1) 0) 0))
# --- predicates over accessors ---
(lambda (x) (if (and (pair? x) (number? (car x)) (number? (cdr x))) (+ (car x) (cdr x)) 0))
# --- contradictory path is never ---
(lambda (x) (if (number? x) (if (string? x) (string-length x) (+ x 1)) 0))
# --- truthiness of false-or-success ---
(lambda (s) (let ((n (string->number s))) (if n (+ n 1) 0)))
# --- when value used in both branches ---
(lambda (x) (if (boolean? x) (not x) x))
# --- let-bound alias refinement ---
(lambda (x) (let ((y x)) (if (string? y) (string-length x) 0)))
# --- begin in branch ---
(lambda (x) (if (string? x) (begin (display x) (string-length x)) 0))
# --- refinements through do loop result ---
(lambda (n) (do ((i 0 (+ i 1)) (acc 1 (* acc i))) ((= i n) acc)))
# --- char/eof union from read-char ---
(lambda (port) (let ((c (read-char port))) (if (eof-object? c) #\a c)))
# --- equality literal refinement ---
(lambda (x) (if (eqv? x 1) (+ x 1) 0))
(lambda (x) (if (eq? x 'foo) (symbol->string x) "no"))
# --- memq result usage ---
(lambda (x lst) (let ((hit (memq x lst))) (if hit (car hit) #f)))
# --- assoc result usage ---
(lambda (key table) (let ((entry (assv key table))) (if entry (cdr entry) #f)))
# --- higher order latent predicates ---
(lambda (pred proc x) (if (pred x) (proc x) #f))
# --- user-defined predicate ---
(define stringy? (lambda (x) (string? x)))
# --- predicate used after definition ---
(begin (define stringy? (lambda (x) (string? x))) (lambda (x) (if (stringy? x) (string-length x) 0)))
# --- or returning the tested value ---
(lambda (x) (or (string->number x) 0))
# --- and returning last value ---
(lambda (x) (and (number? x) (+ x 1)))
# --- multiple refinements joined at merge ---
(lambda (x) (if (string? x) x (if (number? x) (number->string x) "other")))
# --- vector refinement ---
(lambda (v) (if (vector? v) (vector-length v) 0))
# --- procedure? refinement ---
(lambda (f) (if (procedure? f) (f 1) 0))
# --- null?/pair? list walk ---
(define my-length (lambda (lst) (if (null? lst) 0 (+ 1 (my-length (cdr lst))))))
# --- list? refinement ---
(lambda (x) (if (list? x) (length x) 0))
# --- zero? guard ---
(lambda (n) (if (zero? n) 1 (* n n)))
# --- negative guard with not ---
(lambda (x) (if (not (pair? x)) 0 (car x)))
# --- string->symbol round trip ---
(lambda (s) (if (string? s) (string->symbol s) 'default))
# --- deep cond with arrow ---
(lambda (s) (cond ((string->number s) => (lambda (n) (+ n 1))) (else 0)))
# --- and guard then accessor in else of inner if ---
(lambda (x) (if (and (pair? x) (string? (car x))) (string-length (car x)) 0))
# --- subtraction from union built by params ---
(lambda (x) (if (number? x) x (if (string? x) (string-length x) 0)))
# --- not with or ---
(lambda (x) (if (not (or (string? x) (number? x))) 0 1))
# --- result of and as value ---
(lambda (x) (and (pair? x) (car x)))
# --- nested lambda capture refinement ---
(lambda (x) (if (number? x) (lambda () (+ x 1)) (lambda () 0)))
# --- set! interplay: var keeps type ---
(lambda (x) (begin (set! x (+ x 1)) x))
# --- letrec polymorphism ---
(letrec ((map2 (lambda (f lst) (if (null? lst) '() (cons (f (car lst)) (map2 f (cdr lst))))))) map2)
# --- apply with list ---
(lambda (x) (apply + (list 1 2 x)))
# --- multiple value flow ---
(call-with-values (lambda () (values 1 2)) +)
# --- equal? on strings refinement: should refine enough for string-length ---
(lambda (x) (if (equal? x "hi") (string-length x) 0))
# --- boolean=? style double test ---
(lambda (x) (if (if (string? x) #t (number? x)) x 0))
