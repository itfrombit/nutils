;; @file       cl_utils.nu
;; @discussion Nu versions of popular Common Lisp functions and macros.
;;
;; @copyright  Copyright (c) 2009 Jeff Buck
;;
;;   Licensed under the Apache License, Version 2.0 (the "License");
;;   you may not use this file except in compliance with the License.
;;   You may obtain a copy of the License at
;;
;;       http://www.apache.org/licenses/LICENSE-2.0
;;
;;   Unless required by applicable law or agreed to in writing, software
;;   distributed under the License is distributed on an "AS IS" BASIS,
;;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;   See the License for the specific language governing permissions and
;;   limitations under the License.


;; These functions are part of Common Lisp.
;; The subsequent examples in the book assume they are defined.


(function mapcar-1 (f l)
     (cond
          ((null? l) nil)
          (else
               (cons (f (car l)) (mapcar-1 f (cdr l))))))

;; Nu's cadr-type built-ins are postfix.
;; Not as suitable for lispy mapping functions.
(function caar (l)
     (car (car l)))

(function cadr (l)
     (car (cdr l)))

(function cddr (x)
     (cdr (cdr x)))


(function 1+ (n)
     (+ n 1))

(function 1- (n)
     (- n 1))

(macro-1 incf (n *delta)
     (if (not (eq *delta '()))
         (then `(set ,n (+ ,n ,(car *delta))))
         (else `(set ,n (+ ,n 1)))))

(macro-1 decf (n *delta)
     (if (not (eq *delta '()))
         (then `(set ,n (- ,n ,(car *delta))))
         (else `(set ,n (- ,n 1)))))


(function evenp (x)
     ((eq 0 (% x 2))))

(function oddp (x)
     (not (evenp x)))

(set even? evenp)
(set odd? oddp)

(function select-if (f l)
     (function select-if-acc (f l acc)
          (if (null? l)
              (then acc)
              (else
                   (if (f (car l))
                       (then (select-if-acc f (cdr l) (append acc (list (car l)))))
                       (else (select-if-acc f (cdr l) acc))))))
     (select-if-acc f l nil))


(function nth (n source)
     (cond
          ((null? source) nil)
          ((== n 0) (car source))
          ((> n (source length)) nil)
          (else (nth (- n 1) (cdr source)))))

(function nthcdr (n source)
     (cond ((eq n 0)
            source)
           ((> n (source length)) nil)
           (else (nthcdr (- n 1) (cdr source)))))


(function subseq (l start end)
     (if (eq (l class) ("a" class))
         (then
              (if (>= start end)
                  (then "")
                  (else
                       ;; String - use substring
                       (l substringWithRange:(list start (- end start))))))
         (else
              ;; Assume a list - use cdrs
              (set len (l length))
              (set i start)
              (set result nil)
              (while (and (< i end) (< i len))
                     (set result (append result (list (car (nthcdr i l)))))
                     (set i (+ i 1)))
              result)))


(function last (l *n)
     (let ((len (l length)))
          (if *n
              (then (set count (car *n)))
              (else (set count 1)))
          (if (> count len)
              (then (set count len)))
          (subseq l (- len count) len)))


(function butlast (l *n)
     (if (not (eq *n '()))
         (then (set count (car *n)))
         (else (set count 1)))
     (let ((len (l length)))
          (if (>= count len)
              (then '())
              (else (subseq l 0 (- len count))))))



(macro-1 let* (bindings *body)
     (if (null? bindings)
         (then
              `(progn
                     ,@*body))
         (else
              (set __nextcall `(let* ,(cdr bindings) ,@*body))
              `(let (,(car bindings))
                    ,__nextcall))))


;; Simple stack operations
(macro push (v l)
     `(set ,l (cons ,v ,l)))

(macro pop (l)
     `(progn
            (cond ((null? ,l) nil)
                  (else
                       (set __v (car ,l))
                       (set ,l (cdr ,l))
                       __v))))

;; simulates CL's m-v-b using a list
(macro multiple-value-bind (vars vals *body)
     (set __i 0)
     (set __evalvals (eval vals))
     (set __letvars nil)
     (while (< __i (vars length))
            (set __letvars (append __letvars (list (list (nth __i vars)
                                                         `',(nth __i __evalvals)))))
            (set __i (+ __i 1)))
     `(let ,__letvars
           ,@*body))


;; anaphoric-if, assign test clause to 'it'
(macro aif (test-clause then-clause else-clause)
     `(let ((it ,test-clause))
           (if it
               (then ,then-clause)
               (else ,else-clause))))

;; anaphoric-if with mvb
;; first return value is test clause return value
;; second return value is test clause outcome
;; useful for test clauses that return bindings and true/false
(macro aif2 (test-clause then-clause else-clause)
     `(multiple-value-bind (it __win) ,test-clause
           (if (or it __win)
               (then ,then-clause)
               (else ,else-clause))))

;(macro acond2 (*clauses)
;     (if (null? *clauses)
;         (then nil)
;         (else
;              (let ((cl1 (car *clauses)))
;                   `(multiple-value-bind (__val __win) ,(car cl1)
;                         (if (or (not (null? __val)) (not (null? __win)))
;                             (then
;                                  (let ((it __val))
;                                       ,@(cdr cl1)))
;                             (else
;                                  (acond2 ,@(cdr *clauses)))))))))

(macro apply-key (key item)
     `(if ,key
          (then (,key ,item))
          (else ,item)))

; substitutes into tree bindings that appear in alist
; for example:
; % (sublis '((x 1) (y 2)) '(* x y))
; (* 1 2)

(function sublis (alist tree)
     (cond
          ((null? tree) nil)
          ((atom tree)
           (if (assoc tree alist)
               (then (cadr (assoc tree alist)))
               (else tree)))
          (else
               (cons (sublis alist (car tree)) (sublis alist (cdr tree))))))

(function union (l1 l2)
     (set r nil)
     ((list l1 l2) map:
      (do (l)
          (set i 0)
          (while (< i (l length))
                 (if (not (member (nth i l) r))
                     (then (push (nth i l) r)))
                 (incf i))
          ))
     r)

;; Fake a gensym and return as a symbol value
(function gensym (*x)
     (set s (+ "G" ((floor (rand 999999999)))))
     (if (and (not (null? *x) (not (null? (car *x)))))
         (then (set s (+ ((car *x) stringValue) s))))
     (s symbolValue))


;; Not part of Common Lisp, but popular functions to have around...

;; Glue up a string from various substrings.
(function mkstr (*rest)
     (set s "")
     (*rest each:
            (do (a)
                (set s (+ s a))))
     s)

;; Make a symbol name out of a list of substrings.
(function symb (*rest)
     ((apply mkstr *rest) symbolValue))


;; Group a flat list into lists of length n.
(function group (source n)
     (function group-rec (source n acc)
          (let ((rest (nthcdr n source)))
               (if (pair? rest)
                   (then
                        (group-rec rest n (cons (subseq source 0 n) acc)))
                   (else
                        (reverse (cons source acc))))))
     (if source
         (then (group-rec source n nil))
         (else nil)))


;; Flatten a nested list.
(function flatten (x)
     (function flatten-rec (x acc)
          (cond
               ((eq x nil) acc)
               ((atom? x) (cons x acc))
               (else (flatten-rec (car x) (flatten-rec (cdr x) acc)))))
     (flatten-rec x nil))


;; Returns the tails of a list starting with the first member that matches == test
(function member (a l)
     (cond
          ((null? l) nil)
          ((atom l) nil)
          ((== a (car l)) l)
          (else (member a (cdr l)))))

;; Returns the cons in list l whose car is == to a
(function assoc (a l)
     (cond
          ((null? l) nil)
          ((atom l) nil)
          ((== a (car (car l))) (car l))
          (else (assoc a (cdr l)))))

;; A few math functions
(function fact (x)
     (if (<= x 0)
         (then 1)
         (else (* x (fact (- x 1))))))

(function choose (n r)
     (/ (fact n)
        (fact (- n r))
        (fact r)))

(function perm (n r)
     (/ (fact n)
        (fact (- n r))))


;; The rest of the functions in this file are not part of
;; Common Lisp, but they are "Common Lispy" enough to include
;; here.  They are provided as a convenience functions that a
;; multi-list mapcar could otherwise provide.

;; returns the obvious on a list of lists.
(function cars (lists)
     (mapcar-1 car lists))

(function cdrs (lists)
     (mapcar-1 cdr lists))

;; Nu's map provides similar functionality to weave,
;; but doesn't work for quoted lists like this:
;;   (map list '(a b c) '(x y z))
;; It tries to eval the list elements.
;;  ^ the internal apply is the guilty party in map.

;; weave only works for two lists.
;;  ex: (weave '(a b c) '(x y z)) -> (a x) (b y) (c z)
(function weave (*lists)
     (function weave-rec (lists)
          (cond
               ((null? lists) nil)
               ((null? (car lists)) nil)
               (else
                    (cons
                         (cars lists)
                         (weave-rec (cdrs lists))))))
     (weave-rec *lists))


; Nu adds a "list" method to NSArray.
; This function turns a string into a list of characters.
(function listify (s)
     (let ((i 0)
           (len (s length))
           (result '()))
          (while (< i len)
                 (set result (append result (list (subseq s i (+ i 1)))))
                 (set i (+ i 1)))
          result))

