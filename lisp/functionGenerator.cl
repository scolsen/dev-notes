;;An example function generator using keyword args

(defun foo (&key a b c) (list a b c))

;; This function uses &key to create key args, which enable optional arguments.
;; Pass order does not matter for keyword args.

;; Example function calls:
(foo (a: 1 b: 2 c: 3)) ;; (1 2 3)
(foo (a: 1))           ;; (1 NIL NIL)
(foo (c: 3 b: 2 a: 1)) ;; (1 2 3)
(foo (a: 1 c: 3))      ;; (1 NIL 3)

;; to capture whether an arg was passed use a supplied-p parameter in a list:
;; You can also provide a defualt val as the second arg in the list
(defun bar (&key a (b 20) (c 30 c-p)) (list a b c c-p))

(bar (a: 1))           ;; (1 20 30 NIL)
(bar (a: 1 c: 3))      ;; (1 20 3 T)

;; Use key args to create a function generator:

;; a function generator for SQL like extraction from a plist 

(defun where (&key title artist rating (ripped nil ripped-p)) 
  #'(lambda (cd) 
    (and 
      (if title (equal (getf cd :title) title) t)
      (if artist (equal (getf cd :artist) artist) t)
      (if rating (equal (getf cd :rating) t) 
      (if ripped-p (equal (getf cd :ripped) t))))

