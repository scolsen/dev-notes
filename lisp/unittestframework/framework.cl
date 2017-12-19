(defmacro combine-results (&body forms)
  (with-gensyms (result)
    `(let ((,result t))
      ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
      ,result)))

(defun report-result (result form)
  (format t "[FAIL;pass]...~a%" result form) result)

;; Take a form, evaluate it for report, and print the same form.
(defmacro check (&body forms)
   ('combine-results 
      @(loop for f in forms collect ('report-result f 'f))))

(defun test-+ ()
  (check
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ 4 5) 9)))
