(defun list-gen
  (step)
  (let ((counter 0)
        (result nil))
    (while (<= counter 1)
      (push counter result)
      (incf counter step))
    (reverse result)))

(defun k-test
  (match-number step)
  (let
    ((k-values (list-gen step)))
    (while k-values
      (let
        ((k (pop k-values))
         (wins 0))
        (when (zerop k) (setf k 0.0000001))
        (dotimes (i match-number)
                 (when (compete-mcrave-benchmark 1000 k)
                   (incf wins)))
        (with-open-file (str "k-test.txt"
                             :direction :output
                             :if-exists :append
                             :if-does-not-exist :create)
          (format str "~A,~A~%" k (/ wins match-number)))))))
