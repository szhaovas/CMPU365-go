(defun list-gen
  (step)
  (let ((counter 0.0000001)
        (result nil))
    (while (<= counter 1)
      (push counter result)
      (incf counter step))
    (reverse result)))

(defun k-test
  (match-number step)
  (let
    ((k-values (list-gen step))
     (win-rates nil))
    (while k-values
      (let
        ((k (pop k-values))
         (wins 0))
        (dotimes (i match-number)
                 (when (compete-mcrave-benchmark 1000 k)
                   (incf wins)))
        (push (/ wins match-number) win-rates)))
    (with-open-file (str "k-test.txt"
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
      (format str "~A" (reverse win-rates)))))
