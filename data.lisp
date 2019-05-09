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
    (reverse win-rates)))
