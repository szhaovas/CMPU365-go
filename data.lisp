(defun normal-random (mean std-dev)
  (do* ((rand-u (* 2 (- 0.5 (random 1.0))) (* 2 (- 0.5 (random 1.0))))
        (rand-v (* 2 (- 0.5 (random 1.0))) (* 2 (- 0.5 (random 1.0))))
        (rand-s (+ (* rand-u rand-u) (* rand-v rand-v))
                (+ (* rand-u rand-u) (* rand-v rand-v))))
    ((not (or (= 0 rand-s) (>= rand-s 1)))
     (+ mean
      (* std-dev
        (* rand-u (sqrt (/ (* -2.0 (log rand-s)) rand-s))))))))

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
                 (when (eq (compete-mcrave-benchmark 1000 k) *black*)
                   (incf wins)))
        (with-open-file (str "k-test.txt"
                             :direction :output
                             :if-exists :append
                             :if-does-not-exist :create)
          (format str "~A,~A~%" k (/ wins match-number)))))))
