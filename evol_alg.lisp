(defun normal-random (mean std-dev)
  (do* ((rand-u (* 2 (- 0.5 (random 1.0))) (* 2 (- 0.5 (random 1.0))))
        (rand-v (* 2 (- 0.5 (random 1.0))) (* 2 (- 0.5 (random 1.0))))
        (rand-s (+ (* rand-u rand-u) (* rand-v rand-v))
                (+ (* rand-u rand-u) (* rand-v rand-v))))
    ((not (or (= 0 rand-s) (>= rand-s 1)))
     (+ mean
      (* std-dev
        (* rand-u (sqrt (/ (* -2.0 (log rand-s)) rand-s))))))))

(defun largest-number-index (list)
  (mapcar #'second
          (stable-sort
           (loop
             for index from 0
             for element in list
             collect (list element index))
           #'>
           :key #'first)))

(defun compute-fitness
  (k)
  (let
    ((fitness 10))
    (dotimes (i 5)
             (case (compete-mcrave-benchmark 1000 k)
               (*black* (incf fitness))
               (*white* (decf fitness))))
    (dotimes (i 5)
             (case (compete-benchmark-mcrave 1000 k)
               (*white* (incf fitness))
               (*black* (decf fitness))))
    fitness))

(defun mutate
  (k mut-sd)
  (let
    ((result (+ k (normal-random 0 mut-sd))))
    (cond
      ((<= result 0) 0.0000001)
      ((> result 1) 1)
      (t result))))

(defun reproduce
  (ks pop-size mut-sd)
  (with-open-file (str "evol-log.txt"
                       :direction :output
                       :if-exists :append
                       :if-does-not-exist :create)
    (let*
      ((fitnesses (mapcar #'compute-fitness ks))
       (total-fitness (apply #'+ fitnesses))
       (scaled-fitnesses (mapcar #'(lambda (x)
                                           (* (round (* (/ x total-fitness) pop-size)) 2))
                                 fitnesses))
       (sorted-indices (largest-number-index scaled-fitnesses))
       (new-ks nil))
      (while (< (length new-ks) pop-size)
        (let*
          ((max-ind (pop sorted-indices))
           (num-children (nth max-ind scaled-fitnesses))
           (k (nth max-ind ks)))
          (dotimes (i num-children)
                   (push new-ks (mutate k mut-sd))
                   (when (= (length new-ks) pop-size)
                     (format str "~A," fitnesses)
                     (format str "~A~%" new-ks)
                     (return-from reproduce new-ks)))))
      (format str "~A," fitnesses)
      (format str "~A~%" new-ks)
      new-ks)))

(defun driver
  (num-gen pop-size mut-sd)
    (let
      ((current-ks (list-gen (/ 1 pop-size))))
      (dotimes (i num-gen)
               (with-open-file (str "evol-log.txt"
                                    :direction :output
                                    :if-exists :append
                                    :if-does-not-exist :create)
                 (format str "~A," i)
                 (format str "~A," current-ks))
               (setf current-ks (reproduce current-ks pop-size mut-sd)))))
