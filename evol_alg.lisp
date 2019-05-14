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
    (dotimes (i 1)
             (let
               ((result (compete-mcrave-benchmark 100 k)))
               (cond
                 ((eq result *black*) (incf fitness))
                 ((eq result *white*) (decf fitness)))))
    (dotimes (i 1)
             (let
               ((result (compete-benchmark-mcrave 100 k)))
               (cond
                 ((eq result *white*) (incf fitness))
                 ((eq result *black*) (decf fitness)))))
    fitness))

(defun compute-all-fitness
  (ks)
  (mapcar #'compute-fitness ks))

(defun mutate
  (k mut-sd)
  (let
    ((result (+ k (normal-random 0 mut-sd))))
    (cond
      ((<= result 0) 0.0000001)
      ((> result 1) 1)
      (t result))))

(defun reproduce
  (ks all-fitness pop-size mut-sd)
  (let*
    ((total-fitness (apply #'+ all-fitness))
     (scaled-fitnesses (mapcar #'(lambda (x)
                                         (* (round (* (/ x total-fitness) pop-size)) 2))
                               all-fitness))
     (sorted-indices (largest-number-index scaled-fitnesses))
     (new-ks nil))
    (while (< (length new-ks) pop-size)
      (let*
        ((max-ind (pop sorted-indices))
         (num-children (nth max-ind scaled-fitnesses))
         (k (nth max-ind ks)))
        (dotimes (i num-children)
                 (push (mutate k mut-sd) new-ks)
                 (when (= (length new-ks) pop-size)
                   (return-from reproduce new-ks)))))
    new-ks))

(defun driver
  (num-gen pop-size mut-sd)
    (let
      ((current-ks (rest (list-gen (/ 1 pop-size)))))
      (dotimes (i num-gen)
               (with-open-file (str "evol-log.txt"
                                    :direction :output
                                    :if-exists :append
                                    :if-does-not-exist :create)
                 (let*
                   ((all-fitness (compute-all-fitness current-ks))
                    (new-ks (reproduce current-ks all-fitness pop-size mut-sd)))
                   (format str "~A," i)
                   (format str "~A," current-ks)
                   (format str "~A," all-fitness)
                   (format str "~A~%" new-ks)
                   (setf current-ks new-ks))))))
