;;  COMPETE
;; --------------------------------------------------
;;  INPUTS:  BLACK-NUM-SIMS, the number of simulations for each of black's moves
;;           BLACK-C, the exploration/exploitation constant used by black
;;           WHITE-NUM-SIMS, the number of simulations for each of white's moves
;;           WHITE-C, the exploration/exploitation constant used by white
;;  OUTPUT:  Don't care
;;  SIDE EFFECT:  Displays the entire game using UCT-SEARCH to compute best moves
;;    for both players according to the specified parameters.

(defun new-gomoku
  (dim win)
  (make-gomoku :num-open (* dim dim)
               :board (make-array (list dim dim) :initial-element *blank*)
               :win-lineup-num win))

(defun compete
    (black-num-sims black-k white-num-sims white-k)
  (setf *verbose* t)
  (let ((g (new-gomoku 7 5)))
    (while (not (game-over? g))
      (cond
       ((eq (gomoku-whose-turn g) *black*)
	(format t "BLACK'S TURN!~%")
	(format t "~A~%"
		(apply #'do-move! g (mc-rave g black-num-sims (* black-num-sims black-k)))))
       (t
	(format t "WHITE'S TURN!~%")
	(format t "~A~%"
		(apply #'do-move! g (mc-rave g white-num-sims (* white-num-sims white-k)))))))))

(defun compete-uct
  (black-num-sims black-k black-c white-num-sims white-k white-c)
  (setf *verbose* t)
  (let ((g (new-gomoku 7 5)))
    (while (not (game-over? g))
      (cond
        ((eq (gomoku-whose-turn g) *black*)
        	(format t "BLACK'S TURN!~%")
        	(format t "~A~%"
               		(apply #'do-move! g (uct-rave g black-num-sims (* black-num-sims black-k) black-c))))
        (t
        	(format t "WHITE'S TURN!~%")
        	(format t "~A~%"
               		(apply #'do-move! g (uct-rave g white-num-sims (* white-num-sims white-k) white-c))))))))


;;  COMPETE-NO-PRINTING
;; --------------------------------------------------
;;  Same as COMPETE, but only shows the end result

(defun compete-no-printing
    (black-num-sims black-k white-num-sims white-k)
  (setf *verbose* nil)
  (let ((g (new-gomoku 7 5)))
    (while (not (game-over? g))
      (cond
       ((eq (gomoku-whose-turn g) *black*)
	(format t "B ")
	(apply #'do-move! g (mc-rave g black-num-sims (* black-num-sims black-k))))
       (t
	(format t "W ")
	(apply #'do-move! g (mc-rave g white-num-sims (* white-num-sims white-k))))))
    (format t "~%~A~%" g)))

(defun compete-uct-no-printing
  (black-num-sims black-k black-c white-num-sims white-k white-c)
  (setf *verbose* nil)
  (let ((g (new-gomoku 7 5)))
    (while (not (game-over? g))
      (cond
        ((eq (gomoku-whose-turn g) *black*)
        	(format t "B ")
        	(apply #'do-move! g (uct-rave g black-num-sims (* black-num-sims black-k) black-c)))
        (t
        	(format t "W ")
        	(apply #'do-move! g (uct-rave g white-num-sims (* white-num-sims white-k) white-c)))))
    (format t "~%~A~%" g)))

(defun compete-i-mcrave
  (black-num-sims black-k)
  (setf *verbose* t)
  (let ((g (new-gomoku 7 5)))
    (while (not (game-over? g))
      (cond
        ((eq (gomoku-whose-turn g) *black*)
        	(format t "BLACK'S TURN!~%")
        	(format t "~A~%"
               		(apply #'do-move! g (mc-rave g black-num-sims (* black-num-sims black-k)))))
        (t
        	(format t "YOUR TURN! enter your move:~%")
        	(format t "~A~%"
               		(apply #'do-move! g (read))))))))

(defun compete-i-uctrave
  (black-num-sims black-k black-c)
  (setf *verbose* t)
  (let ((g (new-gomoku 7 5)))
    (while (not (game-over? g))
      (cond
        ((eq (gomoku-whose-turn g) *black*)
        	(format t "BLACK'S TURN!~%")
        	(format t "~A~%"
               		(apply #'do-move! g (uct-rave g black-num-sims (* black-num-sims black-k) black-c))))
        (t
        	(format t "YOUR TURN! enter your move:~%")
        	(format t "~A~%"
               		(apply #'do-move! g (read))))))))

(defun compete-mcuct
  (black-num-sims black-c white-num-sims white-c)
  (setf *verbose* t)
  (let ((g (new-gomoku 7 5)))
    (while (not (game-over? g))
      (cond
        ((eq (gomoku-whose-turn g) *black*)
        	(format t "BLACK'S TURN!~%")
        	(format t "~A~%"
               		(apply #'do-move! g (uct-search g black-num-sims black-c))))
        (t
        	(format t "WHITE'S TURN!~%")
        	(format t "~A~%"
               		(apply #'do-move! g (uct-search g white-num-sims white-c))))))))

(defun compete-i-mcuct
  (black-num-sims black-c)
  (setf *verbose* t)
  (let ((g (new-gomoku 7 5)))
    (while (not (game-over? g))
      (cond
        ((eq (gomoku-whose-turn g) *black*)
        	(format t "BLACK'S TURN!~%")
        	(format t "~A~%"
               		(apply #'do-move! g (uct-search g black-num-sims black-c))))
        (t
        	(format t "YOUR TURN! enter move:~%")
        	(format t "~A~%"
               		(apply #'do-move! g (read))))))))

(defun compete-mcrave-benchmark
  (black-num-sims black-k)
  (setf *verbose* nil)
  (let ((g (new-gomoku 7 5)))
    (while (not (game-over? g))
      (cond
        ((eq (gomoku-whose-turn g) *black*)
        	(apply #'do-move! g (mc-rave g black-num-sims (* black-num-sims black-k))))
        (t
        	(apply #'do-move! g (compute-move g 1 *white*)))))
    (who-wins? g)))

(defun compete-benchmark-mcrave
  (white-num-sims white-k)
  (setf *verbose* nil)
  (let ((g (new-gomoku 7 5)))
    (while (not (game-over? g))
      (cond
        ((eq (gomoku-whose-turn g) *black*)
        	(apply #'do-move! g (compute-move g 1 *black*)))
        (t
        	(apply #'do-move! g (mc-rave g white-num-sims (* white-num-sims white-k))))))
    (who-wins? g)))

(defun compete-uctrave-benchmark
  (black-num-sims black-k black-c)
  (setf *verbose* nil)
  (let ((g (new-gomoku 7 5)))
    (while (not (game-over? g))
      (cond
        ((eq (gomoku-whose-turn g) *black*)
        	(apply #'do-move! g (uct-rave g black-num-sims (* black-num-sims black-k) black-c)))
        (t
        	(apply #'do-move! g (compute-move g 1 *white*)))))
    (who-wins? g)))

(defun compete-benchmark-uctrave
  (white-num-sims white-k white-c)
  (setf *verbose* nil)
  (let ((g (new-gomoku 7 5)))
    (while (not (game-over? g))
      (cond
        ((eq (gomoku-whose-turn g) *black*)
        	(apply #'do-move! g (compute-move g 1 *black*)))
        (t
        	(apply #'do-move! g (uct-rave g white-num-sims (* white-num-sims white-k) white-c)))))
    (who-wins? g)))
