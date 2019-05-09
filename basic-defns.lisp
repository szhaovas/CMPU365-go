;; ===========================================
;;  CMPU-365, Spring 2019
;;  Basic Definitions for Othello and MCTS
;; ===========================================

;;  To ensure that the compiler efficiently handles tail recursion

(setq compiler:tail-call-self-merge-switch t)
(setq compiler:tail-call-non-self-merge-switch t)

;;  To avoid annoying garbage-collection messages

(setf *global-gc-behavior* :auto)

;;  The list of files for the OTHELLO implementation:

(defparameter *gomoku-files*
  (list
   "basic-defns"
   "gomoku"))

;;  MAKER
;; ------------------------------------
;;  Compiles and loads all files for the Othello/MCTS implementation

(defun maker
  ()
  (dolist (file *gomoku-files*)
          (compile-file file)
          (load file))
  (compile-file "mcts")
  (load "mcts")
  (compile-file "mc-rave")
  (load "mc-rave")
  (compile-file "test")
  (load "test")
  (compile-file "data")
  (load "data"))
