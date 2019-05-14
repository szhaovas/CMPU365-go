;;  Some useful constants
;; -----------------------------------

;;  Colors of tokens/blank spaces

(defconstant *black* -1)
(defconstant *white* 1)
(defconstant *blank* 0)

;;  How tokens of each color are displayed

(defconstant *black-show* 'B)
(defconstant *white-show* 'W)
(defconstant *blank-show* '_)

;;  WIN-LOSS VALUES

(defconstant *win-value* 10000000)
(defconstant *draw-value* 0)
(defconstant *loss-value* -10000000)

;;  NEGATIVE and POSITIVE INFINITY
;; ----------------------------------------

(defconstant *neg-inf* -10000000000000)
(defconstant *pos-inf*  10000000000000)


;; Denote game is over and result is DRAW
(defconstant *draw* 'DRAW)


;;  A POSN is just a number from 0 to N - 1, that refers to one of the
;;  squares on the N-by-N gomoku game board.  The following macros
;;  convert between the POSN and ROW/COL representations.

;;  POSN->ROW
;; ---------------------------------------------------------------------
;;  INPUT:   POSN (i.e., an integer from 0 to N * N - 1)
;;           N is the dimension of board
;;  OUTPUT:  The corresponding ROW (an integer from 0 to N - 1)

(defmacro posn->row (posn n) `(floor ,posn, n))

;;  POSN->COL
;; ---------------------------------------------------------------
;;  INPUT:   POSN (i.e., an integer from 0 to N * N - 1)
;;           N is the dimension of board
;;  OUTPUT:  The corresponding COLUMN (an integer from 0 to N - 1)

(defmacro posn->col (posn n) `(mod ,posn, n))

;;  ROW-COL->POSN
;; ------------------------------------------
;;  INPUTS:  ROW, COL, two integers each between 0 and N - 1,
;;           N is the dimension of board
;;  OUTPUT:  The corresponding POSN (an integer between 0 and N * N - 1)

(defmacro row-col->posn (row col n) `(+ (* ,n ,row) ,col))

;;  IF-BLACK-TURN
;; -------------------------------------------------------
;;  INPUTS:  G, an GOMOKU struct
;;           THEN, ELSE, any two Lisp expressions
;;  OUTPUT:  If it's black's turn, then evaluate THEN;
;;           Otherwise, evaluate ELSE.

(defmacro if-black-turn (g then else)
  `(if (eq (gomoku-whose-turn ,g) ,*black*)
       ,then ,else))

;;  IF-BLACK
;; --------------------------------------------------------
;;  INPUTS:  PLR, either *BLACK* or *WHITE*
;;           THEN, ELSE, any Lisp expressions
;;  OUTPUT:  If PLR is *BLACK* then evaluate THEN;
;;           Otherwise, evaluate ELSE.

(defmacro if-black (plr then else)
  `(if (eq ,plr ,*black*) ,then ,else))

;;  TOGGLE-PLAYER!
;; ---------------------------------------------------------
;;  INPUT:  GAME, an GOMOKU struct
;;  OUTPUT: The player whose turn it now is (either *BLACK* or *WHITE*)
;;  SIDE EFFECT:  Destructively modifies the game to toggle whose turn it is.

(defmacro toggle-player! (game)
  `(setf (gomoku-whose-turn ,game)
     (if-black (gomoku-whose-turn ,game) ,*white* ,*black*)))

;;  PLACE-TOKEN-AT-POSN
;; --------------------------------------------------------------
;;  INPUTS:  BOARD, an two dimensional array of dimension N
;;           TOKEN, either *BLACK* or *WHITE*
;;           POSN, an integer
;;  OUTPUT:  Doesn't matter
;;  SIDE EFFECT:  Destructively modifies BOARD by inserting TOKEN
;;                at the position determined by POSN

(defmacro place-token-at-posn
    (board token posn n)
    `(setf (aref ,board (posn->row ,posn, n) (posn->col ,posn, n)) ,token))

;;  PLACE-TOKEN
;; -------------------------------------------------------------------
;;  INPUTS:  GAME, an GOMOKU struct
;;           BORED, an two dimensional array of dimension N
;;           PLR, either *BLACK* or *WHITE*
;;           ROW, COL, integers between 0 and N - 1
;;  OUTPUT:  Doesn't matter
;;  SIDE EFFECT:  Places TOKEN on BORED at specified ROW/COL.
;;    Also updates the (N*N)-bit vector for the appropriate player
;;    (see the STATE struct)

(defmacro place-token
    (game bored plr row col n)
      `(progn (setf (aref ,bored ,row ,col) ,plr)
          (if-black ,plr (incf (gomoku-black-pieces ,game)
                       (ash 1 (row-col->posn ,row ,col, n)))
                (incf (gomoku-white-pieces ,game)
                  (ash 1 (row-col->posn ,row ,col, n))))))

;;  The GOMOKU struct
;; --------------------------------------------------------

(defstruct (gomoku (:print-function print-gomoku))
  ;; BOARD:  an 9-by-9 array of *white*, *black* or *blank*
  (board (make-array '(9 9) :initial-element *blank*))
  ;; WHOSE-TURN:  either *BLACK* or *WHITE*
  (whose-turn *black*)
  ;; NUM-OPEN:  the number of open spaces on the BOARD (always <= N * N)
  (num-open 81)
  ;; WHITE-PIECES/BLACK-PIECES:  (N*N)-bit integers
  (white-pieces 0)
  (black-pieces 0)
  ;; win policy, i.e., how many in a row is a win
  ;; default is 5
  (win-lineup-num 5)
  (is-legal? `is-legal-default?)
  ;; for easier game-over? check and undo-move!
  ;; stores a list
  ;; (row col)
  (history (make-array 0 :adjustable T :fill-pointer 0))
)

(defun init-gomoku
    (dim win-lineup-num)
    (make-gomoku
        :board (make-array '(dim dim) :initial-element *blank*)
        :num-open (* dim dim)
        :win-lineup-num win-lineup-num
    ))

;;  PRINT-GOMOKU
;; --------------------------------------------------
;;  INPUTS:  G, an GOMOKU struct
;;           STR, output stream (or T)
;;           DEPTH, ignored
;;  OUTPUT:  None
;;  SIDE EFFECT:  Displays the GOMOKU game

(defun print-gomoku
    (g str d)
  (declare (ignore d))
  (let* (
    (bored (gomoku-board g))
    (dim (first (array-dimensions bored)))
    )
    (format str "~%  |")
    (dotimes (i dim) (format str " ~A~A" (if (< i 10) " " "") i))
    (format str "~%  ")
    (dotimes (i (+ 2 (* 3 dim)
      (max 0 (* 2 (- dim 10))))) (format str "-"))
    (format str "~%")
    (dotimes (r dim)
      (format str "~A~A| " (if (< r 10) " " "") r)
      (dotimes (c dim)
        (let ((token (aref bored r c)))
          (format str " ~A "
            (cond
              ((eq token *black*) *black-show*)
              ((eq token *white*) *white-show*)
              (t *blank-show*))
            )))
      (format str "~%"))
    (format str "~%")
    ; (format str "Next Player: ~A~%"
    ;     (if-black-turn g *black-show* *white-show*))
    ; (format str "White Pieces: ~A~%" (gomoku-white-pieces g))
    ; (format str "Black Pieces: ~A~%" (gomoku-black-pieces g))
    ; (format str "Number Open: ~A~%" (gomoku-num-open g))
    ))

;;  COPY-ARRAY
;; -------------------------------------------------
;;  INPUT:   HARRY, a 2-dimensional array
;;  OUTPUT:  A copy of HARRY

(defun copy-array
    (harry)
  (let* ((dims (array-dimensions harry))
     (kopy (make-array dims)))
    (dotimes (r (first dims))
      (dotimes (c (second dims))
    (setf (aref kopy r c) (aref harry r c))))
    kopy))

;;  COPY-HISTORY
;; -------------------------------------------------
;;  INPUT:   HISTORY, a 1-dimensional array
;;  OUTPUT:  A copy of HISTORY

(defun copy-history
    (history)
  (let* ((n (length history))
     (kopy (make-array 0 :adjustable T :fill-pointer 0)))
    (dotimes (r n)
        (vector-push-extend (copy-list (aref history r)) kopy))
    kopy))

;;  COPY-GAME
;; ------------------------------------------
;;  INPUT:   GAME, an GOMOKU struct
;;  OUTPUT:  A copy of GAME

(defmethod copy-game
    ((game gomoku))
  (make-gomoku :board (copy-array (gomoku-board game))
        :whose-turn (gomoku-whose-turn game)
        :num-open (gomoku-num-open game)
        :history (copy-history (gomoku-history game))
        :win-lineup-num (gomoku-win-lineup-num game)
        :is-legal? (gomoku-is-legal? game)
        :white-pieces (gomoku-white-pieces game)
        :black-pieces (gomoku-black-pieces game)))

;;  MAKE-HASH-KEY-FROM-GAME
;; --------------------------------------------
;;  INPUT:  GAME, an GOMOKU struct
;;  OUTPUT:  A list of the form (WHITE-PIECES BLACK-PIECES WHOSE-TURN)
;;    where the contents are as described in the STATE struct

(defmethod make-hash-key-from-game
    ((game gomoku))
  (list (gomoku-white-pieces game)
    (gomoku-black-pieces game)
    (gomoku-whose-turn game)))


;;  *DIRNS*
;; --------------------------------------------------------
;;  An 4-by-2 array representing the 4 different directions
;;  one could travel from a given square on the board.

(defconstant *dirns* (make-array '(4 2)
                 :initial-contents
                 '((1 1) (1 0) (0 1) (-1 1))))

(defconstant *all-dirns*
  '((1 1) (1 0) (1 -1)
           (0 1) (0 -1)
           (-1 1) (-1 0) (-1 -1)))

;;  LEGAL-MOVES
;; -------------------------------------------
;;  INPUT:  GAME, an GOMOKU struct
;;  OUTPUT:  A VECTOR of the legal moves available to the current player.
;;  Note:  If no "legal" moves, then returns empty vector.

;;  LEGAL-MOVES
;; -------------------------------------------
;;  INPUT:  GAME, an GOMOKU struct
;;  OUTPUT:  A VECTOR of the legal moves available to the current player.
;;  Note:  If no "legal" moves, then returns empty vector.

(defun is-legal-coord
  (x y n)
  (not (or (< x 0) (< y 0) (>= x n) (>= y n))))

(defmethod legal-moves
    ((game gomoku))
  (let* (
    (moves nil)
    (history (gomoku-history game))
    (num-moves 0)
    (num-cand (length history))
    (N (first (array-dimensions (gomoku-board game))))
    (collected (make-hash-table))
    (is-legal? (gomoku-is-legal? game))
    )
    (if (= 0 num-cand)
      (return-from legal-moves
        (vector (list (floor (/ (- N 1) 2))
          (floor (/ (- N 1) 2))))))

    (dotimes (i num-cand)
      (let* (
          (cand-move (aref history i))
          (ori-x (first cand-move))
          (ori-y (second cand-move))
          )
        (loop for m in (list 1 2) do
          (dotimes (j 5)
            (let* (
              (dx (- j 2))
              (x (+ dx ori-x))
              )
              (dotimes (k 5)
                (let* (
                  (dy (- k 2))
                  (y (+ dy ori-y))
                  (key (row-col->posn x y n))
                  )
                  (cond
                    ((and (is-legal-coord x y N)
                      (funcall is-legal? game x y)
                      (null (gethash key collected))
                      )
                      (setf (gethash key collected) (list x y))
                      (push (list x y) moves)
                      (incf num-moves)
                    )
                  )
                )
              )
            )
          )
        )
        )
      )
    (if (> num-moves 0)
    (make-array num-moves :initial-contents moves)
      ;; if no legal moves, then returns empty vector.
      (vector))))

;;  IS-LEGAL-DEFUALT?
;; -----------------------------------
;;  INPUTS:  GAME, an GOMOKU struct
;;           ROW, COL, integers
;;  OUTPUT:  T if this is a legal move without extra constraints
(defmethod is-legal-default?
    ((game gomoku) row col)
    (eq *blank* (aref (gomoku-board game) row col)))

;; UNDO-MOVE!
;; -------------------------------------
;;  INPUTS:  GAME, an GOMOKU struct
;;  OUTPUT:  The modified GAME:
;;           affected variables:
;;           board, history, num-open, player-pieces, whose-turn

(defmethod undo-move!
    ((game gomoku))
    (let* (
        (n (first (array-dimensions (gomoku-board game))))
        ;; pop last state from history
        (last-move (vector-pop (gomoku-history game)))
        (r (first last-move))
        (c (second last-move))
        (player-pieces (ash 1 (row-col->posn r c n)))
        )
    ;; num-open += 1
    (incf (gomoku-num-open game))
    ;; toggle whose-turn
    (toggle-player! game)
    ;; set player-pieces
    (if (eq *black* (gomoku-whose-turn game))
        (decf (gomoku-black-pieces game) player-pieces)
        (decf (gomoku-white-pieces game) player-pieces))
    ;; set board
    (setf (aref (gomoku-board game) r c) *blank*)
    ))

;;  DO-MOVE!
;; -------------------------------------
;;  INPUTS:  GAME, an GOMOKU struct
;;           ROW, COL, two integers (between 0 and 7)
;;  OUTPUT:  The modified GAME

(defmethod do-move!
    ((game gomoku) row col)
    (let* (
        (bored (gomoku-board game))
        (player (gomoku-whose-turn game))
        ;; player-pieces before this move
        (player-pieces
            (if (eq player *black*)
                (gomoku-black-pieces game)
                (gomoku-white-pieces game)))
        (n (first (array-dimensions bored)))
    )
    ;; Place token on the board at (ROW COL)
    (place-token game bored player row col n)
    (toggle-player! game)
    (decf (gomoku-num-open game))
    ;; record move
    (vector-push-extend
        (list row col)
        (gomoku-history game))
    game
    )
)

;;  GAME-OVER?
;; --------------------------------------------------------------------------
;;  INPUT:  GAME, an GOMOKU struct
;;  OUTPUT: T if game is over

(defmethod game-over?
    ((game gomoku))
    (not (null (who-wins? game))))


;;  WHO-WINS?
;; --------------------------------------------------------------------------
;;  INPUT:  GAME, an GOMOKU struct
;;  OUTPUT:  NIL if no one wins and game is not over (or (zerop (gomoku-num-open game))
;;           *BLACK* if black wins
;;           *WHITE* if white wins
;;           *DRAW* if game is over and no one wins

(defmethod who-wins?
    ((game gomoku))
    (let* (
        (board (gomoku-board game))
        (history (gomoku-history game))
        (last-move
            (if (> (length history) 0)
                (aref history (- (length history) 1))
                nil))
        (last-player (get-last-player game))
        (win-num (gomoku-win-lineup-num game))
        (N (first (array-dimensions board)))
        )
        ;; game has not started
        (if (not (null last-move))
            ;; game started, check if last move makes consecutive
            (dotimes (i 4)
                (if (>=
                        (count-consecutive-in-row
                            (list (aref *dirns* i 0) (aref *dirns* i 1))
                            board
                            last-move
                            (get-last-player game)
                            N)
                        win-num
                    )
                    (return-from who-wins? last-player))
            ))
        ;; if no more place to place stone, it is a draw
        ;; else game continues
        (if (= 0 (gomoku-num-open game)) *draw* nil)
    )
)

;; BOARD-DIM
;; -------------------------------------------
;; INPUT: BOARD, a 2-dimensional gomoku board (N x N)
;; OUTPUT: board's dimension

(defun board-dim (board) (first (array-dimensions board)))

;;  REVERSE-DIR
;; --------------------------------------------------------------------------
;;  INPUT:  DIR, direction a list of two
;;  OUTPUT: a list of two, representing reversed direction

(defun reverse-dir (dir) (list (* -1 (first dir)) (* -1 (second dir))))

;;  GET-LAST-PLAYER
;; --------------------------------------------------------------------------
;;  INPUT:  GAME, an GOMOKU struct
;;  OUTPUT: the last turn player

(defmethod get-last-player
    ((game gomoku))
    (if (eq (gomoku-whose-turn game) *black*)
        *white*
        *black*))

;; COUNT-CONSECUTIVE-IN-ROW
;; --------------------------------------------------------------------------
;; INPUT: DIR, a list of integers denoting direction
;;        BOARD, a two-dimensional array
;;        last-move, a list of integers denoting row and column and player
;;        PLAYER, last player
;;        N, board dimension
;; OUTPUT: T if player achieves WIN-NUM-in-a-row, otherwise nil.

(defun count-consecutive-in-row
    (dir board last-move player N)
    (let (
        (dx (first dir))
        (dy (second dir))
        (cur-x (first last-move))
        (cur-y (second last-move))
        )
        (-
            (+ (count-consecutive-in-dir
                    board
                    N
                    dir
                    player
                    cur-x
                    cur-y
                    )
                (count-consecutive-in-dir
                    board
                    N
                    (reverse-dir dir)
                    player
                    cur-x
                    cur-y
                    ))
            1))
    )

;; COUNT-CONSECUTIVE-IN-DIR
;; --------------------------------------------------------------------------
;; INPUT: BOARD, a two-dimensional array
;;        DIR, direction
;;        PLAYER, whose stone to check
;;        N, board dimension
;;        CUR-X, current x
;;        CUR-Y, current y
;; OUTPUT: an integer, number of consecutive pieces in on direction

(defun count-consecutive-in-dir
    (board N dir player cur-x cur-y)
    (let* (
        (dx (first dir))
        (dy (second dir))
        (nx (+ dx cur-x))
        (ny (+ dy cur-y))
        )
        (cond
            ;; if position is not valid
            ((or (>= cur-x N) (>= cur-y N) (< cur-x 0) (< cur-y 0))
                0
            )
            ;; if not same piece color
            ((not (eq player (aref board cur-x cur-y))) 0)
            ;; else
            (T (+ 1
                (count-consecutive-in-dir
                    board
                    N
                    dir
                    player
                    nx
                    ny)))
        )
    ))

;;  RANDOM-MOVE
;; ------------------------------------------
;;  INPUT:  GAME, an GOMOKU struct
;;  OUTPUT:  One of the legal moves available to the current
;;   player, chosen randomly.

(defmethod random-move
    ((game gomoku))
  (let* ((moves (legal-moves game)))
    (svref moves (random (length moves)))))

;;  DO-RANDOM-MOVE!
;; ------------------------------------------------
;;  INPUT:   GAME, an GOMOKU struct
;;  OUTPUT:  The modified game
;;  SIDE EFFECT:  Destructively modifies GAME by doing one of the
;;   legal moves available to the current player, chosen randomly.

(defmethod do-random-move!
    ((game gomoku))
  (let ((move (random-move game)))
    (apply #'do-move! game move)))

;;  DEFAULT-POLICY
;; ---------------------------------------------------------------------------
;;  INPUT:  GAME, an GOMOKU struct
;;  OUTPUT: The result (from black's perspective) of playing out the
;;    current game using randomly selected moves.  The amount of the
;;    win/loss is reported by the SQUARE-ROOT of the absolute difference
;;    in the number of tokens for the two players.  For example, if the
;;    game ends with WHITE having 25 tokens and BLACK having 31 tokens,
;;    then black wins by 6 tokens, and the output is approximately 2.45.

(defmethod default-policy
  ((game gomoku))
  ;; Do random moves until the game is over
  (let ((res nil))
    (loop while (null res) do
        (do-random-move! game)
        (setf res (who-wins? game)))
    res
    ))


;; ==============================================

(defconstant *win-shape-score* 100000)
(defconstant *free-space-val* 0.01)

(defun find-shape-from-coord-for-helper
  (board x y color dir)
  (let* (
    (N (board-dim board))
    (rev-dir (reverse-dir dir))
    (c1 (count-consecutive-in-dir
          board N
          dir
          color x y))
    (c2 (count-consecutive-in-dir
          board N
          rev-dir
          color x y))
    (sx (+ x (* (- c1 1) (first dir))))
    (sy (+ y (* (- c1 1) (second dir))))
    (consecutive (- (+ c1 c2) 1))
    )
  (if (> consecutive 1)
    (list (list sx sy) consecutive dir)
    nil)
  )
)

(defun find-shapes-from-coord-for
  (board x y color)
  (let (
    (res (list))
    (tmp nil)
    )
    (dolist (dir (list '(0 1) '(1 0) '(1 1) '(1 -1)))
      (setf tmp (find-shape-from-coord-for-helper
            board x y color dir))
      (if (not (null tmp))
        (setf res
          (cons
            tmp
            res)))
    )
    res
  ))

(defun eval-move
  (kopy-board x y color)
    (setf (aref kopy-board x y) color)
    (let (
      (shapes (find-shapes-from-coord-for kopy-board x y color))
      (threats 0)
      )
      (setf (aref kopy-board x y) *blank*)
      (dolist (shape shapes)
        (let* (
          (consecutive (second shape))
          (help-vals (count-open-end-and-free-space kopy-board shape))
          (open-ends (first help-vals))
          (f1 (second help-vals))
          (f2 (third help-vals))
          (fs (+ f2 f1))
          )
          (cond
            ((>= consecutive 5) (return-from eval-move *pos-inf*))
            ((= 4 consecutive)
              (cond
                ((= 2 open-ends) (incf threats 10))
                ((= 1 open-ends) (incf threats))
              )
            )
            ((= 3 consecutive)
              (if (= 2 open-ends) (incf threats))
            )
          )
        )
      )
      threats
    ))

(defun count-free-space-from
  (board x y n color dx dy)
  (if (not (is-legal-coord x y n))
    0
    (let ((stone (aref board x y)))
      (cond
        ;; found opponent stone
        ((and (not (eq *blank* stone))
            (not (eq stone color)))
          0)
        ;; blank or own stone
        (T (+ 1 (count-free-space-from
          board
          (+ x dx)
          (+ y dy)
          n
          color
          dx
          dy)))
      )
    )))

(defun is-blank
  (board x y n)
  (and (is-legal-coord x y n)
    (eq *blank* (aref board x y))))

(defun count-open-end-and-free-space
  (board shape)
  (let* (
    (N (board-dim board))
    (src (first shape))
    (color (aref board (first src) (second src)))
    (len (second shape))
    (dir (third shape))
    (rev-dir (reverse-dir dir))
    ;; first potential open-end
    (h1 (list
      (+ (first src) (* len (first rev-dir)))
      (+ (second src) (* len (second rev-dir)))
      ))
    ;; second potential open-end
    (h2 (list (+ (first src) (first dir)) (+ (second src) (second dir))))
    ;; num open end
    (open-end 0)
    )
    (if (is-blank board (first h1) (second h1) N) (incf open-end))
    (if (is-blank board (first h2) (second h2) N) (incf open-end))
    (list
      open-end
      (count-free-space-from board (first h1) (second h1) N color (first rev-dir) (second rev-dir))
      (count-free-space-from board (first h2) (second h2) N color (first dir) (second dir))
      )
  ))

(defun fs-weighted
  (score fs)
  (+ score (* fs *free-space-val*)))

(defun eval-shape-score
  (board shape is-my-turn)
  (let* (
    (consecutive (second shape))
    (help-vals (count-open-end-and-free-space board shape))
    (open-ends (first help-vals))
    (f1 (second help-vals))
    (f2 (third help-vals))
    (fs (+ f2 f1))
    )
    (if (>= consecutive 5) (return-from eval-shape-score *win-shape-score*))
    (if (= 0 open-ends) (return-from eval-shape-score 0))
    (cond
      ((= 4 consecutive)
        (if is-my-turn
          *win-shape-score*
          (if (= 2 open-ends)
            (fs-weighted 1000 fs)
            (fs-weighted 300 fs))
        )
      )
      ((= 3 consecutive)
        (cond
          ((= 2 open-ends)
            (if (and (> fs 2) is-my-turn)
              (fs-weighted 500 fs)
              (fs-weighted 100 fs)
              )
          )
          (T
            ;; dead end
            (if (< fs 2) (return-from eval-shape-score 0))
            (if is-my-turn
              (fs-weighted 15 fs)
              (fs-weighted 10 fs))
          )
        )
      )
      ((= 2 consecutive)
        (cond
          ((= 2 open-ends)
            ;; dead end
            (if (< fs 3) (return-from eval-shape-score 0))
            (if is-my-turn
              (fs-weighted 25 fs)
              (fs-weighted 5 fs))
          )
          (T
            ;; dead end
            (if (<= fs 2) (return-from eval-shape-score 0))
            (if is-my-turn
              (fs-weighted 0.5 fs)
              (fs-weighted 0.1 fs))
            )
          )
        )
      (T 0)
    )
  ))

(defun find-line-shapes-for
    (board player is-horiz)
    (let* (
        (N (board-dim board))
        (shapes (list))
        (consecutive 0)
        (dir (if is-horiz '(0 1) '(1 0)))
        (last-coord nil)
        )
    (dotimes (i N)
        (dotimes (j N)
            (let*
                (
                    (r (if is-horiz i j))
                    (c (if is-horiz j i))
                    (stone (aref board r c))
                )
                (cond
                    ;; if found target-player stone
                    ((eq stone player)
                        (incf consecutive)
                    )
                    ;; not finding any consecutive
                    ((= 0 consecutive))
                    ;; it is a shape
                    (T
                      ;; only record shape with more than one consecutive
                      (if (> consecutive 1)
                        (setf shapes
                          (cons
                            (list last-coord consecutive dir)
                            shapes)
                          ))
                      (setf consecutive 0)
                    )
                )
                (setf last-coord (list r c))
            )
        )
        ; when finishing searching a row
        (cond
            ((> consecutive 0)
                ;; only record shape with more than one consecutive
                (if (> consecutive 1)
                  (setf shapes
                    (cons
                      (list last-coord consecutive dir)
                      shapes)
                    ))
                (setf consecutive 0)
            )
        )
    )
    shapes
    ))

(defun find-diagonal-shape-for
    (board player left-to-right)
    (let* (
        (dir (if left-to-right '(1 1) '(1 -1)))
        (N (board-dim board))
        (shapes (list))
        (consecutive 0)
        (last-coor nil)
        ;; number of diagonals
        (num-line (- (* 2 N) 1))
        (dx (first dir))
        (dy (second dir))
        )
    (dotimes (i num-line)
        (let* (
            (start-row
                (if left-to-right
                    (if (< i N) (- N (+ 1 i)) 0)
                    (if (< i N) 0 (- (+ 1 i) N))))
            (start-col
                (if left-to-right
                    (if (< i N) 0 (- (+ i 1) N))
                    (if (< i N) i (- N 1))))
            (num-elm (if (>= i N) (- (* 2 N) (+ 1 i)) (+ 1 i)))
            )
            (dotimes (j num-elm)
                (let*
                    (
                        (r (+ start-row (* j dx)))
                        (c (+ start-col (* j dy)))
                        (stone (aref board r c))
                    )
                    (cond
                        ;; if found target-player stone
                        ((eq stone player)
                            (incf consecutive)
                        )
                        ;; not finding any consecutive
                        ((= 0 consecutive))
                        ;; it is a shape
                        (T
                          ;; only record shape with more than one consecutive
                          (if (> consecutive 1)
                            (setf shapes
                              (cons
                                (list last-coord consecutive dir)
                                shapes)
                              ))
                          (setf consecutive 0)
                        )
                    )
                    (setf last-coord (list r c))
                )
            )
        )
        ; when finishing searching a diagonal
        (cond
            ((> consecutive 0)
                ;; only record shape with more than one consecutive
                (if (> consecutive 1)
                  (setf shapes
                    (cons
                      (list last-coord consecutive dir)
                      shapes)
                    ))
                (setf consecutive 0)
            )
        )
    )
    shapes
    ))

(defun eval-break-threats
  (kopy-board x y color)
  (let ((val (eval-move kopy-board x y color)))
    (cond
      ((= val *pos-inf*) (/ *win-value* 2))
      ((> val 1) 100)
      (T 0)
    )))

(defmethod find-all-shapes-for
  (board player)
  (nconc
    (find-line-shapes-for board player T)
    (find-line-shapes-for board player nil)
    (find-diagonal-shape-for board player T)
    (find-diagonal-shape-for board player nil)
  ))

(defun is-singleton-helper
  (board x y player dirs)
  (cond
    ((null dirs) T)
    (T
      (let* (
        (dir (first dirs))
        (nx (+ x (first dir)))
        (ny (+ y (second dir)))
        (N (board-dim board))
        )
        (and
          (or (not (is-legal-coord nx ny N))
            (not (eq (aref board nx ny) player))
          )
          (is-singleton-helper
            board
            x
            y
            player
            (rest dirs))
          )
      )
    ))
  )

(defun find-singletons-for
  (board player)
  (let* (
    (N (board-dim board))
    (res (list))
    )
    (dotimes (i N)
      (dotimes (j N)
        (let ((stone (aref board i j)))
          (if (and
            (eq stone player)
            (is-singleton-helper board i j player *all-dirns*)
            )
            (setf res (cons (list i j) res))
          )
        )
      )
    )
    res
  ))

(defun eval-singleton-score-for-player-helper
  (board x y whose-turn player dx dy)
  (let* (
    (rdx (* -1 dx))
    (rdy (* -1 dy))
    (N (board-dim board))
    (f1 (- (count-free-space-from
          board x y
          N player
          dx dy) 1)
    )
    (f2 (- (count-free-space-from
          board x y
          N player
          rdx rdy) 1)
    )
    (fs (+ f1 f2))
    (open-ends 0)
    )
    (if (is-blank board (+ x dx) (+ y dy) N) (incf open-ends))
    (if (is-blank board (+ x rdx) (+ y rdy) N) (incf open-ends))
    (if (< fs 4) (return-from eval-singleton-score-for-player-helper 0))
    (cond
      ((= 2 open-ends)
        (fs-weighted 0.7 fs)
      )
      ((= 1 open-ends)
        (fs-weighted 0.02 fs)
      )
      (T 0)
    )
  ))

(defun eval-singleton-score-for-player
  (board x y whose-turn player)
  (let ((score 0))
    (dotimes (i 4)
      (incf score
        (eval-singleton-score-for-player-helper
          board
          x y
          whose-turn player
          (aref *dirns* i 0) (aref *dirns* i 1)
        )
      )
    )
    score
  ))

(defmethod eval-for-player
  ((game gomoku) player)
  (let* (
    (score 0)
    (board (gomoku-board game))
    (kopy-board (copy-array board))
    (N (board-dim board))
    (shapes (find-all-shapes-for board player))
    (singletons (find-singletons-for board player))
    (board (gomoku-board game))
    (whose-turn (gomoku-whose-turn game))
    (is-my-turn (eq player whose-turn))
    (moves (legal-moves game))
    )
    (loop for s in shapes do
      (incf score (eval-shape-score board s is-my-turn)))
    (loop for s in singletons do
      (incf score
        (eval-singleton-score-for-player
          board
          (first s) (second s)
          whose-turn player)))
    (dotimes (i (length moves))
      (incf score
        (eval-break-threats
          kopy-board
            (first (aref moves i))
            (second (aref moves i))
            player)))
    (min *win-value* score)
  ))

(defmethod eval-func
    ((game gomoku) for-player)
    (let (
        (whose-turn (gomoku-whose-turn game))
        (black-score
          (eval-for-player game *black*)
          )
        (white-score
          (eval-for-player game *white*)
          )
    )
    (if (eq for-player *black*)
        (- black-score white-score)
        (- white-score black-score))
    ))


;;  STATS struct
;; ---------------------------
;;  Stats compiled during minimax search

(defstruct stats
  (num-moves-done 0)
  (num-potential-moves 0))

;;  COMPUTE-MOVE
;; -------------------------------------------------------------
;;  INPUTS:  G, a CHESS struct
;;           CUTOFF-DEPTH, to limit depth of minimax search
;;  OUTPUT:  The best move according to MINIMAX with ALPHA-BETA
;;   pruning, using the static eval func, EVAL-FUNC.  Searches to
;;   a depth of CUTOFF-DEPTH.

(defun compute-move (g cutoff-depth for-player)
  (format t "~%COMPUTE-MOVE (cutoff=~A)~%" cutoff-depth)
  (let* (
    (statty (make-stats))
    (result (compute-max g 0 *neg-inf* *pos-inf* statty cutoff-depth for-player))
    (best-move (first result))
    (alpha (second result))
   )
    (format t "   ROOT NODE ALPHA: ~A~%" alpha)
    (format t "   NUM-MOVES-DONE: ~A, NUM-MOVES-PRUNED: ~A~%"
      (stats-num-moves-done statty)
      (- (stats-num-potential-moves statty) (stats-num-moves-done statty)))
    (format t "   BEST MOVE: ~A~%" best-move)
    ;; return my-move
    best-move))


(defmethod legal-moves-with-heuristic
  ((g gomoku))
  (let* (
    (moves (legal-moves g))

    (kopy-board (copy-array (gomoku-board g)))
    (win-moves (list))
    (defensive-moves (list))
    (whose-turn (gomoku-whose-turn g))
    (next-turn (if (eq whose-turn *black*) *white* *black*))

    (candidates moves)
    )

    (dotimes (i (length moves))
      (if (= *pos-inf*
        (eval-move kopy-board
          (first (aref moves i))
          (second (aref moves i))
          whose-turn))
        (setf win-moves (cons (aref moves i) win-moves))
      ))

    (cond
      ((= 0 (length win-moves))
        (dotimes (i (length moves))
          (if (= *pos-inf*
            (eval-move kopy-board
              (first (aref moves i))
              (second (aref moves i))
              next-turn))
            (setf defensive-moves (cons (aref moves i) defensive-moves))
          ))

        (cond
          ((= 0 (length defensive-moves))
            (let ((moves-list (list)))
              (dotimes (i (length moves))
                (setf moves-list (cons (aref moves i) moves-list)))
              (labels (
                (compare-fun
                  (m1 m2)
                  (let (
                    (v1 0)
                    (v2 0)
                    )
                    (apply #'do-move! g m1)
                    (setf v1 (eval-for-player g whose-turn))
                    (undo-move! g)
                    (apply #'do-move! g m2)
                    (setf v2 (eval-for-player g whose-turn))
                    (undo-move! g)
                    (>= v1 v2)
                  )
                ))
                (setf moves-list (sort moves-list #'compare-fun))
                (setf candidates
                  (make-array (length moves-list)
                            :initial-contents moves-list))
              )
            ))
          (T
            (setf candidates
              (make-array (length defensive-moves)
                            :initial-contents defensive-moves)))))
      (T
        (setf candidates
          (make-array (length win-moves)
                        :initial-contents win-moves)))
    )

    candidates
  ))

;;  COMPUTE-MAX / COMPUTE-MIN
;; ---------------------------------------------------------------
;;  INPUTS:  G, a CHESS struct
;;           CURR-DEPTH, the current depth in the search
;;           ALPHA, BETA, alpha/beta values for this node in search
;;           STATTY, stats struct
;;           CUTOFF-DEPTH, to limit depth of minimax search
;;  OUTPUT:  If CURR-DEPTH is zero, returns best move
;;           Otherwise returns value of this node according
;;           to MINIMAX with ALPHA-BETA pruning.

(defun compute-max (g curr-depth alpha beta statty cutoff-depth for-player)
    (let* (
      (winner (who-wins? g))
      (is-game-over (not (null winner)))
      )
      (cond
        (is-game-over
          (cond
            ((eq for-player winner) (- *win-value* curr-depth))
            ((eq for-player *draw*) *draw-value*)
            (T (+ *loss-value* curr-depth))
            )
          )
         ;; reach cut-off
        ((>= curr-depth cutoff-depth) (eval-func g for-player))
        ;; otherwise
        (T
          (let* (
            (best-move nil)
            (value *neg-inf*)
            (moves (legal-moves-with-heuristic g))
            )
            ; (format T "~A ~A~%" moves (length moves))

            (if (and (= curr-depth 0) (= 1 (length moves)))
              (return-from compute-max (list (aref moves 0) 0)))

            ;; update num of potential moves
            (incf (stats-num-potential-moves statty) (length moves))

            ;; loop through available moves
            (dotimes (i (length moves))
              (let ((move (aref moves i)))
                  ;; do move!
                  (apply #'do-move! g move)

                  ;; increment move actually done stat
                  (incf (stats-num-moves-done statty))

                  ;; record resulting value of chosen move
                  (let ((result (compute-min g (+ curr-depth 1) alpha beta statty cutoff-depth for-player)))
                    (cond
                      ;; case 1: if this move is better
                      ((> result value)
                        (setf value result)
                        (setf best-move move)
                      )
                      ;; else do nothing
                    )
                    ; (format T "~A ~A~%" move result)
                  )
                  ;; recover state
                  (undo-move! g)
                  ;; break
                  (if (>= value beta) (return-from compute-max value))
                  ;; update alpha if current value is larger
                  (setf alpha (max alpha value))
              )
            )
            ;; if curr-depth is 0, return best move and alpha
            ;; else, return value
            (if (= curr-depth 0)
              (list best-move alpha)
              value)
          )
        )
      )
    ))

;;  COMPUTE-MIN
;; -------------------------------------------------------
;;  INPUTS:  G, a CHESS struct
;;           CURR-DEPTH, the depth of this MIN node
;;           ALPHA, BETA, values received from parent MAX node
;;           STATTY, a stats struct
;;           CUTOFF-DEPTH, to limit depth of minimax search
;;  OUTPUT:  The value of this MIN node according to rules
;;           of MINIMAX with ALPHA-BETA pruning

(defun compute-min (g curr-depth alpha beta statty cutoff-depth for-player)
  (let* (
      (winner (who-wins? g))
      (is-game-over (not (null winner)))
      )
      (cond
        (is-game-over
          (cond
            ((eq for-player winner) (- *win-value* curr-depth))
            ((eq for-player *draw*) *draw-value*)
            (T (+ *loss-value* curr-depth))
            )
          )
         ;; reach cut-off
        ((>= curr-depth cutoff-depth)
          ; (format T "~A ~A~%" (eval-func g for-player) curr-depth)
          (eval-func g for-player))
        ;; otherwise
        (T
          (let (
            (value *pos-inf*)
            (moves (legal-moves-with-heuristic g))
            )
            ;; update num of potential moves
            (incf (stats-num-potential-moves statty) (length moves))

            ;; loop through available moves
            (dotimes (i (length moves))
                (let ((move (aref moves i)))
                    ;; do move!
                    (apply #'do-move! g move)

                    ;; increment move actually done stat
                    (incf (stats-num-moves-done statty))

                    ;; record resulting value of chosen move
                    (let ((result (compute-max g (+ curr-depth 1) alpha beta statty cutoff-depth for-player)))
                        (setf value (min value result))
                    )
                    ;; recover state
                    (undo-move! g)
                    ;; break
                    (if (<= value alpha) (return-from compute-min value))
                    ;; update beta if current value is smaller
                    (setf beta (min beta value))
                )
            )
            value
          )
        ))))


;;  COMPUTE-DO-AND-SHOW-N-MOVES
;; ------------------------------------------
;;  INPUTS:  G, a CHESS struct
;;           N, a positive integer
;;           CUTOFF-DEPTH, the cutoff depth for minimax
;;  OUTPUT:  don't care
;;  SIDE EFFECT:  Computes, does, and shows the results of N
;;                moves generated using COMPUTE-MOVE.

(defun compute-do-and-show
    (g cutoff-depth)
  ;; Do random moves until the game is over
  (let ((res nil))
    (loop while (null res) do
        (format t "~%~A~%" g)
        (apply #'do-move! g (compute-move g cutoff-depth (gomoku-whose-turn g)))
        (setf res (who-wins? g)))
    (format t "~%~A~%" g)
    (format t "Result: ~A~%"
        (cond
            ((eq res *black*) *black-show*)
            ((eq res *white*) *white-show*)
            (T *draw*)
        ))
    res
    ))

(defun play-against-self
  (dim num-think-ahead-b num-think-ahead-w)
  ;; Do random moves until the game is over
  (let (
    (res nil)
    (c1 (+ 1 (* 2 num-think-ahead-b)))
    (c2 (+ 1 (* 2 num-think-ahead-w)))
    (g (
      make-gomoku
        :num-open (* dim dim)
        :board (make-array (list dim dim) :initial-element *blank*)))
  )
    (loop while (null res) do
        (format t "~%~A~%" g)
        (if (eq *black* (gomoku-whose-turn g))
          (apply #'do-move! g (compute-move g c1 (gomoku-whose-turn g)))
          (apply #'do-move! g (compute-move g c2 (gomoku-whose-turn g))))
        (setf res (who-wins? g)))
    (format t "~%~A~%" g)
    (format t "Result: ~A~%"
        (cond
            ((eq res *black*) *black-show*)
            ((eq res *white*) *white-show*)
            (T *draw*)
        ))
    res
    ))


; (play-against-self 19 0 0)
