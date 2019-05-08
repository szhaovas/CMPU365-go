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

(defmacro posn->row (posn n) `(floor ,posn n))

;;  POSN->COL
;; ---------------------------------------------------------------
;;  INPUT:   POSN (i.e., an integer from 0 to N * N - 1)
;;           N is the dimension of board
;;  OUTPUT:  The corresponding COLUMN (an integer from 0 to N - 1)

(defmacro posn->col (posn n) `(mod ,posn n))

;;  ROW-COL->POSN
;; ------------------------------------------
;;  INPUTS:  ROW, COL, two integers each between 0 and N - 1,
;;           N is the dimension of board
;;  OUTPUT:  The corresponding POSN (an integer between 0 and N * N - 1)

(defmacro row-col->posn (row col n) `(+ (* n ,row) ,col))

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
    (format str "~% |")
    (dotimes (i dim) (format str " ~A" i))
    (format str "~%")
    (dotimes (i (+ 2 (* 2 dim))) (format str "-"))
    (format str "~%")
    (dotimes (r dim)
      (format str "~A| " r)
      (dotimes (c dim)
        (let ((token (aref bored r c)))
          (format str "~A " (cond ((eq token *black*) *black-show*)
                      ((eq token *white*) *white-show*)
                      (t *blank-show*)))))
      (format str "~%"))
    (format str "~%")
    (format str "Whose turn: ~A~%"
        (if-black-turn g *black-show* *white-show*))
    (format str "White Pieces: ~A~%" (gomoku-white-pieces g))
    (format str "Black Pieces: ~A~%" (gomoku-black-pieces g))
    (format str "Number Open: ~A~%" (gomoku-num-open g))
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


;;  LEGAL-MOVES
;; -------------------------------------------
;;  INPUT:  GAME, an GOMOKU struct
;;  OUTPUT:  A VECTOR of the legal moves available to the current player.
;;  Note:  If no "legal" moves, then returns empty vector.

(defmethod legal-moves
    ((game gomoku))
  (let (
    (moves nil)
    (num-moves 0)
    (dim (first (array-dimensions (gomoku-board game))))
    (is-legal? (gomoku-is-legal? game))
    )
    ;; Just look at every square, seeing if it's legal
    (dotimes (r dim)
      (dotimes (c dim)
    (when (funcall is-legal? game r c)
      (push (list r c) moves)
      (incf num-moves))))
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
            (dotimes (i (first (array-dimensions *dirns*)))
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


(defmethod gomoku-shape-score
    (consecutive open-ends is-my-turn)
    (if (>= consecutive 5) *win-value*)
    (if (= open-ends 0) 0)
    (cond
        ((= consecutive 4)
            ;; if my turn, guarantees a win
            (if is-my-turn
                *win-value*
                (cond
                    ;; two ends, almost guarantees a win
                    ((= 2 open-ends)
                        (/ *win-value* 2))
                    ;; one ends, still an immediate threat
                    (T 50)
                    )
                )
            )
        ((= consecutive 3)
            (cond
                ;; two ends,
                ;; if my turn, almost a win; else, high threat
                ((= 2 open-ends)
                    (if is-my-turn
                        (/ *win-value* 8)
                        50)
                    )
                ;; one ends, not so much a threat
                (T
                    (if is-my-turn 25 12))
                )
            )
        ((= consecutive 2)
            (cond
                ;; two ends,
                ;; if my turn, potential threat; else, okay threat
                ((= 2 open-ends)
                    (if is-my-turn
                        25
                        6)
                    )
                ;; one ends, not a threat at all
                (T
                    (if is-my-turn 3 1))
                )
            )
        ;; 1 consecutive
        (T
            (cond
                ;; two ends,
                ;; if my turn, potential threat; else, okay threat
                ((= 2 open-ends)
                    (if is-my-turn
                        3
                        1)
                    )
                ;; one ends, not a threat at all
                (T
                    (if is-my-turn 1.5 0.5))
                )
            )
        )
    )

(defmethod gomoku-shape-score2
    (consecutive open-ends is-my-turn)
    (if (>= consecutive 3) *win-value*)
    (if (= open-ends 0) 0)
    (cond
        ((= consecutive 2)
            ;; if my turn, guarantees a win
            (if is-my-turn
                *win-value*
                (cond
                    ;; two ends, almost guarantees a win
                    ((= 2 open-ends)
                        (/ *win-value* 2))
                    ;; one ends, still an immediate threat
                    (T 50)
                    )
                )
            )
        ;; 1 consecutive
        (T
            (cond
                ;; two ends,
                ;; if my turn, potential threat; else, okay threat
                ((= 2 open-ends)
                    (if is-my-turn
                        3
                        1)
                    )
                ;; one ends, not a threat at all
                (T
                    (if is-my-turn 1.5 0.5))
                )
            )
        )
    )

(defmethod eval-line-for
    ((game gomoku) player is-horiz)
    (let* (
        (board (gomoku-board game))
        (N (board-dim board))
        (is-my-turn (eq player (gomoku-whose-turn game)))
        (score 0)
        (consecutive 0)
        (open-ends 0)
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
                    ;; else if blank and already found player stones
                    ((and (> consecutive 0) (eq stone *blank*))
                        (incf open-ends)
                        (incf score (gomoku-shape-score consecutive open-ends is-my-turn))
                        (setf consecutive 0)
                        (setf open-ends 1)
                    )
                    ;; else if blank
                    ((eq stone *blank*)
                        (setf open-ends 1)
                    )
                    ;; else it is opponent's stone and consecutive > 0
                    ((> consecutive 0)
                        (incf score (gomoku-shape-score consecutive open-ends is-my-turn))
                        (setf consecutive 0)
                        (setf open-ends 0)
                    )
                    ;; else it is opponent's stone
                    (T (setf open-ends 0))
                )
            )
        )
        ; when finishing searching a row
        (cond
            ((> consecutive 0)
                (incf score (gomoku-shape-score consecutive open-ends is-my-turn))
                (setf consecutive 0)
                (setf open-ends 0)
            )
        )
    )
    score
    ))

(defmethod eval-diagonal-for
    ((game gomoku) player left-to-right)
    (let* (
        (dir (if left-to-right (list 1 1) (list 1 -1)))
        (board (gomoku-board game))
        (N (board-dim board))
        (is-my-turn (eq player (gomoku-whose-turn game)))
        (score 0)
        (consecutive 0)
        (open-ends 0)
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
                        ;; else if blank and already found player stones
                        ((and (> consecutive 0) (eq stone *blank*))
                            (incf open-ends)
                            (incf score (gomoku-shape-score consecutive open-ends is-my-turn))
                            (setf consecutive 0)
                            (setf open-ends 1)
                        )
                        ;; else if blank
                        ((eq stone *blank*)
                            (setf open-ends 1)
                        )
                        ;; else it is opponent's stone and consecutive > 0
                        ((> consecutive 0)
                            (incf score (gomoku-shape-score consecutive open-ends is-my-turn))
                            (setf consecutive 0)
                            (setf open-ends 0)
                        )
                        ;; else it is opponent's stone
                        (T (setf open-ends 0))
                    )
                )
            )
        )
        ; when finishing searching a diagonal
        (cond
            ((> consecutive 0)
                (incf score (gomoku-shape-score consecutive open-ends is-my-turn))
                (setf consecutive 0)
                (setf open-ends 0)
            )
        )
    )
    score
    ))

(defmethod eval-for
    ((game gomoku) player)
    (+ (eval-line-for game player T)
        (eval-line-for game player nil)
        (eval-diagonal-for game player T)
        (eval-diagonal-for game player nil)))

(defmethod eval-func
    ((game gomoku))
    (let (
        (whose-turn (gomoku-whose-turn game))
        (black-score (eval-for game *black*))
        (white-score (eval-for game *white*))
    )
    (if (eq whose-turn *black*)
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

(defun compute-move (g cutoff-depth from-who)
  (format t "~%COMPUTE-MOVE (cutoff=~A)~%" cutoff-depth)
  (let* (
    (statty (make-stats))
    (result (compute-max g 0 *neg-inf* *pos-inf* statty cutoff-depth from-who))
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

(defun compute-max (g curr-depth alpha beta statty cutoff-depth from-who)
    (cond
      ;; game is over
      ((game-over? g)
        (let ((winner (who-wins? g)))
            (cond
                ((eq from-who winner) (- *win-value* curr-depth))
                ((eq winner *draw*) (if (eq from-who *black*) -100 *draw-value*))
                (T (+ *loss-value* curr-depth))
                )
            ))
      ;; reach cut-off
      ((>= curr-depth cutoff-depth) (eval-func g))
      ;; otherwise
      (T
        (let (
          (best-move nil)
          (value *neg-inf*)
          (moves (legal-moves g))
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
                (let ((result (compute-min g (+ curr-depth 1) alpha beta statty cutoff-depth from-who)))
                  (cond
                    ;; case 1: if this move is better
                    ((> result value)
                      (setf value result)
                      (setf best-move move)
                    )
                    ;; else do nothing
                  )
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
  )

;;  COMPUTE-MIN
;; -------------------------------------------------------
;;  INPUTS:  G, a CHESS struct
;;           CURR-DEPTH, the depth of this MIN node
;;           ALPHA, BETA, values received from parent MAX node
;;           STATTY, a stats struct
;;           CUTOFF-DEPTH, to limit depth of minimax search
;;  OUTPUT:  The value of this MIN node according to rules
;;           of MINIMAX with ALPHA-BETA pruning

(defun compute-min (g curr-depth alpha beta statty cutoff-depth from-who)
  (cond
    ;; game is over
      ((game-over? g)
        (let ((winner (who-wins? g)))
            (cond
                ((eq from-who winner) (- *win-value* curr-depth))
                ((eq winner *draw*) (if (eq from-who *black*) -100 *draw-value*))
                (T (+ *loss-value* curr-depth))
                )
            ))
    ;; reach cut-off
    ((>= curr-depth cutoff-depth) (- *win-value* curr-depth))
    ;; otherwise
    (T
      (let (
        (value *pos-inf*)
        (moves (legal-moves g))
        )
        ;; update num of potential moves
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
                (let ((result (compute-max g (+ curr-depth 1) alpha beta statty cutoff-depth from-who)))
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
    )
  ))



;;  COMPUTE-DO-AND-SHOW-N-MOVES
;; ------------------------------------------
;;  INPUTS:  G, a CHESS struct
;;           N, a positive integer
;;           CUTOFF-DEPTH, the cutoff depth for minimax
;;  OUTPUT:  don't care
;;  SIDE EFFECT:  Computes, does, and shows the results of N
;;                moves generated using COMPUTE-MOVE.

(defun compute-do-and-show-n-moves
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



;(setf g (make-gomoku :num-open 25 :board (make-array '(5 5) :initial-element *blank*) :win-lineup-num 5))
;(compute-do-and-show-n-moves g 4)



; (setf g (make-gomoku :num-open 81 :win-lineup-num 3))
; (print g)
; (setf g (do-move! g 3 3))
; (print g)
; (print (who-wins? g))
; (setf g (do-move! g 0 0))
; (print g)
; (print (who-wins? g))
; (setf g (do-move! g 3 4))
; (print g)
; (print (who-wins? g))
; (setf g (do-move! g 0 1))
; (print g)
; (print (who-wins? g))
; (setf g (do-move! g 3 6))
; (print g)
; (print (who-wins? g))
; (setf g (do-move! g 0 2))
; (print g)
; (print (who-wins? g))

; (setf g (make-gomoku :num-open 81 :win-lineup-num 5))
; (print (default-policy g))
; (print g)

; (setf g (make-gomoku :num-open 81 :win-lineup-num 5))
; (do-random-move! g)
; (print g)
; (do-random-move! g)
; (print g)
; (undo-move! g)
; (print g)
; (undo-move! g)
; (print g)

; (print (default-policy g))
; (print g)
; (print (eval-func g))
