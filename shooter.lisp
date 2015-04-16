(defparameter *width* 45)
(defparameter *height* 20)
(defparameter *creatures* '())
(defparameter *num-creatures* 40)
(defparameter *obstacles* '())
(defparameter *player-x* (ash *width* -1))
(defparameter *player-y* (ash *height* -1))
(defparameter *player-alive* t)
(defparameter *wave-number* 0)
(defparameter *monsters-to-spawn* 0)
(defparameter *reacheable-edges* '())
(defparameter *monsters-eliminated* 0)

(defun random-square ()
    (let ((pos (cons (random *width*) (random *height*))))
        (if (or (not (empty-square pos)) (<= (square-distance pos) 4))
            (random-square)
            pos)))
    

(defun make-creatures ()
    (setf *creatures* '())
    (loop repeat *num-creatures*
        do (push (random-square) *creatures*))
    *creatures*)    
    
(defun calc-reacheable-edges ()
    (let ((front (list (cons *player-x* *player-y*)))
        (reached '(list (cons *player-x* *player-y*))))
        (setf *reacheable-edges* '())
        (loop 
            do (let ((new-front '()))
                    (loop for fr-pt in front
                        do (loop for x from (- (car fr-pt) 1) to (+ (car fr-pt) 1)
                            do (loop for y from (- (cdr fr-pt) 1) to (+ (cdr fr-pt) 1)
                                when (and (not (and (= x (car fr-pt)) (= y (cdr fr-pt)))) 
                                        (and (>= x 0) (>= y 0) (< x *width*) (< y *height*)) 
                                        (not (member (cons x y) reached :test #'equal)))
                                do (progn
                                    (push (cons x y) reached)
                                    (push (cons x y) new-front)
                                    (if (or (= x 0) (= y 0) (= x (- *width* 1)) (= y (- *height* 1)))
                                        (push (cons x y) *reacheable-edges*))))))
                    (setf front new-front))
            while front)))

(defun make-wave ()
    (incf *wave-number*)
    (setf *monsters-to-spawn* (+ 6 (* *wave-number* 3))))
    
(defun update-wave ()
    (let ((to-spawn (max (+ *wave-number* (random 4)) *monsters-to-spawn*)))
        (loop repeat to-spawn
            do (let ((new-pos (nth (random (length *reacheable-edges*)) *reacheable-edges*)))
                (if (empty-square new-pos)
                    (progn (decf *monsters-to-spawn*)
                        (push new-pos *creatures*)))))))

(defun make-obstacles ()
    (flet ((run-automata (born survive)
            (let ((new-obstacles '()))
                (loop for x below *width*
                    do (loop for y below *height*
                        do (let ((neighbors (loop for xn from (- x 1) below (+ x 2)
                                    sum (loop for yn from (- y 1) below (+ y 2) when (not (and (= x 0) (= y 0)))
                                    sum (if (empty-square (cons xn yn)) 0 1)))))
                                (if (>= (if (empty-square (cons x y)) born survive) neighbors) (push (cons x y) new-obstacles))
                            )))
                new-obstacles)))
        (loop repeat (/ (* *width* *height*) 3)
            do (push (random-square) *obstacles*))
        (loop repeat 2 do (setf *obstacles* (run-automata 5 3)))
        ;;(loop repeat (/ (* *width* *height*) 5)
        ;;    do (push (random-square) *obstacles*))
        (loop repeat 2 do (setf *obstacles* (run-automata 4 2)))
        (loop repeat 2 do (setf *obstacles* (run-automata 5 1)))
        ))
        
(defun test ()
    (setf *obstacles* '())
    (make-obstacles)
    (print-field))
    
(defun square-distance (cr)
    (+ 
        (expt (- (car cr) *player-x*) 2)
        (expt (- (cdr cr) *player-y*) 2)))
        
(defun empty-square (pt)
    (and (not (member pt *creatures* :test #'equal))
    (not (member pt *obstacles* :test #'equal))
    (not (equal pt (cons *player-x* *player-y*))) 
    (>= (car pt) 0) (>= (cdr pt) 0)
    (< (car pt) *width*) (< (cdr pt) *height*)))
    
(defun quick-sort (list)
    (and list
        (let ((dist (square-distance (car list))))
            (append 
                (quick-sort (remove-if (lambda (cr) (>= (square-distance cr) dist)) list))
                (list (car list))
                (quick-sort (remove-if-not (lambda (cr) (>= (square-distance cr) dist)) (cdr list)))))))

(defun print-field ()
    (loop for x below (+ *width* 2)
        do (princ "-"))
    (fresh-line)
    (format t (format '() "|~~~a:@<Wave ~~a~~;|~~;Kills: ~~a~~>|" *width*) *wave-number* *monsters-eliminated*)
    (fresh-line)
    (loop for x below (+ *width* 2)
        do (princ "-"))
    (fresh-line)
    (loop for y
        below *height*
        do (progn
            (princ "|")
            (loop for x
                below *width*
                do (princ (cond 
                    ((member (cons x y) *creatures* :test #'equal) "%")
                    ((member (cons x y) *obstacles* :test #'equal) "#")
                    ((and (= *player-x* x) (= *player-y* y)) "@")
                    (t " "))))
            (princ "|")
            (fresh-line)))
    (loop for x below (+ *width* 2)
        do (princ "-"))
    (fresh-line))

(define-condition input-error (error)
        ((message :reader message-argument :initarg :message)))

(defun stop ()
    (setf *player-alive* '()))

(defun move (dir)
    (let ((new-pos (cond
            ((eq dir 'up) (cons *player-x* (- *player-y* 1)))
            ((eq dir 'down) (cons *player-x* (+ *player-y* 1)))
            ((eq dir 'left) (cons (- *player-x* 1) *player-y*))
            ((eq dir 'right) (cons (+ *player-x* 1) *player-y*))
            (t (error 'input-error :message "Invalid direction")))))
        (if (empty-square new-pos)
            (progn (setf *player-x* (car new-pos)) (setf *player-y* (cdr new-pos)))
            '())))

(defun shoot (dir)
    (labels ((trace-direction (pt delt)
        (if (member pt *creatures* :test #'equal)
            (progn 
                (setf *creatures* (remove-if (lambda (cr) (equal cr pt)) *creatures*))
                (incf *monsters-eliminated*))
            (if (and (<= 0 (car pt)) (<= 0 (cdr pt)) (> *width* (car pt)) (> *height* (cdr pt)))
                (trace-direction (cons (+ (car pt) (car delt)) (+ (cdr pt) (cdr delt))) delt)))))
        (trace-direction (cons *player-x* *player-y*)
            (cond
                ((eq dir 'up) '(0 . -1))
                ((eq dir 'down) '(0 . 1))
                ((eq dir 'left) '(-1 . 0))
                ((eq dir 'right) '(1 . 0))
                (t (error 'input-error :message "Invalid direction"))))))

(defun player-move ()
    (handler-case
        (progn 
            (princ ">")
            (let* ((str (concatenate 'string "(" (read-line) ")"))
                    (comup (read-from-string str))
                    (com (cons (car comup) (mapcar (lambda (s) (list 'quote s)) (cdr comup)))))
                (if (member (car com) '(stop move shoot))
                    (eval com)
                    (error "Unknown commaand"))))
        (input-error (e)
            (princ (message-argument e))
            (fresh-line)
            (player-move))
        (system::simple-end-of-file (e)
            (stop))
        (error (e) 
            (princ "I don't understand this command")
            (fresh-line)
            (player-move))))


(defun move-creature (cr)
    (if (<= (square-distance cr) 1)
            (or (setf *player-alive* '()) cr)
            (let ((dx (- *player-x* (car cr)))
                (dy (- *player-y* (cdr cr))))
                (if (< (random (+ (abs dx) (abs dy))) (abs dx))
                    (cons (+ (car cr) (signum dx)) (cdr cr))
                    (cons (car cr) (+ (cdr cr) (signum dy)))))))
            
(defun move-creatures ()
    (setf *creatures* (quick-sort *creatures*)) 
    (mapc (lambda (cr) 
        (let ((new-pos (move-creature cr)))
            (if (empty-square new-pos)
                (progn (setf (car cr) (car new-pos)) (setf (cdr cr) (cdr new-pos)))
                ))) *creatures*))
    

(defun start-game ()
    (setf *player-alive* t)
    (setf *player-x* (ash *width* -1))
    (setf *player-y* (ash *height* -1))
    (setf *monsters-eliminated* 0)
    (setf *obstacles* '())
    (setf *creatures* '())
    (make-obstacles)
    (calc-reacheable-edges)
    (princ "Welcome to Shooter!")
    (fresh-line)
    (princ "Commands:")
    (fresh-line)
    (princ "    shoot <d> - shoot in direction d")
    (fresh-line)
    (princ "    move <d> - move in direction d")
    (fresh-line)
    (princ "    stop - exit the game")
    (fresh-line)
    (princ "direction can be up, down, left, or right")
    (fresh-line)
    (loop 
        do (progn
            (make-wave)
            (loop 
                    do (progn
                        (print-field)
                        (player-move)
                        (update-wave)
                        (move-creatures))
                    while (and *player-alive* *creatures*)))
        while *player-alive*)
    (if *player-alive* 
        (princ "Congradulations, you win!!!")
        (princ "You loose"))
    (fresh-line))
    