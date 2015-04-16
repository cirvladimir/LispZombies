(defparameter *width* 45)
(defparameter *height* 20)
(defparameter *creatures* '())
(defparameter *num-creatures* 40)
(defparameter *obstacles* '())
(defparameter *player-x* (ash *width* -1))
(defparameter *player-y* (ash *height* -1))
(defparameter *player-alive* t)

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

(defun make-obstacles ()
    (flet ((run-automata (born survive)
            (let ((new-obstacles '()))
                (loop for x below *width*
                    do (loop for y below *height*
                        do (let ((neighbors (loop for xn from (- x 1) below (+ x 2)
                                    sum (loop for yn from (- y 1) below (+ y 2) when (or (not (= x 0)) (not (= y 0)))
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
            (fresh-line))))

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
            (setf *creatures* (remove-if (lambda (cr) (equal cr pt)) *creatures*))
            (if (and (<= 0 (car pt)) (<= 0 (cdr pt)) (> *width* (car pt)) (> *height* (cdr pt)))
                (trace-direction (cons (+ (car pt) (car delt)) (+ (cdr pt) (cdr delt))) delt)
                '()))))
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
    (setf *obstacles* '())
    (make-obstacles)
    (make-creatures)
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
    (princ "d can be up, down, left, or right")
    (fresh-line)
    (loop 
        do (progn
            (print-field)
            (player-move)
            (move-creatures))
        while (and *player-alive* *creatures*))
    (if *player-alive* 
        (princ "Congradulations, you win!!!")
        (princ "You loose"))
    (fresh-line))
    