;; A simple random acting agent which terminates if he found food
;; The amount of attempts will be printed to stdout.
;; It's a first and dirty implementation. i guess we should improve
;; many things.
;; 09.11.12 M. Bittorf <info@coding-minds.com>

;; Dependency: usocket

(setf *random-state* (make-random-state t))
;; this initializes the global random state by
;; "some means" (e.g. current time.)

;; create a socket and open a connection to the server
(setq sock (usocket:socket-connect "localhost" 4567))

;; initialize the iterations value
(setq iterations 1)

;; move around until we found some food
(do ((line ;; read the next line from socket and stores it in line
      (read-line (usocket:socket-stream sock) nil)
      (read-line (usocket:socket-stream sock) nil))
     (which 0 (1+ which)) ;; increment the iteration counter
    )
    ((search "food" line)) ;; break if we found some food
  (setq iterations which) ;; store amount of iterations in 'iterations
  
  (setq direction
        ;; Random can return float values,
        ;; so we must round the result to get
        ;; an integer value.
        (round
            ;; Add one to the result, because
            ;; (random 8) yields a number between
            ;; 0 and 7, whereas we want a number
            ;; from 1 to 8 inclusive.
            (+ (random 8) 1)))
  
  ;; tell the server the next position
  (format (usocket:socket-stream sock) "move ~A~%" direction)
  (force-output (usocket:socket-stream sock)))

;; close the socket and leave the world
(usocket:socket-close sock)

;; print final amount of iterations to stdout
(format t "~A~%" iterations)