;;
;; ANTS
;;

; initialize the world 
; run algorithm loop for 10? number of times
;; within each loop
;; throw 50 ants into the system
;; each ant finds a solution after x iterations (100)
;; pheromone evaporation - handled by the daemon
;; pheremone deposition - handled by each ant/ants 
;; 

(load "nomad.lisp")

(defpackage :ants
  (:use :hma :cl))
(in-package :ants)

(defvar *start* nil)
(defvar *best-path* nil)
(defvar *threshold* 20)

(defclass ant ()
  ((history :accessor history :initform '())
   (location :accessor location :initarg :location :initform nil)))

(defmethod print-object ((a ant) stream)
  (format stream "A(~a, ~a)" (row (location a)) (col (location a))))

(defun initialize-ant-world (&key (graph-rows 100) (graph-cols 100)
                                  (start (random 10000)) (end (random 10000)))
  (defparameter *graph* (make-instance 'graph :rows graph-rows :cols graph-cols))  
  (defparameter *best-path* nil)
  (defparameter *goal* (elt (nodes *graph*) end))
  (defparameter *start* (elt (nodes *graph*) start))
  (mapc #'(lambda (x) (setf (pheromone x) (/ 1 (* graph-rows graph-cols))))
        (nodes *graph*)) 
  (hma::create-heuristics (nodes *graph*) *goal*)
  nil 
  )


;; BEGIN DEBUG

(defun print-ants-histories ()
  (loop
   for ant in *individuals*
   do
   (print (history ant))))

;; END DEBUG
   
; number of moves equals the number of iterations the humans make!
(defun dispatch-ants (&key (generations 10) (num-ants 50) (start *start*))
  (loop 
     repeat generations
     do    
     (print "generation")
     (setf *individuals* nil)
     (loop     
      for x below num-ants
	  for ant = (make-instance 'ant :location start)
	  do
          (push ant *individuals*)
          (push start (history ant)))
     (move-ants))
  (calculate-cost *best-path*))

;; for each move
;;; for every ant
;;;; move the ant to a new location
; and the end of all the moves, evaporate global pheromones, deposit global pheromones

(defun move-ants ()
  (loop
   for ant in *individuals*
     do
     (loop 
      for goal? = (ant-goal ant)
      when goal?
      do 
      (print "booya!")
      (deposit-pheromone ant)
      (report-path ant)
      until (or goal?
                (null (move-ant-with-pheromone ant)))))
  (evaporate-pheromone))


(defun move-ant-with-pheromone (ant)
  (let* ((possible-locs (possible-locations ant))
         (total-pheromones (loop
			      for x in possible-locs
			      summing  (* (pheromone x) (expt (/ 1 (hma::fvalue x)) 5)))))
    (when possible-locs
      (loop 	 
	 with rand = (random 1.0)
	 for i in possible-locs
	 sum (get-probability i total-pheromones) into prob		 
	 when (<= rand prob)
	 do
	   (setf (location ant) i)
	   (push i (history ant))
	   (return i)))))

(defun possible-locations (ant)  
  (set-difference (neighbours (location ant))
		  (history ant)))
	

(defun get-probability (j total)
  (/ (* (pheromone j) (expt (/ 1 (hma::fvalue j)) 5)) total))


(defun ant-goal (ant)
  (eql *goal* (location ant)))

(defun report-path (ant) 
  (if (or
       (< (calculate-cost (history ant)) (calculate-cost *best-path*))
       (null *best-path*))
      (setf *best-path* (history ant))))

(defun calculate-cost (path)
  (if path
      (loop
       for x in path
	 sum (terrain-cost x))
    0))                 


(defun evaporate-pheromone (&optional (decrease 0.5))
  (loop
    for x in (nodes *graph*)
    do
    (decf (pheromone x) 
          (* (pheromone x) decrease))))


(defun deposit-pheromone (ant)  
  (loop  
     for spot in (history ant)					
     do
       (incf (pheromone spot) 
             (/ 1 (calculate-cost (history ant))))))
             

