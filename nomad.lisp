(defpackage :hma
  (:use :cl)
  (:export :*individuals* :*graph* :*goal*
	   :broadcast :interaction :interact :history :location :people
	   :add-to-location :remove-from-location
	   :best-destination :nodes :pheromone 
	   :neighbours :random-element :terrain-cost
	   :male :female :villager :node :age :col :row :graph
	   :fvalue :create-heuristics))

(in-package :hma)
(proclaim '(optimize (speed 0) (debug 3) (safety 0)))

(defvar *individuals* nil)
(defvar *graph* nil)
(defvar *village* nil)
(defvar *goal* nil)
(defvar *buddhas* nil)
(defvar *iteration* 0)
(defconstant +max-group-size+ 10)

(defclass village ()
  ((loc :accessor location :initarg :location :initform (error "must provide location"))
   (amount-of-villagers :initarg :amount-of-villagers :initform 10)
   (villagers :accessor villagers :initform '())
   (neighbours :accessor neighbours)))

(defclass graph ()
  ((rows :initarg :rows :accessor rows :initform 0)
   (cols :initarg :cols :accessor cols :initform 0)
   (nodes :accessor nodes)))

(defclass node ()   
  ((neighbours :accessor neighbours :initform '())
   (terrain-cost :accessor terrain-cost :initarg :terrain-cost :initform 1)
   (row :accessor row :initarg :row :initform 0)
   (col :accessor col :initarg :col :initform 0)   
   (cost :accessor cost :initform 0)
   (people :accessor people :initform '())
   (accessible :accessor accessible :initarg :accessible :initform t)
   (parent :accessor parent :initform nil)
   (heuristic :accessor heuristic :initform 0)
   (pheromone :accessor pheromone :initform 0)))

(defclass villager ()
  ((history :accessor history :initform '())
   (location :accessor location :initarg :location :initform nil)
   (lifespan :accessor lifespan :initarg :lifespan :initform 2)
   (location-of-goal :accessor goal :initform nil)
   (age :accessor age :initform 0)
   (mates :accessor mates :initform '())))

(defclass male (villager)
  ((aggression :accessor aggression :initarg :aggression :initform 0.1)))

(defclass female (villager)
  ((male-child-probability :accessor male-child-probability :initarg :male-child-probability :initform 0.5)))

(defun fvalue (node)
  (+ (the number (terrain-cost node)) (the number (heuristic node))))

(defun valid-mate? (a b)  
  (and (> (age a) 19) ; old enough to mate
       (> (age b) 19)
       (null (or 
	      (eql (type-of a) (type-of b)) ; same-gender
	      (find a (mates b))
	      (find b (mates a))))))

(defun move (villager)
  (let* ((loc (location villager))
	 (neighbours (neighbours loc))
	 (dest (best-destination villager neighbours)))        
    (remove-from-location villager)
    (setf (location villager) dest)    
    (pushnew (location villager) (history villager))
    (incf (age villager))
    (add-to-location villager)
    (when (eql loc *goal*)		  
      (pushnew villager *buddhas*)      
      (setf *individuals* (remove villager *individuals*)))))

(defun best-destination (villager neighbours)  
  (if (goal villager)       
      (closest-to-goal neighbours)
      (let* ((mate-location (potential-mate-location villager neighbours))	     
	     (unvisited-spots (unvisited villager neighbours))
	     (safe-spots (safe-nodes villager neighbours))
	     (unvisited-safe-spots (intersection unvisited-spots safe-spots)))
	(cond
	  (unvisited-safe-spots
	   (pick-least-occupied unvisited-safe-spots)) ; go to the potential mate
	  (unvisited-spots
	   (pick-least-occupied unvisited-spots))
	  (mate-location mate-location)	  	 
	  (safe-spots
	   (if (= (length safe-spots) 1)
	       (first safe-spots)
	       (random-element (remove-closest-to-start safe-spots))))
	  (t	   
	   (random-element (remove-closest-to-start neighbours)))))))

(defun remove-closest-to-start (nodes &optional (start (location *village*)))  
  (loop
	 with res = (first nodes)
	 with least-distance = (distance res start)
	 for i below (length nodes)
	 for x = (elt nodes i)
	 for dist = (distance x start)
	 finally (return (remove res nodes))
	 when (< dist least-distance)
	 do
	   (setf least-distance dist)
	   (setf res x)))

(defun potential-mates (v location)
  (loop
     for a in (people location)		     
     when (valid-mate? v a) collect a))

(defun potential-mate-location (v neighbours)
  (loop
     for x in neighbours          
     when (potential-mates v x)
     do
       (return x)))

(defun pick-least-occupied (choices)  
  (loop
     with least = (random-element choices)       
     for x in (remove least choices)
     finally (return least)    
     for amt-people-x = (length (people x))
     for amt-people-least = (length (people least))       
     when (= amt-people-x amt-people-least)	    
     do    
       (if (< (terrain-cost x) (terrain-cost least))
	   (setf least x))
     when (< amt-people-x amt-people-least)
     do
       (setf least x)))

(defun people-within-range (current loc)
  (if (> (length (people loc)) 1)
      t
      (some #'(lambda (y) (and (location-within-range loc y)
			       (people y)))
	    (remove current (locations))))) 

(defun decrease-lifespan (villager)
  (decf (lifespan villager))
  (if (zerop (lifespan villager))
      (setf *individuals* (remove villager *individuals*))))

(defun remove-from-location (v)
  (with-accessors ((loc location)) v    
    (setf (people loc) (delete v (people loc)))))    

(defun add-to-location (v)
  (pushnew v (people (location v)))) 

(defun unvisited (villager places)
  "Locations we haven't visited as yet,
   and also locations that we can visit without
   violating group safety rules"
  (remove-if #'(lambda (x)
		 (find x (history villager)))			 
	     places))

(defun safe-nodes (v places)
  "Nodes that are still within sight of someone else in our group"
  (remove-if #'(lambda (x)		     
		 (not (people-within-range 
		       (location v)
		       x)))
	     places))

(defmethod initialize-instance :after ((v village) &key)
  "We're going to make girls live possibly shorter to help
   control the population. Men right now can already kill each other"
  (setf (neighbours v) (neighbours (location v)))
  ; initialize the villagers and their location  
  (loop
     for x below (slot-value v 'amount-of-villagers)
     for gender = (random 2)    
     do
       (pushnew (if (zerop gender)
		 (make-instance 'male :aggression (random 1.0) 
				:location (location v)
				:lifespan (fertility))
		 (make-instance 'female :male-child-probability 0.5 
				:location (location v)
				:lifespan (fertility)))
	     (villagers v)))
  (mapc #'(lambda (x) (pushnew (location v) (history x))) (villagers v)))

(defun fertility ()
  (1+ (floor (random 2))))

(defmethod initialize-instance :after ((g graph) &key)
  (with-accessors ((rows rows) (cols cols)) g
    (setf (nodes g) (make-list (* rows cols)))
    (loop for i below rows do
	 (loop for j below cols 
	    do	      
	      (setf (elt (nodes g) (+ j (* cols i))) 
		    (make-instance 'node :row i :col j :terrain-cost (+ 1.0 (random 1.0)))))))   
  (set-node-neighbours g))

(defun initialize-keep-graph (start end &key (villager-amount 30))
  (defparameter *individuals* nil) 
  (defparameter *village* (make-instance 'village :location start
					 :amount-of-villagers villager-amount))
  (defparameter *goal* end)
  (defparameter *buddhas* '())
  (defparameter *iteration* 0)
  (setf *individuals* (villagers *village*)))
  
(defun initialize-world (&key (graph-rows 100) (graph-cols 100)
			 (start-index (random 10000)) (end-index (random 10000)) (villager-amount 30))  
  (defparameter *graph* (make-instance 'graph :rows graph-rows :cols graph-cols))
  (initialize-keep-graph (elt (nodes *graph*) start-index) 
			 (elt (nodes *graph*) end-index) :villager-amount villager-amount))

(defun set-node-neighbours (g)
  "initialize the neighbours of nodes based on grid location"
  (with-accessors ((rows rows) (cols cols) (nodes nodes)) g
    (loop for i below rows do
	 (loop for j below cols
	    for neighbours = '()
	    ;finally (setf (neighbours (elt nodes (+ (* i cols) (1- j)))) neighbours)
	    do 	      
	      (if (> j 0) (pushnew (elt nodes (+ (* i cols) (1- j))) neighbours)) ; west
	      (if (< j (1- cols)) (pushnew (elt nodes (+ j 1 (* i cols))) neighbours))
	      (if (> i 0) (pushnew (elt nodes (+ (* (1- i) cols) j)) neighbours))
	      (if (< i (1- rows)) (pushnew (elt nodes (+ (* (1+ i) cols) j)) neighbours))
	      (setf (neighbours (elt nodes (+ (* i cols) j))) neighbours)))))

(defgeneric interact (a b))

(defmethod interact ((f female) (m male))
  (interact m f))

(defmethod interact ((a female) (b female))  
  (exchange-knowledge a b))

(defun tense-situation? (a b &optional (threshold 0.75))
  (and  
   (> (age a) 19)
   (> (age b) 19)
   (>= (+ (aggression a)
	 (aggression b))
      threshold)))

(defun strength (x)
  (length (history x)))

(defmethod interact ((a male) (b male))
  ; will not kill parent or child
  (if (or (find a (mates b))
	  (find b (mates a)))
      (return-from interact nil))
  (if (tense-situation? a b)      
      (do-battle a b)
      (progn
	(increase-aggression a)
	(increase-aggression b)
	(exchange-knowledge a b))))    

(defmethod interact ((m male) (f female))  
  (when (and 
	 (valid-mate? m f) 
	 (<= (my-group-size m) +max-group-size+)
	 (<= (my-group-size f) +max-group-size+))
      (create-offspring m f)
      (exchange-knowledge m f)))

(defun raise-child (x m f)    
  (pushnew x (mates f))
  (pushnew x (mates m))   
  (setf (mates x) (union (mates m) (mates f)))
  (setf (history x) (union (history m) (history f)))  
  (pushnew x *individuals*))

(defun have-baby (m f)
  (let* ((male? (< (random 1.0) (male-child-probability f)))
	 (child (if male?
		    (make-instance 'male :aggression (aggression m) 
				   :location (location m)
				   :lifespan (fertility))
		    (make-instance 'female 
				   :male-child-probability (male-child-probability f)
				   :location (location m)
				   :lifespan (fertility)))))    
    child))

(defun create-offspring (m f)
  (pushnew m (mates f))
  (pushnew f (mates m))
  (let ((child (have-baby m f)))
    (raise-child child m f)
    (check-health m)
    (check-health f)))

(defun check-health (villager)
  (decf (lifespan villager))
  (if (zerop (lifespan villager))
      (setf *individuals* (remove villager *individuals*))))

(defun war-list (x safe-list)
  "What we're willing to lose in a fight"
  (set-difference (history x) safe-list))

(defun do-battle (a b)
  (loop       
     finally
       (let ((lost-by-a (pick-random-elements war-list-a lost-a))
	     (lost-by-b (pick-random-elements war-list-b lost-b)))
	 (setf (history a) (set-difference (history a) lost-by-a))
	 (setf (history b) (set-difference (history b) lost-by-b))	
	 (cond  ; the victor gets to impose his path
	   ((< lost-a lost-b)
	    (setf (history b)
		  (union war-list-b path-a))
	    (check-health b))
	   ((> lost-a lost-b)
	    (setf (history a)
		  (union war-list-a path-b))
	    (check-health a))))	    
     with path-a = (best-path (history a) :end (location a))
     with path-b = (best-path (history b) :end (location b))    
     with total-power = (+ (strength a) (strength b))
     with a-share = (/ (strength a) total-power)         
     with war-list-a = (war-list a path-a)
     with war-list-b = (war-list b path-b)     
     while (tense-situation? a b) 
     while (< lost-a (* (length war-list-a) 0.75))
     while (< lost-b (* (length war-list-b) 0.75)) 
     for risk = (random (1+ (min (length war-list-a) (length war-list-b))))     	    
     for a-winner? = (< (random 1.0) a-share)
     when a-winner? sum risk into lost-b
     when (not a-winner?) sum risk into lost-a     
     do
       (decrease-aggression a)
       (decrease-aggression b)))

(defun my-group-size (v)
  (loop    
     finally (return 1)
     for x in (groups)
     for in-this-group? = (find v x)
     when in-this-group?
     do
       (return (length x))))
       
(defun increase-aggression (x)
  (incf (aggression x) (* (random 0.1) (aggression x))))

(defun decrease-aggression (x)
  (decf (aggression x) (* (random 0.1) (aggression x))))

(defun pick-random-elements (list amount) 
  (loop
     while (> amount 0)
     while (plusp (length list))
     for element = (random-element list)
     collect element
     do
       (decf amount)
       (setf list (remove element list))))
       
(defun broadcast (villager data)
  (loop
     for x in *individuals*
     with buddha? = (eql data *goal*)  
     when buddha?
     do
       (loop
	  for x in *individuals*
	  do
	    (setf (goal x) data))
     when (and (not (eql x villager)) 
	       (within-range x villager)
	       (not (find data (history x))))
     do
       (receive x data :goal-p buddha?)))

(defun receive (x loc &key (goal-p nil))    
  (if goal-p
      (setf (goal x) loc))
  (pushnew loc (history x))
  (broadcast x loc))

(defun distance (posa posb)
  (let* ((deltax (abs (- (col posa) (col posb))))
	 (deltay (abs (- (row posa) (row posb)))))
    (declare (fixnum deltax deltay))
    (sqrt (+ (* deltax deltax) (* deltay deltay)))))

(defun within-range (a b)
  (<= (distance (location a) (location b)) 4))

(defun location-within-range (a b)
  (<= (distance a b) 4))

(defmethod print-object ((n node) stream)
  (format stream "(~a, ~a)" (row n) (col n)))

(defmethod print-object ((m male) stream)
  (format stream "M:(~a, ~a)" (row (location m)) (col (location m))))

(defmethod print-object ((f female) stream)
  (format stream "F:(~a, ~a)" (row (location f)) (col (location f))))

(defun farthest-from-start (neighbours &optional (start (location *village*)))
  (loop
     with res = (first neighbours)
     with most-distance = (distance res start)	
     for x in (cdr neighbours)
     for dist = (distance x start)
     finally (return res)
     when (> dist most-distance)
     do
     (setf most-distance dist)
     (setf res x)))

(defun closest-to-goal (neighbours &optional (goal *goal*))
  (loop
     with res = (first neighbours)
     with least-distance = (distance res goal)
     for i below (length neighbours)
     for x = (elt neighbours i)
     for dist = (distance x goal)
     finally (return res)
     when (< dist least-distance)
     do
     (setf least-distance dist)
     (setf res x)))

(defun goal? (ind)
  (eql (location ind) *goal*))

(defun iteration (amount)
  (when (or (<= amount 0) (null *individuals*))    
    (return-from iteration nil))
         
  (incf *iteration*)
  (loop
     for x in *individuals*
     for loc = (location x)
     for people = (people loc)
     do
       (move x)
       (if (and (>= (length people) 2) (not (goal? x)))
	   (interaction x (remove x people)))
       (broadcast x (location x)))
  (iteration (1- amount)))

(defun interaction (x others)    
  "First priority is reproduction" 
  (interact (random-element others) x))

(defun random-element (list)
  (elt list (random (length list))))

(defun exchange-knowledge (a b &optional (ratio 0.5))
  (let* ((path-a (best-path (history a)  :end (location a)))
	 (path-b (best-path (history b) :end (location b)))
	 (diffab (set-difference (history a) (history b)))
	 (diffba (set-difference (history b) (history a)))
	 (ratio-diffab (pick-random-elements diffab (floor (* ratio (length diffab)))))
	 (ratio-diffba (pick-random-elements diffba (floor (* ratio (length diffba))))))
    (setf (history a) (union (history a) 
			     (union ratio-diffba path-b)))        
    (setf (history b) (union (history b) 
			     (union ratio-diffab path-a)))
    (broadcast-new-information a ratio-diffba)
    (broadcast-new-information b ratio-diffab)))

(defun broadcast-new-information (villager information)
  (loop
     for x in information
     do
       (broadcast villager x)))

(defun population-information ()
  (format t "after ~a iterations.~%" *iteration*)
  (format t "population: ~a normals + ~a buddhas ~%" (length *individuals*) (length *buddhas*))
  (loop 
       for x in *individuals*
       counting (typep x 'female) into females
       counting (typep x 'male) into males
       maximizing (length (history x)) into guru
       finally (format t "~a males and ~a females with smartest person knowing ~a nodes. ~%" males females guru)))

;; A* section - we're going to use A* on the whole map to see how it runs
;; we're also going to use A* to calculate the best path the succeeding agents know
(defclass a-star ()
  ((path :accessor path :initform nil)
   (open-list :accessor open-list :initform nil)
   (closed-list :accessor closed-list :initform nil)
   (nodes :accessor nodes :initarg :nodes)))

(defun min-node (nodes)
  (let ((val nil))
    (loop for i in nodes do
	 (if (and (accessible i) (or (null val) (< (fvalue i) (fvalue val))))
	     (setf val i)))
    val))

(defun create-heuristics (nodes end-node)
  (loop for n in nodes do
       (setf (heuristic n) (heuristic-value n end-node))))

(defun heuristic-value (start end)
 (+ (abs (- (row end) (row start))) (abs (- (col end) (col start)))))

(defun process-neighbours (alg node)
  (loop 
     with added = '()
     finally (return added)
     for n in (neighbours node)
     when (find n (nodes alg))
     do
     (if (accessible n)
	 (cond
	   ((null (or (find n (open-list alg)) (find n (closed-list alg))))
	    (setf (cost n) (+ (cost node) (terrain-cost n)))
	    (setf (parent n) node)
	    (pushnew n added))
	   (t 
	    (when (< (+ (cost node) (terrain-cost n)) (cost n))
	      (setf (parent n) node)
	      (setf (cost n) (+ (cost node) (terrain-cost n)))))))))

(defun find-path (alg start end)
  (with-accessors ((open-list open-list) (closed-list closed-list) (path path)) alg        
    (pushnew start open-list)    
    (loop
       with complete = nil
       until complete
       do 	 
       (if (null open-list)
	   (setf complete t)
	   (let ((min-nde (min-node open-list)))	     
	     (setf open-list (remove min-nde open-list))	     
	     (pushnew min-nde closed-list)
	     (if (equalp min-nde end)
		 (progn
		   (pushnew min-nde path)			    
		   (loop			
		      for n = (parent min-nde) then (parent n)
		      while n 
		      do 
			(pushnew n path))					
		   (setf complete t))
		 (mapc #'(lambda (x) (if x (pushnew x (open-list alg)))) 
		       (process-neighbours alg min-nde))))))    
    path))
	    
(defun reset-costs (nodes)
  "Used to reset the costs of nodes after running A*"
  (loop
     for x in nodes
     do
       (setf (cost x) 0))
  nodes)
       
(defun best-path (nodes &key (end *goal*) (start (location *village*)))  
  (let ((alg (make-instance 'a-star :nodes nodes)))
    (reset-costs nodes)
    (create-heuristics nodes end)
    (find-path alg start end)))  

(defun path-cost (path &optional (goal *goal*))
  (let ((node (find goal path)))
    (when node
	(cost node))))
	
(defun cheapest-path ()
  (loop
     for x in *buddhas*
     for path = (best-path (history x))
     for cost =  (path-cost path)
     when cost minimize cost))
       
(defun locations ()
  (loop
     for x in *individuals*
     with res = '()
     finally (return res)
     do
       (pushnew (location x) res)))

(defun print-locations-and-knowledge ()
  (loop
     for x in *individuals*
     for loc = (location x)          
     do
       (format t "(~a, ~a) : I am ~a, I know about ~a nodes and I have ~a matings left ~%" 
	       (row loc) (col loc) (age x) (length (history x)) (lifespan x))))

(defun a-star-cost ()
  (path-cost (best-path (nodes *graph*))))

(defun run-until-amount-ascended (x &key (iters 100) (max-iters 1000))
  (loop
     while (< *iteration* max-iters)
     while (< (length *buddhas*) x)       
     do 
       (iteration iters)))


(defun groups ()
  (let ((people (copy-list *individuals*)))
    (labels ((connected-to (i)
	       (if i
		   (loop 
		      with res = (list i)
		      finally (return res)
		      for x in (setf people (remove i people))
		      when (within-range i x) 
		      do		    
			(setf res (union res (connected-to x)))))))
      (loop	 	   
	 while people	   
	 collect (connected-to (first people)))))) 

(defun group-sizes ()
  (loop
     for x in (groups)
     collect (length x)))
       

; WEIRD: This is returning a worse path than cheapest path
(defun best-path-for-group ()
  (loop
     with histories = '()
     finally (return (best-path histories))
     for x in *buddhas*
     do
       (setf histories (union histories (history x)))))

(defun check-performance (&optional (iterations 100))
  (loop
     with ratio-total = 0
     with valid = 0
     finally
       (format t "~a comparisons - path cost ratio of ~a~%" valid (/ ratio-total valid))
     until (= valid iterations)
     do
       (initialize-world)
       (unless (eql *goal* (location *village*))
	 (run-until-amount-ascended 3 :max-iters 800)
	 (when (>= (length *buddhas*) 3)
	   (incf valid)
	   (incf ratio-total (/ (cheapest-path) (a-star-cost)))))))    

