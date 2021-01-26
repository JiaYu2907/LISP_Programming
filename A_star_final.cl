;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                    A* STAR ALGORITHM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((distance-info (make-hash-table :size 20))
      (path-predecessor-info (make-hash-table :size 20)) )
  (defun set-distances (x y)
    (setf (gethash x distance-info) y) )
  (defun get-distances (x)
    (gethash x distance-info) )
  (defun set-predecessor (x y)
    (setf (gethash x path-predecessor-info) y) )
  (defun get-predecessor (x)
    (gethash x path-predecessor-info) )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                    ROMANIAN MAP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Here are actual inter-city distances from the Michelin map:
(set-distances 'Arad      '((Zerind . 75) (Sibiu . 140) (Timisoara . 118)))
(set-distances 'Bucharest '((Fagaras . 211) (Pitesti . 101) (Giurgiu . 90)(Urziceni . 85)))
(set-distances 'Craiova	  '((Drobeta . 120) (Rimnicu . 146) (Pitesti . 138)))
(set-distances 'Drobeta	  '((Mehadia . 75) (Craiova . 120)))
(set-distances 'Eforie	  '((Hirsova . 86)))
(set-distances 'Fagaras	  '((Sibiu . 99) (Bucharest . 211)))
(set-distances 'Giurgiu	  '((Bucharest . 90)))
(set-distances 'Hirsova	  '((Urziceni . 98) (Eforie . 86)))
(set-distances 'Iasi	  '((Neamt . 87) (Vaslui . 92)))
(set-distances 'Lugoj     '((Timisoara . 111) (Mehadia . 70)))
(set-distances 'Mehadia   '((Lugoj . 70) (Drobeta . 75)))
(set-distances 'Neamt	  '((Iasi . 87)))
(set-distances 'Oradea	  '((Zerind . 71) (Sibiu . 151)))
(set-distances 'Pitesti	  '((Rimnicu . 97) (Craiova . 138) (Bucharest . 101)))
(set-distances 'Rimnicu	  '((Sibiu . 80) (Pitesti . 97) (Craiova . 146)))
(set-distances 'Sibiu	  '((Arad . 140) (Oradea . 151) (Fagaras . 99)(Rimnicu . 80)))
(set-distances 'Timisoara '((Arad . 118) (Lugoj . 111)))
(set-distances 'Urziceni  '((Bucharest . 85) (Hirsova . 98) (Vaslui . 142)))
(set-distances 'Vaslui	  '((Iasi . 92) (Urziceni . 142)))
(set-distances 'Zerind	  '((Arad . 75) (Oradea . 71)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; And here is the hash table F-VALUES to
;;; remember the heuristic value at each node visited.
;;; We also need a hash table for G-VALUES.
(let ((f-values (make-hash-table :size 20))
      (g-values (make-hash-table :size 20)) )
  (defun set-f-value (x y)
    (setf (gethash x f-values) y) )
  (defun get-f-value (x)
    (gethash x f-values) )
  (defun set-g-value (x y)
    (setf (gethash x g-values) y) )
  (defun get-g-value (x)
    (gethash x g-values) )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((longitude-info (make-hash-table :size 20)))
  (defun set-longitude (x y)
    (setf (gethash x longitude-info) y) )
  (defun get-longitude (x)
    (gethash x longitude-info) )
 )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The longitude of each city is stored in tenths of a degree.
;;; We again use a local function with a LAMBDA form, since
;;; SET-LONGITUDE takes two arguments but we want a
;;; function that takes one argument for this use with MAPCAR.
(mapcar #'(lambda (pair) (apply #'set-longitude pair))
	'((Urziceni 27 )(Vaslui 28)(Zerind 22)(Timisoara 21)
	  (Sibiu 24)(Rimnicu 24)(Pitesti 25)(Oradea 22)
	  (Neamt 26 )(Mehadia 22)(Lugoj 23)
	  (Iasi 28)(Hirsova 28)(Giurgiu 26)(Fagaras 25)
	  (Eforie 29)(Drobeta 23)(Craiova 24)(Bucharest 26)(Arad 21) ) )

;;; Now we are ready for the algorithm itself.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A-STAR-SEARCH is the main searching procedure.
(defun a-star-search (start-node goal-node)
  "Performs a search with the A* algorithm."
  (set-goal goal-node)
  (let ((open (list start-node))                ;step1
        (closed nil)
        x
        successors)
    (set-predecessor start-node nil)
    (set-g-value start-node 0)
    (set-f-value start-node (f start-node))
    (loop
      (if (null open)(return 'failure))         ;step2
      (setf x (select-best open))               ;step3
      (setf open (remove x open))               ;step4
      (push x closed)
      (if (eql x (get-goal))
          (return (extract-path x)) )           ;step5
      (setf successors (get-successors x))      ;step6
      (dolist (y successors)                    ;step7
        (if (not (or (member y open)
                     (member y closed) ))
          (progn
            (increment-count)
            (set-g-value y (g y x))
            (set-f-value y (f y))
            (setf open (insert y open))
            (set-predecessor y x) )
          (let* ((z (get-predecessor y))
                (temp (if z
                        (+ (- (get-f-value y)
                              (get-g-value z)
                              (arc-dist z y) )
                           (get-g-value x)
                           (arc-dist x y) )
                        (get-f-value y) ) ) )
            (if (< temp (get-f-value y))
              (progn
                (set-g-value y
                      (+ (- (get-g-value y)
                            (get-f-value y) )
                         temp) )
                (set-f-value y temp)
                (set-predecessor y x)
                (if (member y open)
                  (progn
                    (setf open (remove y open))
                    (setf open (insert y open)) ) )
                (if (member y closed)
                  (progn
                    (setf open (insert y open))
                    (setf closed
                          (remove y closed) ) ) ) ) ) ) ) )

      ; end of loop -------- this is implicitly  step8
             (format t "Closed List:~s.~%" closed)
      (format t " Open List:~s.~%" open)
      (format t "----------------------------------------------------------------------------------------------~% ")
       ) ) )

;; The supporting functions:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Use local variable to keep track of the goal.
(let (goal)
  (defun set-goal (the-goal) (setf goal the-goal))
  (defun get-goal () goal) )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; F is the sum of G and H.
(defun f (n)
  "Computes F value for node N."
  (+ (get-g-value n) (h n)) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; G computes the distance from the start node to NODE
;;; by adding the distance from X to NODE to X's distance.
(defun g (node x)
  "Returns distance from START-NODE to NODE"
  (+ (get-g-value x) (arc-dist x node)) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; H evaluates the difference in longitude between
;;; the current node N and the goal node.
(defun h (n)
  "Returns an estimate of the distance from N
   to the goal."
  (* 10 (longitude-diff n (get-goal))) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; LONGITUDE-DIFF returns the absolute value of the
;;; difference in longitudes between nodes N1 and N2
;;; in tenths of a degree.
(defun longitude-diff (n1 n2)
  "Computes difference in longitudes."
  (abs (- (get-longitude n1) (get-longitude n2))) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SELECT-BEST chooses a node in step 3...
(defun select-best (lst)
  "Returns the best node on LST for expansion."
  (if (eql (first lst) (get-goal))
      (first lst)
    (better (first lst)(rest lst)) ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The helping function BETTER for SELECT-BEST checks
;;; to see if there is a goal node on LST with FVALUE
;;; as low as that of ELT.  If so, it returns the goal node.
;;; If not, it returns ELT.
(defun better (elt lst)
  "Returns a goal-node on LST if it has an equal value,
   otherwise ELT."
  (cond ((null lst) elt)
        ((< (get-f-value elt)(get-f-value (first lst)))
         elt)
        ((eql (first lst) (get-goal))
         (first lst) )
        (t (better elt (rest lst))) ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; INSERT puts NODE onto LST, which is ordered
;;; by FVALUE.
(defun insert (node lst)
  "Inserts NODE onto LST, according to FVALUE ordering."
  (cond ((null lst)(list node))
        ((< (get-f-value node)
            (get-f-value (first lst)) )
         (cons node lst) )
        (t (cons (first lst)
                 (insert node (rest lst)) )) ) )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EXTRACT-PATH returns the sequence of cities found.
(defun extract-path (n)
  "Returns the path from START-NODE to N."
  (cond ((null n) nil)
        (t (append (extract-path (get-predecessor n))
                   (list n) )) ) )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; GET-SUCCESSORS retrieves the list of cities adjacent
;;; to N from the hash table.
(defun get-successors (n)
  "Returns a list of cities adjacent to N."
  (mapcar #'first (get-distances n)) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Let BIG-DISTANCE represent an impossibly large distance
;;; for this problem:
(defconstant big-distance 9999)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ARC-DIST retrieves the distance between N1 and N2.
(defun arc-dist (n1 n2) 
  "Returns the distance along arc N1 N2. If no such arc
   exists, returns BIG-DISTANCE."
  (or (rest (assoc n1 (get-distances n2))) big-distance) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Total number odes expanded by the Astar algo.
(let (expansion-count)
  (defun initialize-count () (setf expansion-count 0))
  (defun increment-count () (incf expansion-count))
  (defun get-count () expansion-count) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun test ()
    (terpri)
    (princ "Name: Lou Jia Yu ID: 1161104266 ") (terpri)
    (terpri)
    (princ "Name: Perivitta Rajendran ID: 1171101579 ")(terpri)
    (terpri)
  "Runs a test of ASTAR."
  (dotimes (in 3)
(princ "Please enter the city [If no input, Default city will be Iasi]: ")
(let ((in (read)))
  (if (equal in 'Oradae) (princ "Please enter the next city")
    (if (equal in 'Zerind) (princ "Please enter the next city")
      (if (equal in 'Arad) (princ "Please enter the next city")
        (if (equal in 'Timisoara) (princ "Please enter the next city")
          (if (equal in 'Lugoj) (princ "Please enter the next city")
            (if (equal in 'Mehadia) (princ "Please enter the next city")
              (if (equal in 'Drobeta) (princ "Please enter the next city")
                (if (equal in 'Craiova) (princ "Please enter the next city")
                  (if (equal in 'Pitesti) (princ "Please enter the next city")
                    (if (equal in 'Rimnicu) (princ "Please enter the next city")
                      (if (equal in 'Fagaras) (princ "Please enter the next city")
                        (if (equal in 'Bucharest) (princ "Please enter the next city")
                          (if (equal in 'Eforie) (princ "Please enter the next city")
                            (if (equal in 'Hirsova) (princ "Please enter the next city")
                              (if (equal in 'Zerind) (princ "Please enter the next city")
                                (if (equal in 'Vaslui) (princ "Please enter the next city")
                                  (if (equal in 'Urziceni) (princ "Please enter the next city")
                                    (if (equal in 'Neamt) (princ "Please enter the next city")
                                      (if (equal in 'Iasi) (princ "Please enter the next city")
                                        (if (equal in 'Sibiu) (princ "Please enter the next city")
                                          (return-from test "The cities that had selected is invalid, please try again"))))))))))))))))))))))(terpri))

(princ "Please enter the R&R city: ")
(let ((in (read)))
  (if (equal in 'Oradae) (princ "Your R&R city has selected! ")
    (if (equal in 'Zerind) (princ "Your R&R city has selected!")
      (if (equal in 'Arad) (princ "Your R&R city has selected!")
        (if (equal in 'Timisoara) (princ "Your R&R city has selected!")
          (if (equal in 'Lugoj) (princ "Your R&R city has selected!")
            (if (equal in 'Mehadia) (princ "Your R&R city has selected!")
              (if (equal in 'Drobeta) (princ "Your R&R city has selected!")
                (if (equal in 'Craiova) (princ "Your R&R city has selected!")
                  (if (equal in 'Pitesti) (princ "Your R&R city has selected!")
                    (if (equal in 'Rimnicu) (princ "Your R&R city has selected!")
                      (if (equal in 'Fagaras) (princ "Your R&R city has selected!")
                        (if (equal in 'Bucharest) (princ "Your R&R city has selected!")
                          (if (equal in 'Eforie) (princ "Your R&R city has selected!")
                            (if (equal in 'Hirsova) (princ "Your R&R city has selected!")
                              (if (equal in 'Zerind) (princ "Your R&R city has selected!")
                                (if (equal in 'Vaslui) (princ "Your R&R city has selected!")
                                  (if (equal in 'Urziceni) (princ "Your R&R city has selected!")
                                    (if (equal in 'Neamt) (princ "Your R&R city has selected!")
                                      (if (equal in 'Iasi) (princ "Your R&R city has selected!")
                                        (if (equal in 'Sibiu) (princ "Your R&R city has selected!")
                                          (return-from test "The city that had selected is invalid, please try again")))))))))))))))))))))(terpri))


  (initialize-count)
  (format t "A-star-search solution: ~s.~%"
    (a-star-search 'Arad 'Bucharest) )
  (format t "Total Path Cost = ~s.~%~%" 
    (get-f-value 'Bucharest) )
  (format t "~s Total Nodes expanded.~%~%"
    (get-count) )
 )
(test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(list-length '('BUCHAREST 'PITESTI 'RIMNICU 'SIBIU 'ARAD))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-path (start goal-test successors equal-states)
  (do (path-to-extend	          ; path to state currently visited
       current-node		  ; node currently visited
       candidates	          ; list of nodes to consider expanding
       extended-paths	          ; list of newly extended paths
       (closed nil)		  ; list of closed nodes
       (open (list (list start))) ; list of all candidate paths to extend
       )
      ((null open) nil)	          ; if open list is empty, search fails
      ;; comment this out when applying to nontrivial problems
      (format t                   ; lets us watch how algorithm works
	      "~%Open: ~s~%Closed: ~s~%" open closed)
      (setq path-to-extend (pop open))         ; get path at head of open list
      (setq current-node (car path-to-extend)) ; get node at end of this path
      (if (funcall goal-test current-node)     ; if at a goal node, exit
	  (return path-to-extend))  
      (unless (member current-node closed      ; skip if this node is closed
		      :test equal-states)
	(push current-node closed)          ; put this node on closed list
	(setq candidates                    ; list all non-closed successors
	      (set-difference (funcall successors current-node)
			      closed
			      :test equal-states))
	(setq extended-paths                ; construct paths to successors
	      (mapcar #'(lambda (A) (cons A path-to-extend)) candidates))
	(setq open                          ; update open list
	      (append extended-paths open)) ; designed for depth first
	)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun same-state (state1 state2)	; works for symbols but may not
  (eql state1 state2))                  ;  be appropriate for lists, e.g.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun neighbors (state)  ; our map based on the romanian city
  (case state
    ( (U) '(V B H ))
    ( (B) '(G P F))
    ( (P) '(C R B))
    ( (F) '(S B))
    ( (R) '(C S P))
    ( (S) '(R F O A))
    ( (A) '(Z T S))
    ( (Z) '(O A))
    ( (O) '(Z S))
    ( (T) '(L A))
    ( (L) '(M T))
    ( (M) '(L D))
    ( (D) '(M C))
    ( (C) '(D R P))
    ( (G) '(B))
    ( (E) '(H))
    ( (H) '(E U))
    ( (V) '(U I))
    ( (I) '(N V))
    ( (N) '(I ))
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; this the goal state and initial state from the find path function
(defun goal-state (A)
  (same-state A 'B))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;To run this program, we must use the sample code output as shown below, if wan to change goal state
;;;and initial state make changes to the defun find-path and test functions
;;;execute by running this code: (find-path 'A #'goal-state #'neighbors #'same-state)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
