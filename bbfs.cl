
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                    BBFS ALGORITHM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(let ((adjacency-info (make-hash-table :size 20))
      (path-predecessor-info (make-hash-table :size 20)) )
  (defun set-adj (x y)
    (setf (gethash x adjacency-info) y) )
  (defun get-adj (x)
    (gethash x adjacency-info) )
  (defun set-predecessor (x y)
    (setf (gethash x path-predecessor-info) y) )
  (defun get-predecessor (x)
    (gethash x path-predecessor-info) )
 )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                    ROMANIAN MAP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-adj 'Urziceni  '(Bucharest Vaslui))
(set-adj 'Bucharest '(Urziceni Fagaras Pitesti Giurgiu))
(set-adj 'Fagaras   '(Bucharest Sibiu))
(set-adj 'Pitesti   '(Bucharest Craiova Rimnicu))
(set-adj 'Sibiu     '(Fagaras Arad Rimnicu Oradea))
(set-adj 'Rimnicu   '(Sibiu Pitesti Craiova))
(set-adj 'Craiova   '(Pitesti Rimnicu Drobeta))
(set-adj 'Oradea    '(Sibiu Zerind))
(set-adj 'Zerind    '(Oradea Arad))
(set-adj 'Arad      '(Zerind Sibiu Timisora))
(set-adj 'Timisora  '(Arad Lugoj))
(set-adj 'Lugoj     '(Timisora Mehadia))
(set-adj 'Mehadia   '(Lugoj Drobeta))
(set-adj 'Drobeta   '(Mehadia Craiova))
(set-adj 'Giurgiu   '(Bucharest))
(set-adj 'Iasi	    '(Neamt Vaslui))
(set-adj 'Neamt     '(Iasi))
(set-adj 'Vaslui    '(Iasi Urziceni))
(set-adj 'Hirsova   '(Eforie Urziceni))
(set-adj 'Eforie    '(Hirsova))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; BREADTH-FIRST-SEARCH is the main searching procedure.
(defun breadth-first-search (start-node goal-node)
  "Performs a breadth-first search from START-NODE for GOAL-NODE."
  (let ((open (list start-node))                ;step1
        (closed nil)
        n l)
    (set-predecessor start-node nil)
    (loop
      (if (null open)(return 'failure))         ;step2
      (setf n (pop open))                       ;step3
      (push n closed)
      (increment-count)
      (if (eql n goal-node)
          (return (extract-path n)) )
      (setf l (successors n))                   ;step4
      (setf l (list-difference l (append open closed)))
      (setf open (append open l) )              ;step5
      (dolist (x l)
              (set-predecessor x n) )

      ; end of loop -------- this is implicitly  step6
             (format t "Closed List:~s.~%" closed)
      (format t " Open List:~s.~%" open)
      (format t "-------------------------------------------------------------------------------------------------------~% ")
       ) ) )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The supporting functions:

;;; EXTRACT-PATH returns the sequence of cities found.
(defun extract-path (n)
  "Returns the path to N."
  (cond ((null n) nil)
        (t (append (extract-path (get-predecessor n))
                   (list n) )) ) )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SUCCESSORS retrieves the list of cities adjacent
;;; to N from N's property list.
(defun successors (n)
  "Returns a list of the nodes adjacent to N."
  (get-adj n) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; LIST-DIFFERENCE is like the built-in Lisp function
;;; named SET-DIFFERENCE but it preserves the ordering in LST1"
(defun list-difference (lst1 lst2)
  "Returns a list of those elements of LST1 that do not
   occur on LST2."
  (dolist (elt lst2 lst1)
    (setf lst1 (remove elt lst1)) ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Use a local variable EXPANSION-COUNT for counting the
;;; number of nodes expanded by the algorithm.
(let (expansion-count)
  (defun initialize-count () (setf expansion-count 0))
  (defun increment-count () (incf expansion-count))
  (defun get-count () expansion-count) )

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
	      (mapcar #'(lambda (U) (cons U path-to-extend)) candidates))
	(setq open                          ; update open list
	      (append extended-paths open)) ; designed for depth first
	)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun same-state (state1 state2)	; works for symbols but may not
  (eql state1 state2))                  ;  be appropriate for list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun neighbors (state)
  (case state
    ( (U) '(V B H))
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
    ( (N) '(I))
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; changing 
(defun goal-state (U)
  (same-state U 'A))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;(find-path 'U #'goal-state #'neighbors #'same-state)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test ()
    (terpri)
    (princ "Name: Lou Jia Yu ID: 1161104266 ") (terpri)
    (terpri)
    (princ "Name: Perivitta Rajendran ID: 1171101579 ")(terpri)
  (terpri)
  "Runs a test of BBFS"
  (princ "Enter Your Current Location: ")
  (setq start-node (read))
  (princ "Enter Your Destination: ")
  (setq goal-node (read))
  "Tests the function Bidirectional Breath First Search."
  (initialize-count)
  (format t " Breadth first search Path -> ~s.~%~%"
    (breadth-first-search start-node goal-node))
  (format t " Total nodes expanded = ~s nodes ~% "
    (get-count) )
 ;;; (format t "Path cost (1KM each path) = ~s km ~%~% " 
 ;;;   (- (length '('URZICENI 'BUCHAREST 'FAGARAS 'SIBIU 'ARAD)) 1))
  
  (format t "Bidirectional Breadth First Search Path: -> ~s.~%~%"
    (breadth-first-search goal-node start-node) )
  (format t " Total nodes expanded = ~s nodes ~% "
    (get-count) )
 ;;; (format t "Path cost (1KM each path) = ~s km ~%~% " 
 ;;;   (- (length '('ARAD 'SIBIU 'FAGARAS 'BUCHAREST 'URZICENI)) 1))
 
)
(test)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cost()
  (format t "Path cost (1KM each path) = ~s km ~%~% " 
    (- (length ( breadth-first-search goal-node start-node) ) 1))
   (format t "Path cost (1KM each path) = ~s km ~%~% " 
    (- (length ( breadth-first-search start-node goal-node) ) 1))
  
)
;;(cost)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;To run we must used the sample output as below, if wan to change goal state
;;;and initial state make changes to the code above
;;;CG-USER(13): (find-path 'U #'goal-state #'neighbors #'same-state)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;User Guide;;
;; (test)
;; (cost)