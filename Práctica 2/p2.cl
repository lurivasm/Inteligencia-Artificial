;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    Lab assignment 2: Search
;;
;;    Solutions
;;    Created:  Simone Santini, 2019/03/05
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    Problem definition
;;
(defstruct problem
  states               ; List of states
  initial-state        ; Initial state
  f-h                  ; reference to a function that evaluates to the 
                       ; value of the heuristic of a state
  f-goal-test          ; reference to a function that determines whether 
                       ; a state fulfils the goal 
  f-search-state-equal ; reference to a predictate that determines whether
                       ; two nodes are equal, in terms of their search state      
  operators)           ; list of operators (references to functions) to 
                       ; generate successors
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    Node in search tree
;;
(defstruct node 
  state           ; state label
  parent          ; parent node
  action          ; action that generated the current node from its parent
  (depth 0)       ; depth in the search tree
  (g 0)           ; cost of the path from the initial state to this node
  (h 0)           ; value of the heurstic
  (f 0))          ; g + h 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    Actions 
;;
(defstruct action
  name              ; Name of the operator that generated the action
  origin            ; State on which the action is applied
  final             ; State that results from the application of the action
  cost )            ; Cost of the action
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    Search strategies 
;;
(defstruct strategy
  name              ; name of the search strategy
  node-compare-p)   ; boolean comparison
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    END: Define structures
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    BEGIN: Define galaxy
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *cities* '(Calais Reims Paris Nancy Orleans 
                                St-Malo Brest Nevers Limoges 
                                Roenne Lyon Toulouse Avignon Marseille))

(defparameter *trains*
  '((Paris Calais (34.0 60.0))      (Calais Paris (34.0 60.0))
    (Reims Calais (35.0 70.0))      (Calais Reims (35.0 70.0))
    (Nancy Reims (35.0 55.0))       (Reims Nancy (35.0 55.0))
    (Paris Nancy (40.0 67.0))       (Nancy Paris (40.0 67.0))
    (Paris Nevers (48.0 75.0))      (Nevers Paris (48.0 75.0))
    (Paris Orleans (23.0 38.0))     (Orleans Paris (23.0 38.0))
    (Paris St-Malo (40.0 70.0))     (St-Malo Paris (40.0 70.0))
    (St-Malo Nantes (20.0 28.0))    (Nantes St-Malo (20.0 28.0))
    (St-Malo Brest (30.0 40.0))     (Brest St-Malo (30.0 40.0))
    (Nantes Brest (35.0 50.0))      (Brest Nantes (35.0 50.0))
    (Nantes Orleans (37.0 55.0))    (Orleans Nantes (37.0 55.0))
    (Nantes Toulouse (80.0 130.0))  (Toulouse Nantes (80.0 130.0))
    (Orleans Limoges (55.0 85.0))   (Limoges Orleans (55.0 85.0))
    (Limoges Nevers (42.0 60.0))    (Nevers Limoges (42.0 60.0))
    (Limoges Toulouse (25.0 35.0))  (Toulouse Limoges (25.0 35.0))
    (Toulouse Lyon (60.0 95.0))     (Lyon Toulouse (60.0 95.0))
    (Lyon Roenne (18.0 25.0))       (Roenne Lyon  (18.0 25.0))
    (Lyon Avignon (30.0 40.0))      (Avignon Lyon (30.0 40.0))
    (Avignon Marseille (16.0 25.0)) (Marseille Avignon (16.0 25.0))
    (Marseille Toulouse (65.0 120.0)) (Toulouse Marseille (65.0 120.0)))) 
    

(defparameter *canals*
  '((Reims Calais (75.0 15.0)) (Paris Reims (90.0 10.0))
    (Paris Nancy (80.0 10.0)) (Nancy reims (70.0 20.0))
    (Lyon Nancy (150.0 20.0)) (Nevers Paris (90.0 10.0))
    (Roenne Nevers (40.0 5.0)) (Lyon Roenne (40.0 5.0))
    (Lyon Avignon (50.0 20.0)) (Avignon Marseille (35.0 10.0))
    (Nantes St-Malo (40.0 15.0)) (St-Malo Brest (65.0 15.0))
    (Nantes Brest (75.0 15.0))))



(defparameter *estimate* 
  '((Calais (0.0 0.0)) (Reims (25.0 0.0)) (Paris (30.0 0.0)) 
    (Nancy (50.0 0.0)) (Orleans (55.0 0.0)) (St-Malo (65.0 0.0))
    (Nantes (75.0 0.0)) (Brest (90.0 0.0)) (Nevers (70.0 0.0)) 
    (Limoges (100.0 0.0)) (Roenne (85.0 0.0)) (Lyon (105.0 0.0))
    (Toulouse (130.0 0.0)) (Avignon (135.0 0.0)) (Marseille (145.0 0.0))))

(defparameter *origin* 'Marseille)

(defparameter *destination* '(Calais))

(defparameter *forbidden*  '(Avignon))

(defparameter *mandatory* '(Paris))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; BEGIN: Exercise 1 -- Evaluation of the heuristics
;;
;; Returns the value of the heuristics for a given state
;;
;;  Input:
;;    state: the current state (vis. the planet we are on)
;;    sensors: a sensor list, that is a list of pairs
;;                (state (time-est cost-est) )
;;             where the first element is the name of a state and the second
;;             a number estimating the costs to reach the goal
;;
;;  Returns:
;;    The cost (a number) or NIL if the state is not in the sensor list
;;
;;  It is necessary to define two functions: the first which returns the 
;;  estimate of teh travel time, the second which returns the estimate of 
;;  the cost of travel

(defun f-h-time (state sensors)
	(if 
		(assoc state sensors) 								;; Si existe alguna lista en sensors que tenga la información sobre state,
			(first (second (assoc state sensors)))			;; devolvemos el primer elemento de la sublista (tiempo, dinero); si no 
			nil ))												;; está, devolvemos nil


(defun f-h-price (state sensors)
	(if 
		(assoc state sensors) 								;; Si existe alguna lista en sensors que tenga la información sobre state,
			(second (second (assoc state sensors)))			;; devolvemos el segundo elemento de la sublista (tiempo, dinero); si no 
			nil	))											;; está, devolvemos nil
  
;;
;; END: Exercise 1 -- Evaluation of the heuristic
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BEGIN: Exercise 2 -- Navigation operators
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; General navigation function
;;
;;  Returns the actions that can be carried out from the current state
;;
;;  Input:
;;    state:      the state from which we want to perform the action 
;;    lst-edges:  list of edges of the graph, each element is of the 
;;                form: (source destination (cost1 cost2))
;;    c-fun:      function that extracts the correct cost (time or price)
;;                from the pair that appears in the edge
;;    name:       name to be given to the actions that are created (see the 
;;                action structure)
;;    forbidden-cities:  
;;                list of the cities where we can't arrive by train
;;
;;  Returns
;;    A list of action structures with the origin in the current state and
;;    the destination in the states to which the current one is connected
;;
(defun navigate (state lst-edges cfun  name &optional forbidden )
  (when (not (null lst-edges))														; Si la lista de conexiones del grafo es nil devuelve nil
		(let ((edge (car lst-edges)))												; Llamamos edge a la primera conexión de la lista
			(cond 
				((eql state (car edge))												; Si el estado del cual buscamos las conexiones es el primer 																					
					(if (member (cadr edge) forbidden)								; elemento de edge, en el caso de que el destino este en la 
						(navigate state (cdr lst-edges) 							; lista de prohibidos , seguimos con el resto de las conexiones;
										cfun name forbidden)						; en caso contrario, creamos la acción y la concatenamos con el 
						(cons 														; resultado de llamar nuevamente a navigate sobre el resto de 
							(make-action :name name									; la lista de conexiones
										 :origin state								
										 :final (cadr edge)
										 :cost (funcall cfun (caddr edge)))
							(navigate state (cdr lst-edges) 
											cfun name forbidden))))					
				(t (navigate state (cdr lst-edges) 									; En cualquier otro caso, continúa con el resto de la lista de 
									cfun name forbidden))))))						; conexiones
			

			

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Navigation by canal
;;
;; This is a specialization of the general navigation function: given a 
;; state and a list of canals, returns a list of actions to navigate
;; from the current city to the cities reachable from it by canal navigation.
;;
(defun navigate-canal-time (state canals)
	(navigate state canals #'car 'canals-time '())
  )

(defun navigate-canal-price (state canals)
	(navigate state canals #'cadr 'canals-price '())
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Navigation by train
;;
;; This is a specialization of the general navigation function: given a 
;; state and a list of train lines, returns a list of actions to navigate
;; from the current city to the cities reachable from it by train.
;; 
;; Note that this function takes as a parameter a list of forbidden cities.
;;
(defun navigate-train-time (state trains forbidden)
	(navigate state trains #'car 'train-time forbidden)
  )
  
(defun navigate-train-price (state trains forbidden)
	(navigate state trains #'cadr 'train-price forbidden)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BEGIN: Exercise 3 -- Goal test
;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Goal test
;;
;;  Returns T or NIl depending on whether a path leads to a final state
;;
;;  Input:
;;    node:       node structure that contains, in the chain of parent-nodes,
;;                a path starting at the initial state
;;    destinations: list with the names of the destination cities
;;    mandatory:  list with the names of the cities that is mandatoryu to visit
;;
;;  Returns
;;    T: the path is a valid path to the final state
;;    NIL: invalid path: either the final city is not a destination or some
;;         of the mandatory cities are missing from the path.
;;
(defun f-goal-test (node destination mandatory) 
	(if (member (node-state node) destination)
		(mandatory-rec node mandatory)				 
		nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; madatory-rec
;;
;;  Returns T or NIl depending on whether the final stage has visited all the 
;;  mandatory cities
;;
;;  Input:
;;    node:       node structure that contains, in the chain of parent-nodes,
;;                a path starting at the initial state
;;    mandatory:  list with the names of the cities that is mandatory to visit
;;
;;  Returns
;;    T: the path is a valid path to the final state
;;    NIL: invalid path: either the final city is not a destination or some
;;         of the mandatory cities are missing from the path.
;;
(defun mandatory-rec (node mandatory)
	(cond 
		((null mandatory) t)											; Si la lista mandatory está vacía, devuelve true pues todos han sido visitados
		((null (node-parent node)) nil)									; Si el padre del nodo actual es nil, es el nodo origen, devuelve nil porque
		(t  															; no ha visitado todos los nodos de mandatory
			(mandatory-rec (node-parent node) 
						   (remove (node-state node) mandatory)))))     ; En otro caso, devuelve recursivamente la misma función con con nodo padre
																		; y eliminando el nodo actual de mandatory
;;
;; END: Exercise 3 -- Goal test
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BEGIN: Exercise 4 -- Equal predicate for search states
;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Determines if two nodes are equivalent with respect to the solution
;; of the problem: two nodes are equivalent if they represent the same city 
;, and if the path they contain includes the same mandatory cities.
;;  Input:
;;    node-1, node-1: the two nodes that we are comparing, each one
;;                    defining a path through the parent links
;;    mandatory:  list with the names of the cities that is mandatory to visit
;;
;;  Returns
;;    T: the two ndoes are equivalent
;;    NIL: The nodes are not equivalent
;;
(defun f-search-state-equal (node-1 node-2 &optional mandatory)
	(if (eql (node-state node-1)									; Primero comparamos el estado de los nodos (el nombre de las ciudades)
			   (node-state node-2))									; y si son iguales llamamos a la funcion mandatory-equal para comparar
		(mandatory-equal (generate-mandatory node-1 mandatory) 		; las ciudades obligatorias que han visitado
						 (generate-mandatory node-2 mandatory))
		nil))														; Si no tienen el mismo nombre directamente devolvemos nil

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Generate the list of mandatory and not visited cities by the node "node"
;;  Input:
;;    node : we want the mandatory list of this node
;;    mandatory:  list with the names of the cities that is mandatory to visit
;;
;;  Returns
;;    mandatory: the mandatory cities the node has not visited
;;    NIL: the node has visited all the mandatory nodes
;;
(defun generate-mandatory (node mandatory)
	(cond 
		((null mandatory) '())											; Si la lista mandatory está vacía, devuelve nil pues todos han sido visitados
		((null (node-parent node)) mandatory)							; Si el padre del nodo actual es nil, es el nodo origen, devuelve mandatory, es 
		(t  															; decir, devuelve la lista de nodos no visitados de los obligados
			(generate-mandatory (node-parent node) 
						(remove (node-state node) mandatory)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Compares lis1 and list2
;;  Input:
;;    list1 : list with the mandatory and visited cities of node-1
;;    list2 : list with the mandatory and visited cities of node-2
;;
;;  Returns
;;    t :the lists are equal
;;    NIL: the lists are not equal
;;
(defun mandatory-equal (list1 list2)
	(cond 
		((null list1)											; Si list1 y list2 estan vacias devuelve t porque son iguales
			(if (null list2) t									; Si list1 esta vacia y list 2 no esta vacia devuelve nil porque no son iguales
							 nil))								
		((null (member (first list1) list2)) nil)				; Si el primer elemento de list1 no esta en list2 devuelve nil porque no son iguales
		(t 
			(mandatory-equal (rest list1)						; En otro caso devuelve recursivamente la misma funcion con el resto de list1 y 
							 (remove (first list1) list2)))))	; eliminando de list2 el primer elemento de list1  porque hemos comprobado que esta

(defparameter node-nevers
   (make-node :state 'Nevers) )
(defparameter node-paris
   (make-node :state 'Paris :parent node-nevers))
(defparameter node-nancy
   (make-node :state 'Nancy :parent node-paris))
(defparameter node-reims
   (make-node :state 'Reims :parent node-nancy))
(defparameter node-calais
   (make-node :state 'Calais :parent node-reims))
(defparameter node-calais-2
   (make-node :state 'Calais :parent node-paris))

(f-search-state-equal node-calais node-calais-2 '()) ;-> T
(f-search-state-equal node-calais node-calais-2 '(Reims)) ;-> NIL
(f-search-state-equal node-calais node-calais-2 '(Nevers)) ;-> T
(f-search-state-equal node-nancy node-paris '()) ;-> NIL

;;
;; END: Exercise 4 -- Equal predicate for search states
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BEGIN: Exercise 5 -- Define the problem structure
;;
;;
;;  Note that the connectivity of the netowrk using canals and trains
;;  holes is implicit in the operators: there is a list of two
;;  operators, each one takes a single parameter: a state name, and
;;  returns a list of actions, indicating to which states one can move
;;  and at which cost. The lists of edges are placed as constants as
;;  the second parameter of the navigate operators. 
;;
;;  There are two problems defined: one minimizes the travel time,
;;  the other minimizes the cost

(defparameter *travel-cheap*
	(make-problem
		:states 				*cities*																		; Lista de ciudades
		:initial-state 		*origin*																		; Ciudad desde donde partimos
		:f-h 				#'(lambda (state) (f-h-price state *estimate*))										; Heuristica basada en el precio
		:f-goal-test 			#'(lambda (node) (f-goal-test  node *destination* *mandatory*))							; Evalua si hemos llegado al final
		:f-search-state-equal 	#'(lambda (node-1 node-2) (f-search-state-equal node-1 node-2 *mandatory*))				; Evalua nodos de igual estado
		:operators 			(list
								 #'(lambda (node) (navigate-canal-price (node-state node) *canals*))					; Operador por canales y precio
								 #'(lambda (node) (navigate-train-price (node-state node) *trains* *forbidden*)))))		; Operador por trenes y precio

(defparameter *travel-fast*
	(make-problem
		:states 				*cities*																		; Lista de ciudades
		:initial-state 		*origin*																		; Ciudad desde donde partimos
		:f-h 				#'(lambda (state) (f-h-time state *estimate*))										; Heuristica basada en el tiempo
		:f-goal-test 			#'(lambda (node) (f-goal-test node *destination* *mandatory*))							; Evalua si hemos llegado al final
		:f-search-state-equal 	#'(lambda (node-1 node-2) (f-search-state-equal node-1 node-2 *mandatory*))				; Evalua nodos de igual estado
		:operators 			(list
								#'(lambda (node) (navigate-canal-time (node-state node) *canals*))					; Operador por canales y tiempo
								#'(lambda (node) (navigate-train-time (node-state node) *trains* *forbidden*)))))		; Operador por trenes y tiempo


;;
;;  END: Exercise 5 -- Define the problem structure
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BEGIN Exercise 6: Expand node
;;
;; The main function of this section is "expand-node", which receives
;; a node structure (the node to be expanded) and a problem structure.
;; The problem structure has a list of navigation operators, and we
;; are interested in the states that can be reached using anuy one of
;; them.
;;
;; So, in the expand-node function, we iterate (using mapcar) on all
;; the operators of the problem and, for each one of them, we call
;; expand-node-operator, to determine the states that can be reached
;; using that operator.
;;
;; The operator gives us back a list of actions. We iterate again on
;; this list of action and, for each one, we call expand-node-action
;; that creates a node structure with the node that can be reached
;; using that action.
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;  Creates a list with all the nodes that can be reached from the
;;  current one using all the operators in a given problem
;;
;;  Input:
;;    node:   the node structure from which we start.
;;    problem: the problem structure with the list of operators
;;
;;  Returns:
;;    A list (node_1,...,node_n) of nodes that can be reached from the
;;    given one
;;
(defun expand-node (node problem)
	(let ((list1 (funcall (first (problem-operators problem)) node))				; List1 es la lista  de acciones generada por el primer operador del problema,
		 (list2 (funcall (second (problem-operators problem)) node)))			; mientras que list2 es la generada por el segundo operador

	(if (null list1)													; Si la primera lista es null y la segunda también, se devuelve nil
		(if (null list2) 
			nil
			(create-node-list list2 node problem))									; Si la segunda no es nil, se crea la lista de nodos a raiz de list2
		(if (null list2)												; Si la primera lita no es null, y la segunda si , se crea la lista de nodos a raiz 
			(create-node-list list1 node problem)										; de list1
			(create-node-list (append list1 list2) node problem)))))						; Si ambas no son nil, se crea la lista de nodos a raiz de la unión de list1 y list2


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;  Creates a list with all the nodes that can be reached from the
;;  current one using all the operators in a given problem and the list 
;;  of all the possible actions
;;
;;  Input:
;;	 actions : list of all the actions from the node
;;    node:   the node structure from which we start.
;;    problem: the problem structure with the list of operators
;;
;;  Returns:
;;    A list (node_1,...,node_n) of nodes that can be reached from the
;;    given one
;;
(defun create-node-list (actions node problem)
	(let ((action (first actions)))
	(cond
		((null actions) '())
		(t (cons (make-node 
				:state	(action-final action)
				:parent 	node
				:action 	action
				:depth	(+ 1 (node-depth node))
				:g		(+ (action-cost action) (node-g node))
				:h		(funcall (problem-f-h problem) 
							    (action-final action))
				:f		(+ (+ (action-cost action) (node-g node))
						   (funcall (problem-f-h problem) 
							       (action-final action))))
				(create-node-list (rest actions) node problem))))))
				

(defparameter node-marseille-ex6
   (make-node :state 'Marseille :depth 12 :g 10 :f 20) )

(defparameter lst-nodes-ex6
  (expand-node node-marseille-ex6 *travel-fast*))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  BEGIN Exercise 7 -- Node list management
;;;  
;;;  Merges two lists of nodes, one of them ordered with respect to a
;;;  given strategy, keeping the result ordered with respect to the
;;;  same strategy.
;;;
;;;  This is the idea: suppose that the ordering is simply the
;;;  ordering of natural numbers. We have a "base" list that is
;;;  already ordered, for example:
;;;      lst1 --> '(3 6 8 10 13 15)
;;;
;;;  and a list that is not necessarily ordered:
;;;
;;;      nord --> '(11 5 9 16)
;;;
;;;  the call (insert-nodes nord lst1 #'<) would produce
;;;
;;;    (3 5 6 8 9 10 11 13 15 16)
;;;
;;;  The functionality is divided in three functions. The first,
;;;  insert-node, inserts a node in a list keeping it ordered. The
;;;  second, insert-nodes, insert the nodes of the non-ordered list
;;;  into the ordered, one by one, so that the two lists are merged.
;;;  The last function, insert-node-strategy is a simple interface that
;;;  receives a strategy, extracts from it the comparison function, 
;;;  and calls insert-nodes



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Inserts a node in an ordered list keeping the result list
;; ordered with respect to the given comparison function
;;
;; Input:
;;    node: the node to be inserted in the
;;           other list
;;    left: the list of nodes than will be at the left side of the node
;;    right: the list of nodes than will be at the right side of the node
;;    node-compare-p: a function node x node --> 2 that returns T if the 
;;                    first node comes first than the second.
;;
;; Returns:
;;    An ordered list of nodes which includes the node given and 
;;    those of the list "nodes@. The list is ordered with respect to the 
;;   criterion node-compare-p.
;; 
(defun insert-node (node left right node-compare-p)
	(if (null right) 												; Si right es null, significa que el nodo va a final de la lista
			(append left (list node))
			(if (funcall node-compare-p (first right) node)				; Si el primer nodo de right va antes que node, lo metemos en left
				(insert-node node 									; y llamamos a la función sobre el reto de la lista
						  (append left (list (first right)))
						  (rest right) node-compare-p)
				(append left (list node) right))))						; Si node va antes, juntamos left con el nodo y con rigt, resultando la lista ordenada
		

(insert-node 11 '() '(3 6 8 10 13) #'<) ; -> (3 6 8 10 11 13)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Inserts a list of nodes in an ordered list keeping the result list
;; ordered with respect to the given comparison function
;;
;; Input:
;;    nodes: the (possibly unordered) node list to be inserted in the
;;           other list
;;    lst-nodes: the (ordered) list of nodes in which the given nodes 
;;               are to be inserted
;;    node-compare-p: a function node x node --> 2 that returns T if the 
;;                    first node comes first than the second.
;;
;; Returns:
;;    An ordered list of nodes which includes the nodes of lst-nodes and 
;;    those of the list "nodes@. The list is ordered with respect to the 
;;   criterion node-compare-p.
;; 
(defun insert-nodes (nodes lst-nodes node-compare-p)
	(if (null nodes)
		lst-nodes
		(insert-nodes (rest nodes) 
				    (insert-node (first nodes) 
							  '() lst-nodes node-compare-p)
				    node-compare-p)))

 (insert-nodes '(4 7 3 11 15) '(1 5 10 13) #'<) ;-> (1 3 4 5 7 10 11 13 15)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Inserts a list of nodes in an ordered list keeping the result list
;; ordered with respect the given strategy
;;
;; Input:
;;    nodes: the (possibly unordered) node list to be inserted in the
;;           other list
;;    lst-nodes: the (ordered) list of nodes in which the given nodes 
;;               are to be inserted
;;    strategy: the strategy that gives the criterion for node
;;              comparison
;;
;; Returns:
;;    An ordered list of nodes which includes the nodes of lst-nodes and 
;;    those of the list "nodes@. The list is ordered with respect to the 
;;    criterion defined in te strategy.
;; 
;; Note:
;;   You will note that this function is just an interface to
;;   insert-nodes: it allows to call using teh strategy as a
;;   parameter; all it does is to "extract" the compare function and
;;   use it to call insert-nodes.
;;
(defun insert-nodes-strategy (nodes lst-nodes strategy)
	(insert-nodes nodes lst-nodes (strategy-node-compare-p strategy)))
  

(defun node-g-<= (node-1 node-2)
  (<= (node-g node-1)
      (node-g node-2)))

(defparameter *uniform-cost*
  (make-strategy
   :name 'uniform-cost
   :node-compare-p #'node-g-<=))

(defparameter node-paris-ex7
  (make-node :state 'Paris :depth 0 :g 0 :f 0) )

(defparameter node-nancy-ex7
  (make-node :state 'Nancy :depth 2 :g 50 :f 50) )


(defparameter sol-ex7 (insert-nodes-strategy (list node-paris-ex7 node-nancy-ex7) 
                                             lst-nodes-ex6
                                             *uniform-cost*))

(mapcar #'(lambda (x) (node-state x)) sol-ex7) ; -> (PARIS NANCY TOULOUSE)
(mapcar #'(lambda (x) (node-g x)) sol-ex7) ; -> (0 50 75)


;;
;;    END: Exercize 7 -- Node list management
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BEGIN: Exercise 8 -- Definition of the A* strategy
;;
;; A strategy is, basically, a comparison function between nodes to tell 
;; us which nodes should be analyzed first. In the A* strategy, the first 
;; node to be analyzed is the one with the smallest value of g+h
;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Compares the f value of two nodes
;; Input:
;;    node1: first node
;;    node2 : second node
;;
;; Returns: T if the f value of node1 is smaller or equal to the f value
;;		  of node2, nil otherwise
;;  
(defun node-f-<= (node1 node2)
	(<= (node-f node1)
	    (node-f node2)))

(defparameter *A-star*
  (make-strategy 
	:name 'A-star
	:node-compare-p #'node-f-<= ))

;;
;; END: Exercise 8 -- Definition of the A* strategy
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;    BEGIN Exercise 9: Search algorithm
;;;
;;;    Searches a path that solves a given problem using a given search
;;;    strategy. Here too we have two functions: one is a simple
;;;    interface that extracts the relevant information from the
;;;    problem and strategy structure, builds an initial open-nodes
;;;    list (which contains only the starting node defined by the
;;;    state), and initial closed node list (the empty list), and calls
;;;    the auxiliary function.
;;;
;;;    The auxiliary is a recursive function that extracts nodes from
;;;    the open list, expands them, inserts the neighbors in the
;;;    open-list, and the expanded node in the closed list. There is a
;;;    caveat: with this version of the algorithm, a node can be
;;;    inserted in the open list more than once. In this case, if we
;;;    extract a node in the open list and the following two condition old:
;;;
;;;     the node we extract is already in the closed list (it has
;;;     already been expanded)
;;;       and
;;;     the path estimation that we have is better than the one we
;;;     obtain from the node in the open list
;;;
;;;     then we ignore the node.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Auxiliary search function (the one that actually does all the work
;;
;;  Input:
;;    problem: the problem structure from which we get the general 
;;             information (goal testing function, action operatos, etc.
;;    open-nodes: the list of open nodes, nodes that are waiting to be
;;                visited
;;    closed-nodes: the list of closed nodes: nodes that have already
;;                  been visited
;;    strategy: the strategy that decide which node is the next extracted
;;              from the open-nodes list
;;
;;    Returns:
;;     NIL: no path to the destination nodes
;;     If these is a path, returns the node containing the final state.
;;
;;     Note that what is returned is quite a complex structure: the
;;     node contains in "parent" the node that comes before in the
;;     path, that contains another one in "parents" and so on until
;;     the initial one. So, what we have here is a rather complex
;;     nested structure that contains not only the final node but the
;;     whole path from the starting node to the final.
;;
(defun graph-search-aux (problem open-nodes closed-nodes strategy)
	(if (null open-nodes)															;; Si la lista de abiertos está vacía, se devuelve nil
		nil
		(let ((node (first open-nodes)))												;; En otro caso, si el nodo a expandir es un nodo final, lo devolvemos
			(if (funcall (problem-f-goal-test problem) node)
				node
				(let ((rep (member-if 												;; Si no es final, buscamos si hay ya un nodo de busqueda igual en la lista.
						  #'(lambda (x) (funcall 									;; Si lo hay, member-if devolverá una lista cuyo primer elemento será este nodo
								(problem-f-search-state-equal problem) 
									node x))
						  closed-nodes)))
				(if rep															;; En caso de que rep no sea nil, miramos si este nodo de busqueda encontrado 
					(if (> (node-g node) (node-g (first rep)))							;; tiene un coste g menor que el del nodo a explorar. En tal caso, no modificamos
						(graph-search-aux problem (rest open-nodes)						;; la lista , no expandimos el nodo y seguimos
									   closed-nodes strategy)

						(graph-search-aux problem 									;; En caso de que sea mayor, llamamos a la función añadiendo a open-nodes los nodos
								   (insert-nodes-strategy 							;; obtenidos al expandir node, quitando de closed-nodes el primer elemento de rep
										(expand-node node problem) 					;; , y añadiendo al principio el nodo explorado
										 (rest open-nodes) strategy)					
								   (cons node 
									   (remove (first rep) closed-nodes))
								   strategy))

				(graph-search-aux problem 											;; Si rep es nil, llamamos a la función añadiendo a open-nodes los nodos
						(insert-nodes-strategy 										;; obtenidos al expandir node, y añadiendo node a closed-nodes
							 (expand-node node problem) 					
							 (rest open-nodes) strategy)					
						(cons node closed-nodes) strategy)))))))
							 
																		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Interface function for the graph search. 
;;
;;  Input:
;;    problem: the problem structure from which we get the general 
;;             information (goal testing function, action operatos,
;;             starting node, heuristic, etc.
;;    strategy: the strategy that decide which node is the next extracted
;;              from the open-nodes list
;;
;;    Returns:
;;     NIL: no path to the destination nodes
;;     If these is a path, returns the node containing the final state.
;;
;;    See the graph-search-aux for the complete structure of the
;;    returned node. 
;;    This function simply prepares the data for the auxiliary
;;    function: creates an open list with a single node (the source)
;;    and an empty closed list.
;;
(defun graph-search (problem strategy)
	(graph-search-aux problem (list (make-node 
								:state (problem-initial-state problem) 
								:parent nil
								:action nil
								:h (funcall (problem-f-h problem) *origin*)
								:f (funcall (problem-f-h problem) *origin*)))
						  '() strategy))
  

;
;  A* search is simply a function that solves a problem using the A* strategy
;
(defun a-star-search (problem)
	(graph-search problem *A-star*))
  


(a-star-search *travel-fast*) ;->
;;
;; END: Exercise 9 -- Search algorithm
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;    BEGIN Exercise 10: Solution path
;;;
;*** solution-path ***

(defun solution-path (node)
  )

;*** action-sequence ***
; Visualize sequence of actions

(defun action-sequence (node)
  )

;;; 
;;;    END Exercise 10: Solution path / action sequence
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
