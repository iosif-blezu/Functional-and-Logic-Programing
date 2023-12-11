; 5. Return the level (depth) of a node in a tree of type (1). The level of the root element is 0.

    ; traverse_left(sublist, numNodes, numEdges) = 
    ; = nil, if sublist is empty
    ; = nil, if numNodes = 1 + numEdges
    ; = {first element} U {second element} U traverse_left(rest of sublist, numNodes + 1, second element + numEdges), otherwise
(defun traverse_left (sublist numNodes numEdges)
  (cond
    ((null sublist) nil)
    ((= numNodes (+ 1 numEdges)) nil)
    (t (cons (car sublist) (cons (cadr sublist) (traverse_left (cddr sublist) (+ 1 numNodes) (+ (cadr sublist) numEdges)))))))

    ; traverse_right(sublist, numNodes, numEdges) =
    ; = nil, if sublist is empty
    ; = sublist, if numNodes = 1 + numEdges
    ; = traverse_right(rest of sublist, numNodes + 1, numEdges + second element), otherwise
(defun traverse_right (sublist numNodes numEdges)
  (cond
    ((null sublist) nil)
    ((= numNodes (+ 1 numEdges)) sublist)
    (t (traverse_right (cddr sublist) (+ 1 numNodes) (+ (cadr sublist) numEdges)))))

    ; left(sublist) = 
    ; = traverse_left(skip first two elements of sublist, 0, 0, the node and the number of its children)
(defun left(sublist)
  (traverse_left (cddr sublist) 0 0))

    ; right(sublist) =
    ; = traverse_right(skip first two elements of sublist, 0, 0, the node and the number of its children)
(defun right(sublist)
  (traverse_right (cddr sublist) 0 0))

    ; findDepth(sublist, element, currentLevel) = 
    ; = nil, if sublist is empty
    ; = currentLevel, if first element is the target
    ; = findDepth(left(sublist), element, currentLevel + 1) or findDepth(right(sublist), element, currentLevel + 1), otherwise
(defun findDepth (sublist element currentLevel)
  (cond
    ((null sublist) nil) ;                                             ; If the sublist is empty, the element is not found.
    ((equal (car sublist) element) currentLevel)                       ; If the first element is the target, return the current level.
    (t (or (findDepth (left sublist) element (+ 1 currentLevel))       ; Search the left subtree, then the right subtree if not found, increast the currentLevel each time
           (findDepth (right sublist) element (+ 1 currentLevel))))))

    ; main(treeList, searchElement) = findDepth(treeList, searchElement, 0)
(defun main(treeList searchElement)
  (findDepth treeList searchElement 0))

(print "Tree 1: (A 2 B 0 C 2 D 0 E 0) ")
(print (main '(A 2 B 0 C 2 D 0 E 0) 'A)) ; Should return 0
(print (main '(A 2 B 0 C 2 D 0 E 0) 'B)) ; Should return 1
(print (main '(A 2 B 0 C 2 D 0 E 0) 'C)) ; Should return 1
(print (main '(A 2 B 0 C 2 D 0 E 0) 'D)) ; Should return 2
(print (main '(A 2 B 0 C 2 D 0 E 0) 'E)) ; Should return 2
(print "Tree 2: (A 2 B 1 D 0 C 2 E 2 G 0 H 0 F 0) ")
(print (main '(A 2 B 1 D 0 C 2 E 2 G 0 H 0 F 0) 'A)) ;        A
(print (main '(A 2 B 1 D 0 C 2 E 2 G 0 H 0 F 0) 'B)) ;       / \
(print (main '(A 2 B 1 D 0 C 2 E 2 G 0 H 0 F 0) 'C)) ;     B   C
(print (main '(A 2 B 1 D 0 C 2 E 2 G 0 H 0 F 0) 'D)) ;    /   / \
(print (main '(A 2 B 1 D 0 C 2 E 2 G 0 H 0 F 0) 'E)) ;   D   E   F
(print (main '(A 2 B 1 D 0 C 2 E 2 G 0 H 0 F 0) 'F)) ;      / \
(print (main '(A 2 B 1 D 0 C 2 E 2 G 0 H 0 F 0) 'G)) ;     G   H
(print (main '(A 2 B 1 D 0 C 2 E 2 G 0 H 0 F 0) 'H))
(print "Tree 3: (A 2 B 1 D 1 E 1 F 2 G 0 H 2 J 0 K 0 C 2 L 2 N 0 Q 2 P 0 R 0 M 0) ")
(terpri)
(princ "A: ")
(write (main '(A 2 B 1 D 1 E 1 F 2 G 0 H 2 J 0 K 0 C 2 L 2 N 0 Q 2 P 0 R 0 M 0) 'A))
(terpri)
(princ "B: ")
(write (main '(A 2 B 1 D 1 E 1 F 2 G 0 H 2 J 0 K 0 C 2 L 2 N 0 Q 2 P 0 R 0 M 0) 'B))
(terpri)
(princ "C: ")
(write (main '(A 2 B 1 D 1 E 1 F 2 G 0 H 2 J 0 K 0 C 2 L 2 N 0 Q 2 P 0 R 0 M 0) 'C))
(terpri)
(princ "D: ")
(write (main '(A 2 B 1 D 1 E 1 F 2 G 0 H 2 J 0 K 0 C 2 L 2 N 0 Q 2 P 0 R 0 M 0) 'D))
(terpri)
(princ "E: ")
(write (main '(A 2 B 1 D 1 E 1 F 2 G 0 H 2 J 0 K 0 C 2 L 2 N 0 Q 2 P 0 R 0 M 0) 'E))
(terpri)
(princ "F: ")
(write (main '(A 2 B 1 D 1 E 1 F 2 G 0 H 2 J 0 K 0 C 2 L 2 N 0 Q 2 P 0 R 0 M 0) 'F))
(terpri)
(princ "G: ")
(write (main '(A 2 B 1 D 1 E 1 F 2 G 0 H 2 J 0 K 0 C 2 L 2 N 0 Q 2 P 0 R 0 M 0) 'G))
(terpri)
(princ "H: ")
(write (main '(A 2 B 1 D 1 E 1 F 2 G 0 H 2 J 0 K 0 C 2 L 2 N 0 Q 2 P 0 R 0 M 0) 'H))
(terpri)
(princ "J: ")
(write (main '(A 2 B 1 D 1 E 1 F 2 G 0 H 2 J 0 K 0 C 2 L 2 N 0 Q 2 P 0 R 0 M 0) 'J))
(terpri)
(princ "K: ")
(write (main '(A 2 B 1 D 1 E 1 F 2 G 0 H 2 J 0 K 0 C 2 L 2 N 0 Q 2 P 0 R 0 M 0) 'K))
(terpri)
(princ "L: ")
(write (main '(A 2 B 1 D 1 E 1 F 2 G 0 H 2 J 0 K 0 C 2 L 2 N 0 Q 2 P 0 R 0 M 0) 'L))
(terpri)
(princ "M: ")
(write (main '(A 2 B 1 D 1 E 1 F 2 G 0 H 2 J 0 K 0 C 2 L 2 N 0 Q 2 P 0 R 0 M 0) 'M))
(terpri)
(princ "N: ")
(write (main '(A 2 B 1 D 1 E 1 F 2 G 0 H 2 J 0 K 0 C 2 L 2 N 0 Q 2 P 0 R 0 M 0) 'N))
(terpri)
(princ "P: ")
(write (main '(A 2 B 1 D 1 E 1 F 2 G 0 H 2 J 0 K 0 C 2 L 2 N 0 Q 2 P 0 R 0 M 0) 'P))
(terpri)
(princ "Q: ")
(write (main '(A 2 B 1 D 1 E 1 F 2 G 0 H 2 J 0 K 0 C 2 L 2 N 0 Q 2 P 0 R 0 M 0) 'Q))
(terpri)
(princ "R: ")
(write (main '(A 2 B 1 D 1 E 1 F 2 G 0 H 2 J 0 K 0 C 2 L 2 N 0 Q 2 P 0 R 0 M 0) 'R))
