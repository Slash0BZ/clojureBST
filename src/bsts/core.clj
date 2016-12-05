(ns bsts.core)

(defrecord Node [left key value right])

(defn add
  "Adds key `k` and value `v` to tree `bst`."
  [bst k v]
  (cond (nil? bst) (Node. nil k v nil)
        (< 0 (compare k (:key bst))) (Node. (add (:left bst) k v)
                                   (:key bst)
                                   (:value bst)
                                   (:right bst))
        :else  (Node. (:left bst)
                      (:key bst)
                      (:value bst)
                      (add (:right bst) k v)) ) )

(defn make-tree
  "We may want to change the representation of this later."
  []
  nil)

(defn show [bst]
  (cond (nil? bst) " x "
        (and (nil? (:left bst))
             (nil? (:right bst)))  (str " " (:key bst) "/" (:value bst) " ")
        :otherwise (str "(" (:key bst) "/" (:value bst) " " (show (:left bst)) " "
                        (show (:right bst)) ")")))

(defn find [bst desire]
  (cond (nil? bst) nil
        (= desire (:key bst)) (:value bst)
        (< 0 (compare desire (:key bst))) (find (:left bst) desire)
        :else (find (:right bst) desire) ))


(defn size
  "Return the number of nodes in a BST."
  [bst]
  (cond (nil?bst) 0
        :else (+ 1 (size (:left bst)) (size (:right bst)))))


;;First Version of Delete

(defn delete
  "Deletes the node with `victim` as its key."
  [bst victim]
  (cond (nil? bst) nil
        (= 0 (compare victim (:key bst))) (comNode bst)
        (< 0 (compare victim (:key bst))) (Node. (delete (:left bst) victim)
                                               (:key bst)
                                               (:value bst)
                                               (:right bst))
        :otherwise  (Node. (:left bst)
                      (:key bst)
                     (:value bst)
                      (delete (:right bst) victim)) ))

;;Return the rebuilt version of the deleted node

(defn comNode
  "Return the number of nodes in a BST."
  [bst]
  (cond (nil? bst) nil
        (and
          (nil? (:left bst))
          (not nil? (:right bst))) (:right bst)
        (and
          (nil? (:right bst))
          (not nil? (:left bst))) (:left bst)
        :otherwise (Node. (:left bst)
                          (:key (getIop (:right bst)))
                          (:value (getIop (:right bst)))
                          (iop (:right bst)))))

;;Find the leftmost of the right node

(defn iop
  [bst]
  (cond (nil? bst) nil
        (nil? (:left bst) （:right bst)
        (nil? (:left (:left bst))) (Node. (:right (:left bst))
                                          (:key bst)
                                          (:value bst)
                                          (:right bst))
        :otherwise (Node. (iop (:left bst))
                          (:key bst)
                          (:value bst)
                          (:right bst)))))

;;Get the value of the replacing node
(def getIop
  [bst]
  (cond (nil? bst) nil
        (nil? (:left bst)) bst
        :otherwise: (getIop(:left bst))))



;;Get the value of the replacing node
(defn getIop
  [bst]
  (cond (nil? bst) nil
        (nil? (:left bst)) bst
        :otherwise (getIop(:left bst))))


;;Find the leftmost of the right node

(defn iop
  [bst]
  (cond (nil? bst) nil
        (nil? (:left bst)) (:right bst)
        (nil? (:left (:left bst))) (Node. (:right (:left bst))
                                          (:key bst)
                                          (:value bst)
                                          (:right bst))
        :otherwise (Node. (iop (:left bst))
                          (:key bst)
                          (:value bst)
                          (:right bst))))

(defn comNode
  "Return the number of nodes in a BST."
  [bst]
  (cond (nil? bst) nil
        (and
          (nil? (:left bst))
          (not nil? (:right bst))) (:right bst)
        (and
          (nil? (:right bst))
          (not nil? (:left bst))) (:left bst)
        :otherwise (Node. (:left bst)
                          (:key (getIop (:right bst)))
                          (:value (getIop (:right bst)))
                          (iop (:right bst)))))



(defn size
  "Return the number of nodes in a BST."
  [bst]
  (cond (nil? bst) 0
        :else (+ 1 (size (:left bst)) (size (:right bst)))))


;;First Version of Delete

(defn delete2
  "Deletes the node with `victim` as its key."
  [bst victim]
  (cond (nil? bst) nil
        (= 0 (compare victim (:key bst))) (comNode bst)
        (< 0 (compare victim (:key bst))) (Node. (delete (:left bst) victim)
                                               (:key bst)
                                               (:value bst)
                                               (:right bst))
        :otherwise  (Node. (:left bst)
                      (:key bst)
                     (:value bst)
                      (delete (:right bst) victim)) ))


(defn Min [bst] 
  "Return the iop in a BST."
      (cond (nil? bst) nil
            (nil? (:right bst)) bst 
            :else (Min (:right bst))))

(defn delete
  "Deletes the node with `victim` as its key."
  [bst victim]
    (cond (nil? bst) bst 
          (< 0 (compare victim (:key bst))) (Node. (:left bst)
                (:key bst)
                (:value bst)
                (delete (:right bst) victim))
          (> 0 (compare victim (:key bst))) (Node. (delete (:left bst) victim)
                                  (:key bst)
                                  (:value bst)
                                  (:right bst))
    :otherwise
      ((if (and (nil? (:left bst))
             (nil? (:right bst))) nil)
      (if (nil? (:left bst)) (Node. (:left (:right bst)) 
                                    (:key (:right bst))
                                    (:value (:right bst))
                                    (:right (:right bst))))
      (if (nil? (:right bst)) (Node. (:left (:left bst)) 
                                    (:key (:left bst))
                                    (:value (:left bst))
                                    (:right (:left bst))))
      (Node. (delete (:left bst) (:key (Min (:left bst))))
                (:key (Min (:left bst)))
                (:value (Min (:left bst)))
                (:right bst)))
                        ))