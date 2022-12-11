(defn my-map [func collection]
  (reduce (fn [accCol, value] (conj accCol (func value))) 
          [] 
          collection
  )
)

(defn my-filter [predicat, collection]
  (reduce (fn [accCol, value]
            (if (predicat value)
            (conj accCol value)
            accCol)
          ) 
          [] 
          collection
  )
)

(defn find-alphabet-letters-to-add [alphabet, word]
  (my-filter 
    (fn [alphWord] (not= (last word) alphWord))
    alphabet
  )
)

(defn create-words [alphabet, word]
(my-map
 (fn [alphWord] (conj word alphWord))
 (find-alphabet-letters-to-add alphabet, word)
)
)

(defn new-words-list [alphabet, prevWords]
  (my-map (fn [word] (create-words alphabet (if (coll? word) word [word]))) prevWords)
)

(defn create-words-sequence [alphabet, wordsSequence]
  (reduce
   (fn [acc, word] (concat acc word))
   (new-words-list alphabet wordsSequence)
  )
)

(defn permute [alphabet, sequenceSize]
  (reduce 
   (fn [acc, element] (create-words-sequence alphabet acc))
   '([])
   (range 0 sequenceSize)
   ) 
)

(doseq [item (permute '(a b c) 4)]
   (println item))

(println (count (permute '("a" "b" "c", "e") 4))) ;; 4 * 3 * 3 * 3 == 108