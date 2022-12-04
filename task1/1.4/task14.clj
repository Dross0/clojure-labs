(defn my_map [func collection]
  (reduce (fn [accCol, value] (conj accCol (func value))) 
          [] 
          collection
  )
)

(defn my_filter [predicat, collection]
  (reduce (fn [accCol, value]
            (if (predicat value)
            (conj accCol value)
            accCol)
          ) 
          [] 
          collection
  )
)

(defn findAlphabetLettersToAdd [alphabet, word]
  (my_filter 
    (fn [alphWord] (not= (last word) alphWord))
    alphabet
  )
)

(defn createWords [alphabet, word]
(my_map
 (fn [alphWord] (conj word alphWord))
 (findAlphabetLettersToAdd alphabet, word)
)
)

(defn newWordsList [alphabet, prevWords]
  (my_map (fn [word] (createWords alphabet (if (coll? word) word [word]))) prevWords)
)

(defn createWordsSequence [alphabet, wordsSequence]
  (reduce
   (fn [acc, word] (concat acc word))
   (newWordsList alphabet wordsSequence)
  )
)

(defn permute [alphabet, sequenceSize]
  (reduce 
   (fn [acc, element] (createWordsSequence alphabet acc))
   alphabet
   (range 1 sequenceSize)
   ) 
)

(doseq [item (permute '(a b c) 4)]
   (println item))

(println (count (permute '("a" "b" "c", "e") 4))) ;; 4 * 3 * 3 * 3 == 108