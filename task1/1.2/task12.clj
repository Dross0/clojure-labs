(defn create-words
  [alphabet, word, out]
  (if (empty? alphabet)
    out
    (let [
          currentLetter (first alphabet),
          restAlph (rest alphabet),
          wordTail (last word)
          ]
     (if (= wordTail currentLetter)
       (recur restAlph word out)
       (recur restAlph word (conj out (conj word currentLetter)))
    )
    )
  )
)

(defn create-words-sequence
  [alphabet, wordsSequence, out]
  (if (empty? wordsSequence)
    out
    (let [currentWord (first wordsSequence), restWords (rest wordsSequence)]
      (recur 
       alphabet restWords (create-words alphabet (if (coll? currentWord) currentWord [currentWord]) out))
      )
  )  
)

(defn permute
    ([alphabet, wordSize] (permute alphabet, alphabet, wordSize))
    ([alphabet, wordsSequence, wordSize]
    (
      if (= wordSize 1)
      wordsSequence
      (recur alphabet (create-words-sequence alphabet wordsSequence []) (dec wordSize))
    )
    )
)

(doseq [item (permute '(a b c) 4)]
   (println item))

(println (count (permute '("a" "b" "c", "e") 4))) ;; 4 * 3 * 3 * 3 == 108