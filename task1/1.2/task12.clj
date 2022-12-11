(defn create-words [initialAlphabet, wordToBuild]
  (loop [
         out '(),
         alphabet initialAlphabet,
         word wordToBuild
        ]
    (if (empty? alphabet)
      out
      (let [
            currentLetter (first alphabet),
            restAlph (rest alphabet),
            wordTail (last word)
            ]
       (if (= wordTail currentLetter)
         (recur out restAlph word)
         (recur (cons (conj word currentLetter) out) restAlph word)
      )
      )
    )
  )
)

(defn create-words-sequence [alphabet, initialWordsSequence]
  (loop [
         out '(),
         wordsSequence initialWordsSequence
        ]
    (if (empty? wordsSequence)
      out
      (let [currentWord (first wordsSequence)]
        (recur 
          (concat out (create-words alphabet (if (coll? currentWord) currentWord [currentWord]))) (rest wordsSequence))
        )
    )
  )  
)

(defn permute [alphabet, wordSize]
  (loop [
         wordsSequence '([]),
         currentWordSize wordSize
        ]
    (
      if (= currentWordSize 0)
      wordsSequence
      (recur (create-words-sequence alphabet wordsSequence) (dec currentWordSize))
    )
  )
)

(doseq [item (permute '(a b c) 4)]
   (println item))

(println (count (permute '("a" "b" "c", "e") 4))) ;; 4 * 3 * 3 * 3 == 108