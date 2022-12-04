
(defn createWords
  [alphabet, word]
  (if (empty? alphabet)
    []
    (let [
          currentLetter (first alphabet),
          restAlph (rest alphabet),
          wordTail (last word)
          ]
     (if (= wordTail currentLetter)
       (createWords restAlph word)
       (cons (conj word currentLetter) (createWords restAlph word))
    )
    )
  )
)

(defn createWordsSequence
  [alphabet, wordsSequence]
  (if (empty? wordsSequence)
    []
    (let [currentWord (first wordsSequence), restWords (rest wordsSequence)]
       (concat 
        (createWords alphabet (if (coll? currentWord) currentWord [currentWord]))
        (createWordsSequence alphabet restWords)
      )
    )  
  )
)

(defn permute
    ([alphabet, wordSize] (permute alphabet, alphabet, wordSize))
    ([alphabet, wordsSequence, wordSize]
    (
      if (= wordSize 1)
      wordsSequence
      (permute alphabet (createWordsSequence alphabet wordsSequence) (dec wordSize))
    )
    )
)

(doseq [item (permute '(a b c) 4)]
   (println item))

(println (count (permute '("a" "b" "c", "e") 4))) ;; 4 * 3 * 3 * 3 == 108