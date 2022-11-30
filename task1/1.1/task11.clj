
(defn createWord
  [alphabet, alphabetWord]
  (if (empty? alphabet)
    []
    (let [currentLetter (first alphabet), restAlph (rest alphabet)]
     (if (.endsWith alphabetWord currentLetter)
       (createWord restAlph alphabetWord)
       (cons (str alphabetWord currentLetter) (createWord restAlph alphabetWord))
      )
    )
  )
)

(defn createWordsSequence
  [alphabet, wordsSequence]
  (if (empty? wordsSequence)
    []
    (let [currentAlphabetWord (first wordsSequence), restWords (rest wordsSequence)]
       (concat (createWord alphabet currentAlphabetWord) (createWordsSequence alphabet restWords))
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

(println (permute '("a" "b" "b" "c") 3))