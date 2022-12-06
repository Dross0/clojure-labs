(defn isEven [value]
  (= 0 (mod value 2))
)

(defn heavyIsEven [value]
  (Thread/sleep 40) 
  (isEven value)
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

(defn extractBatchByIndex [batchIndex, collection, batchSize] 
  (take (* (+ 1 batchIndex) batchSize) (drop (* batchIndex batchSize) collection))
)

(defn buildBatchsFromCoollection [collection batchsCount]
  (let [batchSize (/ (count collection) batchsCount)]
    (map
     (fn [batchIndex] (extractBatchByIndex batchIndex collection batchSize))
     (range 0 batchsCount)
    )
  )
)

(defn futureFilter [predicat, collection]
  (future (my_filter predicat, collection))
)

(defn buildFutureFilterOfCollectionBatch [predicat, collection, threadsCount]
  (->> (buildBatchsFromCoollection collection threadsCount)
       (map (fn [batch] (futureFilter predicat batch)) ,,)
       (doall)
  )
)

(defn parallelFilter [predicat, collection, threadsCount]
  (flatten
   (map deref (buildFutureFilterOfCollectionBatch predicat, collection, threadsCount))
  )
)

(let [collectionForFilter (range 0 1000)]
  (print "Simple filter: ")
  (time (doall (my_filter heavyIsEven collectionForFilter)))
  (print "1 Thread filter: ")
  (time (parallelFilter heavyIsEven collectionForFilter 1))
  (print "2 Thread filter: ")
  (time (parallelFilter heavyIsEven collectionForFilter 2))
  (print "3 Thead filter: ")
  (time (parallelFilter heavyIsEven collectionForFilter 3))
)
