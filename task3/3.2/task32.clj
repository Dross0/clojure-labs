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
  (take batchSize (drop (* batchIndex batchSize) collection))
)

(defn buildBatchsFromCollection [collection batchSize batchsCount]
  (map
   (fn [batchIndex] (extractBatchByIndex batchIndex collection batchSize))
   (range 0 batchsCount)
  )
)

(defn futureFilter [predicat, collection]
  (future (my_filter predicat, collection))
)

(defn buildFutureFilterOfCollectionBatch [predicat, collection, threadBatchSize, threadsCount]
  (->> (buildBatchsFromCollection collection threadBatchSize threadsCount)
       (map (fn [batch] (futureFilter predicat batch)) ,,)
       (doall)
  )
)

(defn subCollectionParallelFilter [predicat, collection, threadBatchSize, threadsCount]
  (map deref (buildFutureFilterOfCollectionBatch predicat collection threadBatchSize threadsCount))
)


(defn buildLazySubCollections [collection threadBatchSize threadsCount]
  (if (empty? collection)
    ()
    (let [
          subCollectionSize (* threadBatchSize threadsCount),
          currentSubCollection (take subCollectionSize collection) 
          restSubCollections (drop subCollectionSize collection)
      ]
      (lazy-seq (cons currentSubCollection (buildLazySubCollections restSubCollections threadBatchSize threadsCount)))
    )
  )
)

(defn lazyParallelFilter 
  (
    [predicat, collection, threadsCount]
    (lazyParallelFilter predicat, collection, threadsCount, 30)
  )
  (
    [predicat, collection, threadsCount, threadBatchSize]
    (flatten
      (map 
       (fn [subCollection] (subCollectionParallelFilter predicat subCollection threadBatchSize threadsCount))
       (buildLazySubCollections collection threadBatchSize threadsCount)
      )
    )
  )
)

(let [collectionForFilter (range 0 1000)]
  (print "Simple filter: ")
  (time (doall (my_filter heavyIsEven collectionForFilter)))
  (print "1 Thread filter: ")
  (time (lazyParallelFilter heavyIsEven collectionForFilter 1))
  (print "2 Thread filter: ")
  (time (lazyParallelFilter heavyIsEven collectionForFilter 2))
  (print "3 Thead filter: ")
  (time (lazyParallelFilter heavyIsEven collectionForFilter 3))
)