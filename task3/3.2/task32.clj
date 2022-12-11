(defn is-even [value]
  (= 0 (mod value 2))
)

(defn heavy-is-even [value]
  (Thread/sleep 40) 
  (is-even value)
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

(defn build-batchs-from-collection [collection batchsCount]
  (if (empty? collection)
    ()
    (lazy-seq 
       (cons 
          (take batchsCount collection)
          (build-batchs-from-collection (drop batchsCount collection) batchsCount)
       )
    )
  )
)

(defn future-filter [predicat, collection]
  (future (my-filter predicat, collection))
)

(defn future-filter-batch [predicat batch threadsCount]
  (map 
   deref
   (doall (map (fn [threadBatch] (future-filter predicat threadBatch)) batch))
  )
)

(defn build-future-filter-of-collection-batch [predicat, collection, threadsCount, threadBatchSize]
  (let [
        batchsCollection (build-batchs-from-collection collection threadBatchSize)
  ]
    (->> (build-batchs-from-collection batchsCollection threadsCount)
         (map (fn [batch] (future-filter-batch predicat batch threadsCount)) ,,)
         (apply concat)
    )
  )
)

(defn lazy-parallel-filter 
  ([predicat, collection, threadsCount]
    (lazy-parallel-filter predicat, collection, threadsCount, 10)
  )
  ([predicat, collection, threadsCount, threadBatchSize]
    (reduce 
       concat 
       [] 
       (build-future-filter-of-collection-batch predicat collection threadsCount threadBatchSize)
    )
  )
)

(let [collectionForFilter (range 0 200)]
  (print "Simple filter: ")
  (time (my-filter heavy-is-even collectionForFilter))
  (print "1 Thread filter: ")
  (time (doall (lazy-parallel-filter heavy-is-even collectionForFilter 1)))
  (print "2 Thread filter: ")
  (time (doall (lazy-parallel-filter heavy-is-even collectionForFilter 2)))
  (print "3 Thead filter: ")
  (time (doall (lazy-parallel-filter heavy-is-even collectionForFilter 3)))
  (println (lazy-parallel-filter #(> (count %) 1) [[1] [2 3] [4] [5 6 7]] 1))
  (println (my-filter #(> (count %) 1) [[1] [2 3] [4] [5 6 7]]))
)
