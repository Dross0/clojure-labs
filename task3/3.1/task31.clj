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

(defn extract-batch-by-index [batchIndex, collection, batchSize] 
  (take batchSize (drop (* batchIndex batchSize) collection))
)

(defn build-batchs-from-collection [collection batchsCount]
  (let [batchSize (int (Math/ceil (/ (count collection) batchsCount)))]
    (map
     (fn [batchIndex] (extract-batch-by-index batchIndex collection batchSize))
     (range 0 batchsCount)
    )
  )
)

(defn future-filter [predicat, collection]
  (future (doall (filter predicat, collection)))
)

(defn build-future-filter-of-collection-batch [predicat, collection, threadsCount]
  (->> (build-batchs-from-collection collection threadsCount)
       (map (fn [batch] (future-filter predicat batch)) ,,)
       (doall)
  )
)

(defn parallel-filter [predicat, collection, threadsCount]
  (mapcat deref (build-future-filter-of-collection-batch predicat, collection, threadsCount))
)

(let [collectionForFilter (range 0 100)]
  (print "Simple filter: ")
  (time (doall (my-filter heavy-is-even collectionForFilter)))
  (print "1 Thread filter: ")
  (time (parallel-filter heavy-is-even collectionForFilter 1))
  (print "2 Thread filter: ")
  (time (parallel-filter heavy-is-even collectionForFilter 2))
  (print "3 Thead filter: ")
  (time (doall (parallel-filter heavy-is-even collectionForFilter 3)))
  (println (parallel-filter #(> (count %) 1) [[1] [2 3] [4] [5 6 7]] 1))
  (println (my-filter #(> (count %) 1) [[1] [2 3] [4] [5 6 7]]))
)
