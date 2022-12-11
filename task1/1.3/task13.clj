(defn neg [value]
(- 0 value)
)

(defn is-even [value]
  (= 0 (mod value 2)
  )
)

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

(println (my-filter is-even [1, 2, 3, 4, 5, 6]))
(println (my-map neg [1, 2, 3, 4, 5, 6]))