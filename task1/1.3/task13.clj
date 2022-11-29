(defn neg [value]
(- 0 value)
)

(defn isEven [value]
  (= 0 (mod value 2)
  )
)

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

(println (my_filter isEven [1, 2, 3, 4, 5, 6]))
(println (my_map neg [1, 2, 3, 4, 5, 6]))