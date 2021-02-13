(ns simple-lang.core)

(defprotocol Expression
  (reducible? [this])
  (->str [this])
  (reduce-e [this]))

(defn inspect [exp]
  (str "<<" (->str exp) ">>"))

(defrecord Number-e [value]
  Expression
  (reducible? [this] false)
  (->str [this] (str (:value this)))
  (reduce-e [this] (:value this)))

(defrecord Add-e [left right]
  Expression
  (reducible? [this] true)
  (->str [this] (str (->str (:left this)) " + " (->str (:right this))))
  (reduce-e [this]
    (let [left (:left this) right (:right this)]
      (cond
        (reducible? left) (->Add-e (reduce-e left) right)
        (reducible? right) (->Add-e left (reduce-e right))
        :else (->Number-e (+ (reduce-e left) (reduce-e right)))))))

(defrecord Mult-e [left right]
  Expression
  (reducible? [this] true)
  (->str [this] (str (->str (:left this)) " * " (->str (:right this))))
  (reduce-e [this]
    (let [left (:left this) right (:right this)]
      (cond
        (reducible? left) (->Mult-e (reduce-e left) right)
        (reducible? right) (->Mult-e left (reduce-e right))
        :else (->Number-e (* (reduce-e left) (reduce-e right)))))))