(ns simple-lang.core)

(defprotocol Expression
  (reducible? [this])
  (->str [this])
  (reduce-e [this env]))

(defn inspect [exp]
  (str "<<" (->str exp) ">>"))

(defrecord Number-e [value]
  Expression
  (reducible? [this] false)
  (->str [this] (str (:value this)))
  (reduce-e [this env] (:value this)))

(defrecord Add-e [left right]
  Expression
  (reducible? [this] true)
  (->str [this] (str (->str (:left this)) " + " (->str (:right this))))
  (reduce-e [this env]
    (let [left (:left this) right (:right this)]
      (cond
        (reducible? left) (->Add-e (reduce-e left env) right)
        (reducible? right) (->Add-e left (reduce-e right env))
        :else (->Number-e (+ (reduce-e left env) (reduce-e right env)))))))

(defrecord Mult-e [left right]
  Expression
  (reducible? [this] true)
  (->str [this] (str (->str (:left this)) " * " (->str (:right this))))
  (reduce-e [this env]
    (let [left (:left this) right (:right this)]
      (cond
        (reducible? left) (->Mult-e (reduce-e left env) right)
        (reducible? right) (->Mult-e left (reduce-e right env))
        :else (->Number-e (* (reduce-e left env) (reduce-e right env)))))))

(defrecord EnvChange-e [environment]
  Expression
  (reducible? [this] false)
  (->str [this] (str "env-change: " (:environment this)))
  (reduce-e [this env] this))

(defrecord Assign-e [name, expression]
  Expression
  (reducible? [this] true)
  (->str [this] (str (:name this) "=" (->str (:expression this))))
  (reduce-e [this env]
    (let [name (:name this) expression (:expression this)])
    (if (reducible? expression)
      (->Assign-e name (reduce-e expression env))
      (->EnvChange-e (assoc env name expression)))))

(defn run [expression]
  (println (inspect expression))
  (if (reducible? expression)
    (recur (reduce-e expression {}))
    expression))