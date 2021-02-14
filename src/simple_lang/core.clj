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
  (reduce-e [this env] [(:value this) env]))

(defrecord Add-e [left right]
  Expression
  (reducible? [this] true)
  (->str [this] (str (->str (:left this)) " + " (->str (:right this))))
  (reduce-e [this env]
    (let [left (:left this) right (:right this)]
      (cond
        (reducible? left) [(->Add-e
                            (first (reduce-e left env))
                            right)
                           env]
        (reducible? right) [(->Add-e
                             left
                             (first (reduce-e right env)))
                            env]
        :else [(->Number-e (+
                            (first (reduce-e left env))
                            (first (reduce-e right env))))
               env]))))

(defrecord Mult-e [left right]
  Expression
  (reducible? [this] true)
  (->str [this] (str (->str (:left this)) " * " (->str (:right this))))
  (reduce-e [this env]
    (let [left (:left this) right (:right this)]
      (cond
        (reducible? left) [(->Mult-e
                            (first (reduce-e left env))
                            right)
                           env]
        (reducible? right) [(->Mult-e
                             left
                             (first (reduce-e right env)))
                            env]
        :else [(->Number-e (*
                            (first (reduce-e left env))
                            (first (reduce-e right env))))
               env]))))

(defrecord DoNothing-e []
  Expression
  (reducible? [this] false)
  (->str [this] "do-nothing")
  (reduce-e [this env] [this env]))

(defrecord Assign-e [name expression]
  Expression
  (reducible? [this] true)
  (->str [this] (str (:name this) "=" (->str (:expression this))))
  (reduce-e [this env]
    (let [name (:name this) expression (:expression this)])
    (if (reducible? expression)
      [(->Assign-e name (first (reduce-e expression env)))
       env]
      [(->DoNothing-e)
       (assoc env name expression)])))

(defn run [expression]
  (loop [exp expression env {}]
    (println (str (inspect exp) " - " env))
    (if (reducible? exp)
      (let [[next-exp next-env] (reduce-e exp env)]
        (recur next-exp next-env))
      exp)))