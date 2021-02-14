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

(defrecord Sequence-e [head tail]
  Expression
  (reducible? [this] true)
  (->str [this] (str (->str (:head this)) "; " (->str (:tail this))))
  (reduce-e [this env]
    (if (= head (->DoNothing-e))
      [tail env]
      (let [[next-exp next-env] (reduce-e head env)]
        [(->Sequence-e next-exp tail) next-env]))))

(defrecord Boolean-e [value]
  Expression
  (reducible? [this] false)
  (->str [this] (case (:value this)
                  true "true"
                  false "false"))
  (reduce-e [this env] [(:value this) env]))

(defrecord If-e [condition consequence alternative]
  Expression
  (reducible? [this] true)
  (->str [this] (str "if (" (->str condition) "){"
                     (->str consequence) "} else{"
                     (->str alternative) "}"))
  (reduce-e [this env]
    (if (reducible? condition)
      [(->If-e (first (reduce-e condition env))
               consequence
               alternative)
       env]
      (cond
        (= condition (->Boolean-e true)) [consequence env]
        (= condition (->Boolean-e false)) [alternative env]))))

(defn run [expression]
  (loop [exp expression env {}]
    (println (str (inspect exp) " - " env))
    (if (reducible? exp)
      (let [[next-exp next-env] (reduce-e exp env)]
        (recur next-exp next-env))
      exp)))