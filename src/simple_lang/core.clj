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

(defrecord LessThan-e [left right]
  Expression
  (reducible? [this] true)
  (->str [this] (str (->str (:left this)) " < " (->str (:right this))))
  (reduce-e [this env]
    (let [{left :left right :right} this]
      (cond
        (reducible? left) [(->LessThan-e (first (reduce-e left env)) right) env]
        (reducible? right) [(->LessThan-e left (first (reduce-e right env))) env]
        :else (let [lval (first (reduce-e left env)) rval (first (reduce-e right env))]
                (if (< lval rval)
                  (->Boolean-e true)
                  (->Boolean-e false)))))))

(defrecord GreaterThan-e [left right]
  Expression
  (reducible? [this] true)
  (->str [this] (str (->str (:left this)) " > " (->str (:right this))))
  (reduce-e [this env]
    (let [{left :left right :right} this]
      (cond
        (reducible? left) [(->LessThan-e (first (reduce-e left env)) right) env]
        (reducible? right) [(->LessThan-e left (first (reduce-e right env))) env]
        :else (let [lval (first (reduce-e left env)) rval (first (reduce-e right env))]
                (if (> lval rval)
                  (->Boolean-e true)
                  (->Boolean-e false)))))))

(defrecord EqualTo-e [left right]
  Expression
  (reducible? [this] true)
  (->str [this] (str (->str (:left this)) " == " (->str (:right this))))
  (reduce-e [this env]
    (let [{left :left right :right} this]
      (cond
        (reducible? left) [(->LessThan-e (first (reduce-e left env)) right) env]
        (reducible? right) [(->LessThan-e left (first (reduce-e right env))) env]
        :else (let [lval (first (reduce-e left env)) rval (first (reduce-e right env))]
                (if (= lval rval)
                  (->Boolean-e true)
                  (->Boolean-e false)))))))

(defrecord Variable-e [name]
  Expression
  (reducible? [this] true)
  (->str [this] (str "var[" (:name this) "]"))
  (reduce-e [this env] [(env name) env]))

(defrecord While-e [condition body]
  Expression
  (reducible? [this] true)
  (->str [this] (str "while(" (->str condition) "){" (->str body) "}"))
  (reduce-e [this env]
    [(->If-e condition (->Sequence-e body this) (->DoNothing-e)) env]))

(defn strenv [env]
  (str "{"
       (->>
        env
        (map (fn [[name val]] (str name ": " (inspect val))))
        (interpose ", ")
        vec
        (apply str))
       "}"))

(defn run
  ([expression] (run expression {}))
  ([expression env]
   (loop [exp expression env env]
     (println (str (inspect exp) " #" (strenv env)))
     (if (reducible? exp)
       (let [[next-exp next-env] (reduce-e exp env)]
         (recur next-exp next-env))
       exp))))
