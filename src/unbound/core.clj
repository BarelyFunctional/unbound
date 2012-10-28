(ns unbound.core
  (:use [clojure.set :only (union)]))

(declare unbound-symbols)

(def evens (partial take-nth 2))
(def odds (comp evens rest))

(defn mapunion [f coll] (apply union (map f coll)))

(defn symbols [form] (filter symbol? (flatten [form])))

(defn unbound-coll-symbols [form] (mapunion unbound-symbols form))

(defmulti seq-syms first)

(defmethod seq-syms 'if [form] (unbound-coll-symbols (rest form)))

(defn rest-fn-syms [form]
  (letfn [(param-body-syms [[params & body]]
            (apply disj (unbound-coll-symbols body) (symbols params)))
          (syms [form]
            (if (vector? (first form))
              (param-body-syms form)
              (mapunion param-body-syms form)))]
    (if (symbol? (first form))
      (disj (syms (rest form)) (first form))
      (syms form))))

(defmethod seq-syms 'fn* [form]
  (rest-fn-syms (rest form)))

(defmethod seq-syms 'letfn* [[_ fns & body]]
  (apply disj
         (union
          (mapunion unbound-symbols (odds fns))
          (unbound-coll-symbols body))
         (evens fns)))

(defn rest-binding-syms [[bindings & body]]
  (apply union
   (apply disj
          (unbound-coll-symbols body)
          (evens bindings))
   (map
    (partial apply disj)
    (map unbound-symbols (odds bindings))
    (reductions conj [] (evens bindings)))))

(defmethod seq-syms 'quote [form] #{})

(defmethod seq-syms 'let* [form]
  (rest-binding-syms (rest form)))

(defmethod seq-syms 'new [form] (unbound-coll-symbols (drop 2 form)))
(defmethod seq-syms 'recur [form] (unbound-coll-symbols (rest form)))
(defmethod seq-syms 'do [form] (unbound-coll-symbols (rest form)))

(defmethod seq-syms 'try [form]
  (let [catch? (every-pred seq? not-empty #(or (= 'catch (first %))))
        finally? (every-pred seq? not-empty #(= 'finally (first %)))]
    (union
     (unbound-coll-symbols (take-while (comp not (some-fn catch? finally?)) (rest form)))
     (mapunion
      (fn [[_ _ e & body]] (disj (unbound-coll-symbols body) e)) (filter catch? form))
     (mapunion
      (comp unbound-coll-symbols rest) (filter finally? form))
     )))

(defmethod seq-syms 'def [form] (disj (unbound-symbols (last form)) (second form)))

(defmethod seq-syms 'loop* [form]
  (rest-binding-syms (rest form)))

(defmethod seq-syms '. [form]
  (union
   (unbound-symbols (second form))
   (unbound-coll-symbols
    (if (symbol? (nth form 2))
      (drop 3 form)
      (rest (nth form 2))))))

(defmethod seq-syms :default [form] (unbound-coll-symbols form))

(defn unbound-symbols [form]
  (let [eform (macroexpand form)]
    (condp #(%1 %2) eform
      seq?  (if (empty? eform) #{} (seq-syms eform))
      coll?  (apply union (map unbound-symbols eform))
      symbol? #{eform}
      #{})))