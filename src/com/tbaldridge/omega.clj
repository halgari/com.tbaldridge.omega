(ns com.tbaldridge.omega
  (:refer-clojure :exclude [or and]))

;; Cursors

(defprotocol ICursor
  (next! [this])
  (current [this])
  (snapshot [this])
  (rewind! [this snapshot]))

(deftype StringCursor [^:volatile-mutable ^long idx ^String s]
  ICursor
  (next! [this]
    (set! idx (inc idx)))
  (current [this]
    (when (< idx (count s))
      (nth s idx)))
  (snapshot [this]
    idx)
  (rewind! [this val]
    (set! idx (long val))))

(defn string-cursor [^String s]
  (StringCursor. 0 s))

;; Mechanics

(deftype ParseFailure [])

(def fail (ParseFailure.))

(defn failure? [v]
  (identical? v fail))

(defn parse-if [pred]
  (fn [cursor]
    (if (pred (current cursor))
      (let [value (current cursor)]
        (next! cursor)
        value)
      fail)))


(defprotocol IParserGenerator
  (to-parser [this]))

(extend-protocol IParserGenerator
  clojure.lang.IFn
  (to-parser [this]
    this)
  java.lang.Character
  (to-parser [this]
    (parse-if #(= % this))))




(defn or
  ([a] a)
  ([a b]
   (let [a (to-parser a)
         b (to-parser b)
         m (atom #{})]
     (fn [cursor]
       (let [key [cursor (snapshot cursor)]]
         (if-let [v (contains? @m key)]
           (b cursor)
           (let [_ (swap! m conj key)
                 state (snapshot cursor)
                 val (a cursor)]
             (swap! m disj key)
             (if (identical? val fail)
               (do (rewind! cursor state)
                   (b cursor))
               val)))))))
  ([a b & more]
   (apply or (or a b) more)))

(defn add-clauses [cursor-sym body [[sym goal] & more]]
  (if sym
    `(let [~sym (~sym ~cursor-sym)]
       (if (identical? ~sym fail)
         fail
         ~(add-clauses cursor-sym body more)))
    body))

(defn -parse-args
  [args]
  (loop [[arg & rest] args
         rules []
         return nil]
    (assert (not (= '-> arg)) "invalid position for ->")
    (if arg
      (if (= '<- arg)
        (let [return (first rest)]
          (recur (next rest)
                 rules
                 return))
        (if (= (first rest) '->)
          (let [binding (-> rest next first)
                rest (-> rest next next)]
            (recur rest
                   (conj rules [binding arg])
                   return))
          (recur rest
                 (conj rules [(gensym "_") arg])
                 return)))
      [rules return])))

(defmacro and [& args]
  (let [[parsed body] (-parse-args args)
        cursor-sym (gensym "cursor")]
    `(let [~@(mapcat
               (fn [[sym parser]]
                 [sym `(to-parser ~parser)])
               parsed)]
       (fn [~cursor-sym]
         ~(add-clauses cursor-sym body parsed)))))




(defprotocol IDeliverable
  (-deliver [this val]))

(deftype PromiseFn [^:volatile-mutable f]
  IDeliverable
  (-deliver [this val]
    (set! f val))
  clojure.lang.IFn
  (invoke [this val]
    (f val)))

(defn promise-fn []
  (PromiseFn. nil))

(defmacro defparser [nm inherits & rules]
  {:pre [(even? (count rules))
         (vector? inherits)]}
  (let [parted (apply merge
                      (conj (mapv (comp ::forms deref resolve) inherits)
                            (apply hash-map rules)))
        rules (apply concat parted)
        syms (keys parted)]
    `(let [~@(mapcat (fn [s]
                       `[~s (promise-fn)])
                     syms)]
       ~@(map (fn [[s goal]]
                `(-deliver ~s ~goal))
              parted)
       (def ~nm ~(assoc (zipmap (map (comp keyword name) syms)
                                syms)
                   ::forms (list 'quote (apply hash-map rules))))
       )))

;; Common parsers

(defn char-range [from to]
  (parse-if (fn [v]
              (when (char? v)
                (<= (int from) (int v) (int to))))))

(defn one+ [g]
  (fn [cursor]
    (loop [acc []]
      (let [v (g cursor)]
        (if (identical? v fail)
          (if (= 0 (count acc))
            fail
            acc)
          (recur (conj acc v)))))))

(defn zero+ [g]
  (fn [cursor]
    (loop [acc []]
      (let [v (g cursor)]
        (if (identical? v fail)
          acc
          (recur (conj acc v)))))))


(defn eat [g]
  (fn [cursor]
    (loop []
      (let [v (g cursor)]
        (if (identical? v fail)
          nil
          (recur))))))

(defn maybe
  ([g]
   (maybe g nil))
  ([g default]
   (let [g (to-parser g)]
     (fn [cursor]
       (let [v (g cursor)]
         (if (failure? v)
           default
           v))))))

(defn one-of [v]
  (parse-if (partial contains? v)))

(def digits (parse-if (fn [^Character c]
                        (when c
                          (Character/isDigit c)))))

(def whitespace (parse-if #{\t \n \r \space \tab}))



