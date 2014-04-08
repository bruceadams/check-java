(ns check-java.properties
  (:require clojure.reflect
            (clojure.test [check :as tc])
            (clojure.test.check [generators :as gen]
                                [properties :as prop])))

(defn fields
  [o]
  (filter #(= (class %) clojure.reflect.Field)
          (:members (clojure.reflect/reflect o))))

(defn non-final
  [fields]
  (filter #(not (:final (:flags %))) fields))

(defn final-non-static
  [fields]
  (filter #(and (:final (:flags %))
                (not (:static (:flags %)))) fields))

(defn ignore-fields
  [fields names-to-ignore]
  (let [ignores (set (map symbol names-to-ignore))]
    (filter #(not (ignores (:name %))) fields)))

(defn get-field
  [inst field-name]
     (-> (class inst)
         (.getDeclaredField (name field-name))
         (doto (.setAccessible true))
         (.get inst)))

(defn field-map
  [i names]
  (zipmap names (map #(get-field i %) names)))

(defn hash-map-intersection
  [a b]
  (reduce conj {} (clojure.set/intersection (set a) (set b))))

;; precomdition: all fields are final
;; grab field names and values for all non-static fields
;; walk through samples looking for changed fields
;; once all fields have seen a change, all is well.
;; too long a walk without a field change? fail.
(defn check-generator
  "Check that a generator will hit different values for each field.
   Also, check that the target class appears to be immutable and that
   the generator always produces the same class."
  [generator builder & options]
  (let [opts (apply hash-map options)
        n (or (:count opts) 100)
        ignore-field-names (or (:ignore-fields opts) #{"$jacocoData"})
        s (gen/sample (gen/fmap builder generator) n)
        i (first s)
        fields-to-consider (ignore-fields (fields i) ignore-field-names)
        fields-to-check (map :name (final-non-static fields-to-consider))
        base-values (field-map i fields-to-check)
        klass (class i)
        modifiables (map :name (non-final fields-to-consider))]

    (when-not (empty? modifiables)
      (throw (IllegalArgumentException.
              (str "Found modifiable fields " (vec modifiables)))))

    (loop [r (rest s) remaining-values base-values c 0]
      (cond (empty? remaining-values) c
            (empty? r) (throw (Exception.
                               (str "No value change seen in fields "
                                    remaining-values
                                    " after generating " n " values.")))
            :else
            (let [i (first r)
                  these-values (field-map i (keys remaining-values))]
              (when-not (= klass (class i))
                (throw (IllegalStateException.
                        (str "Mixed classes. Started with " klass
                             " now see " (class i)))))
              (recur (rest r)
                     (hash-map-intersection remaining-values these-values)
                     (inc c)))))))


(defn copy-builder-prop
  "A new object built via the copy builder must equal the original object."
  [generator tuple-builder copy-builder]
  (prop/for-all [i (gen/fmap tuple-builder generator)]
    (= i (.build (copy-builder i)))))


(defn gen-twin
  "Generate pairs of objects built twice with the same arguments."
  [generator builder]
  (gen/fmap #(vector (builder %) (builder %)) generator))

(defn gen-dyad
  "Generate pairs of objects with not equal arguments."
  [generator builder]
  (gen/fmap (fn [[a b]] (vector (builder a) (builder b)))
            (gen/such-that (fn [[a b]] (not= a b))
                           (gen/tuple generator generator))))

(defn equal-twins-prop
  "Two objects built from the same arguments are equal."
  [generator builder]
  (prop/for-all [[a b] (gen-twin generator builder)]
                (and (.equals a b)
                     (.equals b a))))

(defn compare-to-twins-prop
  "Two objects built from the same arguments .compareTo == 0."
  [generator builder]
  (prop/for-all [[a b] (gen-twin generator builder)]
                (and (= 0 (.compareTo a b))
                     (= 0 (.compareTo b a)))))

(defn hash-code-twins-prop
  "Two objects built from the same arguments have the same hash code."
  [generator builder]
  (prop/for-all [[a b] (gen-twin generator builder)]
                (= (.hashCode a) (.hashCode b))))

(defn to-string-twins-prop
  "Two objects built from the same arguments produce matching toString."
  [generator builder]
  (prop/for-all [[a b] (gen-twin generator builder)]
                (= (.toString a) (.toString b))))

(defn equal-dyad-prop
  "Two objects built from different arguments are not equal."
  [generator builder]
  (prop/for-all [[a b] (gen-dyad generator builder)]
                (and (not (.equals a b))
                     (not (.equals b a)))))

(defn compare-to-dyad-prop
  "Two objects built from different arguments have a consistent .compareTo order."
  [generator builder]
  (prop/for-all [[a b] (gen-dyad generator builder)]
                (and (not= 0 (.compareTo a b))
                     (= (.compareTo a b) (- (.compareTo b a))))))

(defn hash-code-dyad-prop
  "Two objects built from different arguments have different hash codes.
   Note: This is too strict a test. It will fail on occasion,
   when a hashing collision occurs."
  [generator builder]
  (prop/for-all [[a b] (gen-dyad generator builder)]
                (not= (.hashCode a) (.hashCode b))))

(defn to-string-dyad-prop
  "Two objects built from different arguments produce different toString."
  [generator builder]
  (prop/for-all [[a b] (gen-dyad generator builder)]
                (not= (.toString a) (.toString b))))
