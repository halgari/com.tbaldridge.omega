(ns com.tbaldridge.omega-test
  (:refer-clojure :exclude [and or])
  (:require [clojure.test :refer :all]
            [com.tbaldridge.omega :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(defparser SimpleIntegerParser []
           num (and [_ (zero+ whitespace)
                     prefix (maybe (one-of #{\+ \-}))
                     d (one+ digits)]
                    (read-string (apply str prefix d))))

(deftest number-parser-tests
  (tc/quick-check 1000
    (prop/for-all [n gen/int]
                  (is (= ((:num SimpleIntegerParser) (string-cursor (str n))) n)))))


(defparser SimpleIntegerVectorParser [SimpleIntegerParser]
           vector (and [_ (eat whitespace)
                        _ \[
                        itms (zero+ (or num vector))
                        _ \]]
                       (vec itms)))

(deftest vector-of-ints-test
  (tc/quick-check 100
                  (let [vgen (gen/vector
                               (gen/one-of [gen/int (gen/vector
                                                      (gen/one-of [gen/int (gen/vector gen/int)]))]))]
                    (prop/for-all [itms vgen]
                                  (is (= ((:vector SimpleIntegerVectorParser) (string-cursor (str itms))) itms))))))
