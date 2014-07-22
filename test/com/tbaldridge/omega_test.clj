(ns com.tbaldridge.omega-test
  (:refer-clojure :exclude [and or])
  (:require [clojure.test :refer :all]
            [com.tbaldridge.omega :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(defparser SimpleIntegerParser []
           num (and (zero+ whitespace)
                    (maybe (one-of #{\+ \-})) -> prefix
                    (one+ digits) -> d
                    <- (read-string (apply str prefix d))))



(clojure.pprint/pprint (macroexpand '(and (zero+ whitespace)
                                          (maybe (one-of #{\+ \-})) -> prefix
                                          (one+ digits) -> d
                                          <- (read-string (apply str prefix d)))))

(deftest number-parser-tests
  (tc/quick-check 1000
    (prop/for-all [n gen/int]
                  (is (= ((:num SimpleIntegerParser) (string-cursor (str n))) n)))))



(defparser SimpleIntegerVectorParser [SimpleIntegerParser]
           vector (and (eat whitespace)
                       \[
                       (zero+ (or num vector)) -> itms
                       \]
                       <- (vec itms)))

(deftest vector-of-ints-test
  (tc/quick-check 100
                  (let [vgen (gen/vector
                               (gen/one-of [gen/int (gen/vector
                                                      (gen/one-of [gen/int (gen/vector gen/int)]))]))]
                    (prop/for-all [itms vgen]
                                  (is (= ((:vector SimpleIntegerVectorParser) (string-cursor (str itms))) itms))))))



