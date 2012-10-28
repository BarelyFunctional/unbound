(ns unbound.core-test
  (:use clojure.test
        unbound.core))

(deftest test1
  (testing "collections excluding seqs"
    (is (= (unbound-symbols ['a 'b 1 2 'c]) #{'a 'b 'c}))
    (is (= (unbound-symbols [['a 'b 1] 'c 'a 'd]) #{'a 'b 'c 'd}))
    (is (= (unbound-symbols #{'a 'b 'c}) #{'a 'b 'c}))
    (is (= (unbound-symbols {:a 'a :b 'b 'c 'd}) #{'a 'b 'c 'd})))
  (testing "simple functions"
    (is (= (unbound-symbols '(fn [x] (* x y 2))) #{'* 'y})))
  (testing "letfn forms"
    (is (= (unbound-symbols '(letfn [(x [y] (* y 2))] (* x y a))) #{'y 'a '*})))
  (testing "let forms"
    (is (= (unbound-symbols '(let [a (+ b d g) d 3] (+ a b c d e))) #{'+ 'c 'b 'g 'd 'e})))
  (testing "try catch"
    (is (= (unbound-symbols '(try (+ 1 2 a) (catch Throwable e z)))))))