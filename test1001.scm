;; -*- coding: utf-8 -*-
;;
;; noqlistのテスト
;; 2016-10-14
;;
(add-load-path "." :relative)
(use gauche.test)

(test-start "noqlist")
(use noqlist)
(test-module 'noqlist)

(test-section "noqlist-on")
(noqlist-on)
(test* "number-list"  (1 2 3 4 5) (1 2 3 4 5))
(test* "boolean-list" (#t #f) (#t #f))
(define x 100)
(define y 200)
(test* "symbol-list"  (100 200) (x y))
(test* "char-list"    (#\a #\b #\space) (#\a #\b #\space))
(test* "string-list"  ("abc" "de") ("abc" "de"))
(test* "vector-list"  (#(1 2 3) #(4 5)) (#(1 2 3) #(4 5)))

(test-section "noqlist-off")
(noqlist-off)
(test* "number-list"  (test-error <error>) (1 2 3 4 5))
(test* "boolean-list" (test-error <error>) (#t #f))
(test* "symbol-list"  (test-error <error>) (x y))
(test* "char-list"    (test-error <error>) (#\a #\b #\space))
(test* "string-list"  (test-error <error>) ("abc" "de"))
(test* "vector-list"  (test-error <error>) (#(1 2 3) #(4 5)))

(test-end)

