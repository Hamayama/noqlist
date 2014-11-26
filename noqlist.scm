;; -*- coding: utf-8 -*-
;;
;; noqlist.scm
;; 2014-11-26 v1.07
;;
;; ＜内容＞
;;   Gaucheでリストの先頭が手続きでなくてもよいモードにするためのモジュールです。
;;   例えば (1 2 3 4 5) などと入力してもエラーにならなくなります。
;;   現状、いくつかの問題があり実験用という位置づけです。
;;
;;   詳細については、以下のページを参照ください。
;;   https://github.com/Hamayama/noqlist
;;
(define-module noqlist
  (export noqlist-on noqlist-off))
(select-module noqlist)


;; リストの先頭が手続きでなくてもよいモードにする
(define (noqlist-on)
  (rlet1 mold (get-gf-method object-apply 1 #t `(,<top>))
    (define-method object-apply ((x <top>) . args) (cons x args))))

;; リストの先頭が手続きでなくてもよいモードを抜ける
(define (noqlist-off :optional (mold #f))
  (if (is-a? mold <method>)
    (add-method! object-apply mold)
    (delete-gf-method object-apply 1 #t `(,<top>))))


;; ジェネリック関数のメソッドを種別を指定して取得する
;; 引数
;;   gf            ジェネリック関数(例えば object-apply 等)
;;   required      メソッドの引数の数(省略可能引数は除く)
;;   optional      メソッドの省略可能引数の有無(#tまたは#f)
;;   specializers  メソッドの引数の型を示す特定化子リスト(例えば `(,<number> ,<string>) 等)
(define (get-gf-method gf required optional specializers)
  (find
   (lambda (m)
     (and (equal? required (slot-ref m 'required))
          (equal? optional (slot-ref m 'optional))
          (equal? specializers (slot-ref m 'specializers))))
   (slot-ref gf 'methods)))

;; ジェネリック関数のメソッドを種別を指定して削除する
;; 引数
;;   gf            ジェネリック関数(例えば object-apply 等)
;;   required      メソッドの引数の数(省略可能引数は除く)
;;   optional      メソッドの省略可能引数の有無(#tまたは#f)
;;   specializers  メソッドの引数の型を示す特定化子リスト(例えば `(,<number> ,<string>) 等)
(define (delete-gf-method gf required optional specializers)
  (let1 m (get-gf-method gf required optional specializers)
    (if m (delete-method! gf m))))

