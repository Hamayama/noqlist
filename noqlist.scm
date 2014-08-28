;; -*- coding: utf-8 -*-
;;
;; noqlist.scm
;; 2014-8-28 v1.00
;;
;; ＜内容＞
;;   Gaucheでリストの先頭が手続きでなくてもよいモードにするためのモジュールです。
;;   例えば (1 2 3 4 5) などと入力してもエラーにならなくなります。
;;
;; ＜インストール方法＞
;;   noqlist.scm を Gauche でロード可能なフォルダにコピーします。
;;   (例えば (gauche-site-library-directory) で表示されるフォルダ等)
;;
;; ＜使い方＞
;;   リストの先頭が手続きでなくてもよいモードにする場合
;;     (noqlist-on)
;;
;;   リストの先頭が手続きでなくてもよいモードを抜ける場合
;;     (noqlist-off)
;;
;;   ジェネリック関数のメソッドの競合について
;;     もし、ジェネリック関数object-applyの以下のメソッド
;;       (define-method object-apply ((x <top>) . args) ...
;;     を、すでに定義していた場合には、上書きしてしまいます。
;;     そのような場合は、モードに入るときに
;;       (define mold (noqlist-on))
;;     として、元のメソッドを保存してください。
;;     そして、モードを抜けるときに
;;       (noqlist-off mold)
;;     として、元のメソッドに戻してください。
;;     (ただし、モードに入っている間は、元のメソッドは使用できないので注意)
;;
;; ＜注意事項＞
;;   (1)リストの先頭が手続きでなかった場合に、リストをそのまま返すように
;;      ジェネリック関数object-applyにメソッドを追加しています。
;;      このため、手続きを作ったつもりがうまくできていなかった場合などに
;;      エラーが発生せず、デバッグが困難になる可能性があります。
;;      例えば、(x 200 300) というリストは、
;;      (define x 100) であれば (100 200 300) になりますが、
;;      (define x +) であれば 500 になります。
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

