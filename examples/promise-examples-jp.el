;;; promise-examples-jp.el --- Examples using `promise.el' for Japanese.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2017  chuntaro

;; Author: chuntaro <chuntaro@sakura-games.jp>
;; URL: https://github.com/chuntaro/emacs-promise
;; Package-Requires: ((emacs "25"))
;; Version: 1.0
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file is examples using `promise.el' for Japanese.
;;
;; このファイルは `promise-core.el' `promise-es6-extensions.el' で公開してる
;; 関数を全て使用した実践的な(だが実用的ではない…)サンプルコードです。
;;
;; 処理の内容は概ね以下の通りです。
;; 1. HTTP リクエスト(以降省略)で郵便番号(103-0000)から都道府県コードを取得する
;; 2. 都道府県コードから路線コードを取得する
;; 3. 路線コードから駅名一覧を取得する
;; 4. 駅名から Wikipedia と国立国会図書館の両方へ同時にサーチする
;;
;; 実際にはそれぞれの処理間にウェイトを入れたりタイムアウトを設定したりしています。
;;
;; `Promise' を使うと非同期処理をサブルーチン化したり、処理の内容をシンプルに
;; 書き下す事が出来るようになります。
;; (main) 内のコードを見て、上から下へ普通に書いてある何の変哲もないコードに
;; 見える事が `Promise' を使う目的です。
;;
;; 実行する場合は以下の Lisp コードの最後の括弧の後ろにポイントを移動して
;; C-x C-e と押してください。(新しい Emacs を起動して (main) を実行します)
;;
;; (start-process "emacs" nil (file-truename (expand-file-name invocation-name invocation-directory)) "-Q"  "-Q" "--execute" "(package-initialize)" "-L" (concat default-directory "../") "-l" (buffer-file-name) "--execute" "(main)")

;;; Code:

(require 'promise)
(require 'url-http)
(require 'xml)
(require 'dom)

(defun wait-2sec (value)
  "2秒間ウェイトする `Promise' を返す。"
  (promise-new
   (lambda (resolve _reject)
     (let ((time 2.0))
       (run-at-time time nil (lambda ()
                               (message "\nwait complete: %s sec" time)
                               (funcall resolve value)))))))

(defun timeout (time)
  "指定時間後にタイムアウトする `Promise' を返す。"
  (promise-new
   (lambda (_resolve reject)
     (run-at-time time nil (lambda ()
                             (funcall reject (format "timeout: %s" time)))))))

(defun xml-retrieve (url)
  "HTTP リクエストして取得した XML オブジェクトで `resolve' する `Promise' を返す。"
  (promise-new
   (lambda (resolve reject)
     (url-retrieve url
                   (lambda (status)
                     ;; エラーは全て確実に捕捉して適切な値で `reject' する
                     (if (plist-get status :error)
                         (funcall reject (plist-get status :error))
                       (condition-case ex
                           (if (not (url-http-parse-headers))
                                 (funcall reject (buffer-string))
                               (search-forward-regexp "\n\\s-*\n" nil t)
                               (funcall resolve (xml-parse-region)))
                         (error (funcall reject ex)))))))))

(defun get-first-attribute (xml tag attribute)
  "XML 内の TAG と ATTR にマッチした最初の値を返す。"
  (decode-coding-string (cl-reduce (lambda (a b)
                                     (or a (xml-get-attribute-or-nil b attribute)))
                                   (dom-by-tag xml tag)
                                   :initial-value nil)
                        'utf-8 t))

(defun get-state-code (state)
  "都道府県名から都道府県コードを返す。"
  (1+ (cl-position
       state
       ["北海道" "青森県" "岩手県" "宮城県" "秋田県" "山形県" "福島県"
        "茨城県" "栃木県" "群馬県" "埼玉県" "千葉県" "東京都" "神奈川県"
        "新潟県" "富山県" "石川県" "福井県" "山梨県" "長野県" "岐阜県"
        "静岡県" "愛知県" "三重県" "滋賀県" "京都府" "大阪府" "兵庫県"
        "奈良県" "和歌山県" "鳥取県" "島根県" "岡山県" "広島県" "山口県"
        "徳島県" "香川県" "愛媛県" "高知県" "福岡県" "佐賀県" "長崎県"
        "熊本県" "大分県" "宮崎県" "鹿児島県" "沖縄県"]
       :test #'string=)))

(defun get-text-first-tag (xml tag)
  "XML 内の TAG にマッチした最初のテキストを返す。"
  (decode-coding-string (dom-text (car (dom-by-tag xml tag)))
                        'utf-8))

(defun main ()
  (let ((postal-code "1030000")         ; 〒103-0000
        station_name)

    ;; 出力を確認しやすくする為に `*Messages*' バッファを表示しておく
    (switch-to-buffer "*Messages*")

    ;; プロミスチェインを `xml-retrieve' から開始する
    (message "\n* 郵便番号(103-0000)から都道府県コードを取得する HTTP リクエスト")
    (promise-chain (xml-retrieve (concat "http://zip.cgis.biz/xml/zip.php?zn="
                                         postal-code))
      (then
       (lambda (xml)
         (let* ((state (get-first-attribute xml 'value 'state))
                (city (get-first-attribute xml 'value 'city))
                (state-code (get-state-code state)))
           (message " -> 都道府県コード: %s, 都道府県: %s, 市区町村: %s"
                    state-code state city)
           ;; そのまま値を返すと次の `then' に渡される
           state-code)))

      ;; 次の `then' には HTTP リクエストがある為、ここで2秒間ウェイトする
      ;; もらった値はそのまま次の `then' に渡す
      (then #'wait-2sec)

      (then
       (lambda (state-code)
         (message "\n* 都道府県コードから路線コードを取得する HTTP リクエスト")
         ;; `promise-race' は渡された複数の `Promise' の内、最初に `resolve' された値を
         ;; 保持する `Promise' を返す
         ;; タイムアウトと一緒に使うのが典型的な使い方
         (promise-race
          (vector (timeout 10.0)
                  (xml-retrieve
                   (format "http://www.ekidata.jp/api/p/%d.xml" state-code))))))

      (then
       (lambda (xml)
         (let ((line_cd (get-text-first-tag xml 'line_cd))
               (line_name (get-text-first-tag xml 'line_name)))
           (message " -> 路線コード[0]: %s, 路線名[0]: %s" line_cd line_name)
           line_cd)))

      (then #'wait-2sec)

      (then
       (lambda (line_cd)
         (message "\n* 路線コードから駅名一覧を取得する HTTP リクエスト")
         (xml-retrieve (format "http://www.ekidata.jp/api/l/%s.xml" line_cd))))

      (then
       (lambda (xml)
         (setf station_name (get-text-first-tag xml 'station_name))
         (message " -> 駅名[0]: %s" station_name)
         station_name))

      (then #'wait-2sec)

      (then
       (lambda (station_name)
         (message "\n* 駅名でWikipediaと国立国会図書館を同時にサーチする HTTP リクエスト")
         ;; `promise-all' は渡された複数の `Promise' の `resolve' された値を vector 内に
         ;; 全て保持する `Promise' を返す
         (promise-all
          (vector
           (xml-retrieve (concat "http://wikipedia.simpleapi.net/api?keyword="
                                 (url-encode-url station_name)))
           (xml-retrieve (concat "http://iss.ndl.go.jp/api/opensearch?title="
                                 (url-encode-url station_name)))))))

      (then
       (lambda (xml-vector)
         (let ((wikipedia (get-text-first-tag (aref xml-vector 0) 'body))
               (title (get-text-first-tag (aref xml-vector 1) 'dc:title)))
           (message " -> Wikipedia: %s" wikipedia)
           (message " -> タイトル[0]: %s" title)
           title)))

      (then
       (lambda (title)
         ;; タイトルにはリクエストの度に駅名が含まれたり含まれなかったりする
         (if (string-match station_name title)
             (promise-resolve "完了!")
           (promise-reject "タイトルに駅名が含まれていない (⇒ エラーにする)"))))

      ;; この例では、全てのエラーは以下の `promise-catch' で捕捉する
      (promise-catch
       (lambda (reason)
         ;; (setf reason (/ 1 0)) ; コメントアウトをはずしてして確認すべし
         ;; ↑ここでエラー(0割り)が発生すると、後続の `done' で捕捉されるが、
         ;; 続く処理(この場合、message)は実行される事がない為、エラー処理内で
         ;; エラーが発生しないように気を付ける必要がある
         (message "promise-catch: %s" reason)
         "エラーで終了"))

      ;; `done' は内部や先行の処理でエラーが発生するとそのまま Emacs のエラーを
      ;; 発生させるので、最後に行なう処理は `done' に書くとエラーが飲み込まれる
      ;; 事がなくなる
      ;; 飲み込まれるとはどういう事かは、1つ上の0割りのコメントアウトをはずして
      ;; 以下の `done' を `then' に書き換えると確認出来る
      (done #'message))))

;;; promise-examples-jp.el ends here
