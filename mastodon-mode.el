;; 開始    : (mstdn-timeline-reader-start)
;; クリア  : (mstdn-timeline-process-buffer-clear)
;; 停止    : (mstdn-timeline-reader-stop)
;; 消去    : (mstdn-clear-entries)


(require 'cl)
(require 'json)
(require 'subr-x)
(require 'request)
(require 'xah-replace-pairs)

(defvar timeline-buffer-name "*TIMELINE*")  ;; timelineのバッファ名

;; timelineに表示されるエントリーのフォーマット
(defvar timeline-mstdn-format "-----------------------------------------------
%s: by %s @%s %s from %s
..............................
%s

")

;; 投稿エントリー編集用バッファのフォーマット
(setq mstdn-toot-footer "%s

#----------------------------------------------------------------
# in_reply_to_id: %s;
# sensitive: ; NSFW CW
# spoiler_text: ;
# visibility: %s; direct, private, unlisted, public
# ---------------------------------------------------------------
")
(defvar mstdn-url-user "https://mstdn.jp/api/v1/streaming/user/")  ;; home timelineのurl
(defvar mstdn-url-local "https://mstdn.jp/api/v1/streaming/public/local/")  ;; local timelineのurl
(defun mstdn-url-get-current ()  mstdn-url-user)
(defvar mstdn-timeline-process-name "*MSTDN*")  ;; timelineを取得するcurlのプロセス名
(defvar mstdn-timeline-process-buffer-name "*MSTDN/mstdn.jp/global/local*")  ;; timelineを取得するcurlのプロセスバッファ名
(defvar mastodon-edit-buffer-name "*MASTODON*")  ;; 編集バッファ名
(defvar mastodon-auth-token-file "~/.config/mastodon/mstdn.jp/user.txt")  ;; auth token保持ファイル
(defvar mastodon-edit-mode-name "mastodon-edit")  ;; 編集モード名
(defvar mastodon-timeline-mode-map (make-sparse-keymap))  ;; タイムラインモードのキーマップ
(defvar mastodon-edit-mode-map (make-sparse-keymap))  ;; 編集モードのキーマップ
(defvar mastodon-timeline-mode-name "mastodon timeline")  ;; タイムラインモード名
(defvar mastodon-timeline-buffer-name timeline-buffer-name)  ;; タイムラインバッファ名
(defvar mastodon-api-endpoint "https://mstdn.jp")  ;; APIサーバ
(defvar mastodon-api-endpoint-accounts-follow (concat mastodon-api-endpoint "/api/v1/accounts/%d/follow"))  ;; フォローエンドポイント
(defvar mastodon-api-endpoint-statuses (concat mastodon-api-endpoint "/api/v1/statuses"))  ;; トゥーとエンドポイント
(defvar mastodon-api-endpoint-favourite (concat mastodon-api-endpoint "/api/v1/statuses/%d/favourite"))  ;; ファボエンドポイント
(defvar mastodon-api-endpoint-reblog (concat mastodon-api-endpoint "/api/v1/statuses/%d/reblog"))  ;; リブログエンドポイント


(setq mstdn-timeline-process-timer nil)  ;; timeline取得用のプロセスタイマー
(setq mstdn-timeline-process-ctl-timer nil)  ;; timeline取得用のプロセスのコントロール用タイマー

(defun mstdn-credential-api-token ()
  "auth tokenを取得する"
  (with-temp-buffer
    (insert-file-contents "~/.config/mastodon/mstdn.jp/user.txt")
    (string-trim
     (buffer-substring-no-properties (point-min) (point-max)))))


(defun mstdn-api-build-header (api-token)
  "APIヘッダをビルドする"
  (concat "Authorization: " (format "Bearer %s" api-token)))


(defun mstdn-api-header ()
  "APIヘッダを取得する"
  (mstdn-api-build-header (mstdn-credential-api-token)))


(defun mstdn-curl-cmd (url)
  "curlコマンドを生成する"
  `("curl" "--no-buffer" "--header" ,(mstdn-api-header) ,url))


;; mastodon protocol user adapter
(defun mstdn-entry-id (entry)
  (cdr (assoc 'id (assoc :data entry))))

(defun mstdn-entry-content (entry)
  (cdr (assoc 'content (assoc :data entry))))

(defun mstdn-entry-in-reply-to-account-id (entry)
  (cdr (assoc 'in_reply_to_account_id (assoc :data entry))))

(defun mstdn-entry-application (entry)
  (cdr (assoc 'application (assoc :data entry))))

(defun mstdn-application-name (app)
  (cdr (assoc 'name app)))

(defun mstdn-entry-user (entry)
  (cdr (assoc 'account (assoc :data entry))))

(defun mstdn-user-id (user)
  (cdr (assoc 'id user)))

(defun mstdn-user-username (user)
  (cdr (assoc 'username user)))

(defun mstdn-user-display-name (user)
  (cdr (assoc 'display_name user)))


(defun mstdn-clear-entries ()
  "全削除"
  (with-current-buffer (get-buffer-create timeline-buffer-name)
    (erase-buffer)))


(defun mstdn-insert-entry (entry)
  "書き込み"
  (with-current-buffer (get-buffer-create timeline-buffer-name)
    (let* ((cur (point))
           (pos (point-marker))
           (user (mstdn-entry-user entry))
           (app (mstdn-entry-application entry))
           (text (format timeline-mstdn-format 1 1 "a" "b" "c" "d"))
           ;; (text (format timeline-mstdn-format
           ;;               "" ; mstdn-entry-id entry)
           ;;               "" ; (mstdn-user-id user)
           ;;               "" ; (mstdn-user-username user)
           ;;               "" ; (mstdn-user-display-name user)
           ;;               "" ; (mstdn-application-name app)
           ;;               "" ;(mstdn-entry-content entry)
           ;;               ))
           )
      (message text))))



(defun mstdn-insert-entry (entry)
  "書き込み"
  (with-current-buffer (get-buffer-create timeline-buffer-name)
    (let* ((cur (point))
           (pos (point-marker))
           (user (mstdn-entry-user entry))
           (in-reply-to-account-id (mstdn-entry-in-reply-to-account-id entry))
           (app (mstdn-entry-application entry))
           (text (format timeline-mstdn-format
                         (mstdn-entry-id entry)
                         (mstdn-user-id user)
                         (mstdn-user-username user)
                         (mstdn-user-display-name user)
                         (mstdn-application-name app)
                         (mstdn-entry-content entry))))
      (if (and (not (equal "sync.twi2mstdn.space" (mstdn-application-name app)))
               (or (equal nil in-reply-to-account-id)
                   (equal "24312" in-reply-to-account-id)))
          (progn
            (goto-char 1)
            (insert (mstdn-text-plain text))
            (if (< cur 50) ;; なんとなくこの文字以内なら下に流れないようにする
                (goto-char cur)
              (goto-char (marker-position pos))))))))

(defun mstdn-output-filter (process output)
  "一時バッファに書き込み"
  (with-current-buffer (process-buffer process)
    (insert output)))



(defun mstdn-timeline-reader-start ()
  "TLを取得すプロセスを実行する"
  (if (not (get-process mstdn-timeline-process-name))
      (progn
        (apply 'start-process
               (append `(,mstdn-timeline-process-name
                         ,mstdn-timeline-process-buffer-name)
                       (mstdn-curl-cmd (mstdn-url-get-current))))
        (setq mstdn-timeline-process-ctl-timer
          (run-at-time "0 sec" 4 'mstdn-insert-entry-timer-ctl)))))


(defun mstdn-timeline-process-timer-cancel ()
  (if mstdn-timeline-process-timer
      (progn
        (cancel-timer mstdn-timeline-process-timer)
        (setq mstdn-timeline-process-timer nil))))


(defun mstdn-insert-entry-sync ()
  (ignore-errors
    (progn
      (mstdn-insert-entry (mstdn-get-entry))
      (if (with-current-buffer (get-buffer mstdn-timeline-process-buffer-name)
            (< (buffer-size) 10))
          (mstdn-timeline-process-timer-cancel)))
    t))


(defun mstdn-insert-entry-timer-ctl ()
  "エントリー挿入タイマーの制御"
  (ignore-errors
    (progn
      (mstdn-timeline-process-timer-cancel) ;; エントリー挿入タイマーの停止
      (if (with-current-buffer (get-buffer mstdn-timeline-process-buffer-name)
            (> (buffer-size) 10))
          (progn
            ;; エントリー挿入タイマーの生成
            (setq mstdn-timeline-process-timer
                  (run-at-time
                   "0 sec" 0.3
                   'mstdn-insert-entry-sync)))))))


(defun mstdn-timeline-process-buffer-clear ()
  "書き込み一時バッファをクリアする"
  (with-current-buffer (get-buffer-create mstdn-timeline-process-buffer-name)
    (erase-buffer)))


(defun mstdn-timeline-reader-stop ()
  "TLを取得すプロセスを停止する"
  (mstdn-timeline-process-buffer-clear)
  (let* ((proc (get-process mstdn-timeline-process-name)))
    (kill-process proc))
  (kill-buffer mstdn-timeline-process-buffer-name)
  (mstdn-timeline-process-timer-cancel))



(defun mstdn-get-entry ()
  "Mastodonのstreamの1エントリを取得する"
  (with-current-buffer (get-buffer mstdn-timeline-process-buffer-name)
    (if (not (eq 1 (point-max)))
        (progn
          (goto-char (point-min))
          (let ((event_ (buffer-substring-no-properties (re-search-forward "event: ")
                                                        (re-search-forward "\n")))
                (data_ (buffer-substring-no-properties (re-search-forward "data: ")
                                                       (re-search-forward "\n"))))
            (delete-region (point-min) (point))
            (goto-char (point-max))
            `((:event . ,(string-trim event_))
              (:data . ,(json-read-from-string (string-trim data_)))))))))




(defun mastodon-edit-buffer ()
  "ステータス編集用バッファ"
  (or
   (get-buffer mastodon-edit-buffer-name)
   (generate-new-buffer mastodon-edit-buffer-name)))


(defun mastodon-edit-buffer-close ()
  "編集用バッファを削除する"
  (kill-buffer (mastodon-edit-buffer)))

(defun mastodon-edit-active ()
  "編集用バッファに移動する"
  (interactive)
  (mastodon-edit-mode)
  (with-current-buffer (mastodon-edit-buffer)
    (if (eq (point-max) 1)
        (progn
          (insert (format mstdn-toot-footer "" "" "unlisted"))
          (goto-char 1)))))



(defun mastodon-edit-reply-active ()
  "リプライ編集用バッファに移動する"
  (interactive)
  (let ((current-id (mstdn-timeline-entry-current-id))
        (reply-to (mstdn-timeline-entry-username)))
    (mastodon-edit-mode)
    (with-current-buffer (mastodon-edit-buffer)
      (if (eq (point-max) 1)
          (progn
            (insert (format mstdn-toot-footer reply-to current-id "private"))
            (goto-char 1))))))

(defun mstdn-timeline-entry-goto-current-entry ()
  (let ((cur (point)))
    (move-end-of-line 1)
    (search-backward "-------------------------------")
    (move-beginning-of-line 2)
    cur))


(defun mstdn-timeline-entry-goto-next-entry-active ()
  "次のエントリーに移動"
  (interactive)
  (mstdn-timeline-entry-goto-current-entry)
  (move-beginning-of-line -1)
  (mstdn-timeline-entry-goto-current-entry))

(defun mstdn-timeline-entry-goto-before-entry-active ()
  "前のエントリーに移動"
  (interactive)
  (mstdn-timeline-entry-goto-current-entry)
  (search-forward "-------------------------------")
  (mstdn-timeline-entry-goto-current-entry))

(defun mstdn-timeline-entry-current-id ()
  "TL上のトゥートからトゥートidを取得する"
  (let ((cur (mstdn-timeline-entry-goto-current-entry))
        (start (point)))
    (forward-word)
    (let ((end (point)))
      (goto-char cur)
      (string-to-number (buffer-substring-no-properties start end)))))

(defun mstdn-re-search (pattern)
  "カレントバッファから正規表現にマッチする文字列を取得"
  (re-search-forward pattern nil t)
  (match-string 0))

(defun mstdn-timeline-entry-username ()
  "TL上のトゥートからトゥートしたusernameを取得する"
  (let ((cur (mstdn-timeline-entry-goto-current-entry))
        (username (mstdn-re-search "\@[^\s]+")))
    (goto-char cur)
    username))


(defun mastodon-fav-active ()
  "ファボ投下コマンド"
  (interactive)
  (let ((current-id (mstdn-timeline-entry-current-id)))
    (mastodon-api-favourite current-id)
    (message (format "%d favorited!!" current-id))))


(defun mastodon-reblog-active ()
  "BTコマンド"
  (interactive)
  (let ((current-id (mstdn-timeline-entry-current-id)))
    (mastodon-api-reblog current-id)
    (message (format "%d rebloged!!" current-id))))


(defun mastodon-fabre-active ()
  "ファボ + BT + トゥートコマンド"
  (interactive)
  (let ((current-id (mstdn-timeline-entry-current-id)))
    (mastodon-api-favourite current-id)
    (mastodon-api-reblog current-id)
    (mastodon-edit-mode)
    (message (format "%d faved and rebloged and toot!!" current-id))))


(defun mastodon-timeline-entry-id-active ()
  "テスト用"
  (interactive)
  (let ((current-id (mstdn-timeline-entry-current-id)))
    (message (format "%d" current-id))))


(defun mastodon-edit-post-active ()
  "投稿する"
  (interactive)
  (mastodon-api-post)
  (message "posted!!")
  (mastodon-edit-buffer-close)
  (switch-to-buffer (mastodon-timeline-buffer)))


(defun mastodon-auth-token ()
  "auth tokenを取得する"
  (with-temp-buffer
    (insert-file-contents mastodon-auth-token-file)
    (string-trim
     (buffer-substring-no-properties (point-min) (point-max)))))


(defun* mstdn-status-edit-attr (name text &optional (coerce nil))
  "statusの属性を取得す"
  (string-match (format "^# %s\:.\\(.*\\);.*$" name) text)
  (let ((value (match-string 1 text)))
    (if (equal "" value) nil
      (if coerce (funcall coerce value)
        value))))


(defun mastodon-new-status ()
  "新規投稿"
  (with-current-buffer (mastodon-edit-buffer)
    (let ((txt (buffer-substring-no-properties (point-min) (point-max))))
      `(("status" . ,(car (split-string txt "#-----------")))
        ("in_reply_to_id" . ,(or (mstdn-status-edit-attr "in_reply_to_id" txt) ""))
        ("sensitive" . ,(or (mstdn-status-edit-attr "sensitive" txt) ""))
        ("spoiler_text" . ,(or (mstdn-status-edit-attr "spoiler_text" txt) ""))
        ;; ("visibility" . ,(or (mstdn-status-edit-attr "visibility" txt) ""))
        ("visibility" . "unlisted")
        ))))

(defun mastodon-api-headers ()
  "API リクエストヘッダ"
  `(("Authorization" . ,(concat "Bearer " (mastodon-auth-token)))))



(defun mastodon-api-payload-new-status ()
  "投稿用のペイロード"
  `(("status" . ,(mastodon-new-status))))


(defun mastodon-api-post ()
  "Mastodonにstatusをpostする"
  (request mastodon-api-endpoint-statuses
           :type "POST"
           :headers (mastodon-api-headers)
           :data (mastodon-new-status)
           :success (cl-function (lambda (&key data &allow-other-key)
                                   (mastodon-edit-buffer-close)))
           :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                                 (message "Got error: %S" error-thrown)))))



(defun mastodon-api-reply ()
  "Mastodonにstatusをreplyする"
  (request mastodon-api-endpoint-statuses
           :type "POST"
           :headers (mastodon-api-headers)
           :data (mastodon-api-payload-reply-status)
           :success (cl-function (lambda (&key data &allow-other-key)
                                   (mastodon-edit-buffer-close)))
           :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                                 (message "Got error: %S" error-thrown)))))


(defun mastodon-api-favourite (id)
  "Mastodonにstatusをfavする"
  (request (format mastodon-api-endpoint-favourite id)
           :type "POST"
           :headers (mastodon-api-headers)
           :success (cl-function (lambda (&key data &allow-other-key)
                                   (mastodon-edit-buffer-close)))
           :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                                 (message "Got error: %S" error-thrown)))))


(defun mastodon-api-reblog (id)
  "Mastodonにstatusをreblogする"
  (request (format mastodon-api-endpoint-reblog id)
           :type "POST"
           :headers (mastodon-api-headers)
           :success (cl-function (lambda (&key data &allow-other-key)
                                   (mastodon-edit-buffer-close)))
           :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                                 (message "Got error: %S" error-thrown)))))

(defun mastodon-api-follow (id)
  "followする"
  (request (format mastodon-api-endpoint-accounts-follow id)
           :type "POST"
           :headers (mastodon-api-headers)
           :success (cl-function (lambda (&key data &allow-other-key)
                                   (message "followed!!")))
           :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                                 (message "Got error: %S" error-thrown)))))

(defun mastodon-timeline-buffer ()
  "タイムライン用バッファ"
  (or
   (get-buffer mastodon-timeline-buffer-name)
   (generate-new-buffer mastodon-timeline-buffer-name)))


(defun mastodon-timeline-buffer-close ()
  "編集用バッファを削除する"
  (kill-buffer (mastodon-timeline-buffer)))


(defun mastodon-timeline-active ()
  "タイムラインバッファに移動する"
  (interactive)
  ;; (kill-all-local-variables)
  (switch-to-buffer (mastodon-timeline-buffer))
  (setq mode-name mastodon-timeline-mode-name)
  (use-local-map mastodon-mode-map))


(defun mastodon-timeline-mode ()
  "タイムラインモード開始"
  (interactive)
  (let ((buf (mastodon-timeline-buffer)))
    (kill-all-local-variables)
    (mstdn-timeline-reader-start)
    (set-buffer buf)
    (switch-to-buffer buf)
    (setq mode-name mastodon-timeline-mode-name)
    (use-local-map mastodon-timeline-mode-map)))

(when mastodon-timeline-mode-map
  (let ((km mastodon-timeline-mode-map))
    (define-key km (kbd "C-c C-s") 'mastodon-edit-active)  ;; トゥート
    (define-key km (kbd "C-j") 'mstdn-timeline-entry-goto-before-entry-active) ;; 前のエントリーに移動
    (define-key km (kbd "C-k") 'mstdn-timeline-entry-goto-next-entry-active) ;; 次のエントリに移動
    (define-key km (kbd "RET") 'mastodon-edit-reply-active) ;; リプライ
    (define-key km (kbd "C-i") 'mastodon-fav-active) ;; ファボ
    (define-key km (kbd "C-d") 'mastodon-reblog-active) ;; ブースト
    (define-key km (kbd "C-u") 'mastodon-fabre-active) ;; ファブリ
    (define-key km (kbd "C-c C-@") 'mstdn-refresh-client-active) ;; 再起動
    ))


(when mastodon-edit-mode-map
  (let ((km mastodon-edit-mode-map))
    (define-key km (kbd "C-c C-c") 'mastodon-edit-post-active)))


(defun mastodon-mode ()
  "Major mode for mastodon."
  (interactive)
  (mastodon-timeline-mode))


(defun mastodon-edit-mode ()
  "投稿編集モード"
  ;; バッファ生成
  (let ((buf (mastodon-edit-buffer)))
    (set-buffer buf)
    ;; バッファ切り替え
    (switch-to-buffer buf)
    ;; バッファローカル変数初期化
    (kill-all-local-variables)
    ;; major-mode名の設定
    (setq mode-name mastodon-edit-mode-name)
    ;; key mapの適応
    (use-local-map mastodon-edit-mode-map)
    ))

(defun mastodon ()
  "Start mastodon mode."
  (interactive)
  (mastodon-mode))


(defun mstdn-fabre (status-id)
  "ファボ -> BT -> エアリプ の流れ"
  (mastodon-api-favourite status-id)
  (mastodon-api-reblog status-id)
  (mastodon-edit-mode))


(defun mstdn-text-plain (txt)
  "mastodonのbodyはhtmlで帰ってくるためタグを表示用のtextに変換する"
  (replace-regexp-in-string
   "<br />" "\n"
   (replace-regexp-in-string
    "</p>" ""
    (replace-regexp-in-string
     "<p>" "" txt))))

(defun mstdn-text-plain (txt)
  "mastodonのbodyはhtmlで帰ってくるためタグを表示用のtextに変換する"
  (xah-replace-regexp-pairs-in-string txt
                                      '(("<br />" "\n")
                                        ("<[^\<\>]+>" ""))))



(defun mstdn-refresh-client-active ()
  "クライアント再起動"
  (interactive)
  (ignore-errors
    (mstdn-timeline-reader-stop))
  (mstdn-timeline-reader-start))



(provide 'mastodon-mode)
