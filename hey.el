(defun hey-create-edit-buffer ()
  "投稿編集バッファの作成"
  (let ((buf-name "*HEY EDIT*"))
    (or
     (get-buffer buf-name)
     (generate-new-buffer buf-name))))

(defun hey-edit-mode ()
  "投稿編集モード"
  (interactive)
  (let ((buf (hey-create-edit-buffer)))
    (set-buffer buf)
    (switch-to-buffer buf)  ;; バッファ切り替え
    (kill-all-local-variables)  ;; バッファローカル変数初期化
    (setq mode-name "HEY")  ;; major-mode名の設定
    (use-local-map hey-edit-mode-map)  ;; key mapの適応
    ))

(defun hey-post (&rest args)
  "投稿"
  (interactive)
  (message "posted!!")
  (kill-buffer "*HEY EDIT*"))

(defvar hey-edit-mode-map (make-sparse-keymap))  ;; 編集モードのキーマップ

(when hey-edit-mode-map
  (let ((km hey-edit-mode-map))
    (define-key km (kbd "C-c C-c") 'hey-post)))

(bind-key* "C-t C-w" 'hey-edit-mode)

(provide 'hey)
