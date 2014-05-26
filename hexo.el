;; hexo.el - Utilities which make Hexo + Emacs more convenient.
;; Author: kuanyui <azazabc123@gmail.com>
(defun hexo-new ()
  "Call `hexo new` anywhere as long as in any child directory
 under a Hexo repository.
That's to say, you can use this function to create new post, even though
under theme/default/layout/"
  (interactive)
  (let* (BUFFER-STRING
         OUTPUT
         (DEF-DIR (if (boundp 'DEF-DIR)
                      DEF-DIR
                    default-directory)))
    (if (file-exists-p (format "%s%s" DEF-DIR "_config.yml"))
        (progn
          (with-temp-buffer
            (insert-file-contents (format "%s%s" DEF-DIR "_config.yml"))
            (setq BUFFER-STRING (buffer-string)))
          (if (and (string-match "title: " BUFFER-STRING)
                   (string-match "url: " BUFFER-STRING)
                   (string-match "new_post_name: " BUFFER-STRING))
              ;; call `hexo new` command
              (let ((default-directory DEF-DIR))
                (setq OUTPUT (shell-command-to-string
                              (concat "hexo new '"
                                      (read-from-minibuffer
                                       "Title of the new article: ") "'")))
                (string-match "/.*\\.md$" OUTPUT)
                (find-file (match-string 0 OUTPUT)))
            (progn (setq DEF-DIR (file-truename (concat DEF-DIR "../")))
                   (hexo-new))))
      (progn
        (if (not (equal DEF-DIR "/"))
            (progn (setq DEF-DIR (file-truename (concat DEF-DIR "../")))
                   (hexo-new))
          (progn
            (message "Not in a hexo or its child directory.")))))))

;; 根據文章內容來 touch -t 文章以方便按照時間排序。
(defun hexo-touch-files-in-dir-by-time ()
  "`touch' markdown article files according their \"date: \" to
make it easy to sort file according date in Dired.
Please run this under _post/ or _draft/ within Dired buffer."
  (interactive)
  (if (and (eq major-mode 'dired-mode)
           (or (equal (buffer-name) "_post")
               (equal (buffer-name) "_draft")))
      (progn
        (let (current-file-name file-list)
          (setq file-list (directory-files (dired-current-directory)))
          (progn
            (mapcar
             (lambda (current-file-name)
               (if (and (not (string-match "#.+#$" current-file-name))
                        (not (string-match ".+~$" current-file-name))
                        (not (string-match "^\.\.?$" current-file-name))
                  (string-match ".+\.md$" current-file-name))
                   (let (touch-cmd head)
                     (setq head
                           (shell-command-to-string
                            (format "head -n 5 '%s'" current-file-name)))
                     (save-match-data
                       (string-match "^date: \\([0-9]+\\)-\\([0-9]+\\)-\\([0-9]+\\) \\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\)$" head)
                       (setq touch-cmd
                             (format "touch -t %s%s%s%s%s.%s %s"
                                     (match-string 1 head)
                                     (match-string 2 head)
                                     (match-string 3 head)
                                     (match-string 4 head)
                                     (match-string 5 head)
                                     (match-string 6 head)
                                     current-file-name
                                     )))
                     (shell-command touch-cmd))
                 ))
             file-list))) ;; 這個file-list為lambda的arg
        (revert-buffer)
        (message "Done."))
    (message "Please run this under _post/ or _draft/ within Dired buffer.")))


;; 將當前檔案在 _post 與 _drafts 兩者之間切換（mv）。
(defun hexo-move-article ()
  "Move current file between _post and _draft"
  (interactive)
  (let* ((cur-file (buffer-name))
         (cur-dir DEF-DIR)
         (cur-path (buffer-file-name)))
    (save-buffer)
    (save-match-data
      (if (string-match "\\(.+/\\)_posts/$" cur-dir)
          (let* ((new-file-name (format "%s%s%s" (match-string 1 cur-dir) "_drafts/" cur-file)))
            (kill-buffer)
            (rename-file cur-path new-file-name)
            (find-file new-file-name)
            (message "Now in \"_drafts\""))
        (if (string-match "\\(.+/\\)_drafts/$" cur-dir)
            (let* ((new-file-name (format "%s%s%s" (match-string 1 cur-dir) "_posts/" cur-file)))
              (kill-buffer)
              (rename-file cur-path new-file-name)
              (find-file new-file-name)
              (message "Now in \"_posts\""))
          (message "Current file doesn't in _posts or _drafts directory."))))))

