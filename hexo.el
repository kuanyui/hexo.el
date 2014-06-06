;; hexo.el - Utilities which make Hexo + Emacs more convenient.
;; Author: kuanyui <azazabc123@gmail.com>

;;;###autoload
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

;;;###autoload
(defun hexo-touch-files-in-dir-by-time ()
  "`touch' markdown article files according their \"date: \" to
make it easy to sort file according date in Dired.
Please run this under _posts/ or _draft/ within Dired buffer."
  (interactive)
  (if (and (eq major-mode 'dired-mode)
           (or (equal (buffer-name) "_posts")
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
    (message "Please run this under _posts/ or _drafts/ within Dired buffer.")))


;;;###autoload
(defun hexo-move-article ()
  "Move current file between _post and _draft;
You can run this function in dired or a hexo article."
  (interactive)
  (if (string-match "/\\(_posts/\\|_drafts/\\)$" default-directory)
      (let* ((parent-dir (file-truename (concat default-directory "../")))
             (dest-dir (if (string-match "_drafts/$" default-directory) "_posts/" "_drafts/")))
        (cond ((eq major-mode 'markdown-mode)
               (let* ((cur-file (buffer-file-name))
                      (new-file (concat parent-dir dest-dir (buffer-name))))
                 (save-buffer)
                 (kill-buffer)
                 (rename-file cur-file new-file)
                 (find-file new-file)
                 (message (format "Now in %s" dest-dir))))
              ((eq major-mode 'dired-mode)
               (dired-rename-file (dired-get-filename nil)
                                  (concat parent-dir dest-dir (dired-get-filename t))
                                  nil)
               (message (format "The article has been moved to %s" dest-dir)))))
    (message "You have to run this in a hexo article buffer or dired")))

;;;###autoload
(defun hexo-update-current-article-date ()
  "Update article's date by current time.
Please run this function in the article."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (save-match-data
      (if (re-search-forward "^date: [0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}" nil :no-error)
          (let ((current-time (format-time-string "date: %Y-%m-%d %H:%M:%S")))
            (replace-match current-time)
            (save-buffer)
            (message (concat "Date updated: " current-time)))
        (message "Didn't find any time stamp in this article, abort.")))))

(provide 'hexo)
