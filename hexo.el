;; hexo.el - Utilities which make Hexo + Emacs more convenient.
;; Author: kuanyui <azazabc123@gmail.com>
;; License: WTFPL 1.0
;; Code:

(defun hexo--find-command ()
  (let ((root-dir (hexo--find-root-dir)))
    (if root-dir
        (format "%s/node_modules/hexo/bin/hexo" root-dir)
      nil)))


(defun hexo--find-root-dir (&optional current-path)
  (let ((PWD (or current-path default-directory)))
    (cond ((equal (file-truename PWD) "/")
           nil)
          ((and (file-exists-p (concat PWD "/_config.yml"))
                (file-exists-p (concat PWD "/node_modules/")))
           PWD)
          (t
           (hexo--find-root-dir (file-truename (concat PWD "../")))))))

(defun hexo-run-shell-command (args-string)
  "If not found hexo, return nil"
  (if (executable-find "hexo")
      (shell-command-to-string (concat "hexo" args-string))
    (let ((hexo (hexo--find-command)))
      (if hexo
          (shell-command-to-string (concat hexo args-string))
        nil))))

;;;###autoload
(defun hexo-new ()
  "Call `hexo new` anywhere as long as in any child directory
 under a Hexo repository.
That's to say, you can use this function to create new post, even though
under theme/default/layout/"
  (interactive)
  (let* (stdout
         created-file
         (hexo (hexo--find-command)))
    (if (null hexo)
        (message "Not found hexo in your $PATH nor node_modules/, or you're not under a hexo project's directory at all."))
    (progn (setq stdout (shell-command-to-string
                         (format "%s new '%s'"
                                 hexo
                                 (read-from-minibuffer "Article URI: "))))
           (string-match "Created: \\(.+\\)$" stdout)
           (setq created-file (match-string 1 stdout))
           (find-file created-file)
           (goto-char 0)
           (when (y-or-n-p "Rename arcitle title? ")
             (replace-regexp "title: .+$" (format "title: \"%s\""
                                                  (read-from-minibuffer "Article Title: ")))
             (save-buffer)))))

(defun hexo--head (file-path)
  "get first 5 lines of a file as a string"
  (with-temp-buffer
    (insert-file-contents file-path)
    (let ((lines (split-string (buffer-string) "\n" t)))
      (if (>= (length lines) 5)
          (mapconcat #'identity (loop for i from 0 to 4 collect (nth i lines)) "\n")
        (buffer-string)
        ))))

;;;###autoload
(defun hexo-touch-files-in-dir-by-time ()
  "`touch' markdown article files according their \"date: \" to
make it easy to sort file according date in Dired.
Please run this under _posts/ or _draft/ within Dired buffer."
  (interactive)
  (if (and (eq major-mode 'dired-mode)
           (or (equal (buffer-name) "_posts")
               (equal (buffer-name) "_draft")))
      
      (lexical-let (file-list touch-commands)
        (setq file-list (directory-files (dired-current-directory)))
        (progn
          (mapcar
           (lambda (current-file-name)
             (if (and (not (string-match "#.+#$" current-file-name))
                      (not (string-match ".+~$" current-file-name))
                      (not (string-match "^\.\.?$" current-file-name))
                      (string-match ".+\.md$" current-file-name))
                 (lexical-let (touch-cmd head)
                   (setq head (hexo--head current-file-name))
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
                                   current-file-name)))
                   (push touch-cmd touch-commands))
               ))
           file-list)) ;; 這個file-list為lambda的arg
        (shell-command (mapconcat #'identity touch-commands ";"))
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
  "Update article's date stamp (at the head) by current time.
Please run this function in the article."
  (interactive)
  (cond
   ((not (eq major-mode 'markdown-mode))
    (message "Please run this function in a markdown file. Action cancelled."))
   ((yes-or-no-p "This operation may *change the permanent link* of this article, continue? ")
    (save-excursion
      (goto-char (point-min))
      (save-match-data
        (if (re-search-forward "^date: [0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}" nil :no-error)
            (let ((current-time (format-time-string "date: %Y-%m-%d %H:%M:%S")))
              (replace-match current-time)
              (save-buffer)
              (message (concat "Date updated: " current-time)))
          (message "Didn't find any time stamp in this article, abort.")))))
   ))

(defun hexo-insert-article-link ()
  "Insert a link to other article in _posts/."
  (interactive)
  (if (or
       (not (or                         ;if not exist "../_posts" directory
             (mapcar (lambda (x)
                       (and (file-directory-p (concat "../" x))
                            (equal "_posts" x)))
                     (directory-files "../"))))
       (not (eq major-mode 'markdown-mode)))
      (message "Please run this command in an article of a Hexo repository.")

    (let* ((config-file (file-truename (file-truename (concat default-directory "../../_config.yml"))))
           permalink-format article-file-name article-link original-article-title)
      (if (and (file-exists-p config-file))
          (progn
            (setq article-file-name
                  (ido-completing-read "Select Article: "
                                       (mapcar
                                        (lambda (x) (substring x 0 -3)) ;remove ".md$"
                                        (directory-files "../_posts" nil "^[^#\.].*\\.md$")) nil t))
            (with-temp-buffer
              (insert-file config-file)
              (string-match "^permalink: \\(.+\\)" (buffer-string))
              (setq permalink-format (replace-regexp-in-string
                                      ":year" "%Y"
                                      (replace-regexp-in-string
                                       ":month" "%m"
                                       (replace-regexp-in-string
                                        ":day" "%d"
                                        (replace-regexp-in-string
                                         ":title" article-file-name
                                         (match-string 1 (buffer-string)))))))
              (string-match "^root: \\(.+\\)" (buffer-string)) ;concat root
              (setq permalink-format (concat (match-string 1 (buffer-string)) permalink-format)))

            (with-temp-buffer
              (insert-file-contents (format "../_posts/%s.md" article-file-name))
              (string-match "^date: *\\([^ ].+$\\)" (buffer-string))
              (message (match-string 1 (buffer-string)))
              (setq article-link
                    (format-time-string permalink-format
                                        (apply #'encode-time
                                               (parse-time-string (match-string 1 (buffer-string))))
                                        ))
              (string-match "^title: [\"']?\\(.+\\)[\"']? *$" (buffer-string))
              (setq original-article-title (match-string 1 (buffer-string)))
              )

            (if (y-or-n-p (format "Use original article title \"%s\" ? " original-article-title))
                (insert (format "[%s](%s)" original-article-title article-link))
              (insert (format "[%s](%s)" (read-from-minibuffer "Title: ") article-link)))
            )))))

;; [TODO] hexo-tag-remove, hexo-tag-add, hexo-tag-select-article

  (provide 'hexo)
