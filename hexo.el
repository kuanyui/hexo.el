;; hexo.el - Utilities which make Hexo + Emacs more convenient.
;; Author: kuanyui <azazabc123@gmail.com>
;; License: WTFPL 1.0
;; Code:

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
  (if (yes-or-no-p "This operation may *change the permanent link* of this article, continue? ")
      (save-excursion
        (goto-char (point-min))
        (save-match-data
          (if (re-search-forward "^date: [0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}" nil :no-error)
              (let ((current-time (format-time-string "date: %Y-%m-%d %H:%M:%S")))
                (replace-match current-time)
                (save-buffer)
                (message (concat "Date updated: " current-time)))
            (message "Didn't find any time stamp in this article, abort."))))
    (message "Canceled.")))

;; (defun hexo-insert-article-link ()
;;   ;; permalink: :year/:month/:day/:title/
;;   (let* ((config-file (file-truename (file-truename (concat default-directory "../../_config.yml"))))
;;          permalink-format dest-file dest-date-list dest-formated-date)
;;     (if (and (file-exists-p config-file))
;;         (progn
;;           (with-temp-buffer
;;             (insert-file config-file)
;;             (string-match "^permalink: \\(.+\\)" (buffer-string))
;;             (setq permalink-format (match-string 1)))
;;           (setq dest-file
;;                 (completing-read "Select File: "
;;                                  (directory-files "." nil "^[^#\.].*[^~]$") nil t))
;;           (with-temp-buffer
;;             (insert-file dest-file)
;;             (save-match-data
;;               (string-match "^date: \\([0-9]+\\).\\([0-9]+\\).\\([0-9]+\\)"
;;                             (buffer-string))
;;               (setq dest-date-list (list (match-string 1) (match-string 2) (match-string 3)))))
;;           (setq dest-formated-date
;;                 (mapconcat
;;                  (lambda (x)
;;                    (cond ((equal ":year" x) (elt dest-date-list 1))
;;                          ((equal ":month" x) (elt dest-date-list 2))
;;                          ((equal ":day" x) (elt dest-date-list 3))
;;                          ((equal ":title" x) (replace-regexp-in-string "\.\\(md\\|org\\)$"))
;;                          ))
;;                  (split-string "" "/")
;;                  "/")
;;                                         ; split-string:
;;                                         ; ":year/:month/:day/:title/"
;;                                         ; => (":year" ":month" ":day" ":title" "")
;; 
;;           )

(defun hexo-toc-remove ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (replace-regexp "^<div id=[\"']text-table-of-contents[\"'].+$" "")
    (goto-char (point-min))
    (replace-regexp "<a id=[\"']sec-[0-9]+[\"'] name=[\"']sec-[0-9]+[\"']></a>$" "")
  ))

(defvar hexo-toc-header-and-footer-html
  ["<div id=\"text-table-of-contents\" style='background-color:#f0f0f7;border-left:5px solid #568DBE;padding:10px 20px;margin:1em 0;display:table;'><h3 style='color:#505050;margin-top:10px !important;'>Contents</h3>"
   "</div>"])

(defun hexo-toc-insert ()
  "Insert a table of contents in the position of cursor.
Note this is only for Markdown format."
  (interactive)
  (hexo-remove-toc)
  (let (toc fin (num 1))
    (save-excursion
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^# ?\\([^#] ?.+\\)" nil :no-error)
          (when (and (not (eq (face-at-point) 'markdown-inline-code-face))
                     (not (eq (face-at-point) 'markdown-pre-face)))
            (push (cons num (match-string 1)) toc)
            (end-of-line)
            (insert (format "<a id=\"sec-%s\" name=\"sec-%s\"></a>" num num))
            (incf num))))
    (insert
     (format "%s%s%s"
             (elt hexo-toc-header-and-footer-html 0)
             (mapconcat
              (lambda (x)
                (format "<a href=\"#sec-%s\" style=\"color:#505050\">%s. %s</a><br>"
                        (car x) (car x) (hexo-toc-string-replacer (cdr x))))
              (reverse toc)
              "")
             (elt hexo-toc-header-and-footer-html 1))))))

(defun hexo-toc-string-replacer (string)
  "Remove markdown markup symbols (e.g. `*_ )."
  (mapcar
   (lambda (x)
     (setq string (replace-regexp-in-string x "" string)))
   '("`" "\\*" "_"))
  string)



;; [TODO] hexo-tag-remove, hexo-tag-add, hexo-tag-select-article

(provide 'hexo)
