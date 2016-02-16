;; hexo.el - Utilities which make Hexo + Emacs more convenient.
;; Author: kuanyui <azazabc123@gmail.com>
;; License: WTFPL 1.0
;; Code:

;; ======================================================
;; Small tools
;; ======================================================

(defun hexo-get-file-head-lines (file-path &optional n)
  "Get the first N lines of a file as a list."
  (with-temp-buffer
    (insert-file-contents file-path)
    (let ((lines (split-string (buffer-string) "\n" t)))
      (if (null n)
          lines
        (subseq lines 0 (1- n))))))

(defun hexo-get-file-head-lines-as-string (file-path &optional n)
  "Get first N lines of a file as a string."
  (mapconcat #'identity (hexo-get-file-head-lines file-path n) "\n"))

(defun hexo-find-command ()
  "Try to find hexo in node_modules/ directory.
If not found, try to `executable-find' hexo in your system."
  (let* ((root-dir (hexo-find-root-dir))
         (guessed-hexo (format "%s/node_modules/hexo/bin/hexo" root-dir)))
    (if (and root-dir (file-exists-p guessed-hexo))
        guessed-hexo
      (executable-find "hexo"))))

(defun hexo-find-root-dir (&optional current-path)
  "Try to find the root dir of a Hexo repository."
  (let ((PWD (or current-path default-directory)))
    (cond ((equal (file-truename PWD) "/")
           nil)
          ((and (file-exists-p (concat PWD "/_config.yml"))
                (file-exists-p (concat PWD "/node_modules/")))
           (directory-file-name PWD))   ;remove final slash of PWD
          (t
           (hexo-find-root-dir (file-truename (concat PWD "../")))))))

(defun hexo-ask-for-root-dir ()
  (let ((dir (hexo-find-root-dir (read-directory-name
                                  "Please input the root path of an exist Hexo repository: "))))
    (if dir
        dir
      (progn (message "Seems not a valid Hexo repository. Please try again.")
             (sit-for 5)
             (hexo-ask-for-root-dir)))))


(defun hexo-run-shell-command (args-string)
  "If not found hexo, return nil"
  (if (executable-find "hexo")
      (shell-command-to-string (concat "hexo" args-string))
    (let ((hexo (hexo-find-command)))
      (if hexo
          (shell-command-to-string (concat hexo args-string))
        nil))))

;; ======================================================
;; Article manager
;; ======================================================
(require 'tabulated-list)

(defvar-local hexo-root-dir nil
  "Root directory of a hexo-mode buffer")
(put 'hexo-root-dir 'permanent-local t)

(define-derived-mode hexo-mode tabulated-list-mode "Hexo"
  "Major mode for manage Hexo articles."
  (hl-line-mode 1)
  (setq tabulated-list-format
        `[("Status" 6 nil)
          ("Filename" 48 nil)
          ("Title" 48 nil)
          ("Date"  12 nil)
          ("Categories"  16 nil)
          ("Tags"  0 nil)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Title" nil))
  (setq hexo-root-dir (or (hexo-find-root-dir)
                          (hexo-ask-for-root-dir)))
  (add-hook 'tabulated-list-revert-hook 'hexo-refresh nil t)
  (tabulated-list-init-header))

(defun hexo-refresh ()
  ;; Each element in `tabulated-list-entries' is like:
  ;; (FileFullPath ["test.md" "Title" "2013/10/24" "category" "tag, tag2"])
  ;; ^ id           ^ entry
  (setq tabulated-list-entries
        (hexo-generate-list-entries hexo-root-dir)))

(defun hexo-directory-files (dir-path)
  "The same as `directory-files', but remove:
0. all not .md files
1. temporary files
2. special files (e.g. '..')
3. invalid files (e.g. a broken symbolic link)
"
  (remove-if (lambda (x) (or
                      (not (file-exists-p x))
                      (not (string-suffix-p ".md" x))
                      (member (file-name-base x) '("." ".."))
                      ;;(string-suffix-p "#" x) ;useless
                      (string-suffix-p "~" x)))
             (directory-files dir-path 'full)))

(defun hexo-generate-list-entries (&optional repo-root-dir)
  (mapcar #'hexo-generate-file-entry
          (hexo-get-all-article-files repo-root-dir)))

(defun hexo-get-all-article-files (&optional repo-root-dir)
  "Return a files list containing full-paths of all articles."
  (let* ((root (or repo-root-dir hexo-root-dir (hexo-find-root-dir)))
         (posts-dir (format "%s/source/_posts/" root))
         (drafts-dir (format "%s/source/_drafts/" root)))
    (append (hexo-directory-files posts-dir)
            (hexo-directory-files drafts-dir))))

(defun hexo-remove (regexp string)
  (replace-regexp-in-string regexp "" string t))

(defun hexo-trim-quotes (string)
  (hexo-remove "[\"']$" (hexo-remove "^[\"']" string)))

(defun hexo-trim-spaces (string)
  (hexo-remove " *$" (hexo-remove "^ *" string)))

(defun hexo-trim (string)
  (hexo-trim-quotes (hexo-trim-spaces string)))

(defun hexo-parse-tags (string)
  "Return a list containing tags"
  (cond ((string-match "\\[\\(.+\\)\\]" string)
         (let* ((raw (match-string 1 string)) ; "this", "is", "tag"
                (raw (replace-regexp-in-string ", " "," raw 'fixedcase)))
           (mapcar #'hexo-trim-quotes (split-string raw ","))))
        ((string-match "^ *$" string)
         '())
        (t
         (list (hexo-trim string)))))

(defun hexo-generate-file-entry (file-path)
  "Generate entry of a markdown FILE-PATH"
  (let* ((lines (hexo-get-file-head-lines file-path 6))
         (assoc-list
          (remove-if #'null
                     (mapcar (lambda (line)
                               (cond ((string-match "^title: ?\\(.+\\)" line)
                                      (cons 'title (hexo-trim (match-string 1 line))))
                                     ((string-match "^date: ?\\([0-9].+\\) " line) ;hide time
                                      (cons 'date (match-string 1 line)))
                                     ((string-match "^tags: ?\\(.+\\)" line)
                                      (cons 'tags (hexo-parse-tags (match-string 1 line))))
                                     ((string-match "^categories: ?\\(.+\\)" line)
                                      (cons 'categories (hexo-parse-tags (match-string 1 line))))
                                     (t nil)))
                             lines))))
    (list file-path
          (vector
           ;; status
           (if (equal (hexo-get-article-parent-dir-name file-path) "_posts") "post" "draft")
           ;; filename
           (file-name-base file-path)
           (cdr (assq 'title assoc-list))
           (cdr (assq 'date assoc-list))
           (mapconcat #'identity (cdr (assq 'categories assoc-list)) " ")
           (mapconcat #'identity (cdr (assq 'tags assoc-list)) " ")
           ))))

(defun hexo-get-article-parent-dir-name (file-path)
  "Return _posts or _drafts"
  (file-name-nondirectory
   (directory-file-name
    (file-name-directory file-path))))

(defun hexo ()
  (interactive)
  (require 'finder-inf nil t)
  (let* ((buf (get-buffer-create "*Hexo*"))
         (win (get-buffer-window buf)))
    (with-current-buffer buf
      (hexo-mode))
    (if win
        (select-window win)
      (switch-to-buffer buf))
    (hexo-refresh)
    (tabulated-list-print 'remember-pos)))



;; ======================================================
;; Commands for hexo-mode
;; ======================================================

(defmacro please-in-hexo-buffer (&rest body)
  `(if (eq major-mode 'hexo-mode)
       (progn ,@body)
     (message "Please run his command in `hexo-mode' buffer (M-x `hexo').")))

(defun hexo-open-file ()
  (interactive)
  (please-in-hexo-buffer
   (find-file (tabulated-list-get-id))))



(define-key hexo-mode-map (kbd "RET") 'hexo-open-file)
(define-key hexo-mode-map (kbd "n") 'hexo-new)

;; ======================================================
;; Universal Commands
;; ======================================================
;; Following commands are available outside hexo-mode.

;;;###autoload
(defun hexo-new ()
  "Call `hexo new` anywhere as long as in any child directory
 under a Hexo repository.
That's to say, you can use this function to create new post, even though
under theme/default/layout/"
  (interactive)
  (let* (stdout
         created-file
         (hexo-command (hexo-find-command)))
    (cond ((and (eq major-mode 'hexo-mode) hexo-root-dir) ; in hexo-mode
           (cd hexo-root-dir)
           (hexo--new-interactively hexo-command))
          ((not (hexo-find-root-dir))                     ; not in a hexo repo
           (message "You should run this command under a Hexo repo, or in a hexo-mode buffer"))
          ((null hexo-command)                            ; not found hexo command
           (message "Not found hexo command in your node_modules/ nor $PATH,"))
          (t (hexo--new-interactively hexo-command)))))

(defun hexo--new-interactively (hexo-command)
  (let (stdout created-file)
    (setq stdout (shell-command-to-string
                  (format "%s new '%s'"
                          hexo-command
                          (read-from-minibuffer "Article URI: "))))
    (string-match "Created: \\(.+\\)$" stdout)
    (setq created-file (match-string 1 stdout))
    (find-file created-file)
    (goto-char 0)
    (when (y-or-n-p "Rename arcitle title? ")
      (replace-regexp "title: .+$" (format "title: \"%s\""
                                           (read-from-minibuffer "Article Title: ")))
      (save-buffer))))

;;;###autoload
(defun hexo-touch-files-in-dir-by-time ()
  "`touch' markdown article files according their \"date: \" to
make it easy to sort file according date in Dired or `hexo-mode'."
  (interactive)
  (if (not (hexo-find-root-dir))
      (message "Please run this command under a hexo repository.")
    (let ((touch-commands-list (mapcar (lambda (file)
                                         (let ((head (hexo-get-file-head-lines-as-string file 5)))
                                           (if (string-match "^date: \\([0-9]+\\)-\\([0-9]+\\)-\\([0-9]+\\) \\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\)$" head)
                                               (format "touch -t %s%s%s%s%s.%s %s"
                                                       (match-string 1 head)
                                                       (match-string 2 head)
                                                       (match-string 3 head)
                                                       (match-string 4 head)
                                                       (match-string 5 head)
                                                       (match-string 6 head)
                                                       file)
                                             " ")))    ;If not found "date: ", return an empty command
                                       (hexo-get-all-article-files))))
      (shell-command (mapconcat #'identity touch-commands-list ";"))
      (revert-buffer)
      (message "Done."))))

;;;###autoload
(defun hexo-move-article ()
  "Move current file between _post and _draft;
You can run this function in dired or a hexo article."
  (interactive)
  (cond ((and (eq major-mode 'hexo-mode) hexo-root-dir)
         (hexo--move-article (tabulated-list-get-id)))
        ((and (eq major-mode 'markdown-mode)
              (hexo-find-root-dir))
         (hexo--move-article (buffer-file-name)))
        ((and (eq major-mode 'dired-mode)
              (hexo-find-root-dir)
              (string-suffix-p ".md" (dired-get-file-for-visit))
              (member (hexo-get-article-parent-dir-name (dired-get-file-for-visit)) '("_posts" "_drafts")))
         (hexo--move-article (dired-get-file-for-visit)))
        (t
         (message "You can only run this command in either:
1. The buffer of an article
2. Hexo-mode
3. Dired-mode (remember to move your cursor onto a valid .md file first)"))))

(defun hexo--move-article (file-path)
  "Move file between _posts and _drafts"
  (let* ((from (hexo-get-article-parent-dir-name file-path))
         (to (if (string= from "_posts") "_drafts" "_posts"))
         (to-path (format "%s/source/%s/%s"
                          (hexo-find-root-dir file-path) to (file-name-nondirectory file-path))))
    (if (file-exists-p to-path)
        (message (format "A file with the same name has existed in %s, please rename and try again." to))
      (progn (rename-file file-path to-path)
             (message (format "Moved to %s." to))))))


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

;;;###autoload
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
      (message "Please run this command in hexo article buffer.")

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
