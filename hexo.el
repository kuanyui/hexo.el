;; hexo.el - Major mode & tools for Hexo
;; Author: kuanyui <azazabc123@gmail.com>
;; License: MIT
;; Code:

(require 'cl)
(require 'tabulated-list)
(require 'ido)
(require 'ansi-color)

(defgroup hexo nil
  "Manage Hexo with Emacs"
  :prefix "hexo" :link '(url-link "http://github.com/kuanyui/hexo.el"))

(defgroup hexo-faces nil
  "Faces used in `hexo-mode'"
  :group 'hexo :group 'faces)

(defvar-local hexo-root-dir nil
  "Root directory of a hexo-mode buffer")
(put 'hexo-root-dir 'permanent-local t)

(defvar-local hexo--tabulated-list-entries-filter nil
  "Function for filtering entries")
(put 'hexo--tabulated-list-entries-filter 'permanent-local t)

(defvar hexo-process nil
  "Hexo process object")

;; ======================================================
;; Faces
;; ======================================================

(defface hexo-status-post
  '((((class color) (background light)) (:bold t :foreground "#008700" :background "#d7ff87"))
    (((class color) (background dark)) (:bold t :foreground "#d7ff00" :background "#4e4e4e")))
  ""
  :group 'hexo-faces)

(defface hexo-status-draft
  '((((class color) (background light)) (:bold t :foreground "#ff5d17" :background "#ffd787"))
    (((class color) (background dark)) (:bold t :foreground "#ff8700" :background "#4e4e4e")))
  ""
  :group 'hexo-faces)

(defface hexo-tag
  '((((class color) (background light)) (:foreground "#005f87" :background "#afd7ff"))
    (((class color) (background dark)) (:foreground "#87dfff" :background "#3a3a3a":underline t)))
  ""
  :group 'hexo-faces)

(defface hexo-category
  '((((class color) (background light)) (:foreground "#6c0099" :background "#ffd5e5"))
    (((class color) (background dark)) (:foreground "#d18aff" :background "#3a3a3a" :underline t)))
  ""
  :group 'hexo-faces)

(defface hexo-date
  '((((class color) (background light)) (:foreground "#875f00"))
    (((class color) (background dark)) (:foreground "#ffffaf")))
  ""
  :group 'hexo-faces)

(defface hexo-title
  '((((class color) (background light)) (:foreground "#236f73"))
    (((class color) (background dark)) (:foreground "#87d7af")))
  ""
  :group 'hexo-faces)

;; ======================================================
;; Small tools
;; ======================================================

(defun hexo-message (format-string &rest args)
  "The same as `message', but it `propertize' all ARGS with
`font-lock-keyword-face'"
  (apply #'message format-string (mapcar
                                  (lambda (arg)
                                    (propertize (format "%s" arg) 'face 'font-lock-keyword-face))
                                  args)))

(defun hexo-get-file-content-as-string (file-path)
  (with-temp-buffer
    (insert-file file-path)
    (buffer-string)))

(defun hexo-write-file (file-path string)
  "Overwrite the whole file content to STRING."
  (with-temp-file file-path (insert string)))

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

(defun hexo-find-command (&optional from-path)
  "Try to find hexo in node_modules/ directory.
If not found, try to `executable-find' hexo in your system."
  (let* ((root-dir (hexo-find-root-dir from-path))
         (guessed-hexo (format "%s/node_modules/hexo/bin/hexo" root-dir)))
    (if (and root-dir (file-exists-p guessed-hexo))
        guessed-hexo
      (executable-find "hexo"))))

(defun hexo-find-root-dir (&optional from-path)
  "Try to find the root dir of a Hexo repository."
  (let ((PWD (or from-path default-directory)))
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

(defun hexo-sort-string-list (string-list)
  (sort string-list #'string<))

(defun hexo-remove-duplicates-in-string-list (string-list)
  (remove-duplicates string-list :test #'string=))

;; ======================================================
;; Main
;; ======================================================

(define-derived-mode hexo-mode tabulated-list-mode "Hexo"
  "Major mode for manage Hexo articles."
  (hl-line-mode 1)
  (setq tabulated-list-format
        `[("Status" 6 t)
          ("Filename" 20 t)
          ("Title" 48 t)
          ("Date"  12 t)
          ("Categories"  16 t)
          ("Tags"  0 t)])
  (setq tabulated-list-sort-key '("Date" . t))
  (setq tabulated-list-padding 2)
  (add-hook 'tabulated-list-revert-hook 'hexo-refresh nil t)
  (tabulated-list-init-header))

(defun hexo-refresh ()
  "See `hexo-generate-tabulated-list-entries'"
  (setq tabulated-list-entries
        (hexo-generate-tabulated-list-entries hexo-root-dir hexo--tabulated-list-entries-filter)))

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

(defun hexo-generate-tabulated-list-entries (&optional repo-root-dir filter)
  "Each element in `tabulated-list-entries' is like:
(FileFullPath [\"post\" \"test.md\" \"Title\" \"2013/10/24\" \"category\" \"tag, tag2\"])
id           ^ entry
FILTER is a function with one arg."
  (let ((entries (mapcar #'hexo-generate-file-entry-for-tabulated-list
                         (hexo-get-all-article-files repo-root-dir 'include-drafts))))
    (if filter
        (remove-if-not filter entries)
      entries)))

(defun hexo-get-all-article-files (&optional repo-root-dir include-drafts)
  "Return a files list containing full-paths of all articles."
  (let* ((root (or (hexo-find-root-dir repo-root-dir)
                   hexo-root-dir
                   (hexo-find-root-dir)))
         (posts-dir (format "%s/source/_posts/" root))
         (drafts-dir (format "%s/source/_drafts/" root)))
    (append (hexo-directory-files posts-dir)
            (if include-drafts (hexo-directory-files drafts-dir) '()))
    ))

(defun hexo-remove-regexp (regexp string)
  (replace-regexp-in-string regexp "" string t))

(defun hexo-trim-quotes (string)
  (hexo-remove-regexp "[\"']$" (hexo-remove-regexp "^[\"']" string)))

(defun hexo-trim-spaces (string)
  (hexo-remove-regexp " *$" (hexo-remove-regexp "^ *" string)))

(defun hexo-trim (string)
  (hexo-trim-quotes (hexo-trim-spaces string)))

(defun hexo-parse-tags (string)
  "Return a list containing tags"
  (cond ((string-match "\\[\\(.+\\)\\]" string)
         (let* ((raw (match-string 1 string)) ; "this", "is", "tag"
                (raw (replace-regexp-in-string ", " "," raw 'fixedcase)))
           (remove "" (mapcar #'hexo-trim-quotes (split-string raw ",")))))
        ((string-match "^ *$" string)
         '())
        (t
         (list (hexo-trim string)))))

(defun hexo-get-article-info (file-path)
  "Return a list:
'((title . title)
  (date . date)
  (tags . (tags ...))
  (categories . (categories ...)))"
  (let ((head-lines (hexo-get-file-head-lines file-path 6)))
    (remove-if
     #'null
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
             head-lines))))

(defun hexo-generate-file-entry-for-tabulated-list (file-path)
  "Generate the entry of FILE-PATH for `tabulated-list-mode'.
<ex> (FileFullPath [\"post\" \"test.md\" \"Title\" \"2013/10/24\" \"category\" \"tag, tag2\"])
     ^ id           ^ entry
----------------------------------------------------------
In `tabulated-list-mode', use `tabulated-list-get-id' and
`tabulated-list-get-entry' to get it."
  (let ((info (hexo-get-article-info file-path)))
    (list file-path
          (vector
           ;; status
           (if (equal (hexo-get-article-parent-dir-name file-path) "_posts")
               (propertize "post" 'face 'hexo-status-post)
             (propertize "draft" 'face 'hexo-status-draft))
           ;; filename
           (file-name-base file-path)
           (propertize (cdr (assq 'title info)) 'face 'hexo-title)
           (propertize (cdr (assq 'date info)) 'face 'hexo-date)
           (mapconcat (lambda (x) (propertize x 'face 'hexo-category))
                      (cdr (assq 'categories info)) "/")
           (mapconcat (lambda (x) (propertize x 'face 'hexo-tag))
                      (hexo-sort-string-list (cdr (assq 'tags info))) " ")
           ))))

(defun hexo-get-attribute-in-file-entry (key file-entry)
  "FILE-ENTRY is a `vector' generated by
`hexo-generate-file-entry-for-tabulated-list'.

The struct of FILE-ENTRY is defined in `tabulated-list-format'.
<ex> [(\"Status\" 6 nil) (\"Filename\" 48 nil) (\"Title\" 48 nil)]
KEY is a downcased symbol. <ex> 'status "
  (let ((index (position key tabulated-list-format
                         :test (lambda (ele lst)
                                 (equal (capitalize (symbol-name ele))
                                        (car lst))))))
    (aref file-entry index)))

(defun hexo-get-article-parent-dir-name (file-path)
  "Return _posts or _drafts"
  (file-name-nondirectory
   (directory-file-name
    (file-name-directory file-path))))

;;;###autoload
(defun hexo ()
  "Open Hexo-mode buffer"
  (interactive)
  (require 'finder-inf nil t)
  (let* ((buf (get-buffer-create "*Hexo*"))
         (win (get-buffer-window buf))
         (--hexo-root (or hexo-root-dir ;if already under `hexo-mode', this var must be non-nil
                          (hexo-find-root-dir default-directory))))
    (if --hexo-root   ;When calling `hexo', under a hexo repo
        (with-current-buffer buf
          (setq hexo-root-dir --hexo-root)
          (hexo-mode))
      (with-current-buffer buf          ;not under a hexo repo
        (setq hexo-root-dir (hexo-ask-for-root-dir))
        (hexo-mode)))
    (if win
        (select-window win)
      (switch-to-buffer buf))
    (hexo-refresh)
    (delete-other-windows)
    (tabulated-list-print 'remember-pos)))

;; ======================================================
;; Commands for hexo-mode only
;; ======================================================

(define-key hexo-mode-map (kbd "RET") 'hexo/open-file)
(define-key hexo-mode-map (kbd "SPC") 'hexo/show-article-info)
(define-key hexo-mode-map (kbd "n") 'hexo-new)
(define-key hexo-mode-map (kbd "t") nil)
(define-key hexo-mode-map (kbd "t s") 'hexo-toggle-article-status)
(define-key hexo-mode-map (kbd "t t") 'hexo-touch-files-in-dir-by-time)
(define-key hexo-mode-map (kbd "t a") 'hexo/tags-edit)
(define-key hexo-mode-map (kbd "f") 'hexo/filter-tag)
(define-key hexo-mode-map (kbd "R") 'hexo/rename-file)
(define-key hexo-mode-map (kbd "<f2>") 'hexo/rename-file)
(define-key hexo-mode-map (kbd "h") 'hexo/help)
(define-key hexo-mode-map (kbd "?") 'hexo/help)
(define-key hexo-mode-map (kbd "S") 'tabulated-list-sort)
(define-key hexo-mode-map (kbd "s") nil)
(define-key hexo-mode-map (kbd "s r") 'hexo:run-server)
(define-key hexo-mode-map (kbd "s s") 'hexo:stop-server)
(define-key hexo-mode-map (kbd "s d") 'hexo:deploy)
(define-key hexo-mode-map (kbd "Q") 'kill-buffer-and-window)

(defun hexo/help ()
  (interactive)
  (hexo-buffer-only
   (let* ((help-str (concat
                     (propertize
                      "File             View              Edit                 Server             Mode\n" 'face 'header-line)
                     "[RET] Open       [  g] Refresh     [t t] Touch time     [s r] Run server   [  ?] Show this help\n"
                     "[SPC] Show Info  [  S] Sort        [t s] Toggle status  [s s] Stop server  [  Q] Quit\n"
                     "[  N] New        [  f] Filter tag  [t a] Edit Tags      [s d] Deploy\n"
                     "[  R] Rename"))
          (help-str-without-brackets (replace-regexp-in-string "[][]" " " help-str 'fixedcase)))
     (mapc (lambda (begin-end)
             (add-face-text-property (car begin-end)
                                     (cdr begin-end)
                                     '(face 'hexo-status-post)
                                     t help-str-without-brackets))
           (hexo-string-match-positions "\\(\\[.+?\\]\\)" help-str 1)) ;all position
     (message help-str-without-brackets))))

(defun hexo-string-match-positions (regexp string &optional subexp-depth)
  "Get all matched REGEXP position in a STRING.
SUBEXP-DEPTH is 0 by default."
  (if (null subexp-depth)
      (setq subexp-depth 0))
  (let ((pos 0) result)
    (while (and (string-match regexp string pos)
                (< pos (length string)))
      (let ((m (match-end subexp-depth)))
        (push (cons (match-beginning subexp-depth) (match-end subexp-depth)) result)
        (setq pos m)))
    (nreverse result)))

(defmacro hexo-buffer-only (&rest body)
  `(if (eq major-mode 'hexo-mode)
       (progn ,@body)
     (message "Please run his command in `hexo-mode' buffer (M-x `hexo').")))

(defmacro hexo-repo-only (&rest body)
  `(let ((dir (or hexo-root-dir (hexo-find-root-dir))))
     (if dir
         (progn (cd dir) ,@body)
       (message "Please run his command under a Hexo repo directory."))))

(defun hexo/open-file ()
  (interactive)
  (hexo-buffer-only
   (find-file (tabulated-list-get-id))))

(defun hexo/rename-file (&optional init-value)
  (interactive)
  (hexo-buffer-only
   (let ((status (hexo-get-attribute-in-file-entry 'status (tabulated-list-get-entry))))
     (if (or (equal status "draft")
             (and (equal status "post")
                  (yes-or-no-p "This article is a post instead a draft.\nRenaming it may change its permanemt link. Continue?")))
         (let* ((original-file-path (tabulated-list-get-id))
                (pwd (file-name-directory original-file-path))
                (original-name-without-ext (or init-value (file-name-base original-file-path)))
                (new-name-without-ext (read-from-minibuffer
                                       (format "Rename '%s' to: " original-name-without-ext)
                                       original-name-without-ext))
                (new-file-path (format "%s/%s.md" pwd new-name-without-ext)))
           (if (file-exists-p new-file-path)
               (progn (hexo-message "Filename '%s' already existed. Please try another name." new-name-without-ext)
                      (sit-for 5)
                      (hexo/rename-file new-name-without-ext))
             (progn (rename-file original-file-path new-file-path)
                    (revert-buffer)
                    (search-forward new-name-without-ext)
                    (message "Rename successful!"))))
       (message "Rename cancelled.")))))

(defun hexo/tags-edit ()
  (interactive)
  (hexo-buffer-only
   (let* ((file-path (tabulated-list-get-id))
          (info (hexo-get-article-info file-path))
          (old-tags-list (cdr (assq 'tags info)))
          (new-tags-list (hexo--edit-tags-iter old-tags-list (hexo-get-all-tags)))
          (formatted-new-tags-list (hexo-format-tags-list new-tags-list))
          (old-file-content (hexo-get-file-content-as-string file-path))
          (new-file-content (with-temp-buffer
                              (insert old-file-content)
                              (goto-char (point-min))
                              (re-search-forward "tags:.*" nil t)
                              (replace-match (format "tags: %s" formatted-new-tags-list))
                              (buffer-string))))
     (hexo-write-file file-path new-file-content)
     (revert-buffer)
     (message "Done!"))))

(defun hexo-get-all-tags (&optional root-dir)
  (hexo-sort-string-list
   (hexo-remove-duplicates-in-string-list
    (mapcan (lambda (file-path)
              (cdr (assq 'tags (hexo-get-article-info file-path))))
            (hexo-get-all-article-files root-dir 'include-drafts)))))

(defun hexo--edit-tags-iter (this-file-tags-list all-tags)
  (let ((tag (ido-completing-read
              (format "Add / Remove Tags (C-j to apply) :\n Current tags => %s\n" this-file-tags-list)
              all-tags nil nil)))
    (cond ((string= "" tag)
           this-file-tags-list)
          ((member tag this-file-tags-list) ;tag exist in this file
           (hexo--edit-tags-iter (remove tag this-file-tags-list) all-tags))
          (t
           (hexo--edit-tags-iter
            (hexo-sort-string-list (cons tag this-file-tags-list))
            (hexo-sort-string-list (hexo-remove-duplicates-in-string-list (cons tag all-tags))))))))

(defun hexo-format-tags-list (tags-list)
  (format "[%s]"
          (mapconcat #'identity tags-list ", ")))

(defun hexo/show-article-info ()
  (interactive)
  (hexo-buffer-only
   (let ((formatted-file-entry (hexo-format-file-entry (tabulated-list-get-entry))))
     (message formatted-file-entry))))

(defun hexo-format-file-entry (file-entry)
  "FILE-ENTRY is a `vector'. See `hexo-get-attribute-in-file-entry'
 and `hexo-generate-file-entry-for-tabulated-list'"
  (let* ((keys-string-list (map 'list #'car tabulated-list-format)) ; ("Status" "Filename"...)
         (max-length (apply #'max (mapcar #'length keys-string-list))))
    (concat
     (propertize "  File Information\n" 'face 'header-line 'bold t)
     (mapconcat
      (lambda (key-string)
        (concat (propertize
                 (concat "  "
                         ;;align with space (32)
                         (make-string (- max-length (length key-string)) 32)
                         key-string
                         " ") 'face 'linum)
                " "
                (hexo-get-attribute-in-file-entry (intern (downcase key-string)) file-entry)
                "  "))
      keys-string-list
      "\n"
      ))))

(defun hexo/filter-tag ()
  (interactive)
  (hexo-buffer-only
   (let ((tag (completing-read "Filter tag: "
                               (hexo-get-all-tags "~/source-kuanyui.github.io/") nil t)))
     (if (string= "" tag)
         (message "No tag inputed, abort.")
       ;; Assign variable `hexo--tabulated-list-entries-filter' as our filter function
       (let ((hexo--tabulated-list-entries-filter (lambda (x) ;car is id (file-path), cdr is ([status ...])
                                                    (let* ((info (hexo-get-article-info (car x)))
                                                           (tags-list (cdr (assq 'tags info))))
                                                      (member tag tags-list)))))
         (tabulated-list-revert)
         (hexo-message "Press %s to disable filter" "g"))))))

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
  (let ((hexo-command (hexo-find-command)))
    (cond ((and (eq major-mode 'hexo-mode) hexo-root-dir) ; in hexo-mode
           (cd hexo-root-dir)
           (hexo--new-interactively hexo-command))
          ((not (hexo-find-root-dir))                     ; not in a hexo repo
           (message "You should run this command under a Hexo repo, or in a hexo-mode buffer"))
          ((null hexo-command)                            ; not found hexo command
           (message "Not found hexo command in your node_modules/ nor $PATH,"))
          (t (hexo--new-interactively hexo-command)))))

(defun hexo--new-interactively (hexo-command)
  (let* ((stdout (shell-command-to-string (format "%s new '%s'"
                                                  hexo-command
                                                  (read-from-minibuffer "Article URI: "))))
         (created-file-path (progn (string-match "Created: \\(.+\\)$" stdout)
                                   (match-string 1 stdout))))
    (find-file created-file-path)
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
  (if (not (or hexo-root-dir (hexo-find-root-dir)))
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
(defun hexo-toggle-article-status ()
  "Move current file between _post and _draft;
You can run this function in dired or a hexo article."
  (interactive)
  (cond ((and (eq major-mode 'hexo-mode) hexo-root-dir)
         (let* ((file-path (tabulated-list-get-id))
                (file-name (file-name-base file-path)))
           (if (hexo--toggle-article-status file-path)
               (progn (tabulated-list-revert)
                      (search-forward file-name nil t))
             (message "Two filenames duplicated in _posts/ and _drafts/. Abort."))))
        ((and (eq major-mode 'markdown-mode)
              (hexo-find-root-dir))
         (let ((new-path (hexo--toggle-article-status (buffer-file-name))))
           (if new-path
               (progn (find-alternate-file new-path)
                      (hexo-message "Now this file is in '%s'"
                                 (hexo-get-article-parent-dir-name new-path)))
             (message "Two filenames duplicated in _posts/ and _drafts/. Abort."))))
        ((and (eq major-mode 'dired-mode)
              (hexo-find-root-dir)
              (string-suffix-p ".md" (dired-get-file-for-visit))
              (member (hexo-get-article-parent-dir-name (dired-get-file-for-visit)) '("_posts" "_drafts")))
         (hexo--toggle-article-status (dired-get-file-for-visit)))
        (t
         (message "You can only run this command in either:
1. The buffer of an article
2. Hexo-mode
3. Dired-mode (remember to move your cursor onto a valid .md file first)"))))

(defun hexo--toggle-article-status (file-path)
  "Move file (`rename-file') between _posts and _drafts.
If success, return the new file path, else nil."
  (let* ((from (hexo-get-article-parent-dir-name file-path))
         (to (if (string= from "_posts") "_drafts" "_posts"))
         (to-path (format "%s/source/%s/%s"
                          (hexo-find-root-dir file-path) to (file-name-nondirectory file-path))))
    (if (file-exists-p to-path)
        (prog1 nil
          (message (format "A file with the same name has existed in %s, please rename and try again." to)))
      (prog1 to-path
        (rename-file file-path to-path)
        (hexo-message "Now article '%s' is in '%s'" (file-name-base to-path) to)
        ))))


;; ======================================================
;; Commands for article buffers only
;; ======================================================

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


(defun hexo-get-permalink-format (&optional root-or-file-path)
  "Get permalink format string from config file. ex: /%Y/%m/%d/:title/"
  (let* ((config-file-path (format "%s/_config.yml" (hexo-find-root-dir root-or-file-path)))
         (config-text (with-temp-buffer (insert-file-contents config-file-path)
                                        (buffer-string)))
         (permalink-format-raw (progn (string-match "^permalink: ?\\(.+\\)" config-text)
                                      (match-string 1 config-text)))
         (permalink-format (replace-regexp-in-string
                            ":year" "%Y"
                            (replace-regexp-in-string
                             ":month" "%m"
                             (replace-regexp-in-string
                              ":day" "%d"
                              permalink-format-raw))))
         (root (progn (string-match "^root: \\(.+\\)" config-text)
                      (match-string 1 config-text))))
    (concat root permalink-format)))


(defun hexo-get-article-title-and-permalink (file-path)
  "return a dotted pair (TITLE . PERMALINK)"
  (let* ((head (hexo-get-file-head-lines-as-string file-path 5))
         (date (progn (string-match "date:\\(.+\\)" head)
                      (hexo-trim (match-string 1 head))))
         (title (progn (string-match "title:\\(.+\\)" head)
                       (hexo-trim (match-string 1 head)))))
    (cons title
          (replace-regexp-in-string ":title" (file-name-base file-path)
                                    (format-time-string (hexo-get-permalink-format file-path)
                                                        (apply #'encode-time (parse-time-string date)))))))

(defun hexo-completing-read-post (&optional repo-root-dir)
  "Use `ido-completing-read' to read filename in _posts/.
Return absolute path of the article file."
  (format "%s/source/_posts/%s.md"
          (hexo-find-root-dir repo-root-dir)
          (ido-completing-read
           "Select Article: "
           (mapcar #'file-name-base (hexo-get-all-article-files repo-root-dir nil)) ;not include drafts
           nil t)))

(defun hexo-get-article-title (file-path)
  (let ((head (hexo-get-file-head-lines-as-string file-path 3)))
    (string-match "title:\\(.+\\)" head)
    (hexo-trim (match-string 1 head))))

;;;###autoload
(defun hexo-insert-article-link ()
  "Insert a link to other article in _posts/."
  (interactive)
  (if (or (not (eq major-mode 'markdown-mode))
          (not (hexo-find-root-dir))
          (not (member (hexo-get-article-parent-dir-name (buffer-file-name)) '("_posts" "_drafts"))))
      (message "This command only usable in a hexo article buffer (markdown).")
    (let* ((file-path (hexo-completing-read-post))
           (title+permalink (hexo-get-article-title-and-permalink file-path))
           (title (car title+permalink))
           (permalink (cdr title+permalink))
           )
      (insert (format "[%s](%s)"
                      (if (y-or-n-p (format "Use original article title '%s' ? " title))
                          title
                        (read-from-minibuffer "Title: "))
                      permalink)))))

;;;###autoload
(defun hexo-follow-post-link ()
  "`find-file' a markdown format link.
  [FIXME] Currently, this function only support a link with
  file-name as suffix. e.g.
  [Link](/2015/08/19/coscup-2015/)
  [Link](/2015/08/19/coscup-2015)
  [Link](/coscup-2015/)
"
  (interactive)
  (let* ((permalink (hexo-substring-permalink-under-cursor))
         (file-path (if permalink (hexo-get-file-path-from-post-permalink permalink) nil)))
    (cond ((null permalink)
           (message "Cursor is not on a link. Abort"))
          ((and permalink (file-exists-p file-path))
           (find-file file-path)
           (hexo-message "Open '%s'" file-path))
          ((not (file-exists-p file-path))
           (message "The article seems not to exist, or not a file in your hexo repo. Abort")))))

(defun hexo-substring-permalink-under-cursor ()
  "[Link](/2015/coscup-2015/) => /2015/coscup-2015/
Return the link. If not found link under cursor, return nil."
  (interactive)
  (save-excursion
    (let* ((original (point))
           (beg (search-backward "["))
           (end (search-forward ")"))
           (str (buffer-substring beg end))
           (url (progn (string-match "\\[.+?\\](\\(.+?\\))" str)
                       (match-string 1 str))))
      (if (and (>= original beg) (<= original end))
          url
        nil))))

(defun hexo-get-file-path-from-post-permalink (permalink &optional repo-root-dir)
  "/2015/coscup-2015/ <= this is permalink
This is only resonable for files in _posts/."
  (let ((filename-without-ext (progn (string-match "/?\\([^/]+\\)/?$" permalink)
                                     (match-string 1 permalink))))
    (format "%s/source/_posts/%s.md"
            (hexo-find-root-dir repo-root-dir)
            filename-without-ext)))

;; ======================================================
;; Run Hexo process in Emacs
;; ======================================================

(defvar hexo-process-buffer-name "*hexo-process*")

(defun hexo-start-process-shell-command (command-string &optional repo-path)
  "COMMAND-STRING example:
\"hexo clean;hexo generate;hexo server --debug\""
  (if (process-live-p hexo-process)
      (kill-process hexo-process))
  (setq hexo-process (start-process-shell-command
                      "hexo-process"
                      hexo-process-buffer-name
                      (hexo-replace-hexo-command-to-path command-string repo-path)))
  ;; [SHIT] Why no ANSI color?! Why?!
  (set-process-filter hexo-process (lambda (process string)
                                     (with-current-buffer (process-buffer process)
                                       (insert (ansi-color-apply string)))))
  (pop-to-buffer hexo-process-buffer-name))

;;(term-send-string )
;;(shell (get-buffer-create "*Hexo process*"))
;;(process-send-string (get-buffer-process "my-shell-buf") (concat cmd "\n"))

(defun hexo-replace-hexo-command-to-path (command-string &optional repo-path)
  "Replace all 'hexo' in COMMAND-STRING to hexo command's path"
  (replace-regexp-in-string "hexo"
                            (hexo-find-command repo-path)
                            command-string))

(defun hexo:run-server ()
  (interactive)
  (hexo-repo-only
   (let ((type (ido-completing-read "[Hexo server] Type: " '("posts-only" "posts+drafts") nil t)))
     (cond ((string= type "posts+drafts")
            (hexo-start-process-shell-command "hexo clean;hexo generate;hexo server --debug --drafts"))
           ((string= type "posts-only")
            (hexo-start-process-shell-command "hexo clean;hexo generate;hexo server --debug"))))))

(defun hexo:deploy ()
  (interactive)
  (hexo-repo-only
   (hexo-start-process-shell-command "hexo clean;hexo generate;hexo deploy")))

(defun hexo:stop-server ()
  (interactive)
  (if (process-live-p hexo-process)
      (progn (kill-process hexo-process)
             (message "Server stopped ~!"))
    (message "No active server found")))

(provide 'hexo)
