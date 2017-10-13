(require 'projectile)

;; Enable projectile
(projectile-global-mode t)

(setq projectile-switch-project-action 'projectile-dired
      projectile-indexing-method 'git
      projectile-enable-caching nil
      projectile-require-project-root t
      projectile-use-git-grep t
      projectile-completion-system 'ido
      projectile-globally-ignored-files (append '("*.pyc" "*._flymake.*") projectile-globally-ignored-files)
      projectile-globally-ignored-directories (append '("doc" ".testrepository" "*.egg-info" "specs" "tests")
                                                      projectile-globally-ignored-directories)
      ;; Fix https://github.com/bbatsov/projectile/issues/1183
      projectile-mode-line '(:eval (format " Projectile[%s]"
                                           (projectile-project-name))))
;; find-file
;; if current working drectory is project => use helm-projectile
;; else use find-file
(defun sr-open-file ()
  "Open file using projectile+Helm or ido"
  (interactive)
  (if (projectile-project-p)
      (helm-projectile)
    (helm-for-files)))


(defun my-projectile-find-test (file-name)
  "Given a FILE-NAME return the matching implementation or test filename."
  (unless file-name (error "The current buffer is not visiting a file"))
  (when (not (projectile-test-file-p file-name))
    ;; find the matching test file
    (let ((test-file (projectile-find-matching-test file-name)))
      (if test-file
          (projectile-expand-root test-file)
        (error "No matching test file found")))))

;; TODO: correct to be PYTHON independant
(defun my-toggle-src-test ()
  (interactive)
  (if (not (projectile-test-file-p (buffer-file-name)))
      (let* ((testtable (my-testable))
            (test_to_find (if (string-equal (substring testtable 0 1) "_")
                              (concat "test_+" (substring testtable 1 nil))
                            (concat "test_+" testtable)
                            )))
        (find-file (my-projectile-find-test (buffer-file-name)))
        (goto-char 1)
        (if (not (re-search-forward test_to_find nil t))
            (progn
              (message (concat "No unit test method for: " test_to_find))
              (switch-to-prev-buffer)
              )
          (highlight-regexp test_to_find)
          (recenter)))
    (error " not a PYTHON source file.")))

(defun my-testable ()
  (let* ((inner-obj (inner-testable))
         (outer (outer-testable))
         ;; elisp can't return multiple values
         (outer-def (car outer))
         (outer-obj (cdr outer)))
    (cond ((equal outer-def "def") outer-obj)
          ((equal inner-obj outer-obj) outer-obj)
          (t (format "%s" inner-obj)))))

(defun inner-testable ()
  (save-excursion
    (re-search-backward
     "^\\(?: \\{0,4\\}\\|\t\\)\\(class\\|def\\)[ \t]+\\([a-zA-Z0-9_]+\\)" nil t)
    (buffer-substring-no-properties (match-beginning 2) (match-end 2))))

(defun outer-testable ()
  (save-excursion
    (re-search-backward
     "^\\(class\\|def\\)[ \t]+\\([a-zA-Z0-9_]+\\)" nil t)
    (let ((result
            (buffer-substring-no-properties (match-beginning 2) (match-end 2))))
      (cons
       (buffer-substring-no-properties (match-beginning 1) (match-end 1))
       result))))

(defun projectile-grep-without-git ()
  "projectile grep with git grep disabled"
  (interactive)
  (let ((projectile-use-git-grep nil))
    (projectile-grep)))

;; customize grep-find command
;;(setq grep-find-command "find . -type f '!' -wholename '*/.git/*' -print0 | xargs -0 -e grep -nH -e ")
(defun helm-projectile-grep-ori ()
  "helm projectile without git"
  ()
  (
   (setq projectile-use-git-grep nil)
   (eval-after-load 'grep
  '(progn
     (add-to-list 'grep-find-ignored-directories ".git")
     (add-to-list 'grep-find-ignored-directories ".tox")
     (add-to-list 'grep-find-ignored-directories "locale")
     (add-to-list 'grep-find-ignored-directories "tests"))))
  'helm-projectile-grep)

;(setq grep-find-command "find . -type f '!' -wholename '*/.tox/*' -a '!' -wholename '*/.git/*' -a '!' -wholename '*/.eggs/*' -a '!' -wholename '*/tests/*' -print0 | xargs -0 -e grep -nH -e ")
(global-set-key (kbd "C-<f4>") 'helm-projectile-grep-ori)

(global-set-key (kbd "<f4>") 'projectile-grep)
;(global-set-key (kbd "C-<f4>") 'projectile-grep-without-git)
(global-set-key (kbd "C-S-<f4>") 'grep-find)
(global-set-key (kbd "C-<f9>") 'projectile-toggle-between-implementation-and-test)
(global-set-key (kbd "<f9>") 'my-toggle-src-test)

;; remove next projectile version (upstream)
(eval-after-load "projectile"
  '(defun projectile-test-prefix (project-type)
    "Find default test files prefix based on PROJECT-TYPE."
    (cond
     ((member project-type '(django python python-tox)) "test_")
     ((member project-type '(lein-midje)) "t_"))))
