;;; padawan.el --- Use tox.ini to create PYTHON virtual env
;; configure JEDI and flycheck to take virtual env account

;; Copyright © 2015 Bertrand Lallau

;; Author: Bertrand LALLAU <bertrand.lallau@gmail.com>
;; URL: https://github.com/blallau/emacs-config
;; Package-Version:
;; Package-Requires: ((magit "2.1.0") (projectile "0.13.0"))
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see http://www.gnu.org/licenses/.


(require 'projectile)
(require 'jedi)
(require 'flycheck)
(declare-function python-shell-calculate-exec-path "python")

;; Define a defvar-local macro for Emacs < 24.3
(unless (fboundp 'defvar-local)
  (defmacro defvar-local (var val &optional docstring)
    `(progn
       (defvar ,var ,val ,docstring)
       (make-variable-buffer-local ',var))))

(defvar-local pip-install-jedi-command "pip install -U jedi" "Command to install Jedi by PIP")
(defvar-local pip-install-pylint-command "pip install -U pylint" "Command to install Pylint by PIP")

(defvar-local python-version "py27" "python version")
(defvar-local venv-dir (concat ".tox/" python-version) "venv directory")
(defvar-local venv-bin-dir (concat venv-dir "/bin/") "venv binary directory")
(defvar-local launch-tox-command (concat "tox -v -e" python-version " --notest") "tox command")
(defvar-local venv-activate-script (concat venv-bin-dir "activate") "Bash script file to activate venv")
(defvar-local padawan-not-jedi-projects-cache (make-hash-table :test 'equal)
  "A hashmap used to cache projects where venv is not wished.")


(defun new-async-window ()
  "Create a new window for displaying tox-async process and switch to that NEW window"
  (let ((async-window-height (- (window-total-height (frame-root-window)) 10)))
    (let ((async-window (split-window (frame-root-window) async-window-height 'below)))
      (select-window async-window)
      (set-window-parameter async-window 'no-other-window t))))

(defun kill-async-window (process)
  "Kill async buffer && kill async window"
  (let ((current-async-buffer (process-buffer process)))
    (let ((current-async-window (get-buffer-window current-async-buffer)))
      (kill-buffer current-async-buffer)
      (if (not (eq current-async-window nil))
          (delete-window current-async-window)))))

(defun async-tox-handler (process event)
  "Handler for window that displays the async process.

The function notify user that process is completed and
automatically kill buffer and window that run the process."

  ;; check if the process status is exit, then kill the buffer and the window
  ;; that contain that process after 5 seconds (for the user to see the output)
  (when (equal (process-status process) 'exit)
    ;; get the current async buffer and window
    (let ((current-async-buffer (process-buffer process)))
      (let ((current-async-window (get-buffer-window current-async-buffer))
            (project-root (projectile-project-root)))
        ;; INSTALL emacs-jedi in VirtualEnv (synchro)
        (let ((default-directory project-root))
          (shell-command (concat venv-bin-dir pip-install-jedi-command)))
        (let ((default-directory project-root))
          (shell-command (concat venv-bin-dir pip-install-pylint-command)))
        (ding)
        (print "Process completed.\nWindow will be closed in 5s." current-async-buffer)
        (set-window-point current-async-window (buffer-size current-async-buffer))
        ;; kill the buffer and window after 5 seconds
        (run-at-time "5 sec" nil 'kill-async-window process)))))

;;;###autoload
(defun ensure-venv ()
  (interactive)
  (let ((project-root (projectile-project-root))
        (project-name (projectile-project-name)))
    ;; TEST NO venv && tox.ini file && project-name NOT in padawan-not-jedi-projects-cache
    (when (and (not (file-exists-p (concat project-root venv-activate-script)))
               (file-exists-p (concat project-root "tox.ini"))
               (not (gethash project-name padawan-not-jedi-projects-cache)))

      ;; CREATE venv
      (if (y-or-n-p "Create venv? ")
        ;; unset PYTHONPATH env
        (progn
          (setenv "PYTHONPATH")
          (let ((default-directory project-root)
                (async-window-before (selected-window))
                (tox-output-buffer (concat "*" project-name "-tox*")))

            ;; make a new window
            (new-async-window)
            ;; not allow popup
            (add-to-list 'same-window-buffer-names tox-output-buffer)
            ;; create VENV with TOX
            (async-shell-command launch-tox-command tox-output-buffer)
            ;; set event handler for the async process
            (set-process-sentinel (get-buffer-process tox-output-buffer)
                                  'async-tox-handler)
            ;; switch the the previous window
            (select-window async-window-before)))
        (puthash project-name "no-venv" padawan-not-jedi-projects-cache)))))

(defun start-jedi-with-venv ()
  (set (make-local-variable 'jedi:server-args)
       (list "--virtual-env" (concat (projectile-project-root) ".tox/" python-version)))
  (jedi:setup))

;;;###autoload
(defun my-jedi-starter ()
  (interactive)
  (when (and (derived-mode-p 'python-mode) (projectile-project-p))
    ;; ensure VirtualEnv
    (ensure-venv)
    ;; Start Jedi in VirtualEnv
    (start-jedi-with-venv)

    ;; Configure flycheck in VirtualEnv
    ;; (let ((python-shell-virtualenv-path (concat (projectile-project-root) ".tox/")))
    ;;   (let ((exec-path (python-shell-calculate-exec-path)))
    ;;     (setq-local flycheck-python-pylint-executable (executable-find "pylint"))
    ;;     (setq-local flycheck-python-flake8-executable (executable-find "flake8"))
    ;;     )
    ;;   )

    ))

(defun set-python-virtualenv-path ()
  "Set `python-shell-virtualenv-path' to the virtualenv directory."
  (when (and (derived-mode-p 'python-mode) (projectile-project-p))
    (let ((virtualenv-path (file-truename (concat (projectile-project-root) venv-dir))))
      (when (file-directory-p virtualenv-path)
        (setq python-shell-virtualenv-path virtualenv-path)))))

(defun flycheck-python-set-executables ()
  "Set flycheck python executables for the current virtualenv."
  (print (format "python-shell-virtualenv-path=%s" python-shell-virtualenv-path))
  (let ((exec-path (python-shell-calculate-exec-path)))
    (print (format "exec-path=%s" exec-path))
    (print (format "pylint=%s" (executable-find "pylint")))
;;    (print (format "flake8=%s" (executable-find "flake8")))
    (setq-local flycheck-python-pylint-executable (executable-find "pylint"))
;;    (setq-local flycheck-python-flake8-executable (executable-find "flake8"))
    ))

(defun flycheck-venv-python-setup ()
  "Setup flycheck for Python with virtualenvs. "
  ;; flycheck-python-set-executables uses buffer-local variables
  (add-hook 'hack-local-variables-hook #'flycheck-python-set-executables nil 'local))

(add-hook 'projectile-switch-project-hook #'my-jedi-starter)
;; (add-hook 'projectile-switch-project-hook #'set-python-virtualenv-path)
;; (add-hook 'projectile-switch-project-hook #'flycheck-venv-python-setup)

(add-hook 'python-mode-hook #'my-jedi-starter)
;; (add-hook 'python-mode-hook #'set-python-virtualenv-path)
;; (add-hook 'python-mode-hook #'flycheck-venv-python-setup)

;;; End padawan.el ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'padawan)
;;; padawan.el ends here
