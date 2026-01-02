;;; private/gtd/config.el -*- lexical-binding: t; -*-

(defvar gtd-directory "~/gtd/"
  "Directory where GTD org files are stored.")

(defvar gtd-inbox-file (expand-file-name "inbox.org" gtd-directory))
(defvar gtd-projects-file (expand-file-name "projects.org" gtd-directory))
(defvar gtd-someday-file (expand-file-name "someday.org" gtd-directory))
(defvar gtd-tickler-file (expand-file-name "tickler.org" gtd-directory))
(defvar gtd-reference-file (expand-file-name "reference.org" gtd-directory))

;;; Org-mode GTD Configuration

(after! org
  ;; Set org-agenda files
  (setq org-agenda-files (list gtd-directory))

  ;; GTD workflow states
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "WAITING(w@/!)" "|" "DONE(d!)" "CANCELLED(c@)")
          (sequence "SOMEDAY(s)" "|")))

  ;; Capture templates for GTD
  (setq org-capture-templates
        `(("i" "Inbox" entry
           (file ,gtd-inbox-file)
           "* TODO %?\n%U\n%a\n" :prepend t)

          ("t" "Tickler" entry
           (file ,gtd-tickler-file)
           "* %?\n%U\n" :prepend t)

          ("r" "Reference" entry
           (file ,gtd-reference-file)
           "* %?\n%U\n%a\n" :prepend t)))

  ;; Refile targets
  (setq org-refile-targets
        `((,gtd-projects-file :maxlevel . 3)
          (,gtd-someday-file :level . 1)
          (,gtd-tickler-file :maxlevel . 2)))

  ;; Custom agenda views
  (setq org-agenda-custom-commands
        '(("g" "GTD View"
           ((agenda "" ((org-agenda-span 'day)))
            (todo "NEXT"
                  ((org-agenda-overriding-header "Next Actions")))
            (todo "TODO"
                  ((org-agenda-overriding-header "Inbox")
                   (org-agenda-files (list gtd-inbox-file))))
            (todo "WAITING"
                  ((org-agenda-overriding-header "Waiting For")))
            (todo "SOMEDAY"
                  ((org-agenda-overriding-header "Someday/Maybe")))))

          ("n" "Next Actions" todo "NEXT"
           ((org-agenda-overriding-header "Next Actions")))

          ("w" "Waiting" todo "WAITING"
           ((org-agenda-overriding-header "Waiting For"))))))

;;; Auto Git Sync

(defvar gtd-auto-push-enabled t
  "Enable automatic git push after saving GTD files.")

(defun gtd-git-commit-and-push ()
  "Commit and push changes in GTD directory."
  (when (and gtd-auto-push-enabled
             (file-exists-p (expand-file-name ".git" gtd-directory)))
    (let ((default-directory gtd-directory))
      (shell-command "git add -A")
      (shell-command (format "git commit -m 'Auto-commit: %s'"
                             (format-time-string "%Y-%m-%d %H:%M:%S")))
      (shell-command "git push"))))

(defun gtd-git-pull ()
  "Pull latest changes from git repository."
  (interactive)
  (when (file-exists-p (expand-file-name ".git" gtd-directory))
    (let ((default-directory gtd-directory))
      (shell-command "git pull --rebase"))))

;; Auto-commit and push after saving GTD files
(defun gtd-auto-commit-after-save ()
  "Hook to auto-commit after saving GTD files."
  (when (and buffer-file-name
             (string-prefix-p (expand-file-name gtd-directory) buffer-file-name))
    (run-with-idle-timer 2 nil #'gtd-git-commit-and-push)))

(add-hook 'after-save-hook #'gtd-auto-commit-after-save)

;; Pull on Emacs startup
(add-hook 'emacs-startup-hook #'gtd-git-pull)

;;; Key Bindings

(map! :leader
      (:prefix ("g" . "gtd")
       :desc "GTD Agenda" "g" #'org-agenda
       :desc "Capture Inbox" "c" #'org-capture
       :desc "Refile" "r" #'org-refile
       :desc "Git Pull" "p" #'gtd-git-pull))

;;; Initialize GTD files if they don't exist

(defun gtd-init-files ()
  "Create GTD files if they don't exist."
  (unless (file-exists-p gtd-directory)
    (make-directory gtd-directory t))

  (dolist (file (list gtd-inbox-file gtd-projects-file
                      gtd-someday-file gtd-tickler-file
                      gtd-reference-file))
    (unless (file-exists-p file)
      (with-temp-file file
        (insert (format "#+TITLE: %s\n#+STARTUP: overview\n\n"
                        (file-name-base file)))))))

(gtd-init-files)
