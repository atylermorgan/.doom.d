;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; ============================================================================
;; PERSONAL INFORMATION
;; ============================================================================

(setq user-full-name "Tyler Morgan"
      user-mail-address "a.tyler.morgan@gmail.com")

;; ============================================================================
;; APPEARANCE & THEME
;; ============================================================================

;; Theme selection
(setq doom-theme 'doom-material)
;; Alternative themes (uncomment to use):
;; (setq doom-theme 'doom-solarized-light)
;; (setq doom-theme 'spacemacs-light)
;; (setq doom-theme 'doom-tomorrow-day)

;; Splash screen
(setq fancy-splash-image "~/.doom.d/images/monkey_brain.png")

;; Font configuration
(setq doom-font (font-spec :family "Fira Code"
                           :size 14
                           :height 1.0
                           :weight 'regular))

;; Line numbers
(setq display-line-numbers-type t)

;; ============================================================================
;; DIRECTORY SETTINGS
;; ============================================================================

(setq org-directory "~/org/")
(setq default-directory org-directory)

;; ============================================================================
;; UI CONFIGURATION
;; ============================================================================

;; Treemacs
(setq treemacs-position 'right)

;; Org inline images
;; Fix for org inline images (see https://github.com/hlissner/doom-emacs/issues/3185)
(defadvice! no-errors/+org-inline-image-data-fn (_protocol link _description)
  :override #'+org-inline-image-data-fn
  "Interpret LINK as base64-encoded image data. Ignore all errors."
  (ignore-errors
    (base64-decode-string link)))

(after! org
  (setq org-image-actual-width 400))

;; ============================================================================
;; ORG-MODE CONFIGURATION
;; ============================================================================

;; --- Org Journal ---
(setq org-journal-dir "~/org/journal/"
      org-journal-file-type 'monthly
      org-journal-file-format "%Y_%B.org")

;; --- Org Mac Links ---
(after! org
  (require 'org-mac-link))

(load! "modules/org-mac-outlook-link")
(setq url-capture-default-browser 'firefox
      url-capture-format 'org)

;; Org Mac Outlook link settings
(setq org-mac-outlook-link-format 'full)

(after! org-mac-outlook-link
  (setq org-mac-outlook-max-attempts 3
        org-mac-outlook-timeout 2
        org-mac-outlook-link-format 'with-sender))

;; Keybindings for org links
(map! :after org
      :map org-mode-map
      :localleader
      :desc "Insert Outlook link" "l o" #'org-mac-outlook-message-insert-selected
      :desc "Test Outlook connection" "l t" #'org-mac-outlook-link-test-connection
      :desc "Manual email entry" "l m" #'org-mac-outlook-link-manual-entry
      :desc "Insert url link" "l l" #'url-capture-insert-url
      :desc "Insert url link (prompt)" "l L" #'url-capture-insert-url-prompt)

;; --- Org Download ---
;; Custom download method for organizing images by org file
(defun drestivo/org-download-method (link)
  "Download images to ./images/{org-filename}/{date}/ directory.
More info: https://github.com/abo-abo/org-download/issues/40"
  (let* ((filename
          (file-name-nondirectory
           (car (url-path-and-query
                 (url-generic-parse-url link)))))
         (dir (concat
               (file-name-directory (buffer-file-name))
               (format "%s/%s/%s"
                       "images"
                       (file-name-base (buffer-file-name))
                       (org-download--dir-2))))
         (filename-with-timestamp
          (format "%s%s.%s"
                  (file-name-sans-extension filename)
                  (format-time-string org-download-timestamp)
                  (file-name-extension filename))))
    (unless (file-exists-p dir)
      (make-directory dir t))
    (message (format "Image: %s saved!" (expand-file-name filename-with-timestamp dir)))
    (expand-file-name filename-with-timestamp dir)))

(setq org-download-method 'drestivo/org-download-method)

;; ============================================================================
;; ORG-GCAL INTEGRATION
;; ============================================================================

;; Load environment variables from .env file
(defun load-env-file (file)
  "Load environment variables from FILE."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (re-search-forward "^\\([A-Za-z_][A-Za-z0-9_]*\\)=\\(.+\\)$" nil t)
        (let ((key (match-string 1))
              (value (match-string 2)))
          ;; Remove quotes if present
          (setq value (replace-regexp-in-string "^['\"]\\|['\"]$" "" value))
          (setenv key value))))))

;; Load .env file
(load-env-file (expand-file-name ".env" doom-user-dir))

;; Configure org-gcal
(after! org
  (setq org-gcal-client-id (getenv "GCAL_ID")
        org-gcal-client-secret (getenv "GCAL_SECRET")
        org-gcal-file-alist `((,(getenv "EMAIL") . "~/org/schedules/personal.org")
                              (,(getenv "EMAIL") . "~/projects/ftrmri/README.org")
                              (,(getenv "GROUP_EMAIL") . "~/org/schedules/nih.org"))))

;; ============================================================================
;; BIBLIOGRAPHY & RESEARCH (ORG-REF, ORG-ROAM-BIBTEX)
;; ============================================================================

;; --- Org-ref ---
(use-package! org-ref
  :config
  (setq org-ref-completion-library 'org-ref-ivy-cite
        org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
        org-ref-default-bibliography (list "~/gdrive/library.bib")
        org-ref-bibliography-notes "~/org/roam/literature/bibnotes.org"
        org-ref-note-title-format "* NOTES %y - %t\n :PROPERTIES:\n
                                                      :Custom_ID: %k\n
                                                      :ROAM_KEY: cite:%k\n
                                                      :AUTHOR: %9a\n
                                                      :JOURNAL: %j\n
                                                      :YEAR: %y\n
                                                      :VOLUME: %v\n
                                                      :PAGES: %p\n
                                                      :DOI: %D\n
                                                      :URL: %U\n
                                                      :END:\n\n"
        org-ref-notes-directory "~/org/roam/literature/bibnotes.org"
        org-ref-notes-function 'orb-edit-notes))

;; --- Bibtex completion ---
(after! org-ref
  (setq bibtex-completion-notes-path "~/org/roam/literature/bibnotes.org"
        bibtex-completion-bibliography "~/gdrive/library.bib"
        bibtex-completion-pdf-field "file"
        bibtex-completion-notes-template-multiple-files
        (concat
         "#+TITLE: ${title}\n"
         "#+ROAM_KEY: cite:${=key=}"
         "#+ROAM_TAGS: ${keywords}"
         "#+CREATED:%<%Y-%m-%d-%H-%M-%S>"
         "Time-stamp: <>\n"
         "- tags :: \n"
         "* NOTES \n"
         ":PROPERTIES:\n"
         ":Custom_ID: ${=key=}\n"
         ":AUTHOR: ${author-abbrev}\n"
         ":JOURNAL: ${journaltitle}\n"
         ":DATE: ${date}\n"
         ":YEAR: ${year}\n"
         ":DOI: ${doi}\n"
         ":URL: ${url}\n"
         ":END:\n\n")))

;; --- Org-roam-bibtex ---
(use-package! org-roam-bibtex
  :after (org-roam)
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (setq orb-preformat-keywords
        '("=key=" "title" "url" "file" "author-or-editor" "keywords"))
  (setq orb-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           ""
           :file-name "literature/%<%Y-%m-%d-%H-%M-%S>-${slug}"
           :head "#+TITLE: ${=key=}: ${title}
#+ROAM_KEY: ${ref}
#+ROAM_TAGS:
Time-stamp: <>
- tags :: ${keywords}

* ${title}
  :PROPERTIES:
  :Custom_ID: ${=key=}
  :URL: ${url}
  :AUTHOR: ${author-or-editor})
  :NOTER_PAGE:
  :END:

** TODO Take notes on ${title}

"
           :unnarrowed t))))

;; ============================================================================
;; DEVELOPMENT TOOLS
;; ============================================================================

;; --- C/C++ (ccls) ---
(require 'ccls)
(setq ccls-executable "/usr/local/bin/ccls")

(after! ccls
  (setq ccls-initialization-options '(:index (:comments 2)
                                      :completion (:detailedLabel t)))
  (set-lsp-priority! 'ccls 2))

;; --- Git/Magit SSH Configuration ---
(require 'exec-path-from-shell)
(exec-path-from-shell-copy-env "SSH_AGENT_PID")
(exec-path-from-shell-copy-env "SSH_AUTH_SOCK")

;; ============================================================================
;; EMAIL & CALENDAR
;; ============================================================================

;; Excorporate (Exchange/Outlook integration)
(use-package! excorporate
  :ensure t
  :config
  (setq excorporate-configuration
        (quote ("tyler.morgan@nih.gov" . "https://outlook.office365.com/EWS/Exchange.asmx"))))

;; ============================================================================
;; COMMENTED OUT / OPTIONAL CONFIGURATIONS
;; ============================================================================

;; --- Alternative font configurations ---
;; (setq doom-font (font-spec :family "Roboto Mono for Powerline"
;;                            :size 14 :height 1.0 :weight 'regular))
;; (setq doom-variable-pitch-font (font-spec :family "ETBookOT"
;;                                           :style "RomanOSF"
;;                                           :height 1.5))
;; (add-hook! 'org-mode-hook #'mixed-pitch-mode)
;; (add-hook! 'org-mode-hook #'doom-big-font-mode)
;; (setq mixed-pitch-variable-pitch-cursor nil)

;; --- LaTeX exports ---
;; (after! org
;;   (add-to-list 'org-export-backends 'pdflatex))

;; --- TRAMP settings ---
;; (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
;; (setq remote-file-name-inhibit-cache nil)
;; (setq vc-ignore-dir-regexp
;;       (format "%s\\|%s"
;;               vc-ignore-dir-regexp
;;               tramp-file-name-regexp))
;; (setq tramp-verbose 1)

;; ============================================================================
;; END OF CONFIG
;; ============================================================================
