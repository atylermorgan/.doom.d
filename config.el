;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Andrew Morgan"
      user-mail-address "a.tyler.morgan@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; theme
;;(setq doom-theme 'doom-solarized-light)
(setq doom-theme 'doom-material)
;; (setq doom-theme 'spacemacs-light)
;;(setq doom-theme 'doom-tomorrow-day)
;;(setq fancy-splash-image "~/Downloads/human_brain_sagittal.png")
(setq fancy-splash-image "~/.doom.d/images/monkey_brain.png")

;; set the default directory
(setq default-directory org-directory)

;; treemacs
(setq treemacs-position 'right)

;; Customize fonts
;; (setq doom-font (font-spec :family "Roboto Mono for Powerline"
;;                            :size 14 :height 1.0 :weight 'regular))
(setq doom-font (font-spec :family "Fira Code"
                           :size 14 :height 1.0 :weight 'regular))
;;       doom-variable-pitch-font (font-spec :family "ETBookOT"
;;                                           :style "RomanOSF"
;;                                           :height 1.5))
;; (add-hook! 'org-mode-hook #'mixed-pitch-mode)
;; (add-hook! 'org-mode-hook #'doom-big-font-mode)
;; (setq mixed-pitch-variable-pitch-cursor nil)

;; org-journal stuff
(setq org-journal-dir         "~/org/journal/"
      ;;org-journal-date-prefix "#+TITLE: "
      ;;org-journal-time-prefix "* "
      ;;org-journal-time-format "%A (%d) - %R"
      org-journal-file-type   'monthly
      ;;org-journal-date-format "Journal entries for %B"
      org-journal-file-format "%Y_%B.org"
      )

;; org-agenda files
(after! org
  (setq org-agenda-files
        (list org-directory
              org-journal-dir
              org-roam-directory
              "~/org/schedules"
              "~/org/roam/literature")))


;; org-super-agenda
(use-package! org-super-agenda
  :after org-agenda
  :init
  (setq org-super-agenda-groups '((:name "Important" :priority "A")
                                  ;;(:auto-dir-name t)
                                  ))
  :config
  (org-super-agenda-mode))

;; linking to outlook (requires this to work on startup)
(after! org
  (require 'org-mac-link))

;; org-gcal
(after! org
  (setq org-gcal-client-id "779424764391-5fspo69ufle15tbuibs447kbegmrpkdo.apps.googleusercontent.com"
      org-gcal-client-secret "_S1Nv2BTpiY0tm1uRYtQpWG9"
      org-gcal-file-alist '(("a.tyler.morgan@gmail.com" . "~/org/schedules/personal.org")
                            ("d9c9ebg96qo1npftl5h535ocek@group.calendar.google.com" . "~/org/schedules/nih.org")
                            ("5c0vfmm2tuils2qb0rr2a52opo@group.calendar.google.com" . "~/org/schedules/nih_talks.org"))))

;; org exports for latex pdfs
(after! org
  (add-to-list 'org-export-backends 'pdflatex))

;; org capture templates
(after! org
  (setq org-capture-templates
        '(("t" "Todo" entry
           (file+headline "~/org/todo.org" "Inbox")
           "* %?\n:PROPERTIES:\n:CREATED:%U\n:END:\n%i\n"
           :prepend t :kill-buffer t)
          ("i" "Idea!" entry
           (file+headline "~/projects/eln_morgan/source/org/inbox.org" "Ideas")
           "* %?\n:PROPERTIES:\n:CREATED:%U\n:END:\n%i\n"
           :prepend t :kill-buffer t)
          ("l" "Linescan" entry
           (file+headline "~/projects/eln_morgan/source/org/inbox.org" "Line Scanning")
           "** %?\n:PROPERTIES:\n:CREATED:%U\n:END:\n%i\n"
           :prepend t :kill-buffer t)
          ("v" "Visual Representations" entry
           (file+headline "~/projects/eln_morgan/source/org/inbox.org" "Visual Representations")
           "* %?\n:PROPERTIES:\n:CREATED:%U\n:END:\n%i\n"
           :prepend t :kill-buffer t)
          ("c" "Connectivity" entry
           (file+headline "~/projects/eln_morgan/source/org/inbox.org" "Connectivity")
           "* %?\n:PROPERTIES:\n:CREATED:%U\n:END:\n%i\n"
           :prepend t :kill-buffer t)
          ("e" "Encoding" entry
           (file+headline "~/projects/eln_morgan/source/org/inbox.org" "Encoding")
           "* %?\n:PROPERTIES:\n:CREATED:%U\n:END:\n%i\n"
           :prepend t :kill-buffer t)
          ("u" "UK7T" entry
           (file+headline "~/projects/eln_morgan/source/org/inbox.org" "UK7T")
           "* %?\n:PROPERTIES:\n:CREATED:%U\n:END:\n%i\n"
           :prepend t :kill-buffer t)
          )))

;; try to fix org inline images (see https://github.com/hlissner/doom-emacs/issues/3185)
(defadvice! no-errors/+org-inline-image-data-fn (_protocol link _description)
  :override #'+org-inline-image-data-fn
  "Interpret LINK as base64-encoded image data. Ignore all errors."
  (ignore-errors
    (base64-decode-string link)))
(after! org
  (setq org-image-actual-width 400))

;; org-roam-bibtex setup
(use-package! org-ref
    ;; :init
    ; code to run before loading org-ref
    :config
    (setq
         org-ref-completion-library 'org-ref-ivy-cite
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
         org-ref-notes-function 'orb-edit-notes
    ))

(after! org-ref
  (setq
   bibtex-completion-notes-path "~/org/roam/literature/bibnotes.org"
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
    ":END:\n\n"
    )
   )
  )

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

;; org-download customized location
(defun drestivo/org-download-method (link)
  "This is an helper function for org-download.
It creates an \"./image\" folder within the same directory of the org file.
Images are separated inside that image folder by additional folders one per
org file.
More info can be found here: https://github.com/abo-abo/org-download/issues/40.
See the commit message for an example:
https://github.com/abo-abo/org-download/commit/137c3d2aa083283a3fc853f9ecbbc03039bf397b"
  (let ((filename
         (file-name-nondirectory
          (car (url-path-and-query
                (url-generic-parse-url link)))))
        (dir (concat
              (file-name-directory (buffer-file-name))
              (format "%s/%s/%s"
                      "images"
                      (file-name-base (buffer-file-name))
                      (org-download--dir-2)))))
    (progn
      (setq filename-with-timestamp (format "%s%s.%s"
                                            (file-name-sans-extension filename)
                                            (format-time-string org-download-timestamp)
                                            (file-name-extension filename)))
      ;; Check if directory exists otherwise creates it
      (unless (file-exists-p dir)
        (make-directory dir t))
      (message (format "Image: %s saved!" (expand-file-name filename-with-timestamp dir)))
      (expand-file-name filename-with-timestamp dir))))

(setq org-download-method  'drestivo/org-download-method)

;; Set up ccls for lsp-mode (for c++)
(require 'ccls)
(setq ccls-executable "/usr/local/bin/ccls")
(after! ccls
  (setq ccls-initialization-options '(:index (:comments 2) :completion (:detailedLabel t)))
  (set-lsp-priority! 'ccls 2)) ; optional as ccls is the default in Doom
;; (require 'company-lsp) ; I think this no longer works...
;; (push 'company-lsp company-backends)

;; tramp stuff
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)
;; (setq remote-file-name-inhibit-cache nil)
;; (setq vc-ignore-dir-regexp
;;       (format "%s\\|%s"
;;                     vc-ignore-dir-regexp
;;                     tramp-file-name-regexp))
;; (setq tramp-verbose 1)
