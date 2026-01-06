;;; org-outlook-graph.el --- Outlook integration via Microsoft Graph API -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Your Name
;; Keywords: org, outlook, email, graph-api
;; Version: 2.0

;;; Commentary:
;; This module provides Outlook email linking for org-mode using the
;; Microsoft Graph API. Works with new Outlook and is cross-platform.

;;; Code:

(require 'org)
(require 'json)

(defgroup org-outlook-graph nil
  "Options for linking to Outlook emails via Graph API."
  :group 'org-link)

(defcustom org-outlook-graph-python-script
  (expand-file-name "outlook_api.py" "~/.doom.d/modules/")
  "Path to the Outlook Graph API Python script."
  :type 'file
  :group 'org-outlook-graph)

(defcustom org-outlook-graph-python-command "python3"
  "Python command to use."
  :type 'string
  :group 'org-outlook-graph)

(defcustom org-outlook-graph-link-format 'subject-only
  "Format for the link description.
- 'subject-only: Just the email subject
- 'with-sender: Subject (from Sender)
- 'with-date: Subject [Date]
- 'full: Subject (from Sender, Date)"
  :type '(choice (const :tag "Subject only" subject-only)
                 (const :tag "With sender" with-sender)
                 (const :tag "With date" with-date)
                 (const :tag "Full details" full))
  :group 'org-outlook-graph)

(defun org-outlook-graph--call-api (args)
  "Call the Python API script with ARGS and return parsed JSON result."
  (let* ((cmd (format "%s %s %s"
                     org-outlook-graph-python-command
                     (shell-quote-argument org-outlook-graph-python-script)
                     args))
         (output (shell-command-to-string cmd)))
    (condition-case err
        (let ((json-array-type 'list)
              (json-object-type 'alist)
              (json-key-type 'symbol))
          (json-read-from-string output))
        (error
         (message "API Error: %s" output)
         nil))))

(defun org-outlook-graph--format-description (email format-type)
  "Format EMAIL description based on FORMAT-TYPE."
  (let ((subject (alist-get 'subject email))
        (from (alist-get 'from_name email))
        (date (alist-get 'date email)))
    (pcase format-type
      ('subject-only subject)
      ('with-sender (format "%s (from %s)" subject from))
      ('with-date (format "%s [%s]" subject (substring date 0 10)))
      ('full (format "%s (from %s, %s)" subject from (substring date 0 10)))
      (_ subject))))

;;;###autoload
(defun org-outlook-graph-recent-emails (&optional count)
  "Show COUNT recent emails in a buffer for selection.
Default is 10 emails."
  (interactive "p")
  (let* ((count (or count 10))
         (origin-buffer (current-buffer))
         (origin-window (selected-window))
         (emails (org-outlook-graph--call-api (format "--recent %d" count))))
    (if (and emails (not (alist-get 'error emails)))
        (org-outlook-graph--show-email-selector emails origin-buffer origin-window)
      (message "Failed to fetch emails: %s" (alist-get 'error emails)))))

(defun org-outlook-graph--show-email-selector (emails origin-buffer origin-window)
  "Show EMAILS in a selection buffer.
ORIGIN-BUFFER and ORIGIN-WINDOW track where to insert the link."
  (let ((buf (get-buffer-create "*Outlook Emails*")))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert "Recent Outlook Emails - Press number to insert link, q to quit\n")
      (insert "===============================================================\n\n")
      (let ((idx 1)
            (start-pos nil))
        (dolist (email emails)
          (when (<= idx 9)  ; Only show first 9
            (let ((subject (alist-get 'subject email))
                  (from (alist-get 'from_name email))
                  (date (alist-get 'date email)))
              (setq start-pos (point))
              (insert (format "%d. %s\n" idx subject))
              (insert (format "   From: %s\n" from))
              (insert (format "   Date: %s\n\n" (substring date 0 10)))
              ;; Store email data on the number line
              (put-text-property start-pos (1+ start-pos) 'email-data email)
              (put-text-property start-pos (1+ start-pos) 'email-index idx)
              (setq idx (1+ idx))))))
      (setq buffer-read-only t)
      (goto-char (point-min))
      ;; Store origin buffer/window and emails as buffer-local variables
      (setq-local org-outlook-origin-buffer origin-buffer)
      (setq-local org-outlook-origin-window origin-window)
      (setq-local org-outlook-emails emails)
      (local-set-key (kbd "q") 'kill-buffer-and-window)
      (dotimes (i 9)
        (local-set-key (kbd (number-to-string (1+ i)))
                      `(lambda () (interactive)
                         (org-outlook-graph--insert-link-from-buffer ,(1+ i))))))
    (pop-to-buffer buf)))

(defun org-outlook-graph--insert-link-from-buffer (num)
  "Insert link for email number NUM from the selector buffer."
  (let* ((emails org-outlook-emails)
         (email (nth (1- num) emails))
         (origin-buffer org-outlook-origin-buffer)
         (origin-window org-outlook-origin-window)
         (selector-window (selected-window)))
    (when email
      ;; Return to original window/buffer first
      (when (window-live-p origin-window)
        (select-window origin-window))
      (when (buffer-live-p origin-buffer)
        (switch-to-buffer origin-buffer))
      ;; Kill the selector buffer and its window
      (when (buffer-live-p (get-buffer "*Outlook Emails*"))
        (kill-buffer "*Outlook Emails*"))
      (when (and (window-live-p selector-window)
                 (not (eq selector-window origin-window)))
        (delete-window selector-window))
      ;; Now insert the link
      (org-outlook-graph--insert-email-link email))))

(defun org-outlook-graph--get-emails-from-buffer ()
  "Extract email data from current buffer."
  (save-excursion
    (goto-char (point-min))
    (let (emails)
      (while (not (eobp))
        (let ((email (get-text-property (point) 'email-data)))
          (when email
            (push email emails)))
        (forward-line 1))
      (nreverse emails))))

(defun org-outlook-graph--insert-email-link (email)
  "Insert org link for EMAIL."
  (let* ((id (alist-get 'id email))
         (web-link (alist-get 'webLink email))
         (description (org-outlook-graph--format-description
                      email org-outlook-graph-link-format))
         (link (or web-link (format "outlook-graph:%s" id))))
    (insert (format "[[%s][%s]]" link description))
    (message "Outlook link inserted")))

;;;###autoload
(defun org-outlook-graph-insert-link ()
  "Insert link to Outlook email.
Shows recent emails for selection."
  (interactive)
  (org-outlook-graph-recent-emails 10))

;;;###autoload
(defun org-outlook-graph-insert-by-subject ()
  "Search for email by subject (from clipboard) and insert link.
Useful workflow: Copy email subject in Outlook, then run this."
  (interactive)
  (let* ((subject (current-kill 0 t))
         (query (read-string "Search subject: " subject))
         (emails (org-outlook-graph--call-api
                 (format "--search %s" (shell-quote-argument query)))))
    (if (and emails (not (alist-get 'error emails)) (> (length emails) 0))
        (if (= (length emails) 1)
            ;; Only one match, insert directly
            (progn
              (org-outlook-graph--insert-email-link (car emails))
              (message "✓ Link inserted"))
          ;; Multiple matches, show selector
          (let ((origin-buffer (current-buffer))
                (origin-window (selected-window)))
            (org-outlook-graph--show-email-selector emails origin-buffer origin-window)))
      (message "No emails found matching: %s" query))))

;;;###autoload
(defun org-outlook-graph-search ()
  "Search Outlook emails and insert link to selected result."
  (interactive)
  (let* ((query (read-string "Search Outlook: "))
         (origin-buffer (current-buffer))
         (origin-window (selected-window))
         (emails (org-outlook-graph--call-api
                 (format "--search %s" (shell-quote-argument query)))))
    (if (and emails (not (alist-get 'error emails)) (> (length emails) 0))
        (org-outlook-graph--show-email-selector emails origin-buffer origin-window)
      (message "No emails found matching: %s" query))))

;;;###autoload
(defun org-outlook-graph-authenticate ()
  "Authenticate with Microsoft Graph API."
  (interactive)
  (message "Starting authentication... Check your terminal/browser")
  (let ((result (org-outlook-graph--call-api "--auth")))
    (if (alist-get 'authenticated result)
        (message "✓ Authentication successful!")
      (message "✗ Authentication failed: %s" (alist-get 'error result)))))

;;;###autoload
(defun org-outlook-graph-test ()
  "Test Graph API connection."
  (interactive)
  (message "Testing Graph API connection...")
  (let ((result (org-outlook-graph--call-api "--test")))
    (if (alist-get 'authenticated result)
        (message "✓ Graph API working! You're authenticated.")
      (message "✗ Not authenticated. Run M-x org-outlook-graph-authenticate"))))

;; Link type handlers
(defun org-outlook-graph-open-link (link)
  "Open Outlook email LINK in browser."
  ;; If it's an outlook-graph: link, we need to fetch the web link
  (if (string-prefix-p "http" link)
      (browse-url link)
    ;; It's an ID, fetch the email and open web link
    (let* ((email (org-outlook-graph--call-api (format "--get %s" link)))
           (web-link (alist-get 'webLink email)))
      (if web-link
          (browse-url web-link)
        (message "Could not open email: %s" (alist-get 'error email))))))

(defun org-outlook-graph-export-link (link description format)
  "Export Outlook LINK with DESCRIPTION to FORMAT."
  (let ((desc (or description link))
        (url (if (string-prefix-p "http" link) link "")))
    (pcase format
      ('html (if url (format "<a href=\"%s\">%s</a>" url desc) desc))
      ('latex (if url (format "\\href{%s}{%s}" url desc) desc))
      ('ascii desc)
      (_ desc))))

;;;###autoload
(defun org-outlook-graph-setup ()
  "Set up org-link handlers for Outlook Graph links."
  (org-link-set-parameters "outlook-graph"
                           :follow #'org-outlook-graph-open-link
                           :export #'org-outlook-graph-export-link)
  ;; Also handle https://outlook links
  (advice-add 'org-link-open :around #'org-outlook-graph--link-advice)
  (message "Outlook Graph API link handler installed"))

(defun org-outlook-graph--link-advice (orig-fun &rest args)
  "Advice to handle outlook.office.com links."
  (let ((link (car args)))
    (if (and (stringp link) (string-match-p "outlook\\.office" link))
        (browse-url link)
      (apply orig-fun args))))

;; Auto-setup
;;;###autoload
(with-eval-after-load 'org
  (org-outlook-graph-setup))

(provide 'org-outlook-graph)
;;; org-outlook-graph.el ends here
