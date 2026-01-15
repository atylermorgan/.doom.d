;;; org-mac-outlook-link.el --- Grab links from Outlook for org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Your Name
;; Keywords: org, outlook, mac, email
;; Version: 1.0

;;; Commentary:
;; This module provides functionality to insert links to Outlook emails
;; into org-mode, similar to the old org-mac-link package.
;; Works with Classic Outlook for Mac using AppleScript.

;;; Code:

(require 'org)

(defgroup org-mac-outlook-link nil
  "Options for linking to Outlook emails from org-mode."
  :group 'org-link)

(defcustom org-mac-outlook-max-attempts 3
  "Maximum number of attempts to retrieve Outlook email."
  :type 'integer
  :group 'org-mac-outlook-link)

(defcustom org-mac-outlook-timeout 2
  "Timeout in seconds for each Outlook query attempt."
  :type 'integer
  :group 'org-mac-outlook-link)

(defcustom org-mac-outlook-link-format 'subject-only
  "Format for the link description.
- 'subject-only: Just the email subject
- 'with-sender: Subject (from Sender)
- 'with-date: Subject [Date]
- 'full: Subject (from Sender, Date)"
  :type '(choice (const :tag "Subject only" subject-only)
                 (const :tag "With sender" with-sender)
                 (const :tag "With date" with-date)
                 (const :tag "Full details" full))
  :group 'org-mac-outlook-link)

(defun org-mac-outlook-link--get-email-data ()
  "Get email data from selected Outlook message.
Returns a plist with :id, :subject, :from, and :date, or nil on failure."
  (condition-case err
      (let* ((script "
tell application \"Microsoft Outlook\"
    set selectedMessages to selected objects
    if (count of selectedMessages) is 0 then
        error \"No message selected\"
    end if
    set theMessage to item 1 of selectedMessages
    set msgSubject to subject of theMessage as text
    -- Use message id (Internet Message-ID) instead of local id
    set msgId to message id of theMessage as text
    set msgFrom to sender of theMessage
    set msgDate to time received of theMessage
    set result to msgId & \"|||\"
    set result to result & msgSubject & \"|||\"
    set result to result & (name of msgFrom as text) & \"|||\"
    set result to result & (msgDate as text)
    return result
end tell")
             (result (do-applescript script)))
        (when (and result (string-match "\\(.+?\\)|||\\(.+?\\)|||\\(.+?\\)|||\\(.+\\)" result))
          (list :id (match-string 1 result)
                :subject (match-string 2 result)
                :from (match-string 3 result)
                :date (match-string 4 result))))
    (error nil)))

(defun org-mac-outlook-link--format-description (data)
  "Format link description based on DATA plist and user preference."
  (let ((subject (plist-get data :subject))
        (from (plist-get data :from))
        (date (plist-get data :date)))
    (pcase org-mac-outlook-link-format
      ('subject-only subject)
      ('with-sender (format "%s (from %s)" subject from))
      ('with-date (format "%s [%s]" subject date))
      ('full (format "%s (from %s, %s)" subject from date))
      (_ subject))))

(defun org-mac-outlook-link--retrieve-with-retry ()
  "Attempt to retrieve Outlook email with retries.
Returns a plist with email data or nil on failure."
  (let ((attempt 0)
        (success nil)
        (data nil))
    (while (and (< attempt org-mac-outlook-max-attempts) (not success))
      (setq attempt (1+ attempt))
      (message "Retrieving Outlook email (attempt %d/%d)..."
               attempt org-mac-outlook-max-attempts)

      (with-timeout (org-mac-outlook-timeout
                     (message "Timeout on attempt %d" attempt))
        (setq data (org-mac-outlook-link--get-email-data))
        (when data
          (setq success t))))

    (if success
        (progn
          (message "✓ Email retrieved successfully")
          data)
      (message "❌ Failed to retrieve email after %d attempts"
               org-mac-outlook-max-attempts)
      nil)))

;;;###autoload
(defun org-mac-outlook-message-insert-selected ()
  "Insert link to selected Outlook message at point.
This is a drop-in replacement for the org-mac-link function."
  (interactive)
  (let ((data (org-mac-outlook-link--retrieve-with-retry)))
    (if data
        (let* ((link (format "message:%s" (plist-get data :id)))
               (description (org-mac-outlook-link--format-description data)))
          (insert (format "[[%s][%s]]" link description))
          (message "Outlook link inserted"))
      ;; Fallback: manual entry
      (when (y-or-n-p "Failed to get link automatically. Enter manually? ")
        (org-mac-outlook-link-manual-entry)))))

;;;###autoload
(defun org-mac-outlook-link-manual-entry ()
  "Manually create an email reference entry.
Useful as fallback when AppleScript fails."
  (interactive)
  (let* ((subject (read-string "Email subject: "))
         (from (read-string "From: "))
         (date (read-string "Date (optional): "))
         (description (if (not (string-empty-p from))
                         (format "%s (from %s)" subject from)
                       subject)))
    (insert (format "Email: %s" description))
    (when (not (string-empty-p date))
      (insert (format " [%s]" date)))
    (message "Manual email reference created")))

;;;###autoload
(defun org-mac-outlook-link-test-connection ()
  "Test connection to Outlook and display email info.
Useful for debugging."
  (interactive)
  (message "Testing Outlook connection...")
  (let ((data (org-mac-outlook-link--get-email-data)))
    (if data
        (message "✓ Success!\nSubject: %s\nFrom: %s\nDate: %s\nID: %s"
                 (plist-get data :subject)
                 (plist-get data :from)
                 (plist-get data :date)
                 (plist-get data :id))
      (message "❌ Failed to retrieve email. Make sure:\n1. Classic Outlook is running\n2. An email is selected\n3. AppleScript access is enabled"))))

;; Setup org-link type for message: links
;;;###autoload
(defun org-mac-outlook-link-setup ()
  "Set up org-link handler for message: links."
  (org-link-set-parameters "message"
                           :follow #'org-mac-outlook-link-open
                           :export #'org-mac-outlook-link-export)
  (message "Outlook message link handler installed"))

(defun org-mac-outlook-link-open (id)
  "Open Outlook email with ID (Message-ID)."
  (do-applescript
   (format "
tell application \"Microsoft Outlook\"
    activate
    -- Search for message by Message-ID in headers
    set allMessages to messages of inbox
    repeat with theMsg in allMessages
        set msgHeaders to headers of theMsg
        if msgHeaders contains \"%s\" then
            open theMsg
            return
        end if
    end repeat
    display dialog \"Email not found with Message-ID: %s\" buttons {\"OK\"} default button 1
end tell" id id)))

(defun org-mac-outlook-link-export (link description format)
  "Export message LINK with DESCRIPTION to FORMAT."
  (let ((desc (or description link)))
    (pcase format
      ('html (format "<a href=\"message:%s\">%s</a>" link desc))
      ('latex (format "\\href{message:%s}{%s}" link desc))
      ('ascii desc)
      (_ desc))))

;; Auto-setup on load
;;;###autoload
(with-eval-after-load 'org
  (org-mac-outlook-link-setup))

(provide 'org-mac-outlook-link)
;;; org-mac-outlook-link.el ends here
