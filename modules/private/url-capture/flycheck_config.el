;;; private/url-capture/config.el -*- lexical-binding: t; -*-

(defgroup url-capture nil
  "Capture URLs from web browsers."
  :group 'external
  :prefix "url-capture-")

(defcustom url-capture-default-browser 'safari
  "Default browser to capture URLs from.
Valid values are 'safari, 'chrome, 'firefox, or 'brave."
  :type '(choice (const :tag "Safari" safari)
                 (const :tag "Google Chrome" chrome)
                 (const :tag "Firefox" firefox)
                 (const :tag "Brave" brave))
  :group 'url-capture)

(defcustom url-capture-format 'org
  "Default format for captured URLs.
Valid values are 'org (Org-mode link), 'markdown, or 'plain."
  :type '(choice (const :tag "Org-mode link" org)
                 (const :tag "Markdown link" markdown)
                 (const :tag "Plain URL" plain))
  :group 'url-capture)

(defcustom url-capture-firefox-use-manual nil
  "If non-nil, use manual clipboard capture for Firefox.
This works around macOS Sequoia System Events restrictions."
  :type 'boolean
  :group 'url-capture)

(defcustom url-capture-firefox-fetch-title t
  "If non-nil, fetch page title from Firefox URLs by downloading the page.
If nil, just use the URL as the title."
  :type 'boolean
  :group 'url-capture)

(defcustom url-capture-firefox-refocus-emacs t
  "If non-nil, automatically refocus Emacs after Firefox capture.
This counteracts Firefox stealing focus during URL capture."
  :type 'boolean
  :group 'url-capture)

(defun url-capture--applescript-safari ()
  "Return AppleScript string to get URL and title from Safari."
  "tell application \"Safari\"
    if (count of windows) > 0 then
        set currentURL to URL of current tab of front window
        set currentTitle to name of current tab of front window
        return currentURL & \"::SEPARATOR::\" & currentTitle
    else
        return \"\"
    end if
end tell")

(defun url-capture--applescript-chrome ()
  "Return AppleScript string to get URL and title from Chrome."
  "tell application \"Google Chrome\"
    if (count of windows) > 0 then
        set currentURL to URL of active tab of front window
        set currentTitle to title of active tab of front window
        return currentURL & \"::SEPARATOR::\" & currentTitle
    else
        return \"\"
    end if
end tell")

(defun url-capture--applescript-brave ()
  "Return AppleScript string to get URL and title from Brave."
  "tell application \"Brave Browser\"
    if (count of windows) > 0 then
        set currentURL to URL of active tab of front window
        set currentTitle to title of active tab of front window
        return currentURL & \"::SEPARATOR::\" & currentTitle
    else
        return \"\"
    end if
end tell")

(defun url-capture--fetch-title-from-url (url)
  "Fetch the page title from URL by downloading and parsing HTML.
Returns the title string or nil on failure."
  (condition-case err
      (with-current-buffer (url-retrieve-synchronously url t nil 5)
        (goto-char (point-min))
        (when (re-search-forward "<title>\\([^<]+\\)</title>" nil t)
          (let ((title (match-string 1)))
            (kill-buffer)
            ;; Decode HTML entities
            (with-temp-buffer
              (insert title)
              (goto-char (point-min))
              (while (re-search-forward "&#\\([0-9]+\\);" nil t)
                (replace-match (char-to-string (string-to-number (match-string 1)))))
              (goto-char (point-min))
              (while (re-search-forward "&quot;" nil t)
                (replace-match "\""))
              (goto-char (point-min))
              (while (re-search-forward "&amp;" nil t)
                (replace-match "&"))
              (goto-char (point-min))
              (while (re-search-forward "&lt;" nil t)
                (replace-match "<"))
              (goto-char (point-min))
              (while (re-search-forward "&gt;" nil t)
                (replace-match ">"))
              (string-trim (buffer-string))))))
    (error
     (message "Failed to fetch title from %s: %S" url err)
     nil)))

(defun url-capture--refocus-emacs ()
  "Refocus Emacs window."
  (do-applescript
   (format "tell application \"%s\" to activate"
           (if (boundp 'mac-app-name)
               mac-app-name
             "Emacs"))))

(defun url-capture--get-firefox-url-manual ()
  "Get URL from Firefox using manual clipboard approach.
Prompts user to copy the URL themselves."
  (let ((saved-clipboard (gui-get-selection 'CLIPBOARD)))
    ;; Activate Firefox
    (do-applescript "tell application \"Firefox\" to activate")
    (sit-for 0.3)

    ;; Prompt user
    (message "Firefox activated. Press Cmd+L, then Cmd+C to copy the URL, then press any key...")
    (read-event)

    ;; Get URL from clipboard
    (let ((url (gui-get-selection 'CLIPBOARD)))

      ;; Restore clipboard
      (when (and saved-clipboard
                 (not (equal saved-clipboard url)))
        (run-with-timer 0.5 nil
                       (lambda (old-clip)
                         (gui-set-selection 'CLIPBOARD old-clip))
                       saved-clipboard))

      ;; Validate and return
      (if (and url
               (stringp url)
               (not (string-empty-p url))
               (string-match-p "^https?://" url))
          (let ((title (if url-capture-firefox-fetch-title
                          (or (url-capture--fetch-title-from-url url) url)
                        url)))
            (message "Captured URL from Firefox: %s" url)
            (cons (string-trim url) (string-trim title)))
        (progn
          (message "No valid URL found in clipboard")
          nil)))))

(defun url-capture--get-firefox-url-auto ()
  "Get URL from Firefox using automated keyboard simulation.
Returns (url . title) cons cell or nil on failure."
  (condition-case err
      (let ((saved-clipboard (gui-get-selection 'CLIPBOARD)))
        (message "Capturing from Firefox...")

        ;; Activate Firefox and simulate keystrokes
        (do-applescript
         "tell application \"Firefox\"
    activate
end tell

delay 0.3

tell application \"System Events\"
    keystroke \"l\" using command down
    delay 0.1
    keystroke \"c\" using command down
    delay 0.2
    keystroke return
end tell

delay 0.1")

        ;; Refocus Emacs if configured
        (when url-capture-firefox-refocus-emacs
          (run-with-timer 0.1 nil #'url-capture--refocus-emacs))

        ;; Get URL from clipboard
        (sit-for 0.3)
        (let ((url (gui-get-selection 'CLIPBOARD)))

          ;; Restore clipboard
          (when (and saved-clipboard
                     (not (equal saved-clipboard url)))
            (run-with-timer 0.5 nil
                          (lambda (old-clip)
                            (gui-set-selection 'CLIPBOARD old-clip))
                          saved-clipboard))

          ;; Validate and return
          (if (and url
                   (stringp url)
                   (not (string-empty-p url))
                   (string-match-p "^https?://" url))
              (let ((title (if url-capture-firefox-fetch-title
                              (progn
                                (message "Fetching page title...")
                                (or (url-capture--fetch-title-from-url url) url))
                            url)))
                (message "Captured URL from Firefox: %s" url)
                (cons (string-trim url) (string-trim title)))
            (progn
              (message "Failed to capture valid URL from Firefox")
              nil))))
    (error
     (message "Firefox auto-capture failed: %S" err)
     (message "Try setting url-capture-firefox-use-manual to t")
     nil)))

(defun url-capture--get-firefox-url ()
  "Get URL from Firefox.
Uses manual or automatic method depending on url-capture-firefox-use-manual."
  (if url-capture-firefox-use-manual
      (url-capture--get-firefox-url-manual)
    ;; Try automatic first, fall back to manual on error
    (or (url-capture--get-firefox-url-auto)
        (when (y-or-n-p "Auto-capture failed. Try manual capture? ")
          (url-capture--get-firefox-url-manual)))))

(defun url-capture--run-applescript (script)
  "Run SCRIPT with osascript and return the output."
  (condition-case err
      (let ((result (do-applescript script)))
        (when result
          (string-trim result)))
    (error
     (message "AppleScript error: %S" err)
     nil)))

(defun url-capture--parse-result (result)
  "Parse RESULT from AppleScript into (url . title) cons cell."
  (if (or (null result) (string-empty-p result))
      nil
    (let ((parts (split-string result "::SEPARATOR::")))
      (cons (string-trim (car parts))
            (if (cadr parts)
                (string-trim (cadr parts))
              (string-trim (car parts)))))))

(defun url-capture--format-link (url title format)
  "Format URL and TITLE according to FORMAT.
FORMAT can be 'org, 'markdown, or 'plain."
  (pcase format
    ('org (format "[[%s][%s]]" url title))
    ('markdown (format "[%s](%s)" title url))
    ('plain url)
    (_ url)))

(defun url-capture--get-from-browser (browser)
  "Get URL and title from BROWSER."
  (if (eq browser 'firefox)
      ;; Firefox uses special handling
      (url-capture--get-firefox-url)
    ;; Other browsers use AppleScript
    (let* ((script (pcase browser
                     ('safari (url-capture--applescript-safari))
                     ('chrome (url-capture--applescript-chrome))
                     ('brave (url-capture--applescript-brave))
                     (_ (error "Unknown browser: %s" browser))))
           (result (url-capture--run-applescript script)))
      (url-capture--parse-result result))))

;;;###autoload
(defun url-capture-get-url (&optional browser format)
  "Get URL from BROWSER in FORMAT.
If BROWSER is nil, use `url-capture-default-browser'.
If FORMAT is nil, use `url-capture-format'.
Returns the formatted link as a string."
  (interactive)
  (let* ((browser (or browser url-capture-default-browser))
         (format (or format url-capture-format))
         (data (url-capture--get-from-browser browser)))
    (if data
        (url-capture--format-link (car data) (cdr data) format)
      (error "Could not capture URL from %s" browser))))

;;;###autoload
(defun url-capture-insert-url (&optional browser format)
  "Insert URL from BROWSER in FORMAT at point.
If BROWSER is nil, use `url-capture-default-browser'.
If FORMAT is nil, use `url-capture-format'."
  (interactive)
  (let ((link (url-capture-get-url browser format)))
    (when link
      (insert link)
      (message "Inserted URL from %s" (or browser url-capture-default-browser)))))

;;;###autoload
(defun url-capture-insert-url-prompt ()
  "Prompt for browser and insert URL at point."
  (interactive)
  (let* ((browser (intern (completing-read "Browser: "
                                          '("safari" "chrome" "firefox" "brave")
                                          nil t)))
         (format (intern (completing-read "Format: "
                                         '("org" "markdown" "plain")
                                         nil t nil nil
                                         (symbol-name url-capture-format)))))
    (url-capture-insert-url browser format)))

;;;###autoload
(defun url-capture-kill-url (&optional browser format)
  "Copy URL from BROWSER in FORMAT to kill ring.
If BROWSER is nil, use `url-capture-default-browser'.
If FORMAT is nil, use `url-capture-format'."
  (interactive)
  (let ((link (url-capture-get-url browser format)))
    (when link
      (kill-new link)
      (message "Copied URL from %s to kill ring" (or browser url-capture-default-browser)))))

;; Convenience functions for specific browsers
;;;###autoload
(defun url-capture-safari ()
  "Insert URL from Safari at point."
  (interactive)
  (url-capture-insert-url 'safari))

;;;###autoload
(defun url-capture-chrome ()
  "Insert URL from Chrome at point."
  (interactive)
  (url-capture-insert-url 'chrome))

;;;###autoload
(defun url-capture-firefox ()
  "Insert URL from Firefox at point."
  (interactive)
  (url-capture-insert-url 'firefox))

;;;###autoload
(defun url-capture-brave ()
  "Insert URL from Brave at point."
  (interactive)
  (url-capture-insert-url 'brave))

(provide 'url-capture)
