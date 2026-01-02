#!/bin/bash
# Helper script for Firefox URL capture
# This runs as a separate process to get proper accessibility permissions

osascript <<'APPLESCRIPT'
tell application "Firefox"
    activate
end tell

delay 0.3

tell application "System Events"
    tell process "Firefox"
        keystroke "l" using command down
        delay 0.1
        keystroke "c" using command down
        delay 0.2
        keystroke return
    end tell
end tell

delay 0.1
return the clipboard
APPLESCRIPT
