"""
Outlook Email Link Tool - Command Line Interface for Emacs Integration

Prerequisites:
1. Install: pip install msal requests pywin32 (Windows) or appscript (Mac)
2. Configure credentials below
3. Run initial auth: python outlook_api.py --auth

Usage from command line:
  python outlook_api.py --selected           # Get currently selected email in Outlook
  python outlook_api.py --recent 10          # List recent emails
  python outlook_api.py --get MESSAGE_ID     # Get specific email link
  python outlook_api.py --search "subject"   # Search emails
"""

import msal
import requests
import json
import sys
import argparse
import os
from pathlib import Path
import platform

# Configuration - UPDATE THESE VALUES
CLIENT_ID = "4ae68297-e83b-41a1-8a0f-659db3f6ad01"
TENANT_ID = "14b77578-9773-42d5-8507-251ca2dc2b06"

# Token cache location
CACHE_FILE = os.path.join(str(Path.home()), ".outlook-api-token-cache")

# Microsoft Graph API endpoint
AUTHORITY = f"https://login.microsoftonline.com/{TENANT_ID}"
SCOPES = ["Mail.Read"]
GRAPH_API_ENDPOINT = "https://graph.microsoft.com/v1.0"


def get_token_cache():
    """Load token cache from file"""
    cache = msal.SerializableTokenCache()
    if os.path.exists(CACHE_FILE):
        with open(CACHE_FILE, 'r') as f:
            cache.deserialize(f.read())
    return cache


def save_token_cache(cache):
    """Save token cache to file"""
    if cache.has_state_changed:
        with open(CACHE_FILE, 'w') as f:
            f.write(cache.serialize())


def get_access_token(interactive=False):
    """Get access token with caching"""
    cache = get_token_cache()
    app = msal.PublicClientApplication(
        CLIENT_ID,
        authority=AUTHORITY,
        token_cache=cache
    )

    # Try silent authentication first
    accounts = app.get_accounts()
    if accounts:
        result = app.acquire_token_silent(SCOPES, account=accounts[0])
        if result and "access_token" in result:
            save_token_cache(cache)
            return result["access_token"]

    # Need interactive auth
    if not interactive:
        return None

    # Use device code flow for terminal
    flow = app.initiate_device_flow(scopes=SCOPES)
    if "user_code" not in flow:
        raise ValueError("Failed to create device flow")

    print(flow["message"], file=sys.stderr)

    result = app.acquire_token_by_device_flow(flow)

    if "access_token" in result:
        save_token_cache(cache)
        return result["access_token"]

    raise Exception(f"Authentication failed: {result.get('error_description')}")


def get_selected_email_mac():
    """Get currently selected email in Outlook on Mac using AppleScript"""
    try:
        import subprocess
        script = '''
tell application "Microsoft Outlook"
    set selectedMessages to selected objects
    if (count of selectedMessages) is 0 then
        error "No message selected"
    end if
    set theMessage to item 1 of selectedMessages
    set msgSubject to subject of theMessage as text
    set msgFrom to sender of theMessage
    set msgHeaders to headers of theMessage as text
    return msgSubject & "|||" & (name of msgFrom as text) & "|||" & msgHeaders
end tell
'''
        result = subprocess.run(['osascript', '-e', script],
                              capture_output=True, text=True, check=True)

        parts = result.stdout.strip().split('|||')
        if len(parts) >= 3:
            subject = parts[0]
            from_name = parts[1]
            headers = parts[2]

            # Extract Message-ID from headers
            import re
            match = re.search(r'Message-ID:\s*<?([^<>\s]+@[^<>\s]+)>?', headers)
            if match:
                message_id_header = match.group(1)
                # Return in format compatible with API
                return {
                    'subject': subject,
                    'from_name': from_name,
                    'message_id_header': message_id_header
                }

        return None
    except Exception as e:
        raise Exception(f"Could not get selected email: {e}")


def get_selected_email_windows():
    """Get currently selected email in Outlook on Windows using COM"""
    try:
        import win32com.client
        outlook = win32com.client.Dispatch("Outlook.Application")
        explorer = outlook.ActiveExplorer()

        if explorer is None:
            raise Exception("Outlook is not running or no window is active")

        selection = explorer.Selection
        if selection.Count == 0:
            raise Exception("No message selected")

        mail = selection.Item(1)

        # Get Message-ID from headers
        try:
            # PR_TRANSPORT_MESSAGE_HEADERS
            headers = mail.PropertyAccessor.GetProperty("http://schemas.microsoft.com/mapi/proptag/0x007D001E")
            import re
            match = re.search(r'Message-ID:\s*<?([^<>\s]+@[^<>\s]+)>?', headers)
            message_id_header = match.group(1) if match else None
        except:
            message_id_header = None

        return {
            'subject': mail.Subject,
            'from_name': mail.SenderName,
            'message_id_header': message_id_header
        }
    except Exception as e:
        raise Exception(f"Could not get selected email: {e}")


def get_selected_email():
    """Get currently selected email (cross-platform)"""
    system = platform.system()

    if system == "Darwin":  # macOS
        return get_selected_email_mac()
    elif system == "Windows":
        return get_selected_email_windows()
    else:
        raise Exception("Unsupported platform. Selected email only works on Mac and Windows.")


def find_email_by_message_id_header(token, message_id_header):
    """Find email in mailbox by Message-ID header"""
    headers = {
        'Authorization': f'Bearer {token}',
        'Content-Type': 'application/json'
    }

    # Search by Internet Message ID
    filter_query = f"internetMessageId eq '{message_id_header}'"
    url = f"{GRAPH_API_ENDPOINT}/me/messages?$filter={filter_query}&$select=id,subject,from,receivedDateTime,webLink"

    response = requests.get(url, headers=headers)

    if response.status_code == 200:
        results = response.json().get('value', [])
        if results:
            return results[0]

    # Fallback: search recent emails (in case filter doesn't work)
    url = f"{GRAPH_API_ENDPOINT}/me/messages?$top=100&$select=id,subject,from,receivedDateTime,webLink,internetMessageId"
    response = requests.get(url, headers=headers)

    if response.status_code == 200:
        messages = response.json().get('value', [])
        for msg in messages:
            if msg.get('internetMessageId') == message_id_header:
                return msg

    return None


def get_recent_emails(token, count=10):
    """Get recent emails"""
    headers = {
        'Authorization': f'Bearer {token}',
        'Content-Type': 'application/json'
    }

    url = f"{GRAPH_API_ENDPOINT}/me/messages?$top={count}&$select=id,subject,from,receivedDateTime,webLink"
    response = requests.get(url, headers=headers)

    if response.status_code == 200:
        return response.json().get('value', [])
    else:
        raise Exception(f"API Error {response.status_code}: {response.text}")


def get_email_by_id(token, message_id):
    """Get email details by ID"""
    headers = {
        'Authorization': f'Bearer {token}',
        'Content-Type': 'application/json'
    }

    url = f"{GRAPH_API_ENDPOINT}/me/messages/{message_id}?$select=id,subject,from,receivedDateTime,webLink"
    response = requests.get(url, headers=headers)

    if response.status_code == 200:
        return response.json()
    else:
        raise Exception(f"API Error {response.status_code}: {response.text}")


def search_emails(token, query, count=10):
    """Search emails by query"""
    headers = {
        'Authorization': f'Bearer {token}',
        'Content-Type': 'application/json'
    }

    url = f"{GRAPH_API_ENDPOINT}/me/messages?$search=\"{query}\"&$top={count}&$select=id,subject,from,receivedDateTime,webLink"
    response = requests.get(url, headers=headers)

    if response.status_code == 200:
        return response.json().get('value', [])
    else:
        raise Exception(f"API Error {response.status_code}: {response.text}")


def format_email_json(email):
    """Format email as JSON for Emacs"""
    return {
        'id': email.get('id'),
        'subject': email.get('subject', 'No Subject'),
        'from': email.get('from', {}).get('emailAddress', {}).get('address', 'Unknown'),
        'from_name': email.get('from', {}).get('emailAddress', {}).get('name', 'Unknown'),
        'date': email.get('receivedDateTime', 'Unknown'),
        'webLink': email.get('webLink', '')
    }


def main():
    parser = argparse.ArgumentParser(description='Outlook Graph API Tool for Emacs')
    parser.add_argument('--auth', action='store_true', help='Authenticate interactively')
    parser.add_argument('--selected', action='store_true', help='Get currently selected email in Outlook')
    parser.add_argument('--recent', type=int, metavar='N', help='Get N recent emails')
    parser.add_argument('--get', metavar='MESSAGE_ID', help='Get specific email by ID')
    parser.add_argument('--search', metavar='QUERY', help='Search emails')
    parser.add_argument('--test', action='store_true', help='Test authentication')

    args = parser.parse_args()

    try:
        # Get token (interactive if --auth flag)
        token = get_access_token(interactive=args.auth)

        if not token:
            print(json.dumps({
                'error': 'Not authenticated. Run with --auth first.',
                'authenticated': False
            }))
            sys.exit(1)

        # Execute command
        if args.test or args.auth:
            # Just verify auth works
            emails = get_recent_emails(token, 1)
            print(json.dumps({
                'authenticated': True,
                'message': 'Authentication successful'
            }))

        elif args.selected:
            # Get selected email from Outlook
            selected = get_selected_email()
            if selected and selected.get('message_id_header'):
                # Find in mailbox via API
                email = find_email_by_message_id_header(token, selected['message_id_header'])
                if email:
                    print(json.dumps(format_email_json(email)))
                else:
                    print(json.dumps({
                        'error': 'Email found in Outlook but not in mailbox via API',
                        'subject': selected.get('subject'),
                        'from_name': selected.get('from_name')
                    }))
            else:
                print(json.dumps({'error': 'Could not get selected email or Message-ID'}))

        elif args.recent:
            emails = get_recent_emails(token, args.recent)
            result = [format_email_json(e) for e in emails]
            print(json.dumps(result))

        elif args.get:
            email = get_email_by_id(token, args.get)
            print(json.dumps(format_email_json(email)))

        elif args.search:
            emails = search_emails(token, args.search)
            result = [format_email_json(e) for e in emails]
            print(json.dumps(result))

        else:
            parser.print_help()
            sys.exit(1)

    except Exception as e:
        print(json.dumps({'error': str(e)}), file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
"""
Outlook Email Link Retriever using Microsoft Graph API

Prerequisites:
1. Install required package: pip install msal requests
2. Register an app in Azure AD: https://portal.azure.com/#view/Microsoft_AAD_RegisteredApps
3. Set up API permissions: Mail.Read (Delegated)
4. Add Redirect URI: http://localhost (under "Mobile and desktop applications")
5. Update the credentials below
"""

import msal
import requests
import json
import webbrowser

# Configuration - UPDATE THESE VALUES
CLIENT_ID = "your-client-id-here"
TENANT_ID = "your-tenant-id-here"  # or "common" for multi-tenant

# Microsoft Graph API endpoint
AUTHORITY = f"https://login.microsoftonline.com/{TENANT_ID}"
SCOPES = ["Mail.Read"]  # Using delegated permissions
GRAPH_API_ENDPOINT = "https://graph.microsoft.com/v1.0"


def get_access_token():
    """Authenticate and get access token using device code flow"""
    app = msal.PublicClientApplication(
        CLIENT_ID,
        authority=AUTHORITY,
    )

    # Try to get token from cache first
    accounts = app.get_accounts()
    if accounts:
        print("Found cached account, attempting silent sign-in...")
        result = app.acquire_token_silent(SCOPES, account=accounts[0])
        if result and "access_token" in result:
            print("Silent authentication successful!")
            return result["access_token"]

    # If no cached token, use device code flow (works in terminal)
    print("No cached token found. Starting interactive authentication...")
    print("\nChoose authentication method:")
    print("1. Device Code Flow (recommended for terminal/SSH)")
    print("2. Interactive Browser Flow")

    choice = input("Enter choice (1 or 2): ").strip()

    if choice == "2":
        # Interactive flow - opens browser
        result = app.acquire_token_interactive(SCOPES)
    else:
        # Device code flow - shows a code to enter in browser
        flow = app.initiate_device_flow(scopes=SCOPES)

        if "user_code" not in flow:
            raise ValueError("Failed to create device flow")

        print("\n" + "="*60)
        print(flow["message"])
        print("="*60 + "\n")

        result = app.acquire_token_by_device_flow(flow)

    if "access_token" in result:
        print("Authentication successful!")
        return result["access_token"]
    else:
        print(f"Error acquiring token: {result.get('error')}")
        print(f"Description: {result.get('error_description')}")
        return None


def get_email_link(access_token, message_id):
    """
    Get a web link to an email message

    Args:
        access_token: Valid access token
        message_id: The ID of the email message

    Returns:
        Web link to the email
    """
    headers = {
        'Authorization': f'Bearer {access_token}',
        'Content-Type': 'application/json'
    }

    # Get message details (uses /me endpoint for current user)
    url = f"{GRAPH_API_ENDPOINT}/me/messages/{message_id}"
    response = requests.get(url, headers=headers)

    if response.status_code == 200:
        message = response.json()
        web_link = message.get('webLink')
        return web_link
    else:
        print(f"Error: {response.status_code}")
        print(response.text)
        return None


def list_recent_emails(access_token, top=10):
    """
    List recent emails to help find message IDs

    Args:
        access_token: Valid access token
        top: Number of emails to retrieve (default: 10)

    Returns:
        List of email messages with basic info
    """
    headers = {
        'Authorization': f'Bearer {access_token}',
        'Content-Type': 'application/json'
    }

    # Use /me endpoint for current authenticated user
    url = f"{GRAPH_API_ENDPOINT}/me/messages?$top={top}&$select=id,subject,from,receivedDateTime,webLink"
    response = requests.get(url, headers=headers)

    if response.status_code == 200:
        messages = response.json().get('value', [])
        return messages
    else:
        print(f"Error: {response.status_code}")
        print(response.text)
        return []


def get_current_user(access_token):
    """Get information about the currently authenticated user"""
    headers = {
        'Authorization': f'Bearer {access_token}',
        'Content-Type': 'application/json'
    }

    url = f"{GRAPH_API_ENDPOINT}/me"
    response = requests.get(url, headers=headers)

    if response.status_code == 200:
        user = response.json()
        return user.get('userPrincipalName') or user.get('mail')
    return None


def main():
    # Get access token
    print("Authenticating...")
    token = get_access_token()

    if not token:
        print("Failed to acquire access token")
        return

    print("\nAuthentication successful!\n")

    # Get current user info
    user_email = get_current_user(token)
    if user_email:
        print(f"Authenticated as: {user_email}\n")

    # List recent emails
    print(f"Fetching recent emails...\n")
    emails = list_recent_emails(token, top=5)

    if not emails:
        print("No emails found or error occurred")
        return

    print("Recent emails:")
    print("-" * 80)
    for i, email in enumerate(emails, 1):
        sender = email.get('from', {}).get('emailAddress', {}).get('address', 'Unknown')
        subject = email.get('subject', 'No Subject')
        date = email.get('receivedDateTime', 'Unknown')
        message_id = email.get('id')

        print(f"{i}. Subject: {subject}")
        print(f"   From: {sender}")
        print(f"   Date: {date}")
        print(f"   Message ID: {message_id}")
        print(f"   Web Link: {email.get('webLink')}")
        print()

    # Example: Get link for the first email
    if emails:
        first_email_id = emails[0]['id']
        print(f"\nRetrieving link for first email...")
        link = get_email_link(token, first_email_id)

        if link:
            print(f"\nEmail Link: {link}")
        else:
            print("Failed to retrieve email link")


if __name__ == "__main__":
    main()
