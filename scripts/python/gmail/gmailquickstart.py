#!/usr/bin/env python3
from __future__ import print_function
import pickle
import os.path
from googleapiclient.discovery import build
from google_auth_oauthlib.flow import InstalledAppFlow
from google.auth.transport.requests import Request
from plumbum import local
from IPython import embed
from oauth2client import file, client, tools
from httplib2 import Http

# If modifying these scopes, delete the file token.pickle.
SCOPES = ['https://www.googleapis.com/auth/gmail.readonly']

def init(userId='me', tokenFile='token.json', credentialsFile='credentials.json', _raiseException=True):
    """This function must be called before any other function in EZGmail (and is automatically called by them anyway, so you don't have to explicitly call this yourself).

    This function populates the SERVICE_GMAIL global variable used in all Gmail API cals. It also populates EMAIL_ADDRESS with a string of the Gmail accont's email address. This
    account is determined by the credentials.json file, downloaded from Google, and token.json. If the tokenFile file hasn't been generated yet, this function will open
    the browser to a page to let the user log in to the Gmail account that this module will use.

    If you want to switch to a different Gmail account, call this function again with a different `tokenFile` and `credentialsFile` arguments.
    """
    global SERVICE_GMAIL, EMAIL_ADDRESS, LOGGED_IN

    EMAIL_ADDRESS = False # Set this to False, in case module was initialized before but this current initialization fails.
    LOGGED_IN = False

    try:
        if not os.path.exists(credentialsFile):
            raise EZGmailException('Can\'t find credentials file at %s. You can download this file from https://developers.google.com/gmail/api/quickstart/python and clicking "Enable the Gmail API". Rename the downloaded file to credentials.json.' % (os.path.abspath(credentialsFile)))

        store = file.Storage(tokenFile)
        creds = store.get()
        if not creds or creds.invalid:
            flow = client.flow_from_clientsecrets(credentialsFile, SCOPES)
            creds = tools.run_flow(flow, store)
        SERVICE_GMAIL = build('gmail', 'v1', http=creds.authorize(Http()))
        EMAIL_ADDRESS = SERVICE_GMAIL.users().getProfile(userId=userId).execute()['emailAddress']
        LOGGED_IN = bool(EMAIL_ADDRESS)

        return EMAIL_ADDRESS
    except:
        if _raiseException:
            raise
        else:
            return False


def main():
    """Shows basic usage of the Gmail API.
    Lists the user's Gmail labels.
    """
    tokenpath = local.env['HOME'] + '/.gmail.token'
    credpath = local.env['HOME'] + '/.gmail.credentials.json'
    
    # creds = None
    # # The file token.pickle stores the user's access and refresh tokens, and is
    # # created automatically when the authorization flow completes for the first
    # # time.
    # if os.path.exists(tokenpath):
    #     with open(tokenpath, 'rb') as token:
    #         # creds = pickle.load(token)
    # # If there are no (valid) credentials available, let the user log in.
    # if not creds or not creds.valid:
    #     if creds and creds.expired and creds.refresh_token:
    #         creds.refresh(Request())
    #     else:
    #         flow = InstalledAppFlow.from_client_secrets_file(
    #             local.env['HOME'] + '/.gmail.credentials.json', SCOPES)
    #         creds = flow.run_local_server(port=0)
    #     # Save the credentials for the next run
    #     with open(tokenpath, 'wb') as token:
    #         pickle.dump(creds, token)

    # service = build('gmail', 'v1', credentials=creds)
    init(tokenFile=tokenpath, credentialsFile=credpath)
    service=SERVICE_GMAIL

    # Call the Gmail API
    results = service.users().labels().list(userId='me').execute()
    labels = results.get('labels', [])

    if not labels:
        print('No labels found.')
    else:
        print('Labels:')
        for label in labels:
            print(label['name'] + " "+label['id'])
    embed()

if __name__ == '__main__':
    main()
