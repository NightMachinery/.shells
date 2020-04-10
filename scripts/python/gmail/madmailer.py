#!/usr/bin/env python3
import os.path
from IPython import embed
from plumbum import local
import tempfile
import base64
from email.mime.audio import MIMEAudio
from email.mime.base import MIMEBase
from email.mime.image import MIMEImage
from email.mime.multipart import MIMEMultipart
from email.mime.text import MIMEText
import mimetypes
import os
import datetime
import re
import copy
import sys

zsh = local['zsh']['-c']
lblProcessed = 'Label_7772537585229918833'
lblTest = 'Label_4305264623189976109'

def ecerr(str):
    print(str, file=sys.stderr)

tokenpath = local.env['HOME'] + '/.gmail.token'
credpath = local.env['HOME'] + '/.gmail.credentials.json'
import ezgmail as g
g.init(tokenFile=tokenpath, credentialsFile=credpath)
service=g.SERVICE_GMAIL

def printLabels():
    results = service.users().labels().list(userId='me').execute()
    labels = results.get('labels', [])

    if not labels:
        print('No labels found.')
    else:
        print('Labels:')
        for label in labels:
            print(label['name'] + " "+label['id'])

def parseContentTypeHeaderForEncoding(value):
    """Helper function called by GmailMessage:__init__()."""
    mo = re.search('charset="(.*?)"', value)
    if mo is None:
        emailEncoding = 'UTF-8' # We're going to assume UTF-8 and hope for the best. Safety not guaranteed.
    else:
        emailEncoding = mo.group(1)
    return emailEncoding

def getHtml(messageObj):
    emailEncoding = 'UTF-8' # We're going to assume UTF-8 and hope for the best. Safety not guaranteed.
    r = ''
    def frompart(part):
        if part['mimeType'].upper() == 'TEXT/HTML' and 'data' in part['body']:
            for header in part['headers']:
                if header['name'].upper() == 'CONTENT-TYPE':
                    emailEncoding = parseContentTypeHeaderForEncoding(header['value'])
            return base64.urlsafe_b64decode(part['body']['data']).decode(emailEncoding)
            
    if 'parts' in messageObj['payload'].keys():
        for part in messageObj['payload']['parts']:
            r = frompart(part)
            if r:
                return r
    elif 'body' in messageObj['payload'].keys():
        r = frompart(messageObj['payload'])
    return r
    

# unprocessed
news = g.search('((from:tldrnewsletter.com) AND NOT label:auto/processed)', maxResults=20)
def labelnews(msg):
    # msg.addLabel(lblTest)
    msg.addLabel(lblProcessed)

for t in news:
    # actually these are threads not messages
    for m in t.messages:
        bodyhtml = getHtml(m.messageObj)
        if not bodyhtml:
            ecerr(f"Couldn't extract html body of message {m.subject}")
            ecerr('Printing its messageObj:')
            ecerr(m.messageObj)
            ecerr('')
            ecerr('')
            labelnews(m)
            continue
        body = tempfile.NamedTemporaryFile(mode = 'w', suffix='.html')
        print(bodyhtml, file=body, flush=True)
        cmd = f"dpan h2e 'TLDR | {m.subject}' {body.name}"
        e, out, err = zsh[f"arger {cmd}"].run()
        # embed()
        if e == 0:
            labelnews(m)
        else:
            ecerr("")
            ecerr(f"h2e exited nonzero; cmd: {cmd}")
            ecerr("Out:")
            ecerr(out)
            ecerr("")
            ecerr("Err:")
            ecerr(err)
            ecerr("")
            ecerr("End of current report")
        body.close() # This file is deleted immediately upon closing it.
# embed()
