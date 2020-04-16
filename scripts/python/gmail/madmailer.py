#!/usr/bin/env python3
#  --noauth_local_webserver
try:
    from ipydex import IPS, ip_syshook, ST, activate_ips_on_exception, dirsearch
#    activate_ips_on_exception()

    from brish import z, zq, zs
    z('cdm ~/tmp/delme/')

    import datetime
    now = datetime.datetime.now()
    cutoff_date = (now - datetime.timedelta(days=7)).strftime("%Y/%m/%d")

    import os.path
    import traceback
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
    from bs4 import BeautifulSoup, SoupStrainer
    import requests
    from urllib.parse import urlparse

    import importlib.util

    spec = importlib.util.spec_from_file_location(
        "tsend", local.env["NIGHTDIR"] + "/python/telegram-send/tsend.py"
    )
    ts = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(ts)
    from syncer import sync
    tsend = sync(ts.tsend)

    # zsh = local["zsh"]["-c"]
    lblProcessed = "Label_7772537585229918833"
    lblTest = "Label_4305264623189976109"


    def ecerr(str):
        print(f"{now}           {str}", file=sys.stderr)


    tokenpath = local.env["HOME"] + "/.gmail.token"
    credpath = local.env["HOME"] + "/.gmail.credentials.json"
    import ezgmail as g

    g.init(tokenFile=tokenpath, credentialsFile=credpath)
    service = g.SERVICE_GMAIL


    def printLabels():
        results = service.users().labels().list(userId="me").execute()
        labels = results.get("labels", [])

        if not labels:
            print("No labels found.")
        else:
            print("Labels:")
            for label in labels:
                print(label["name"] + " " + label["id"])


    def parseContentTypeHeaderForEncoding(value):
        """Helper function called by GmailMessage:__init__()."""
        mo = re.search('charset="(.*?)"', value)
        if mo is None:
            emailEncoding = "UTF-8"  # We're going to assume UTF-8 and hope for the best. Safety not guaranteed.
        else:
            emailEncoding = mo.group(1)
        return emailEncoding


    def getHtml(messageObj):
        emailEncoding = "UTF-8"  # We're going to assume UTF-8 and hope for the best. Safety not guaranteed.
        r = ""

        def frompart(part):
            if part["mimeType"].upper() == "TEXT/HTML" and "data" in part["body"]:
                for header in part["headers"]:
                    if header["name"].upper() == "CONTENT-TYPE":
                        emailEncoding = parseContentTypeHeaderForEncoding(header["value"])
                return base64.urlsafe_b64decode(part["body"]["data"]).decode(emailEncoding)

        if "parts" in messageObj["payload"].keys():
            for part in messageObj["payload"]["parts"]:
                r = frompart(part)
                if r:
                    return r
        elif "body" in messageObj["payload"].keys():
            r = frompart(messageObj["payload"])
        return r

    def labelprocessed(msg):
        # msg.addLabel(lblTest)
        msg.addLabel(lblProcessed)

    fics = g.search(
        f"after:{cutoff_date} AND ((from:fanfiction) AND NOT label:auto/processed)", maxResults=200
    )

    ficnetPattern = re.compile(r".*(https://www.fanfiction.net/s/\S*)", flags=re.DOTALL)
    for t in reversed(fics):
        for m in t.messages:
            match = re.match(ficnetPattern, m.originalBody)
            if not match:
                ecerr(f"Could not find link in\n{m.originalBody}")
                labelprocessed(m)
                continue
            link = match.group(1)
            print(f"Processing {link} ...")
            kRes = z('tl -p "fic | " {link}')
            if not kRes:
                kRes.print(file=sys.stderr)
                continue
            labelprocessed(m)

    news = g.search(
        f"after:{cutoff_date} AND ((from:tldrnewsletter.com) AND NOT label:auto/processed)", maxResults=200
    )



    for t in reversed(news):
        # actually these are threads not messages
        for m in t.messages:
            print(f"Processing '{m.subject}' ...")
            bodyhtml = getHtml(m.messageObj)
            if not bodyhtml:
                ecerr(f"Couldn't extract html body of message {m.subject}")
                ecerr("Printing its messageObj:")
                ecerr(m.messageObj)
                ecerr("")
                ecerr("")
                labelprocessed(m)
                continue
            s = BeautifulSoup(bodyhtml, features="lxml")
            for link in s.select('a'):
                link['href'] = z("urlfinalg {link['href']}").out
            bodyhtml = s.prettify() #( formatter="html" )
            body = tempfile.NamedTemporaryFile(mode="w", suffix=".html")
            print(bodyhtml, file=body, flush=True)
            bodytxt_file = tempfile.NamedTemporaryFile(mode="w", suffix=".txt")
            print(m.originalBody, file=bodytxt_file, flush=True)
            
            z('cp {body.name} {bodytxt_file.name} ./') # DEBUG also set pkDel

            # The html uses a table which doesn't reflow. Calibre hangs on the input, so at least use pandoc.
            # kRes = z('''pkDel=y dpan h2e {f'TLDR | {m.subject}'} {body.name}''')
            kRes = z('''pkDel='' t2e {f'TLDR | {m.subject}'} {bodytxt_file.name}''')
            e, out, err = kRes.summary
            cmd = kRes.cmd

            # wread doesn't work well for TLDR.
            # wres = z('''h2e {f'wread TLDR | {m.subject}'}  =(wread --file {body.name} {"https://www.tldrnewsletter.com/"} html | tee ~/tmp/a.html) ''')
            # print(repr(wres))
            if e == 0:
                labelprocessed(m)
                print(f"Sent '{m.subject}' to kindle ...")
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
                continue

            if os.environ.get('mmNoTlg', '') == '':
                items = s.find_all("div", {"class": "text-block"})
                try:
                    tsend_cmd = ["--parse-mode", "html", "--link-preview", "https://t.me/tldrnewsletter", '']
                    tsend_args = ts.parse_tsend(tsend_cmd)
                    for n in items[1:2] + items[4:-2]:
                        # for link in n.select('a'):
                            # link['href'] = z("urlfinalg {link['href']}").out
                        tsend_args['<message>'] = n
                        tsend(tsend_args)
                except:
                    ecerr("")
                    ecerr("Failed to send this item to Telegram:")
                    ecerr("")
                    ecerr(traceback.format_exc())
                    ecerr("")
            else:
                print(f"Telegram disabled for '{m.subject}'")
            print(f"Processed '{m.subject}'!")

            body.close()  # This file is deleted immediately upon closing it.
    # embed()
except KeyboardInterrupt:
    pass
