#!/usr/bin/env python

import smtplib, ssl

smtp_server = "mail2.lilf.ir"
port = 587  # For starttls
sender_email = "discourse@mail2.lilf.ir"
password = input("Type your password and press enter: ")

receiver_email = "rudiwillalwaysloveyou@gmail.com"
message = f"""\
From: {sender_email}
Subject: Hi there

This message is sent from Python."""

# Create a secure SSL context
context = ssl.create_default_context()

# Try to log in to server and send email
try:
    server = smtplib.SMTP(smtp_server,port)
    server.ehlo() # Can be omitted
    # server.starttls(context=context) # Secure the connection
    server.ehlo() # Can be omitted
    server.login(sender_email, password)

    server.sendmail(sender_email, receiver_email, message)
except Exception as e:
    # Print any error messages to stdout
    print(e)
finally:
    server.quit()
