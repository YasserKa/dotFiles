#!/bin/python3
import configparser
import smtplib

config = configparser.ConfigParser()

config.read_file(open('./config'))
mail_pass = config.get('mail', 'password')
from_mail = config.get('mail', 'mail')
to_mail = config.get('mail', 'mail')


def send_mail():
    server = smtplib.SMTP('smtp.gmail.com', 587)
    server.starttls()
    server.login(from_mail, mail_pass)

    msg = """Subject: Updating reminder
    Update Arch, vim & emacs packages
        """

    server.sendmail(from_mail, to_mail, msg)
    server.quit()


if __name__ == '__main__':
    send_mail()
