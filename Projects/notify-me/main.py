#!/bin/python
import os
import subprocess
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.common.exceptions import TimeoutException

URL_DISCORD = 'https://discordapp.com/channels/@me'
URL_WHATS_APP = 'https://web.whatsapp.com/'

executable_path = './chromedriver'
profile_path = './myProfile'

discord = ' Discord notification'
reddit = ' Reddit notification'
whatsapp = 'Whatsapp notification'

options = webdriver.ChromeOptions()

options.add_argument('user-data-dir=' + profile_path)

os.system("pkill chromedrive")
driver = webdriver.Chrome(executable_path=executable_path, options=options)


def check_discord():

    notificaitons_el = driver.find_elements(
        By.CSS_SELECTOR, 'div[class*=lowerBadge-]')
    notificaiton_exists = len(notificaitons_el) > 0

    if notificaiton_exists:
        subprocess.run(["dunstify", discord])

    return notificaiton_exists


def check_whatsapp():
    unread_messages = driver.find_elements(
        By.CSS_SELECTOR, 'span[aria-label*="unread message"]')
    unread_messages_exist = len(unread_messages) > 0

    if unread_messages_exist:
        subprocess.run(["dunstify", whatsapp])

    return unread_messages_exist


def main():
    try:
        # driver.get(URL_DISCORD)
        # WebDriverWait(driver, 10).until(lambda x: check_discord())
        driver.get(URL_WHATS_APP)
        WebDriverWait(driver, 10).until(lambda x: check_whatsapp())
    except TimeoutException:
        pass

    # driver.close()


if __name__ == '__main__':
    main()
