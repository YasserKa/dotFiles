#!/bin/python
import os
import subprocess
from selenium import webdriver
from selenium.webdriver.common.by import By
from time import sleep
from selenium.webdriver.support.ui import WebDriverWait
from selenium.common.exceptions import TimeoutException

URL_DISCORD = 'https://discordapp.com/channels/@me'
URL_WHATS_APP = 'https://web.whatsapp.com/'
URL_FACEBOOK = 'https://www.facebook.com/'

executable_path = './chromedriver'
profile_path = './myProfile'

discord = ' Discord notification'
facebook = ' Facebook notification'
reddit = ' Reddit notification'
whatsapp = 'Whatsapp notification'

options = webdriver.ChromeOptions()

options.add_argument('user-data-dir=' + profile_path)

os.system("pkill chromedriver")

driver = webdriver.Chrome(executable_path=executable_path, options=options)


def check_discord():

    driver.get(URL_DISCORD)
    WebDriverWait(driver, 10).until(
        lambda x: len(x.find_elements_by_css_selector('div[aria-label="Download Apps"]')) > 0)

    notificaitons_el = driver.find_elements(
        By.CSS_SELECTOR, 'div[class*=lowerBadge-]')
    notificaiton_exists = len(notificaitons_el) > 0

    if notificaiton_exists:
        subprocess.run(["dunstify", discord])


def check_facebook():
    driver.get(URL_FACEBOOK)
    WebDriverWait(driver, 2).until(
        lambda x: len(x.find_elements_by_xpath(
            './/div[contains(@aria-label, "unread")]')) > 0)

    notificaitons_el = driver.find_elements(
        By.XPATH, './/div[contains(@aria-label, "unread")]')
    notificaiton_exists = len(notificaitons_el) > 0

    if notificaiton_exists:
        subprocess.run(["dunstify", facebook])


def check_whatsapp():
    driver.get(URL_WHATS_APP)
    WebDriverWait(driver, 10).until(
        lambda x: len(x.find_elements_by_xpath(
            './/*[contains(text(),"Search or start new chat")]')) > 0)

    unread_messages = driver.find_elements(
        By.XPATH, './/*[contains(@aria-label,"unread message")]')
    unread_messages_exist = len(unread_messages) > 0

    if unread_messages_exist:
        subprocess.run(["dunstify", whatsapp])


def main():
    try:
        check_discord()
        check_whatsapp()
        check_facebook()
    except TimeoutException:
        pass

    driver.close()


if __name__ == '__main__':
    main()
