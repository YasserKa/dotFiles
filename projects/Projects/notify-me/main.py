#!/bin/python
import os
import subprocess
from selenium import webdriver
from time import sleep
from selenium.webdriver.common.by import By
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
        lambda x: len(x.find_elements_by_xpath('.//*[contains(text(), "Friends")]')) > 0)
    sleep(1)

    notificaitons_el = driver.find_elements(
        By.CSS_SELECTOR, 'div[class*=lowerBadge-]')

    discord_servers = driver.find_elements(
        By.XPATH, './/div[contains(@aria-label, "Servers")]/*/div/span[contains(@class, "item")]')

    notificaiton_exists = len(notificaitons_el) > 0 or len(discord_servers) > 0

    if notificaiton_exists:
        subprocess.run(["dunstify", discord, '--timeout=999999'])


def check_facebook():
    driver.get(URL_FACEBOOK)
    WebDriverWait(driver, 5).until(
        lambda x: len(x.find_elements_by_xpath(
            './/div[contains(@aria-label, "Messenger")]')) > 0)

    notificaitons_el = driver.find_elements(
        By.XPATH, './/div[contains(@aria-label, "Messenger")]//child::span[1][(text()=number())]')

    notificaiton_exists = len(notificaitons_el) > 0

    if notificaiton_exists:
        subprocess.run(["dunstify", facebook, '--timeout=999999'])


def check_whatsapp():
    driver.get(URL_WHATS_APP)
    WebDriverWait(driver, 20).until(
        lambda x: len(x.find_elements_by_xpath(
            './/*[contains(text(),"Search or start new chat")]')) > 0)

    unread_messages = driver.find_elements(
        By.XPATH, './/*[contains(@aria-label,"unread message")]')
    unread_messages_exist = len(unread_messages) > 0

    if unread_messages_exist:
        subprocess.run(["dunstify", whatsapp, '--timeout=999999'])


def main():
    try:
        check_discord()
        check_facebook()
        check_whatsapp()
    except TimeoutException:
        pass

    driver.close()
    os.system("pkill chromedriver")


if __name__ == '__main__':
    main()
