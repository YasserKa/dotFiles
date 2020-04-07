#!/bin/python
import subprocess
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.common.exceptions import TimeoutException

URL_DISCORD = 'https://discordapp.com/channels/@me'
URL_SUBMISSION = 'https://developer.riotgames.com/app/427810/info'

executable_path = './chromedriver'
profile_path = './myProfile'

discord = ' Discord notification'
reddit = ' Reddit notification'
submission = 'Riot Submission'

options = webdriver.ChromeOptions()

options.add_argument('user-data-dir=' + profile_path)
driver = webdriver.Chrome(executable_path=executable_path, options=options)


def check_discord():

    notificaitons_el = driver.find_elements(
        By.CSS_SELECTOR, 'div[class*=lowerBadge-]')
    notificaiton_exists = len(notificaitons_el) > 0

    if notificaiton_exists:
        subprocess.run(["dunstify", discord])

    return notificaiton_exists


def check_submission():

    pending_el = driver.find_elements(
        By.XPATH, './/span[contains(text(), "Pending")]')
    pending_exists = len(pending_el) > 0

    if pending_exists == 0:
        subprocess.run(["dunstify", submission])

    return pending_exists


def main():
    try:
        driver.get(URL_DISCORD)
        WebDriverWait(driver, 10).until(lambda x: check_discord())
    except TimeoutException:
        pass
    try:
        driver.get(URL_SUBMISSION)
        WebDriverWait(driver, 10).until(lambda x: check_submission())
    except TimeoutException:
        pass

    driver.close()


if __name__ == '__main__':
    main()
