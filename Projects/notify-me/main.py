#!/bin/python
import subprocess
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.common.exceptions import TimeoutException

URL = 'https://discordapp.com/channels/@me'

delay = 5
executable_path = './chromedriver'
profile_path = './myProfile'

discord = ' Discord notification'
reddit = ' Reddit notification'

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


def main():
    try:
        driver.get(URL)
        WebDriverWait(driver, 10).until(lambda x: check_discord())
    except TimeoutException:
        pass

    driver.close()


if __name__ == '__main__':
    main()
