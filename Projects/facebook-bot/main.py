#!/bin/python
import os
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.common.exceptions import TimeoutException
from datetime import date
from time import sleep
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.keys import Keys
from random import choice

# https://www.facebook.com/messages/t/100057748813194
# https://www.facebook.com/messages/t/3295737910517634
URL_FACEBOOK_CHAT = 'https://www.facebook.com/messages/t/3295737910517634'

executable_path = './chromedriver'
profile_path = './bot'

CLEANING_TEAMS = [
    ['Yasser', '김가연'],
    ['Elema', 'Anton'],
    ['David', 'Lise'],
    ['Dennis', 'Josh'],
]

options = webdriver.ChromeOptions()

options.add_argument('user-data-dir=' + profile_path)

os.system("pkill chromedriver")

driver = webdriver.Chrome(executable_path=executable_path, options=options)


def use_random_emoji():
    driver.find_element(
        By.XPATH, ".//*[contains(@aria-label, 'Välj en emoji')]").click()

    # ::div/ancestor::img[contains(@src, "emoji")]
    element_present = EC.presence_of_element_located(
        (By.XPATH, './/*[contains(text(), "Smileys och personer")]'))
    WebDriverWait(driver, 10).until(element_present)
    emoji_list = driver.find_elements(
        By.XPATH, './/*[contains(text(), "Smileys och personer")]/'
        'following-sibling::div/descendant::img[contains(@src, "emoji")]')

    choice(emoji_list).click()


def get_message():

    original_date = date(2021, 1, 18)
    today = date.today()
    days_difference = today - original_date
    number_of_weeks = int(days_difference.days / 7)
    on_duty = CLEANING_TEAMS[number_of_weeks % len(CLEANING_TEAMS)]

    on_duty_string = ', '.join([x for x in on_duty[:-1]])
    on_duty_string += f" and {on_duty[-1]}"

    message = f"The Masters of Cleaning (MoC) this week are {on_duty_string}. "
    return message


def check_facebook():

    driver.get(URL_FACEBOOK_CHAT)

    element_present = EC.presence_of_element_located(
        (By.XPATH, './/div[contains(@style, "white-space: pre-wrap;")]'))
    WebDriverWait(driver, 10).until(element_present)

    # iframe = driver.find_element(By.XPATH, "//iframe")
    # driver.switch_to.frame(iframe)

    message = get_message()

    text_area = driver.find_elements(
        By.XPATH, './/div[contains(@style, "white-space: pre-wrap;")]')[-1]
    text_area.send_keys(message)

    use_random_emoji()
    text_area.send_keys(Keys.RETURN)
    sleep(1)


def main():
    try:
        check_facebook()
    except TimeoutException:
        pass
    driver.close()


if __name__ == '__main__':
    main()
