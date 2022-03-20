#!/bin/env python

import os
import subprocess
from selenium.webdriver.common.by import By
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium import webdriver
from time import sleep
from webdriver_manager.chrome import ChromeDriverManager

options = webdriver.ChromeOptions()

options.add_argument("--headless")

os.system("pkill chromedriver")

driver = webdriver.Chrome(service=Service(ChromeDriverManager().install()), options=options)

URL = "https://audionline.bankaudi.com.lb/CustomLogin/RetailLogin.aspx?sID=-1"

def fill_sign_in():
    user_name = subprocess.getoutput('bw get username audi')
    password = subprocess.getoutput('bw get password audi')

    driver.find_element(
        By.ID, "txtName").send_keys(user_name)
    driver.find_element(
        By.ID, "txtStep1Pass_fake").send_keys(password)
    driver.find_element(
        By.CLASS_NAME, "loginButton").click()

def get_amount():
    element_present = EC.presence_of_element_located(
        (By.ID, 'menuId_23'))
    WebDriverWait(driver, 10).until(element_present)
    driver.find_element(
        By.ID, "menuId_23").click()
    driver.find_element(
        By.ID, "menuId_1068").click()

    iframe = driver.find_element(By.ID, "frame_content")
    driver.switch_to.frame(iframe)

    amount = driver.find_elements(By.XPATH, ".//*[contains(text(), 'USD')]")[3].text[:-4]
    return amount

def notify(amount):
    subprocess.run(f"dunstify {amount}", shell=True)

def main():
    driver.get(URL)
    fill_sign_in()
    amount = get_amount()
    notify(amount)
    driver.close()

if __name__ == '__main__':
    main()
