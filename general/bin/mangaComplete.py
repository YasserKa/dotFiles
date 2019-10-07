import os
import requests
from bs4 import BeautifulSoup

manga_list = [
        'https://myanimelist.net/manga/90125/\
                Kaguya-sama_wa_Kokurasetai__Tensai-tachi_no_Renai_Zunousen',
        'https://myanimelist.net/manga/48399/\
                No_Game_No_Life',
        'https://myanimelist.net/manga/44489/\
                Houseki_no_Kuni',
        'https://myanimelist.net/manga/40171/\
                Yahari_Ore_no_Seishun_Love_Comedy_wa_Machigatteiru',
        'https://myanimelist.net/manga/91941/\
                Made_in_Abyss',
        'https://myanimelist.net/manga/74697/\
                Re_Zero_kara_Hajimeru_Isekai_Seikatsu',
]

finished_manga = []

for manga in manga_list:
    res = requests.get(manga)
    soup = BeautifulSoup(res.content, 'html.parser')
    spans = soup.select('.js-scrollfix-bottom > .spaceit:contains("Finished")')
    if len(spans) > 0:
        finished_manga.append(manga)

if len(finished_manga) > 0:
    os.system('zenity \
        --info --title finished manga \
        --text ' + ''.join(finished_manga))
    print(finished_manga)
