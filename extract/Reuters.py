import os, re, time
import csv
import pandas as pd

from bs4 import BeautifulSoup
import logging
import scrapy
from scrapy.spiders import CrawlSpider
from datetime import datetime

logging.basicConfig(filename='log_50.log', level=logging.ERROR)

class SpiderReuters (CrawlSpider):
    name = 'crawler_final_v21_tags_2'
    handle_httpstatus_list = [404]

    custom_settings = {
        'AUTOTHROTTLE_ENABLED': False,
        'CONCURRENT_REQUESTS_PER_DOMAIN': '4',
        'DOWNLOAD_DELAY': '0.2',
        'USER_AGENT': "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:38.0) Gecko/20100101 Firefox/38.0"
    }

    def start_requests(self):
        f = open(r"/home/PATH-to-file/list_of_URLs.txt", 'r')
        start_urls = [url.strip() for url in f.readlines()]
        f.close()
        for urls in start_urls:
            yield scrapy.Request(url=urls, callback=self.parse)

    def parse(self, response):
        if response.status == 404:
            with open('log_404.txt', 'a') as l:
                l.write(str(response) + '\n')
        else:
            soup = BeautifulSoup(response.body, 'lxml')
            extractor_a = soup.find('link', rel='canonical')
            a_return = extractor_a['href']
            extractor_alt = soup.find('link', rel='alternate')
            alt_return = extractor_alt['href']
            extractor_tags = soup.find('meta', property="og:article:tag")
            tags_return = extractor_tags['content']
            extractor_1 = soup.find_all('span', class_='timestamp')
            extractor_2 = soup.find_all('h1', class_='article-headline')
            extractor = soup.find_all('span', id='article-text')
            composer = [extractor_1, a_return, alt_return, extractor_2, extractor, tags_return]
            composer_2 = []
            for element in composer:
                element_2 = ' '.join(str(element).strip('[]').splitlines())
                composer_2.append(element_2)
            composer_text = "|".join([str(item).replace("|", "") for item in composer_2])
            clean_page = BeautifulSoup(composer_text, 'lxml')
            all_text = ' '.join(clean_page.findAll(text=True))
            all_text_uni = all_text.decode('unicode_escape').encode('utf-8', 'strict')
            date = all_text_uni[4:17].replace(',', '').strip(' ')
            date_object = datetime.strptime(date, '%b %d %Y')
            page = str(response).replace("/", "").replace(".", "").replace(":", "").replace("<", "").replace(">", "")
            with open(r'/home/PATH/output_%s.csv' % page, 'a') as a:
                a.write(date_object.strftime('%b %d %Y') + '|')
                a.write(' '.join(all_text_uni.splitlines()).replace("  ", " "))

