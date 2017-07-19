import urllib2
import re

import bs4 as bs
import pandas as pd
import numpy as np


class repec_crawler:
    def __init__(self, url_parts):
        self.browsers = {'User-Agent': 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.11 (KHTML, like Gecko) Chrome/23.0.1271.64 Safari/537.11',
       'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8',
       'Accept-Charset': 'ISO-8859-1,utf-8;q=0.7,*;q=0.3',
       'Accept-Encoding': 'none',
       'Accept-Language': 'en-US,en;q=0.8',
       'Connection': 'keep-alive'}
        
        self.df = None
        self.url_parts = url_parts
        self.names = []
        self.affils = []
        self.links_not_scraped = []

    def processing(self):
        for part in self.url_parts:
            try:
                req = urllib2.Request('https://ideas.repec.org' + part, headers=self.browsers)
                res = urllib2.urlopen(req)
                soup = bs.BeautifulSoup(res, 'lxml')

                self.find_name(soup)
                self.find_affil(soup)

            except Exception as e:
                self.links_not_scraped.append(('https://ideas.repec.org' + part, e))
                continue

        self.df = pd.DataFrame(self.names, columns=['Name'])
        self.df['Affiliations'] = self.affils

        self.df.to_csv('//apptemp/scratch/pl2669/authors/author_affiliations.csv', index=False, encoding='utf-8-sig')
        self.df.to_pickle('//apptemp/scratch/pl2669/authors/author_affiliations.pkl')

    def find_name(self, webpage):
        name = webpage.find('title')
        
        if name is None:
            self.names.append(np.nan)
        else:
            self.names.append(name.text.split('|')[0].lower().strip(' '))

    def find_affil(self, webpage):
        for br in webpage.find_all('br'):
            br.replace_with(' ')

        affils = webpage.find_all('a', attrs={'name' : 'subaffil'})

        if len(affils) != 0:
            self.affils.append([i.text.lower().strip(' ') for i in affils])
        else:
            self.affils.append(np.nan)
