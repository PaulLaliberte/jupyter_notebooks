#!/apps/anaconda-2.3.0/bin/python2.7

from repec_crawl import *

def grab_links():

    browsers = {'User-Agent': 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.11 (KHTML, like Gecko) Chrome/23.0.1271.64 Safari/537.11',
       'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8',
       'Accept-Charset': 'ISO-8859-1,utf-8;q=0.7,*;q=0.3',
       'Accept-Encoding': 'none',
       'Accept-Language': 'en-US,en;q=0.8',
       'Connection': 'keep-alive'}

    #scrape all journals
    req = urllib2.Request('https://ideas.repec.org/i/eall.html', headers=browsers)
    res = urllib2.urlopen(req)

    soup = bs.BeautifulSoup(res, 'lxml')
    find_all_links = soup.find_all('a', href=True)

    #AUTHOR LINK INDEX: 75 - 50487
    return [i.get('href') for i in find_all_links[75:50487]]
    #return [i.get('href') for i in find_all_links[75:100]]


def directory(links):
    new_article = repec_crawler(links)
    new_article.processing()


if __name__ == '__main__':
    links = grab_links()
    directory(links)

