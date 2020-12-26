#!/usr/bin/env python3

import cloudscraper
import sys

scraper = cloudscraper.create_scraper(
browser={
        'browser': 'chrome',
        'platform': 'windows',
        'mobile': False
    }
        )  # returns a CloudScraper instance
print(scraper.get(sys.argv[1]).text) 
