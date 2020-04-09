#!/usr/bin/env python
# Adapted from here:
# Coded by: Hamidreza Moradi
# www.github.com/hamidrezamoradi 
# https://github.com/HamidrezaMoradi/APK-Downloader/blob/master/APK-Downloader.py

import requests
from bs4 import BeautifulSoup
import sys
import traceback


################# colors #################
BLUE = ''
GREEN = ''
YELLOW = ''
RED = ''

RED_BG = ''

CLOSE_COLOR = ''
##########################################

try:
    def details(GP_input):
        if 'https://' in GP_input or 'http://' in GP_input:
            package_id = BeautifulSoup(requests.get(GP_input).text, 'html.parser').find("meta", attrs={'name': 'appstore:bundle_id'})['content']
            return package_id 
        elif '.' in GP_input:
            return GP_input
        else:
            print(f'Invalid app id', file=sys.stderr)
            return None    

    class Services:
        @staticmethod
        def apkdl_in(package_id):
            apkdl = 'https://apkdl.in/app/details?id=%s' % package_id
            r = requests.get(apkdl)
            App_Page = BeautifulSoup(r.text, 'html.parser')
            downloadUrl = App_Page.find("a", itemprop='downloadUrl')

            downloadUrl = 'https://apkdl.in'+downloadUrl['href']
            r = requests.get(downloadUrl)
            DownloadPage = BeautifulSoup(r.text, 'html.parser')
            downloadUrl = DownloadPage.find("a", rel='nofollow')
            return downloadUrl

        @staticmethod
        def apkplz_net(package_id):
            url = 'https://apkplz.net/app/%s' % package_id
            r = requests.get(url)
            App_Page = BeautifulSoup(r.text, 'html.parser')
            downloadUrl = App_Page.find("div", attrs={'class':'col-sm-12 col-md-12 text-center'})

            downloadUrl = downloadUrl.find("a", rel='nofollow')['href']
            r = requests.get(downloadUrl)
            DownloadPage = BeautifulSoup(r.text, 'html.parser')
            downloadUrl = DownloadPage.find("a", string='click here')
            return downloadUrl['href']

        @staticmethod
        def apktada_com(package_id):
            url = 'https://apktada.com/download-apk/%s' % package_id
            r = requests.get(url)
            App_Page = BeautifulSoup(r.text, 'html.parser')
            downloadUrl = App_Page.find("a", string='click here')
            return downloadUrl['href']

        @staticmethod
        def m_apkpure_com(package_id):
            url = 'https://m.apkpure.com/android/%s/download?from=details' % package_id
            r = requests.get(url)
            App_Page = BeautifulSoup(r.text, 'html.parser')
            downloadUrl = App_Page.find("a", string='click here')
            return downloadUrl['href']

    if __name__ == "__main__":
        package_id = details(sys.argv[1])
        print(package_id)
        try:
            print(Services.m_apkpure_com(package_id))
        except:
            print(traceback.format_exc(), file=sys.stderr)
            pass
        try:
            print(Services.apkdl_in(package_id))
        except:
            print(traceback.format_exc(), file=sys.stderr)
            pass
        try:
            print(Services.apkplz_net(package_id))
        except:
            print(traceback.format_exc(), file=sys.stderr)
            pass
        try:
            print(Services.apktada_com(package_id))
        except:
            print(traceback.format_exc(), file=sys.stderr)
            pass

except requests.exceptions.ConnectionError:
    print(traceback.format_exc(), file=sys.stderr)
    # print(f'\n{RED} [!] No Connection.{CLOSE_COLOR}')
except TypeError:
    print(traceback.format_exc(), file=sys.stderr)
    # print(f'\n{RED} [!] App/Game not found.\n [!] Try again later.{CLOSE_COLOR}')
except:
    print(traceback.format_exc(), file=sys.stderr)
    # print(f'\n{RED} [!] There\'s a problem.\n [!] Try another website.{CLOSE_COLOR}')
