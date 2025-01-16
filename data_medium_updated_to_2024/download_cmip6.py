import requests
from bs4 import BeautifulSoup
import os

def download_files(url, dest_folder):
    if not os.path.exists(dest_folder):
        os.makedirs(dest_folder)
    
    response = requests.get(url)
    soup = BeautifulSoup(response.text, 'html.parser')
    
    for link in soup.find_all('a'):
        href = link.get('href')
        if href and href.endswith('.dat'):
            full_path = os.path.join(url, href)
            print(full_path)
            response = requests.get(full_path)
            filename = os.path.join(dest_folder, href.split('/')[-1])
            with open(filename, 'wb') as f:
                f.write(response.content)
            print(f'Downloaded {filename}')


download_files('https://climexp.knmi.nl/CMIP6/Tglobal', '/Users/jiangjitong/Desktop/CMIP6')