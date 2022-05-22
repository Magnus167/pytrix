from selenium import webdriver
from selenium.webdriver.chrome.options import Options

packed_extension_path = '/path/to/packed/extension.crx'

options = Options()
options.add_extension(packed_extension_path)
driver = webdriver.Chrome(options=options)
