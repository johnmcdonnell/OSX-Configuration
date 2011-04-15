#!/Library/Frameworks/Python.framework/Versions/6.0.4/bin//python
import keyring; 


USERNAME = 'jmcdon10@gmail.com'
PASSWORD = keyring.get_password( 'gmail', USERNAME )
MAX_RESULTS = '9999'
CACHE_FILENAME = '~/.goobook_cache'
