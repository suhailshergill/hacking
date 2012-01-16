import mechanize
import gzip
import StringIO

# [[http://stackoverflow.com/a/2982][HOWTO]]

def mechanize_Browser_open(self, *args, **kw):
    """monkey patching the browser.open method (which is now stored under
    browser._open). This function monkey patches the read method for the request
    object which always gives uncompressed output.
    
    Adds a _compress object to request object which holds original data.
    """
    # This is monkeypatching was needed coz some sites seem to ignore the
    # request header which specifies what content encoding is acceptable to
    # client. [[http://torrentfreak.com/bittorrents-future-dht-pex-and-magnet-links-explained-091120/][example]]
    req = self._open(*args, **kw)
    if req._headers.getheader('Content-Encoding') == 'gzip':
        # HACK: need to use StringIO because otherwise zlib complains with
        # "Error -3 while decompressing: incorrect header check". Using gzip's
        # interface instead of zlib works, but gzip wants a filehandle
        # [[http://stackoverflow.com/a/4219350][source]]
        body = StringIO.StringIO(req.read())
        req._gzipFile = gzip.GzipFile(fileobj=body,mode='rb')
        for attr in ['next','read','seek','tell']:
            req.__setattr__(attr,req._gzipFile.__getattribute__(attr))
        # make original content available through _read
        req._compressed = body
        req._compressed.seek(0)
        req.ISMONKEYPATCHED = True
    return req
mechanize.Browser._open = mechanize.Browser.open
mechanize.Browser.open = mechanize_Browser_open
    
