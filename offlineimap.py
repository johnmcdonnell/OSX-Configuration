
# Credit to Tommi Virtanen

prioritized = [ 'flagged', 'priority', 'Notifications', 'INBOX', 'NYU' ]


def mycmp(x, y):
    for prefix in prioritized:
        xsw = x.startswith(prefix)
        ysw = y.startswith(prefix)
        if xsw and ysw:
            return cmp( x,y )
        elif xsw:
            return -1
        elif ysw:
            return +1
    return cmp( x, y )


def test_mycmp():
    import os
    folders = os.listdir( os.path.expanduser( "~/Mail/jmcdon10" ) )
    folders.sort( mycmp )
    print folders
