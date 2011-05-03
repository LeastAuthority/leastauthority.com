import time


# time utilities:
ISO_TIME_FMT = '%Y-%m-%dT%H:%M:%S%z'

def now():
    return time.strftime(ISO_TIME_FMT)

def format_iso_time(t):
    return time.strftime(ISO_TIME_FMT, t)


# dict utilities:
def setitem_without_overwrite(d, key, value):
    assert not d.has_key(key), `key, value, d[key]`
    d[key] = value


def update_without_overwrite(target, source):
    for k, v in source.items():
        setitem_without_overwrite(target, k, v)


def update_by_keywords_without_overwrite(target, **source):
    update_without_overwrite(target, source)


# XML utilities:
def etree_to_python(elem):
    if len(elem) == 0:
        return elem.text.strip()
    else:
        d = {}
        for child in elem:
            tag = child.tag
            prev = d.get(tag)
            if prev is None:
                d[tag] = etree_to_python(child)
            elif type(prev) is list:
                prev.append(etree_to_python(child))
            else:
                d[tag] = [prev, child]
        return d
