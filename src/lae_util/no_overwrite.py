

def setitem_without_overwrite(d, key, value):
    assert not d.has_key(key), `key, value, d[key]`
    d[key] = value


def update_without_overwrite(target, source):
    for k, v in source.items():
        setitem_without_overwrite(target, k, v)


def update_by_keywords_without_overwrite(target, **source):
    update_without_overwrite(target, source)


