
def detect():
    try:
        from twisted.python import _json
    except ImportError:
        return False

    try:
        flatten_event = _json.flattenEvent
    except AttributeError:
        return False

    try:
        flatten_event({"log_format": None})
    except AttributeError:
        return False

    return True



def patch():
    global _original_flatten_event

    import twisted.python._json
    _original_flatten_event = twisted.python._json.flattenEvent
    twisted.python._json.flattenEvent = _safe_flatten_event


def _safe_flatten_event(event):
    if event.get("log_format", None) is None:
        return
    _original_flatten_event(event)
