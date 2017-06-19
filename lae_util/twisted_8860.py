
def detect():
    try:
        from twisted.logger import _json
    except ImportError:
        return False

    try:
        flatten_event = _json.flattenEvent
    except AttributeError:
        return False

    try:
        flatten_event({"log_format": None})
    except AttributeError:
        return True

    return False



def patch():
    global _original_flatten_event

    import twisted.logger._json
    _original_flatten_event = twisted.logger._json.flattenEvent
    twisted.logger._json.flattenEvent = _safe_flatten_event


def _safe_flatten_event(event):
    if event.get("log_format", None) is None:
        return
    _original_flatten_event(event)
