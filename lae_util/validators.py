"""
attrs validators.
"""

def all(*v):
    def _and(inst, attr, value):
        for validator in v:
            validator(inst, attr, value)
    return _and


def after(processor, validator):
    def after_validator(inst, attr, value):
        processed = processor(inst, attr, value)
        return validator(inst, attr, processed)
    return after_validator
