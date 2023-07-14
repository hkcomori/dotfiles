import json
from weakref import WeakValueDictionary


class MetaSingleton(type):
    __instances: WeakValueDictionary = WeakValueDictionary()

    def __call__(cls, *args, **kwargs):
        dict_key = (cls, args, json.dumps(kwargs))
        obj = cls.__instances.get(dict_key)
        if not obj:
            obj = super().__call__(*args, **kwargs)
            cls.__instances[dict_key] = obj
        return obj
