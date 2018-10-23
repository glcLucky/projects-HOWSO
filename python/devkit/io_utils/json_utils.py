# -*- coding: utf-8 -*-

"""
json_utils.py

与json相关的IO函数库

@author: Wu Yudi
@email: jasper.wuyd@gmail.com
@date: 2017.04.24

---------------

FUNCTION LIST:
- json2dict(target_file, encoding='utf-8', validate=False)
- dict2json(target_dict, target_file, encoding='utf-8', log=True)
"""

import json

from . file_utils import validate_file
from .. logger import Logger


def json2dict(target_file, encoding='utf-8', validate=False):
    if validate:
        validate_file(target_file)

    with open(target_file, 'r', encoding=encoding) as f:
        output = json.load(f)
    return output


def dict2json(target_dict, target_file, encoding='utf-8', log=True):
    with open(target_file, 'w', encoding='utf-8') as f:
        f.write(json.dumps(target_dict, sort_keys=True, indent=4, ensure_ascii=False))
        if log:
            Logger.info("Dict is saved to {}.".format(target_file))
