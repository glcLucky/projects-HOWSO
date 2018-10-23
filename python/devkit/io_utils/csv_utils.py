# -*- coding: utf-8 -*-

"""
csv_utils.py

与csv的IO函数库

@author: Wu Yudi
@email: jasper.wuyd@gmail.com
@date: 2017.12.11

---------------

FUNCTION LIST:
- open_csv_as_df(csvpath, encoding="utf-8", validate=False))
"""

import pandas as pd

from . file_utils import validate_file


def open_csv_as_df(csvpath, encoding="utf-8", validate=False):
    if validate:
        validate_file(csvpath)
    return pd.read_csv(csvpath, encoding=encoding)
