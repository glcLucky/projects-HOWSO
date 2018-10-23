# -*- coding: utf-8 -*-

"""
datetime_utils.py

时间处理相关工具函数

@author: Lichao Gui
@email: 
@date: 2017.12.11

---------------

FUNCTION LIST:
- df_sampling(df, nsamples, force=True)
"""
import datetime


def char2datetime(date_in, format="%Y-%m-%d"):
    """
    将"YYYY-mm-dd"形式的日期字符串转换为datetime.date
    """
    if len(date_in) == 0:
        return None
    num = datetime.datetime.strptime(date_in, format).date()
    return num

