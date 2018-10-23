# -*- coding: utf-8 -*-

import os


def strip_suffix(fname, suffix):
    """
    移除文件名的后缀

    @fname (str): 文件名称
    @suffix (str): 后缀名称
    return (str)
    """

    return fname.replace(".{}".format(suffix), "")


def listdir_advanced(file_dir, suffix, strip=False):
    """
    获取在指定文件夹下所有文件的名称

    @file_dir (str): 文件夹路径
    @suffix (str): 文件类型
    @strip_suffix (bool): 是否去除后缀 默认为True表示去除后缀
    return (list) 文件名称列表
    """
    
    files = [f for f in os.listdir(file_dir) if f.endswith(suffix)]
    files.sort()
    if strip:
        files = [strip_suffix(f, suffix) for f in files]
    return files
