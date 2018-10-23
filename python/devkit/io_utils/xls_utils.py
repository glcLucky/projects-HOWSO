# -*- coding: utf-8 -*-

"""
xls_utils.py

与excel相关的IO函数库

@author: Wu Yudi
@email: jasper.wuyd@gmail.com
@date: 2017.04.24

---------------

FUNCTION LIST:
- idx2col(idx)
- col2idx(col)
- offset2cell(offset)
- cell2offset(cell)
- dataframe2worksheet(dataframe, worksheet, target_cell="A1", with_head=True, with_index=True, is_clear=False)
- worksheet2dataframe(worksheet, topleft_cell, bottomright_cell, with_head=True, with_index=True)
- dataframe2xls(dataframe, target_file, target_sheet="Sheet1", target_cell=(1, 1),
                with_head=True, with_index=True, is_clear=False, is_backup=True,
                log=True, validate=False))
- dictofdf2xls(dict_of_dataframe, target_file, is_backup=True, log=True, validate=False)
"""

import os
import shutil
import traceback

import pandas as pd
import pywintypes
import win32com.client as win32

from . file_utils import validate_file
from .. logger import Logger


def idx2col(idx):
    """
    序号转xls列

    @idx (int)

    return (str)
    """

    if idx < 1:
        raise ValueError("idx must larger than 0!")
    elif idx > 26*27:
        raise ValueError("idx too large!")

    ch1 = (idx - 1) // 26
    if ch1:
        ch1 = chr(ch1 + 64)
    else:
        ch1 = ""

    ch2 = idx % 26
    if ch2 == 0:
        ch2 = 26
    ch2 = chr(ch2 + 64)

    return ch1 + ch2


def col2idx(col):
    """
    xls列转序号

    @col (str)

    return (int)
    """

    if len(col) == 1:
        idx = ord(col) - 64
        return idx
    elif len(col) == 2:
        idx1 = (ord(col[0]) - 64) * 26
        idx2 = ord(col[1]) - 64
        return idx1 + idx2
    else:
        raise ValueError("Invalid column!")


def offset2cell(offset):
    """
    xls坐标转单元格，(1, 1) -> A1

    @offset ((int, int))

    return (str)
    """

    row, col = offset
    return "{}{}".format(idx2col(col), row)


def cell2offset(cell):
    """
    xls单元格位置转坐标，A1 -> (1, 1)

    @cell (str)

    return ((int, int))
    """

    row, col = "", ""
    for ch in cell:
        if ch.isdigit():
            row += ch
        elif ch.isalpha():
            col += ch
        else:
            raise ValueError("Unrecognized char {}".format(ch))

    row = int(row)
    col = col2idx(col)
    return(row, col)


def dataframe2worksheet(dataframe, worksheet, target_cell="A1",
                        with_head=True, with_index=True, is_clear=False):
    """
    输出Dataframe到指定的worksheet中

    params:
    @dataframe (pandas.DataFrame): 目标DataFrame
    @worksheet (Worksheet): 使用 win32com 打开的 excel 表格
    @target_cell (str or tuple): 最左上角的目标单元格的位置，默认A1，也可用 (行序号, 列序号) 的方式来指定，如 (1, 1)
    @with_head (bool): 是否存储首行
    @with_index (bool): 是否存储首列
    @is_clear(bool): 是否清空原有数据
    """

    if is_clear:
        worksheet.Cells.ClearContents()

    nrow, ncol = len(dataframe), len(dataframe.columns)
    cell = target_cell
    if isinstance(cell, tuple):
        cell = offset2cell(cell)
    cell = worksheet.Range(cell)

    if with_index:
        if dataframe.index.name:
            cell.Value = dataframe.index.name

        # 需要转置为列
        index = [[i] for i in dataframe.index.tolist()]
        worksheet.Range(cell.Offset(2, 1), cell.Offset(nrow+1, 1)).Value = index

        cell = cell.Offset(1, 2)

    if with_head:
        worksheet.Range(cell, cell.Offset(1, ncol)).Value = dataframe.columns.tolist()
        cell = cell.Offset(2, 1)

    worksheet.Range(cell, cell.Offset(nrow, ncol)).Value = dataframe.as_matrix().tolist()


def worksheet2dataframe(worksheet, topleft_cell, bottomright_cell=None, with_head=True, with_index=True):
    """
    从 worksheet 指定位置读取数据为 Dataframe

    params:
    @worksheet (Worksheet): 使用 win32com 打开的 excel 表格
    @topleft_cell (str or tuple): 最左上角的目标单元格的位置，也可用 (行序号, 列序号) 的方式来指定，如 (1, 1)
    @bottomright_cell (str or tuple): 最右下角的目标单元格的位置，也可用 (行序号, 列序号) 的方式来指定，如 (1, 1)
    @with_head (bool): 是否存储首行
    @with_index (bool): 是否存储首列
    """

    cell1 = topleft_cell
    if isinstance(cell1, tuple):
        cell1 = offset2cell(cell1)

    if bottomright_cell is None:
        data = worksheet.Range(cell1).CurrentRegion.Value
    else:
        cell2 = bottomright_cell
        if isinstance(cell2, tuple):
            cell2 = offset2cell(cell2)

        data = worksheet.Range(cell1, cell2).Value

    data = [[(c.strftime("%Y-%m-%d") if isinstance(c, pywintypes.TimeType) else c) for c in row] for row in data]

    if with_head:
        df = pd.DataFrame(data[1:], columns=data[0])
        if with_index:
            df = df.set_index(data[0][0])
    else:
        df = pd.DataFrame(data)

    return df


def dataframe2xls(dataframe, target_file, target_sheet="Sheet1", target_cell="A1",
                  with_head=True, with_index=True, is_clear=False, is_backup=True,
                  log=True, validate=False):
    """
    输出Dataframe到xls或xlsx

    params:
    @dataframe (pandas.DataFrame): 目标DataFrame
    @target_file (str): 目标文件（全路径）
    @target_sheet (str): 目标sheet，默认"Sheet1"
    @target_cell (str or tuple): 最左上角的目标单元格的位置，默认A1，也可用 (行序号, 列序号) 的方式来指定，如 (1, 1)
    @with_head (bool): 是否存储首行
    @with_index (bool): 是否存储首列
    @is_clear(bool): 是否清空原有数据
    @is_backup (bool): 是否保存副本
    @log (bool): 是否打印日志
    @validate (bool): 是否验证文件存在
    """

    if validate:
        validate_file(target_file)

    if is_backup:
        path, file = os.path.split(target_file)
        backup_file = "{}_backup.{}".format(*(file.split(".")))
        backup_file = os.path.join(path, file)
        shutil.copy(target_file, backup_file)
        if log:
            Logger.info("{} is created.".format(backup_file))

    app = win32.gencache.EnsureDispatch("Excel.Application")
    app.Visible = 0

    try:
        wb = app.Workbooks.Open(target_file)
        if log:
            Logger.info("{} is open.".format(target_file))

        sheets = [sht.Name for sht in wb.Worksheets]
        if target_sheet in sheets:
            sht = wb.Worksheets[target_sheet]
        else:
            sht = wb.Worksheets.Add()
            sht.Name = target_sheet

        dataframe2worksheet(dataframe, sht, target_cell, with_head, with_index, is_clear)
        if log:
            Logger.info("Data is write to {} at {}".format(target_sheet, target_cell))

        wb.Save()
        if log:
            Logger.info("{} is saved.".format(target_file))
    except Exception:
        traceback.print_exc()
    finally:
        wb.Close(0)  # 0 means not save
        if log:
            Logger.info("{} is closed.".format(target_file))

        app.Quit()


def dictofdf2xls(dict_of_dataframe, target_file, is_backup=True, log=True, validate=False):
    """
    输出 Dict of Dataframe 到 xls 或 xlsx，字典的键必须为字符串，并以其为表名

    params:
    @dict_of_dataframe (dict of DataFrame): 目标字典
    @target_file (str): 目标文件（全路径）
    @is_backup (bool): 是否保存副本
    @log (bool): 是否打印日志
    @validate (bool): 是否验证文件存在
    """

    if validate:
        validate_file(target_file)

    if is_backup:
        path, file = os.path.split(target_file)
        backup_file = "{}_backup.{}".format(*(file.split(".")))
        backup_file = os.path.join(path, file)
        shutil.copy(target_file, backup_file)
        if log:
            Logger.info("{} is created.".format(backup_file))

    app = win32.gencache.EnsureDispatch("Excel.Application")
    app.Visible = 0

    try:
        wb = app.Workbooks.Open(target_file)
        if log:
            Logger.info("{} is open.".format(target_file))

        for key, df in dict_of_dataframe.items():
            if not isinstance(key, str):
                Logger.error("{} is not string!".format(key))
                continue

            try:
                sheets = [sht.Name for sht in wb.Worksheets]
                if key in sheets:
                    sht = wb.Worksheets[key]
                    sht.Cells.ClearContents()
                else:
                    sht = wb.Worksheets.Add()
                    sht.Name = key

                if log:
                    Logger.info("Save {} to {}".format(key, target_file))
                dataframe2worksheet(df, sht, target_cell="A1",
                                    with_head=True, with_index=True, is_clear=True)
            except:
                traceback.print_exc()
                Logger.error("Error occurred when writing {}".format(key))

        wb.Save()

        if log:
            Logger.info("{} is saved.".format(target_file))

    except Exception:
        traceback.print_exc()
    finally:
        wb.Close(0)  # 0 means not save
        if log:
            Logger.info("{} is closed.".format(target_file))

        app.Quit()
