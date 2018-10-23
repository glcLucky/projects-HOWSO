# -*- coding: utf-8 -*-

"""
doc_utils.py

与MSWord相关的IO函数库

@author: Wu Yudi
@email: jasper.wuyd@gmail.com
@date: 2017.12.28

---------------

FUNCTION LIST:
- paste_text_from_xls_to_doc(sheet, sheet_location, doc, doc_location)
- paste_table_from_xls_to_doc(sheet, sheet_location, app_wd, doc, doc_location, format_options,
                              is_visible=False, log=True)
- paste_chart_from_xls_to_doc(sheet, sheet_location, doc, doc_location)
"""

import traceback
from time import sleep

import win32com.client as win32

from .. logger import Logger


def paste_text_from_xls_to_doc(sheet, sheet_location, doc, doc_location, log=True):
    """
    从Excel某单元格粘贴内容到Word某表格单元格

    @sheet: 通过win32com打开的Worksheet
    @sheet_location: 形如"A1"的字符串
    @doc: 通过win32com打开的doc或docx文件
    @doc_location: 形如(1, 1, 1)的三元数组，表示（表序号，行号，列号）
    """

    idx, r, c = doc_location

    if log:
        Logger.info("Map Text: xls cell ({}) -> doc table [{}]({}, {})".format(
            sheet_location, idx, r, c)
        )

    try:
        text = sheet.Range(sheet_location).Value
        table = doc.Tables(idx)
        cell = table.Cell(r, c)
        cell.Range.Text = text

        sleep(0.1)
    except:
        traceback.print_exc()


def paste_table_from_xls_to_doc(sheet, sheet_location, app_wd, doc, doc_location, format_options,
                                is_visible=False, log=True):
    """
    从Excel某区域粘贴内容到Word某表格，允许嵌套

    @sheet: 通过win32com打开的Worksheet
    @sheet_location: 形如"A1:B10"的字符串
    @app_wd: win32com Word Application
    @doc: 通过win32com打开的doc或docx文件
    @doc_location: list or tuple，表示嵌套序号
    @format_options: 字典，目前支持以下功能
        width%: int （百分比宽度） 默认100
        fitcontent: Bool （根据内容调整表格） 默认True，若为False则平均分布各列
        align: str （对齐） 默认居中，可选参数 "left", "right", center
    """

    if log:
        Logger.info("Map Table: xls range ({}) -> doc table {}".format(sheet_location, list(doc_location)))

    try:
        xlsrange = sheet.Range(sheet_location)

        table = doc.Tables(doc_location[0])
        for i in range(1, len(doc_location)):
            table = table.Tables(doc_location[i])

        table.Range.Select()
        app_wd.Selection.Cut()

        if is_visible:
            sleep(0.5)
        else:
            sleep(0.1)

        xlsrange.Copy()

        if is_visible:
            sleep(0.5)
        else:
            sleep(0.1)

        app_wd.Selection.PasteSpecial()

        # the raw table is cut, so table must be relocated
        table = doc.Tables(doc_location[0])
        for i in range(1, len(doc_location)):
            table = table.Tables(doc_location[i])

        align = format_options.get("align", "center")
        if align == "left":
            table.Rows.Alignment = win32.constants.wdAlignLeft
        elif align == "right":
            table.Rows.Alignment = win32.constants.wdAlignRight
        elif align == "center":
            table.Rows.Alignment = win32.constants.wdAlignRowCenter

        fitcontent = format_options.get("fitcontent", True)
        if fitcontent:
            table.AutoFitBehavior(win32.constants.wdAutoFitContent)
        else:
            table.Range.Select()
            app_wd.Selection.Cells.DistributeWidth()

        table_width = format_options.get("width%", 100)
        if table_width > 0:
            table.PreferredWidthType = win32.constants.wdPreferredWidthPercent
            table.PreferredWidth = table_width

        sleep(0.1)
    except:
        traceback.print_exc()


def paste_chart_from_xls_to_doc(sheet, sheet_location, doc, doc_location, log=True):
    """
    从Excel某单元格粘贴内容到Word某表格单元格

    @sheet: 通过win32com打开的Worksheet
    @sheet_location: 整数，图片序号
    @doc: 通过win32com打开的doc或docx文件
    @doc_location: 整数，图片序号
    """

    if log:
        Logger.info("Map Chart: xls chart [{}] -> doc chart [{}]".format(sheet_location, doc_location))

    try:
        chart = sheet.ChartObjects(sheet_location)
        chart.Activate()
        chart.Copy()  # 也可使用chart.CopyPicture()，但图片质量较差

        sleep(0.1)

        doc.InlineShapes(doc_location).Range.Paste()

        sleep(0.1)
    except:
        traceback.print_exc()
