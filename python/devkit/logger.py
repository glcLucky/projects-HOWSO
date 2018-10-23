# -*- coding: utf-8 -*-

"""
logger.py

日志记录

@author: Wu Yudi
@email: jasper.wuyd@gmail.com
@date: 2017.05.19
"""

from datetime import datetime

try:
    from termcolor import colored
except ImportError:
    colored = lambda msg, clr: msg


class Logger:
    def __init__(self):
        return

    @classmethod
    def log(cls, message, level="info", color="blue"):
        now = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
        message = "[] {} {}".format(level.upper(), now, message)
        print(colored(message, color))

    @classmethod
    def info(cls, message, color="blue"):
        now = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
        message = "[INFO] {} {}".format(now, message)
        print(colored(message, color))

    @classmethod
    def warn(cls, message, color="red"):
        now = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
        message = "[WARN] {} {}".format(now, message)
        print(colored(message, color))

    @classmethod
    def debug(cls, message, color="blue"):
        now = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
        message = "[DEBUG] {} {}".format(now, message)
        print(colored(message, color))

    @classmethod
    def error(cls, message, color="red"):
        now = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
        message = "[ERROR] {} {}".format(now, message)
        print(colored(message, color))
