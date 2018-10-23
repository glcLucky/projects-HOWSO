# -*- coding: utf-8 -*-

"""
module_utils.py

与单个module代码有关的工具函数库

@author: Wu Yudi
@email: jasper.wuyd@gmail.com
@date: 2017.12.13

---------------

FUNCTION LIST:
- import_single_pyfile(pyname, pyfile)
- generate_init_string(module_path)
"""

import importlib
import inspect
import os


def import_single_pyfile(pyname, pypath):
    mod = importlib.machinery.SourceFileLoader(pyname, pypath)
    mod = mod.load_module()
    return mod


def generate_init_string(module_path):
    modstr = "from . import (\n"
    funstr = []
    allstr = []
    for pyfile in os.listdir(module_path):
        if pyfile.startswith("_"):
            continue
        else:
            pypath = os.path.join(module_path, pyfile)
            mod = import_single_pyfile(pyfile, pypath)

            modname = pyfile.split(".")[0]
            modstr += "    {},\n".format(modname)

            funcs = inspect.getmembers(mod, inspect.isfunction)
            if funcs:
                s = "from . {} import (\n".format(modname)
                for f, obj in funcs:
                    if mod == inspect.getmodule(obj):
                        s += "    {},\n".format(f)
                        allstr.append("    \"{}\",".format(f))
                s += ")"
                allstr.append("")
            funstr.append(s)
    modstr += ")\n"
    funstr = "\n\n".join(funstr)
    allstr = "\n".join(allstr).strip()
    allstr = "\n__all__ = [\n    {}\n]".format(allstr)

    print(modstr)
    print(funstr)
    print(allstr)
