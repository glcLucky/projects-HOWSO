# -*- coding: utf-8 -*-

"""
zip_utils.py

与代码打包和解包有关的工具函数库

@author: Wu Yudi
@email: jasper.wuyd@gmail.com
@date: 2017.12.13

---------------

FUNCTION LIST:
- zipdir(from_dir, to_zip)
- unzipdir(from_zip, to_dir)
"""

import os
import zipfile


def ignore(entry_name):
    if entry_name.startswith(".") or entry_name == "__pycache__":
        return True
    else:
        return False


def mywalk(top):
    """add entry detect based on os.walk()"""

    dirs = []
    nondirs = []

    try:
        if isinstance(top, bytes):
            scandir_it = os._dummy_scandir(top)
        else:
            scandir_it = os.scandir(top)
        entries = list(scandir_it)
    except OSError as error:
        return

    for entry in entries:
        try:
            is_dir = entry.is_dir()
        except OSError:
            is_dir = False

        if ignore(entry.name):
            continue

        if is_dir:
            dirs.append(entry.name)
        else:
            nondirs.append(entry.name)

        if is_dir:
            yield from mywalk(entry.path)

    yield top, dirs, nondirs


def zipdir(from_dir, to_zip):
    """
    from_dir: dir to zip
    to_zip: target zip file
    """

    if from_dir.endswith("\\"):
        from_dir = from_dir[:-1]
    top = os.path.split(from_dir)[-1]

    with zipfile.ZipFile(to_zip, 'w', zipfile.ZIP_STORED) as zipf:
        for root, dirs, files in mywalk(from_dir):
            for file in files:
                path = os.path.join(root.replace(from_dir, top), file)
                zipf.write(path)

    print("{} is zipped to {}".format(from_dir, to_zip))


def unzipdir(from_zip, to_dir):
    with zipfile.ZipFile(from_zip, 'r') as zipf:
        zipf.extractall(to_dir)

    print("{} is unzipped to {}".format(from_zip, to_dir))
