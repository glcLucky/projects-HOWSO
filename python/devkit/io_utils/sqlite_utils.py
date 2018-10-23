# -*- coding: utf-8 -*-

"""
sqlite_utils.py

与sqlite相关的IO函数库

@author: Wu Yudi
@email: jasper.wuyd@gmail.com
@date: 2018.03.06

---------------

FUNCTION LIST:
- SqliteProxy(self)
"""

import traceback

import pandas as pd

import sqlite3
from sqlite3 import Error

from .. logger import Logger


class SqliteProxy:
    """
    数据库长连接，用于各种操作

    control methods:
    - connect(self, db_path)
    - close(self)
    - test_connected(self)
    - execute(self, query)

    schema methods:
    - list_tables(self)
    - list_vars(self, table)
    - show_sample(self, table)
    - describe_table(self, table)

    table methods:
    - create_table(self, name, template)
    - delete_table(self, name)
    - create_index(self, index, cols, table)
    - delete_index(self, index)

    data methods:
    - write_from_dataframe(self, df, table, if_exists="append")
    - query_as_dataframe(self, query)
    """

    def __init__(self, log=True):
        self.db_file = None
        self.conn = None
        self.cursor = None
        self.is_connected = False
        self.log = log

    # ------------ syntax sugar -----------

    def __enter__(self):
        return self

    def __exit__(self, type, value, traceback):
        self.close()

    # ---------- control methods ----------

    def connect(self, db_file):
        try:
            if self.is_connected:
                if self.db_file == db_file:
                    return
                else:
                    self.close()

            conn = sqlite3.connect(db_file)

            self.db_file = db_file
            self.conn = conn
            self.cursor = conn.cursor()
            self.is_connected = True

            if self.log:
                Logger.info("{} is connected.".format(db_file), "green")
        except Error:
            traceback.print_exc()

    def close(self):
        if self.is_connected:
            try:
                self.cursor.close()
            except Error:
                traceback.print_exc()

            try:
                self.conn.commit()
                self.conn.close()
                self.is_connected = False
            except Error:
                traceback.print_exc()

        if not self.is_connected and self.log:
            Logger.info("{} is disconnected.".format(self.db_file), "green")

    def reconnect(self):
        self.close()
        self.connect(self.db_file)

    def test_connected(self):
        if self.log:
            if self.is_connected:
                Logger.info("Connecting {}.".format(self.db_file))
            else:
                Logger.info("No database connected.")
        return self.is_connected

    def execute(self, query):
        if self.log:
            print(query)
        cursor = self.cursor.execute(query)
        return cursor

    # ---------- schema methods ----------

    @property
    def list_tables(self):
        res = self.cursor.execute("SELECT name FROM sqlite_master WHERE type='table';")
        tables = [entry[0] for entry in res.fetchall()]
        return tables

    def list_vars(self, table):
        # query = "pragma table_info('{}');".format(table)
        query = "SELECT sql FROM sqlite_master WHERE type='table' AND name='{}';".format(table)
        res = self.cursor.execute(query)
        vars_info = res.fetchall()[0][0]
        print(vars_info)
        # return vars_info

    def show_sample(self, table):
        query = "SELECT * FROM {} LIMIT 10;".format(table)
        res = self.cursor.execute(query)
        rows = res.fetchall()
        return rows

    def describe_table(self, table):
        print("TABLE: {}".format(table))

        print("\nVARS:")
        query = "pragma table_info('{}');".format(table)
        res = self.cursor.execute(query)
        for var_info in res.fetchall():
            print("    {} {}".format(var_info[1], var_info[2]))

        print("\nINDICES:")
        query = "SELECT name FROM sqlite_master WHERE type='index' AND tbl_name='{}';".format(table)
        res = self.cursor.execute(query)
        for idx_info in res.fetchall():
            print("    {}".format(idx_info[0]))

        print("\nSAMPLES:")
        query = "SELECT * FROM {} LIMIT 5;".format(table)
        res = self.cursor.execute(query)
        for row in res.fetchall():
            row = ", ".join([str(x) for x in row])
            print("    {}".format(row))

    # ---------- table methods ----------

    def create_table(self, table, template):
        """
        template format:
        [
            (column_name, datatype, is_primary_key, is_null),
            (column_name, datatype, is_primary_key, is_null),
            (column_name, datatype, is_primary_key, is_null),
            ...
        ]

        valid datatype: TEXT, CHAR(n), INT, FLOAT
        """

        columns = []
        for col, dtype, is_primary_key, is_null in template:
            desc = "{} {}".format(col, dtype)
            if is_primary_key:
                desc += " PRIMARY KEY"
            if not is_null:
                desc += " NOT NULL"
            columns.append(desc)

        query = "CREATE TABLE [{}] (\n    {}\n);".format(table, ",\n    ".join(columns))
        if self.log:
            print(query)
        res = self.cursor.execute(query)
        Logger.info("TABLE {} is created in {}.".format(table, self.db_file))
        return

    def delete_table(self, table):
        query = "DROP TABLE [{}];".format(table)
        if self.log:
            print(query)
        res = self.cursor.execute(query)
        Logger.warn("TABLE {} is deleted from {}.".format(table, self.db_file))
        return

    def rename_table(self, old_table, new_table):
        # ALTER TABLE {} RENAME TO {};
        pass

    def create_index(self, index, cols, table, is_unique=False):
        unique = "UNIQUE" if is_unique else ""
        query = "CREATE {} INDEX [{}] ON [{}] ({});".format(unique, index, table, ", ".join(cols))
        if self.log:
            print(query)
        res = self.cursor.execute(query)
        Logger.info("INDEX {} is created on TABLE {} of {}.".format(index, table, self.db_file))
        return

    def delete_index(self, index):
        query = "DROP INDEX [{}];".format(index)
        if self.log:
            print(query)
        res = self.cursor.execute(query)
        Logger.warn("INDEX {} is deleted from {}.".format(index, self.db_file))
        return

    def add_column(self, column, dtype, table):
        # proxy.execute("ALTER TABLE {} ADD COLUMN {} {};")
        pass

    # ---------- data methods ----------

    def write_from_dataframe(self, df, table, if_exists="append"):
        """
        从DataFrame向table中写入数据，忽略索引列

        @if_exists: 如果值重复怎么办，可选"fail", "update", "append"，默认append
        """

        df.to_sql(table, self.conn, if_exists='append', index=False)
        if self.log:
            Logger.info("Data is inserted to {} successfully!".format(table))
        return

    def query_as_dataframe(self, query):
        """执行查询语句，输出成DataFrame"""

        if self.log:
            Logger.info(query)
        res = pd.read_sql_query(query, self.conn)
        return res
