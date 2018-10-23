# -*- coding: utf-8 -*-

"""
mysql_utils.py

与mysql相关的IO函数库

@author: Wu Yudi
@email: jasper.wuyd@gmail.com
@date: 2018.03.06

---------------

FUNCTION LIST:
- SqliteProxy(self)
"""

from sqlalchemy import create_engine
import pymysql
import pandas as pd
import traceback
import mysql.connector
import traceback

import pandas as pd

from .. logger import Logger


def df2mysql(user, password, db_name, table_name, df, index=False, index_label=False):
    """
    将panda数据集写入mysql指定数据库的指定表中
    @user (str): 用户名
    @password (str): 密码
    @db_name (str): 数据库名称
    @table_name (str): 表名称
    @df (DataFrame): 待写入DataFrame
    """
    engine = create_engine('mysql+pymysql://{}:{}@localhost:3306/{}?charset=utf8'.format(user, password, db_name))
    try:
        df.to_sql(table_name, con=engine, index=index, if_exists='append', index_label=index_label)
    except Exception:
        traceback.print_exc()


def get_tables_on_given_database(user, password, db_name):
    """
    获取给定mysql数据库下面的所有表的名称 返回list
    @user
    @password
    @db_name
    """
    conn = MySQLProxy()
    conn.connect('root', '123888', 'index_std')
    query = "SELECT table_name FROM information_schema.tables WHERE table_schema = '{}'".format(db_name)
    output = conn.query_as_dataframe(query)['table_name'].tolist()
    return output


class MySQLProxy:
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
        self.database = None
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

    def connect(self, user, password, database):
        try:
            if self.is_connected:
                if self.database == database:
                    return
                else:
                    self.close()

            conn = mysql.connector.connect(user=user, password=password, database=database)

            self.database = database
            self.conn = conn
            self.cursor = conn.cursor()
            self.is_connected = True

            if self.log:
                Logger.info("{} is connected.".format(database), "green")
        except Exception:
            traceback.print_exc()

    def close(self):
        if self.is_connected:
            try:
                self.cursor.close()
            except Exception:
                traceback.print_exc()

            try:
                self.conn.close()
                self.is_connected = False
            except Exception:
                traceback.print_exc()

        if not self.is_connected and self.log:
            Logger.info("{} is disconnected.".format(self.database), "green")

    def reconnect(self):
        self.close()
        self.connect(self.database)

    def test_connected(self):
        if self.log:
            if self.is_connected:
                Logger.info("Connecting {}.".format(self.database))
            else:
                Logger.info("No database connected.")
        return self.is_connected

    def execute(self, query):
        if self.log:
            print(query)
        self.cursor.execute(query)
        return self.cursor.fetchall()

    # ---------- schema methods ----------

    @property
    def list_tables(self):
        self.cursor.execute("show tables;")
        return self.cursor.fetchall()

    def list_vars(self, table):
        self.cursor.execute("DESCRIBE {};".format(table))
        vars_info = [entry[0] for entry in self.cursor.fetchall()]
        print(vars_info)
        # return vars_info

    def show_sample(self, table):
        query = "SELECT * FROM {} LIMIT 5;".format(table)
        print(pd.read_sql_query(query, self.conn))

    def describe_table(self, table):
        self.cursor.execute("DESCRIBE {};".format(table))
        output = pd.DataFrame(self.cursor.fetchall(), columns=['var_name', 'type', 'Null', 'Key', 'Default', 'Extra'])
        print(output)
    # ---------- table methods ----------

    def delete_table(self, table):
        query = "DROP TABLE [{}];".format(table)
        if self.log:
            print(query)
        self.cursor.execute(query)
        Logger.warn("TABLE {} is deleted from {}.".format(table, self.database))
        return

    def rename_table(self, old_table, new_table):
        # ALTER TABLE {} RENAME TO {};
        query = "RENAME TABLE [{}]  TO [{}]".format(old_table, new_table)
        if self.log:
            print(query)
        self.cursor.execute(query)
        Logger.warn("TABLE {} is renamedd from {} to {}.".format(table, old_table, new_table))
        return

    def create_index(self, index, cols, table, is_unique=False):
        unique = "UNIQUE" if is_unique else ""
        query = "CREATE {} INDEX [{}] ON [{}] ({});".format(unique, index, table, ", ".join(cols))
        if self.log:
            print(query)
        self.cursor.execute(query)
        Logger.info("INDEX {} is created on TABLE {} of {}.".format(index, table, self.database))
        return

    def delete_index(self, index):
        query = "DROP INDEX [{}];".format(index)
        if self.log:
            print(query)
        self.cursor.execute(query)
        Logger.warn("INDEX {} is deleted from {}.".format(index, self.database))
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

        df.to_sql(table, con=self.conn, if_exists='append', index=False)
        if self.log:
            Logger.info("Data is inserted to {} successfully!".format(table))
        return
 
    def query_as_dataframe(self, query):
        """执行查询语句，输出成DataFrame"""

        if self.log:
            Logger.info(query)
        res = pd.read_sql_query(query, self.conn)
        return res

