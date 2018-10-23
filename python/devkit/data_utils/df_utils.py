# -*- coding: utf-8 -*-

"""
df_utils.py

DataFrame处理函数库

@author: Wu Yudi
@email: jasper.wuyd@gmail.com
@date: 2017.12.11

---------------

FUNCTION LIST:
- df_sampling(df, nsamples, force=True)
"""


def df_sampling(df, nsamples, force=True):
    """
    从DataFrame中按行均匀抽样

    @force: 当df行数小于所需样本数量时，是否严格和所需样本数量一致
    """

    l = len(df)
    if l < nsamples and force is False:
        return df

    d = (l - 1) / (nsamples - 1)
    sample_index = [min(l-1, int(d*i)) for i in range(nsamples)]
    df_sample = df.iloc[sample_index, :]

    assert len(df_sample) == nsamples

    return df_sample
