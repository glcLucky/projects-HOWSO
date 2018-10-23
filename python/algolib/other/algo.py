# -*- coding: utf-8 -*-
"""
其他类算法整理

@date: 2018-10-16
@author: Jasper Gui
====================
"""

import os
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt


class MinimumBoundingPolygon():
    """
    离散点最小凸包围边界查找  https://segmentfault.com/a/1190000011694769
    """

    def __init__(self, points):
        """
        @points <1d array>: 每个元素是一个散点
        """
        self.points = points

    def calLineLen(self, ws, en):
        """
        计算两个点之间连线的长度
        @ws <np.array>: 西南点
        @en <np.array>: 东北点
        """
        if (ws == en).all():
            return 0.0
        a = np.abs(ws[0] - en[0])
        b = np.abs(ws[1] - en[1])
        min_l = min(a, b)  # 较短的直角边
        max_l = max(a, b)  # 较长的直角边
    #    为防止计算平方时float溢出，做如下转换
    #    √(min²+max²) = √((min/max)²+1) * abs(max)
        res = np.sqrt((min_l / max_l) ** 2 + 1) * max_l
        return res

    def angleOf(self, s, d):
        """
        某个向量与x轴正方向的夹角
        @s <np.array>：向量起点
        @d <np.array>：向量终点 destination
        """
        dist = self.calLineLen(s, d)
        if dist <= 0:
            return 0.0
        x = d[0] - s[0]
        y = d[1] - s[1]
        if y >= 0.0:  # 位于1、2象限
            return np.arccos(x / dist)
        else:  # 位于3,4象限
            return np.arccos(-x / dist) + np.pi

    def reviseAngle(self, angle):
        """
        对给定的角度按如下规则进行修正
        """
        if angle < 0.0:
            angle += 2 * np.pi
        if angle >= 2 * np.pi:
            angle -= 2 * np.pi
        return angle

    def findStartPoint(self):
        coords = np.array(
            sorted(self.points, key=lambda x: [-x[1], x[0]]))  # 根据y降序 x升序排列
        return coords[0]

    def findSmallestPolygon(self):
        if len(self.points) == 0:
            return null
        corner = self.findStartPoint()
        oldAngle = 2 * np.pi
        bound = []
        while True:
            minAngleDif = 2 * np.pi
            bound.append(corner)
            nextPoint = corner
            nextAngle = oldAngle
            for p in self.points:
                if (p == corner).all():  # 重合点
                    continue
                currAngle = self.angleOf(corner, p)  # 当前向量与x轴正方向的夹角
                angleDif = self.reviseAngle(
                    oldAngle - currAngle)  # 两条向量之间的夹角（顺时针旋转的夹角）
                if angleDif < minAngleDif:
                    minAngleDif = angleDif
                    nextPoint = p
                    nextAngle = currAngle
            oldAngle = nextAngle
            corner = nextPoint
            if (corner == bound[0]).all():
                break
        bound.append(corner)
        return np.array(bound)

    def plot_scatter_and_bounds(self, bound, path=None):
        """绘制散点图及寻找到的边界"""
        plt.plot(self.points[:, 0], self.points[:, 1], 'b.')
        plt.plot(bound[:, 0], bound[:, 1], 'r-')
        if path is not None:
            plt.savefig(path)

    def point_in_polygon(self, x, y, bounds):
        """
        PNPoly算法  判断点(x,y)是否在bounds构成的多边形内
        @x <flo>: 单个横坐标
        @y <flo>: 单个纵坐标
        @bounds <ndarray,n*2>: 多边形边界值
        @return inside返回True 否则返回false
        """
        x, y = float(x), float(y)
        vertx = [xyvert[0] for xyvert in bounds]
        verty = [xyvert[1] for xyvert in bounds]
        # N个点中，横坐标和纵坐标的最大值和最小值，判断目标坐标点是否在这个四边形之内
        if len(bounds) == 0 or not min(vertx) <= x <= max(vertx) or not min(verty) <= y <= max(verty):
            return False
        # 上一步通过后，核心算法部分
        nvert = len(bounds)
        is_in = False
        for i in range(nvert):
            j = nvert - 1 if i == 0 else i - 1
            if ((verty[i] > y) != (verty[j] > y)) and \
                    (x < (vertx[j] - vertx[i]) * (y - verty[i]) / (verty[j] - verty[i]) + vertx[i]):
                is_in = not is_in
        return is_in
