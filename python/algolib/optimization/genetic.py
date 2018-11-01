# -*- coding: utf-8 -*-

"""
genetic_algorithm.py

遗传算法包

@author: Jasper Gui
@email: glc_luck@outlook.com
@date: 2018.11.01
"""


class Genetic_algorithm():


def decode(x):
    y = 0 + int(x, 2) / (2**17 - 1) * 9
    return y


def fitness(population, aimFunction):
    value = []
    for i in range(len(population)):
        value.append(aimFunction(decode(population[i])))
        # weed out negative value
        if value[i] < 0:
            value[i] = 0
    return value


def selection(population, value):
    # 轮盘赌选择
    fitness_sum = []
    for i in range(len(value)):
        if i == 0:
            fitness_sum.append(value[i])
        else:
            fitness_sum.append(fitness_sum[i - 1] + value[i])

    for i in range(len(fitness_sum)):
        fitness_sum[i] /= sum(value)

    # select new population
    population_new = []
    for i in range(len(value)):
        rand = np.random.uniform(0, 1)
        for j in range(len(value)):
            if j == 0:
                if 0 < rand and rand <= fitness_sum[j]:
                    population_new.append(population[j])

            else:
                if fitness_sum[j - 1] < rand and rand <= fitness_sum[j]:
                    population_new.append(population[j])
    return population_new
