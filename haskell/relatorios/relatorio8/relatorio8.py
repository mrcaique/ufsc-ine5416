#!/usr/bin/env python3
from math import acos

# Part 1: vectors operations

def norm(vector):
    return sum(i ** 2 for i in vector) ** (1/2)

def scalar_mult_vector(vector, scalar):
    return [i * scalar for i in vector]

def vector_addition(v1, v2):
    if (len(v1) != len(v2)):
        return []
    return sum(v1[i] + v2[i] for i in range(len(v1)))

def dot_product(v1, v2):
    if (len(v1) != len(v2)):
        return 0
    return sum(v1[i] * v2[i] for i in range(len(v1)))

def cross_product(v1, v2):
    if (len(v1) != 3 or len(v2) != 3):
        return []
    result[0] = (v1[1]*v2[2]) - (v2[1]*v1[2])
    result[1] = (v1[0]*v2[2]) + (v2[0]*v1[2])
    result[2] = (v1[0]*v2[1]) - (v2[0]*v1[1])
    return result

def angle_between_vectors(v1, v2):
    return acos(dot_product(v1, v2)/(norm(v1) * norm(v2)))

# Part 2: matrices operations

def transpose(matrix):
    return [list(i) for i in zip(*matrix)]

def scalar_mult_matrix(matrix, scalar):
    return [scalar_mult_vector(i * scalar) for i in matrix]

def matrix_addition(v1, v2):
    if (len(v1) != len(v2)):
        return []
    return [vector_addition(a, b) for a, b in zip(v1, v2)]

def matrix_mult(m1, m2):
    t = transpose(m2)
    for a in m1:
        for b in t:
            for fa, fb in zip(m1, m2):
                sum(fa * fb)

def sarrus_rule(m):
    if (len(m) != 3) or (len(m[0]) != 3):
        return 0

    diag =  m[0][0] * m[1][1] * m[2][2] + \
            m[0][2] * m[1][2] * m[2][0] + \
            m[0][2] * m[1][0] * m[2][1]

    neg =   m[2][0] * m[1][1] * m[0][2] - \
            m[2][1] * m[1][2] * m[0][0] - \
            m[2][2] * m[1][0] * m[0][1]

    return diag - neg
