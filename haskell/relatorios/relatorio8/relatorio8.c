#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#define size(x) (sizeof(x)/sizeof((x)[0])) + 1

// Part 1: vectors operations

float norm(const float* vector)
{
    float sum = 0;
    int size = size(vector);
    int i;
    for (i = 0; i < size; i++) {
        sum += pow(vector[i], 2);
    }
    return sqrt(sum);
}

float* scalar_mult_vector(float* vector, const float scalar)
{
    int i;
    int size = size(vector);
    for (i = 0; i < size; i++) {
        vector[i] *= scalar;
    }
    return vector;
}

float* vector_addition(const float* v1, const float* v2)
{
    int i;
    int size_v1 = size(v1);
    int size_v2 = size(v2);
    float result[] = {0, 0};
    if (size_v1 != size_v2) {
        return result;
    }
    for (i = 0; i < size_v1; i++) {
        result[i] = v1[i] + v2[i];
    }
    return result;
}

float dot_product(const float* v1, const float* v2)
{
    int i;
    float dot = 0;
    int size_v1 = size(v1);
    int size_v2 = size(v2);
    if (size_v1 != size_v2) {
        return 0;
    }
    for (i = 0; i < size_v1; i++) {
        dot += (v1[i] * v2[i]);
    }
    return dot;
}

float* cross_product(const float* v1, const float* v2)
{
    int i;
    int sv1 = size(v1);
    int sv2 = size(v2);
    float result[] = {0, 0, 0};
    if (sv1 != 3 || sv2 != 3) {
        return result;
    }
        result[0] = (v1[1]*v2[2]) - (v2[1]*v1[2]);
        result[1] = (v1[0]*v2[2]) + (v2[0]*v1[2]);
        result[2] = (v1[0]*v2[1]) - (v2[0]*v1[1]);
    return result;
}

float angle_between_vectors(float* v1, float* v2)
{
    return acos(dot_product(v1, v2)/(norm(v1) * norm(v2)));
}

// Part 2: Matrix operations

void transpose(int row, int column, int m[row][column])
{
    int i, j, aux;
    for (i = 0; i < row; i++) {
        for (j = 0; j < column; j++) {
            if(i != j) {
                aux = m[i][j];
                m[i][j] = m[j][i];
                m[j][i] = aux;
            }
        }
    }
}

void scalar_mult_matrix(int row, int column, int m[row][column], int scalar)
{
    int i, j;
    for (i = 0; i < row; i++) {
        for (j = 0; j < row; j++) {
            m[row][column] *= scalar;
        }
    }
}

int** matrix_addition(int row, int column, int m1[row][column], int m2[row][column])
{
    int i, j;
    int result[i][j];
    for (i = 0; i < row; i++) {
        for (j = 0; j < column; j++) {
            result[i][j] = m1[i][j] + m2[i][j];
        }
    }
    return result;
}

int** matrix_mult(int row, int column, int m1[row][column], int m2[row][column])
{
    int i, j, k;
    int mult[row][column];
    for (i = 0; i < row; i++) {
        for (j = 0; j < column; j++) {
            mult[row][column] = 0;
            for(k = 0; k < column; k++) {
                mult[row][column] += m1[row][column] * m2[column][row];
            }
        }
    }
}

int sarrus_rule(int m[3][3])
{
    int diag =  m[0][0] * m[1][1] * m[2][2] +
                m[0][2] * m[1][2] * m[2][0] +
                m[0][2] * m[1][0] * m[2][1];

    int neg =   m[2][0] * m[1][1] * m[0][2] -
                m[2][1] * m[1][2] * m[0][0] -
                m[2][2] * m[1][0] * m[0][1];

    return diag - neg;
}

int main()
{
    return 0;
}