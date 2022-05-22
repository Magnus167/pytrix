from random import SystemRandom as SR

def f1(arr):
    s = 0
    for i in range(len(arr)):
        for j in range(len(arr[i])):
            s += arr[i][j]
    return s

def f2(arr):
    s = 0
    for i in range(len(arr)):
        for j in range(len(arr[i])):
            s += arr[j][i]
    return s

def f3(arr):
    s = 0
    for i in range(len(arr)):
        s += sum(arr[i])
    return s

def f4(arr):
    s = sum([sum(i) for i in arr])
    return s
    
arr = [[SR().randint(0, 100) for _ in range(1000)] for _ in range(1000)]

timeit f1(arr)
timeit f2(arr)
timeit f3(arr)
timeit f4(arr)