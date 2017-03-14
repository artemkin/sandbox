#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import print_function
import random

def input_int(title, f=(lambda x: x)):
    while True:
        try:
            return f(int(raw_input(title + ': ')))
        except ValueError:
            print('Ошибка. Попробуй еще раз...')

def check_bound(x, lower, upper):
    if x < lower or x > upper:
        raise ValueError('')
    return x

count = input_int('Число примеров (от 1 до 100)',              (lambda x: check_bound(x, 1, 100)))
upper = input_int('Верхний порог (от 1 до 10)',                (lambda x: check_bound(x, 1, 10)))
lower = input_int('Нижний порог (от 1 до ' + str(upper) + ')', (lambda x: check_bound(x, 1, upper)))

f = open('mult.log', 'a')

correct = 0

for i in range(count):
    a = random.randint(lower, upper)
    b = random.randint(1, 10)
    c = input_int(str(a) + 'x' + str(b) + "=");

    if a * b == c:
        correct = correct + 1

    print(a, b, c, file=f)

print('Правильных ответов:', correct, 'из', count)

star = '\xE2\xAD\x90'
kiss = '\xF0\x9F\x98\x98'
if correct == count:
    print('УМНИЦА!!!!', star, star, star)
    print('Ты лучшая!', kiss)
elif float(correct) / count >= 0.7:
    print('У тебя почти получилось! Молодец!!!')

