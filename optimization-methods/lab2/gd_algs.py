import math
import numpy as np
import random

from time import time
from memory_profiler import profile, memory_usage


def learn_rate_decay(alpha0, theta, epoch):
    return alpha0 * theta ** epoch


def mini_batch_sgd(gradients, start, learn_rate0, points, batch):
    epoch_len: int
    if len(gradients) % batch == 0:
        epoch_len = len(gradients) // batch
    else:
        epoch_len = len(gradients) // batch + 1
    learn_rate, epoch = learn_rate0, 0
    vec = start
    i = 0
    while True:
        print(learn_rate)
        if i % epoch_len == 0:
            learn_rate = learn_rate_decay(learn_rate0, 0.99, epoch)
            epoch += 1
        prev_vec = vec.copy()
        points.append(prev_vec)
        gr = 0
        for j in range(batch):
            gr += gradients[(i * batch + j) % len(gradients)](vec)
        diff = -learn_rate * gr
        if np.all(np.abs(diff) <= eps):
            break
        vec += diff
        i += 1
    return vec


def batch_sgd(gradients, start, learn_rate, points, batch, N):
    vec = start
    for _ in range(N):
        prev_vec = vec.copy()
        points.append(prev_vec)
        gr = 0
        choices = random.sample(range(len(gradients)), batch)
        for i in choices:
            gr += gradients[i](vec)
        diff = -learn_rate * gr
        if np.all(np.abs(diff) <= eps):
            break
        vec += diff
    return vec


# sgd with momentum
def sgd_momentum(gradients, start, learn_rate, omega, batch, points, N):
    vec = start
    vi = 0
    for _ in range(N):
        prev_vec = vec.copy()
        points.append(prev_vec)
        gr = 0
        choices = random.sample(range(len(gradients)), batch)
        for i in choices:
            gr += gradients[i](vec)
        vi1 = omega * vi + (1 - omega) * gr
        vi = vi1
        diff = -learn_rate * vi
        if np.all(np.abs(diff) <= eps):
            break
        vec += diff
    return vec


# nesterov momentum
def nesterov_momentum(gradients, start, learn_rate, omega, batch, points, N):
    vec = start
    vi = 0
    for _ in range(N):
        prev_vec = vec.copy()
        points.append(prev_vec)
        gr = 0
        choices = random.sample(range(len(gradients)), batch)
        to_dot = vec - learn_rate * omega * vi
        for i in choices:
            gr += gradients[i](to_dot)
        vi1 = omega * vi + (1 - omega) * gr
        vi = vi1
        diff = -learn_rate * vi
        if np.all(np.abs(diff) <= eps):
            break
        vec += diff
    return vec


# classic sgd implementation
@profile
def sgd(gradients, start, learn_rate, points, N):
    vec = start
    for _ in range(N):
        prev_vec = vec.copy()
        points.append(prev_vec)
        rint = random.randint(0, len(gradients) - 1)
        diff = -learn_rate * gradients[rint](vec)
        if np.all(np.abs(diff) <= eps):
            break
        vec += diff
    return vec


# Adaptive gradient descent implementation
def ada_grad_sgd(gradients, start, learn_rate, points, N):
    vec = start
    G = None
    for _ in range(N):
        prev_vec = vec.copy()
        points.append(prev_vec)
        rint = random.randint(0, len(gradients) - 1)
        gr = gradients[rint](vec)
        if G is None:
            G = gr * gr
        else:
            G += gr * gr
        diff = -learn_rate * gr / (G ** 0.5)
        if np.all(np.abs(diff) <= eps):
            break
        vec += diff
    return vec

# RMSprop gradient descent implementation
def RMSprop(gradients, beta, start, learn_rate, points, N):
    vec = start
    G, Si0 = None, None
    for _ in range(N):
        prev_vec = vec.copy()
        points.append(prev_vec)
        rint = random.randint(0, len(gradients) - 1)
        gr = gradients[rint](vec)
        if G is None:
            G = gr**2
        else:
            G += gr ** 2
        if Si0 is None:
            Si1 = (1 - beta) * G
            Si0 = Si1
        else:
            Si1 = beta * Si0 + (1 - beta) * G

        diff = -learn_rate * gr / (Si1**0.5 + 10**-5)
        if np.all(np.abs(diff) <= eps):
            break
        vec += diff
    return vec


# ADAM gradient descent implementation
def adam(gradients, beta1, beta2, start, learn_rate, points, N):
    vec = start
    G, Si0 = None, None
    vi = 0
    for _ in range(N):
        prev_vec = vec.copy()
        points.append(prev_vec)
        rint = random.randint(0, len(gradients) - 1)
        gr = gradients[rint](vec)
        if G is None:
            G = gr**2
        else:
            G += gr ** 2
        if Si0 is None:
            Si1 = (1 - beta1) * G
            Si0 = Si1
        else:
            Si1 = beta1 * Si0 + (1 - beta1) * G
        vi1 = beta2 * vi + (1 - beta2) * gr
        vi = vi1
        diff = -learn_rate * vi1 / (Si1**0.5 + 10**-5)
        if np.all(np.abs(diff) <= eps):
            break
        vec += diff
    return vec


# classic gradient descent implementation
@profile
def gd(gradient, start, learn_rate, points, N):
    vec = start
    while True:
        prev_vec = vec.copy()
        points.append(prev_vec)
        diff = -learn_rate * gradient(vec)
        if np.all(np.abs(diff) <= eps):
            break
        vec += diff
    return vec


eps = 10 ** -5
start = np.array([10.0, 12.0])
points0, points1, points2, points3, points4, points5, points6 = [], [], [], [], [], [], []

# test 0
gradients0 = lambda v: np.array([2 * v[0] - 4, 2 * v[1] + 6])
gradients1 = [lambda v: np.array([2 * v[0] - 4, 0]),
              lambda v: np.array([0, 2 * v[1] + 6])]


# test 1
def foo1(x, y):
    return (2 * x + y - 8) ** 2 + (5 * x + y - 2) ** 2 + (8 * x + y - 7) ** 2


# test 2
def bar(x, y):
    return (2 * x + y - 6) ** 2 + (3 * x + y - 10) ** 2 + (5 * x + y - 11) ** 2 + (7 * x + y - 7) ** 2 + (8 * x + y - 12) ** 2 \
    + (9 * x + y - 8) ** 2 + (11 * x + y - 12) ** 2 + (13 * x + y - 10) ** 2 + (15 * x + y - 15) ** 2 + (16 * x + y - 10) ** 2 \
    + (18 * x + y - 15) ** 2 + (19 * x + y - 12) ** 2 + (21 * x + y - 15) ** 2 + (24 * x + y - 12) ** 2 + (25 * x + y - 18) ** 2 \
    + (28 * x + y - 17) ** 2 + (29 * x + y - 14) ** 2 + (30 * x + y - 19) ** 2 + (33 * x + y - 15) ** 2 + (34 * x + y - 19) ** 2



gradients2 = lambda v: np.array([186 * v[0] + 30 * v[1] - 164, 6 * v[1] + 30 * v[0] - 34])
gradients3 = [lambda v: np.array([8 * v[0] + 4 * v[1] - 32, 4 * v[0] + 2 * v[1] - 16]),
              lambda v: np.array([50 * v[0] + 10 * v[1] - 20, 10 * v[0] + 2 * v[1] - 4]),
              lambda v: np.array([128 * v[0] + 16 * v[1] - 112, 16 * v[0] + 2 * v[1] - 14])]

gradients4 = [lambda v: np.array([8 * v[0] + 4 * v[1] - 24, 4 * v[0] + 2 * v[1] - 12]),
              lambda v: np.array([18 * v[0] + 6 * v[1] - 60, 6 * v[0] + 2 * v[1] - 20]),
              lambda v: np.array([50 * v[0] + 10 * v[1] - 110, 10 * v[0] + 2 * v[1] - 22]),
              lambda v: np.array([98 * v[0] + 14 * v[1] - 98, 14 * v[0] + 2 * v[1] - 14]),
              lambda v: np.array([128 * v[0] + 16 * v[1] - 192, 16 * v[0] + 2 * v[1] - 24]),
              lambda v: np.array([162 * v[0] + 18 * v[1] - 144, 18 * v[0] + 2 * v[1] - 16]),
              lambda v: np.array([242 * v[0] + 22 * v[1] - 264, 22 * v[0] + 2 * v[1] - 24]),
              lambda v: np.array([338 * v[0] + 26 * v[1] - 260, 26 * v[0] + 2 * v[1] - 20]),
              lambda v: np.array([450 * v[0] + 30 * v[1] - 450, 30 * v[0] + 2 * v[1] - 30]),
              lambda v: np.array([512 * v[0] + 32 * v[1] - 320, 32 * v[0] + 2 * v[1] - 20]),
              lambda v: np.array([648 * v[0] + 36 * v[1] - 540, 36 * v[0] + 2 * v[1] - 30]),
              lambda v: np.array([722 * v[0] + 38 * v[1] - 456, 38 * v[0] + 2 * v[1] - 24]),
              lambda v: np.array([882 * v[0] + 42 * v[1] - 630, 42 * v[0] + 2 * v[1] - 30]),
              lambda v: np.array([1152 * v[0] + 48 * v[1] - 576, 48 * v[0] + 2 * v[1] - 24]),
              lambda v: np.array([1250 * v[0] + 50 * v[1] - 900, 50 * v[0] + 2 * v[1] - 36]),
              lambda v: np.array([1568 * v[0] + 56 * v[1] - 952, 56 * v[0] + 2 * v[1] - 34]),
              lambda v: np.array([1682 * v[0] + 58 * v[1] - 812, 58 * v[0] + 2 * v[1] - 28]),
              lambda v: np.array([1800 * v[0] + 60 * v[1] - 1140, 60 * v[0] + 2 * v[1] - 38]),
              lambda v: np.array([2178 * v[0] + 66 * v[1] - 990, 66 * v[0] + 2 * v[1] - 30]),
              lambda v: np.array([2312 * v[0] + 68 * v[1] - 1292, 68 * v[0] + 2 * v[1] - 38]),
              ]

gradients4 =[lambda v: np.array([])]

# TESTS 0
# ans0 = gd(gradients0, start, 0.1, points0)
# ans1 = sgd(gradients1, start, 0.1, points1)

# TESTS 1
ans2 = gd(gradients2, start, 0.01, points2, 500)
print("gradient descent", ans2)
# ans3 = sgd(gradients3, start, 0.0035, points3, 3000)
# ans4 = batch_sgd(gradients3, start, 0.001, points4, 2)
# ans5 = mini_batch_sgd(gradients3, start, 0.05, points5, 2)
# ans6 = sgd_momentum(gradients3, start, 0.1, 0.9, 3, points6, 1000)
# TESTS 0
# ans0 = gd(gradients0, start, 0.1, points0)
# ans1 = sgd(gradients1, start, 0.1, points1)

# TESTS 1
# ans2 = gd(gradients2, start, 0.01, points2)
# memory_usage(sgd, (gradients3, start, 0.0035, points3, 3000))
# ans4 = batch_sgd(gradients3, start, 0.001, points4, 2)
# ans5 = mini_batch_sgd(gradients3, start, 0.05, points5, 2)
# print(*points5, sep='\n')

# print(len(points3))
# print(*points3, sep='\n')


# print(len(points6))
# print(*points6, sep='\n')
# print(*points6, sep='\n')
# print(*points5, sep='\n')

# print(*points3, sep='\n')
ans3 = sgd(gradients3, start, 0.0035, points3, 500)
print("sgd", points3[-1])


# points8 = []
# start8 = np.array([5., 12.])
# ans_rmsprop = RMSprop(gradients3, 0.90, start8, 0.1, points8, 500)
# print(ans_rmsprop)
# ans_adagrad = ada_grad_sgd(gradients3, start8, 0.1, points8, 1000)
# print(ans_adagrad)
# print(len(points8))
# print(*points8, sep='\n')
