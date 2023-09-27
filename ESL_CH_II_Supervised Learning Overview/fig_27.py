import numpy as np
import matplotlib.pyplot as plt

# define the return function
def f(x):
    if(x.ndim == 1):
        x = np.expand_dims(x, axis=-1)
    norm_sq = np.linalg.norm(x, axis=1) ** 2
    return np.exp(-8 * norm_sq)

np.random.seed(200)
# top left plot
n_samples = 10
p = 1
X = np.random.uniform(-1, 1, n_samples * p).reshape((n_samples, p))
y_values = f(X)
x_values = np.linspace(-1, 1, 1000)
fig, axs = plt.subplots(2, 2)
axs[0, 0].plot(x_values, f(x_values), color='mediumseagreen')
xticks = axs[0, 0].set_xticks(np.squeeze(X), labels=None)
argmin_x = np.argmin(np.abs(X), axis=0)[0]
x_min = X[argmin_x][0]
y_min = y_values[argmin_x]
xticks[argmin_x]._apply_params(color='blue')
axs[0, 0].set_xticklabels([])
segment_x = [0, x_min]
segment_y = [1, 1]
axs[0, 0].plot(segment_x, segment_y, '--b')
segment_x = [x_min, x_min]
segment_y = [1, y_min]
axs[0, 0].plot(segment_x, segment_y, '--b')
axs[0, 0].set_title("1-NN in One Dimension")
axs[0, 0].set_ylabel("f(X)")
axs[0, 0].set_xlabel("X")
axs[0, 0].axvline(x = 0, color = 'black')

# top right plot
n_samples = 25
p = 2
X = np.random.uniform(-1, 1, n_samples * p).reshape((n_samples, p))
x_min = X[np.argmin(np.abs(X[:, 0]), axis=-1), 0]
nearest_2d = min(np.linalg.norm(X, axis=1))

axs[0, 1].scatter(X[:, 0], X[:, 1], color='mediumseagreen')
axs[0, 1].scatter(0, 0, color='black')
axs[0, 1].plot([x_min, x_min], [-1, 1], color='tab:orange')
axs[0, 1].plot([-x_min, -x_min], [-1, 1], color='tab:orange')
axs[0, 1].plot([-x_min, x_min], [1, 1], color='tab:orange')
axs[0, 1].plot([-x_min, x_min], [-1, -1], color='tab:orange')
axs[0, 1].set_xlabel('X1')
axs[0, 1].set_ylabel('X2')
axs[0, 1].set_title("1-NN in One vs. Two Dimensions")
circle = plt.Circle((0, 0), nearest_2d, color='b', fill=False)
axs[0, 1].add_patch(circle)

# low left plot
n_samples = 1000
dimensions = [i for i in range(1, 11)]
n_iter = 10
average_distance = []
for d in dimensions:
    min_distance = 0
    for _ in range(n_iter):
        X = np.random.uniform(-1, 1, n_samples * d).reshape((n_samples, d))
        min_distance += min(np.linalg.norm(X, axis=1))
    average_distance.append(min_distance / n_iter)
axs[1, 0].plot(dimensions, average_distance, '.-r')
axs[1, 0].set_title("Distance to 1-NN vs. Dimension")
axs[1, 0].set_xlabel('Dimension')
axs[1, 0].set_ylabel('Average Distance to Nearest Neighbor')

# low right plot
n_samples = 1000
dimensions = [i for i in range(1, 11)]
n_iter = 30
y_true = 1.
mse = []
bias_sq = []
variance = []
for d in dimensions:
    min_distance = 0
    ave_yhat = 0
    y_hat_2 = 0
    for _ in range(n_iter):
        X = np.random.uniform(-1, 1, n_samples * d).reshape((n_samples, d))
        y_values = f(X)
        argmin_x = np.argmin(np.abs(X), axis=0)[0]
        y_hat = y_values[argmin_x]
        y_hat_2 += y_hat ** 2
        ave_yhat += y_hat
    ave_yhat /= n_iter
    var = y_hat_2 / n_iter - ave_yhat ** 2
    bias_sq_ = (ave_yhat - y_true) ** 2
    bias_sq.append(bias_sq_)
    variance.append(var)
    mse.append(bias_sq_ + var)
axs[1, 1].plot(dimensions, bias_sq, '.-b', label='Sq. bias')
axs[1, 1].plot(dimensions, variance, linestyle='-', marker='.', color='mediumseagreen', label='MSE')
axs[1, 1].plot(dimensions, mse, '.-r', label='Variance')
axs[1, 1].set_title("MSE vs. Dimension")
axs[1, 1].set_xlabel('Dimension')
axs[1, 1].set_ylabel('Mse')
axs[1, 1].legend()

plt.show()


