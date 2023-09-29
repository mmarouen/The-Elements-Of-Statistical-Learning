import numpy as np
import matplotlib.pyplot as plt

# define the return function
def f(x):
    if(x.ndim == 1):
        x = np.expand_dims(x, axis=-1)
    return 0.5 * (x[:, 0] + 1) ** 3

np.random.seed(200)
fig, axs = plt.subplots(1, 2)
y_0 = 0.5

# left plot
n_samples = 10
p = 1
X = np.random.uniform(-1, 1, n_samples * p).reshape((n_samples, p))
y_values = f(X)
x_values = np.linspace(-1, 1, 1000)
axs[0].plot(x_values, f(x_values), color='mediumseagreen')
xticks = axs[0].set_xticks(np.squeeze(X), labels=None)
argmin_x = np.argmin(np.linalg.norm(X, axis=1))
x_min = X[argmin_x][0]
y_min = y_values[argmin_x]
xticks[argmin_x]._apply_params(color='blue')
axs[0].set_xticklabels([])
segment_x = [0, x_min]
segment_y = [y_0, y_0]
axs[0].plot(segment_x, segment_y, '--b')
segment_x = [x_min, x_min]
segment_y = [y_0, y_min]
axs[0].plot(segment_x, segment_y, '--b')
axs[0].set_title("1-NN in One Dimension")
axs[0].set_ylabel("f(X)")
axs[0].set_xlabel("X")
axs[0].axvline(x = 0, color = 'black')
axs[0].secondary_xaxis('bottom')

# low right plot
n_samples = 1000
dimensions = [i for i in range(1, 11)]
n_iter = 50
mse = []
bias_sq = []
variance = []
for d in dimensions:
    ave_yhat = 0
    y_hat_2 = 0
    for _ in range(n_iter):
        X = np.random.uniform(-1, 1, n_samples * d).reshape((n_samples, d))
        y_values = f(X)
        argmin_x = np.argmin(np.linalg.norm(X, axis=1))
        y_hat = y_values[argmin_x]
        y_hat_2 += y_hat ** 2
        ave_yhat += y_hat
    ave_yhat /= n_iter
    var = y_hat_2 / n_iter - ave_yhat ** 2
    bias_sq_ = (ave_yhat - y_0) ** 2
    bias_sq.append(bias_sq_)
    variance.append(var)
    mse.append(bias_sq_ + var)
axs[1].plot(dimensions, bias_sq, '.-b', label='Sq. bias')
axs[1].plot(dimensions, mse, linestyle='-', marker='.', color='orange', label='MSE')
axs[1].plot(dimensions, variance, linestyle='-', marker='.', color='cyan', label='Variance')
axs[1].set_title("MSE vs. Dimension")
axs[1].set_xlabel('Dimension')
axs[1].set_ylabel('Mse')
axs[1].legend()

plt.show()