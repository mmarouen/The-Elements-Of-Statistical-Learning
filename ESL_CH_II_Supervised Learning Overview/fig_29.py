import numpy as np
import matplotlib.pyplot as plt

# define the return function
def f(x):
    if(x.ndim == 1):
        x = np.expand_dims(x, axis=-1)
    return 0.5 * ((x[:, 0] + 1) ** 3)

def g(x):
    if(x.ndim == 1):
        x = np.expand_dims(x, axis=-1)
    return x[:, 0]

def beta_(x, y):
    if(y.ndim == 1):
        y = np.expand_dims(y, axis=-1)
    return np.linalg.inv(x.T @ x) @ x.T @ y

# np.random.seed(200)
fig, axs = plt.subplots(1, 1)
f_0 = 0.5
g_0 = 0

n_samples = 1000
dimensions = [i for i in range(1, 11)]
n_iter = 50
mse_knn_f = {}
mse_knn_g = {}
mse_ols_f = []
mse_ols_g = []
mse_knn_f_list = []
mse_knn_g_list = []
mse_ols_f_ = 0
mse_ols_g_ = 0
for i in range(n_iter):
    eps_0 = np.random.normal(0, 1)
    # ols estimate on zero doesnt depend on X
    yhat_ols_f_0 = 0
    yhat_ols_g_0 = 0
    mse_ols_f_ += (f_0 + eps_0 - yhat_ols_f_0) ** 2
    mse_ols_g_ += (g_0 + eps_0 - yhat_ols_g_0) ** 2
    for d in dimensions:
        for j in range(n_iter):
            X = np.random.uniform(-1, 1, n_samples * d).reshape((n_samples, d))
            norms = np.linalg.norm(X, axis=1)
            argmin_x = np.argmin(norms)
            for k in range(n_iter):
                eps_train = np.random.normal(0, 1)
                f_x = f(X) + eps_train
                g_x = g(X) + eps_train
                mse_knn_f[d] = mse_knn_f.get(d, 0) + (f_0 + eps_0 - f_x[argmin_x]) ** 2
                mse_knn_g[d] = mse_knn_g.get(d, 0) + (g_0 + eps_0 - g_x[argmin_x]) ** 2
mse_ols_f = [mse_ols_f_ / n_iter] * len(dimensions)
mse_ols_g = [mse_ols_g_ / n_iter] * len(dimensions)
for key, val in mse_knn_f.items():
    mse_knn_f_list.append(val / (n_iter ** 3))
for key, val in mse_knn_g.items():
    mse_knn_g_list.append(val / (n_iter ** 3))
#axs.plot(dimensions, mse_knn_f_list, linestyle='-', marker='.', color='blue', label='mse knn cubic')
#axs.plot(dimensions, mse_knn_g_list, linestyle='-', marker='.', color='red', label='mse knn linear')
axs.plot(dimensions, [knn / ols for (knn, ols) in zip(mse_knn_f_list, mse_ols_f)], linestyle='-', marker='.', color='cyan', label='Cubic')
axs.plot(dimensions, [knn / ols for (knn, ols) in zip(mse_knn_g_list, mse_ols_g)], linestyle='-', marker='.', color='orange', label='Linear')
axs.set_title("Expected Prediction Error of 1NN vs. OLS")
axs.set_xlabel('Dimension')
axs.set_ylabel('EPE Ratio')
# axs.set_ylim((0, 2.))
axs.legend()

plt.show()