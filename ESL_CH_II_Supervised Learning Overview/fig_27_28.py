import numpy as np
import matplotlib.pyplot as plt

P = 10
N = 1_000
M = 1000

def f(x):
    if x.ndim == 1:
        x = np.expand_dims(x, axis=0)
    return np.exp(-8 * (np.linalg.norm(x, ord=2, axis=1) ** 2))

def g(x):
    if x.ndim == 1:
        x = np.expand_dims(x, axis=0)
    return 0.5 * (x[:, 0] + 1) ** 3

def nearest_neightbor(x0, x):
    norms = np.linalg.norm(x - x0, ord=2, axis=1)
    index = np.argmin(norms)
    distance = norms[index]
    return x[index], distance

def generate_xi_s(p, n):
    return np.random.uniform(-1, 1, p * n).reshape(n, p)

def run_experiment(p, n, fn):
    xi_s = generate_xi_s(p, n)
    x_0 = np.zeros((1, p))
    y_0 = fn(x_0)
    x_hat_0, distance_0 = nearest_neightbor(x_0, xi_s)
    y_hat_0 = fn(x_hat_0)
    return x_hat_0, y_hat_0, y_0, distance_0

def compute_errors(y_val, y_hats):
    y_hat_0_ave = np.mean(y_hats)
    variance = np.var(y_hats)
    bias_sq = (y_hat_0_ave - y_val) ** 2
    mse = variance + bias_sq
    return mse, bias_sq, variance

print(f'Montecarlo simulation to estimate estimator EPE at zero for Exponential and Cubical function\n- {M} Iterations\n-{N} Observations\n-p {1} .. {P}')
f_mse_vector = []
f_bias_sq_vector = []
f_variance_vector = []
f_average_distances = []

g_mse_vector = []
g_bias_sq_vector = []
g_variance_vector = []
g_average_distances = []

for p in range(1, P + 1):
    print(f'Dimension {p}')
    f_y_hat_0_averages = []
    f_x_hat_0_distances = []
    g_y_hat_0_averages = []
    g_x_hat_0_distances = []
    f_y_0 = -1000000
    g_y_0 = -1000000
    for m in range(M):
        _, y_hat_0, y_0_, distance = run_experiment(p, N, f)
        f_y_hat_0_averages.append(y_hat_0)
        f_x_hat_0_distances.append(distance)
        f_y_0 = y_0_

        _, y_hat_0, y_0_, distance = run_experiment(p, N, g)
        g_y_hat_0_averages.append(y_hat_0)
        g_x_hat_0_distances.append(distance)
        g_y_0 = y_0_

    mse, var, bias_sq = compute_errors(f_y_0, f_y_hat_0_averages)
    f_mse_vector.append(mse)
    f_variance_vector.append(var)
    f_bias_sq_vector.append(bias_sq)
    f_average_distances.append(np.mean(f_x_hat_0_distances))

    mse, var, bias_sq = compute_errors(g_y_0, g_y_hat_0_averages)
    g_mse_vector.append(mse)
    g_variance_vector.append(var)
    g_bias_sq_vector.append(bias_sq)
    g_average_distances.append(np.mean(g_x_hat_0_distances))

fig, axs = plt.subplots(2, 2)
dimensions = [i for i in range(1, P + 1)]

# figure 2.7 low left
axs[0, 0].plot(dimensions, f_average_distances, '.-r')
axs[0, 0].set_title("Distance to 1-NN vs. Dimension Fig 2.7")
axs[0, 0].set_xlabel('Dimension')
axs[0, 0].set_ylabel('Average Distance to Nearest Neighbor')

# figure 2.7 low right
axs[0, 1].plot(dimensions, f_bias_sq_vector, '.-b', label='Sq. bias')
axs[0, 1].plot(dimensions, f_variance_vector, linestyle='-', marker='.', color='mediumseagreen', label='Variance')
axs[0, 1].plot(dimensions, f_mse_vector, '.-r', label='MSE')
axs[0, 1].set_title("MSE vs. Dimension Fig 2.7")
axs[0, 1].set_xlabel('Dimension')
axs[0, 1].set_ylabel('Mse')
axs[0, 1].legend()

# figure 2.8 left
axs[1, 0].plot(dimensions, g_average_distances, '.-r')
axs[1, 0].set_title("Distance to 1-NN vs. Dimension Fig 2.8")
axs[1, 0].set_xlabel('Dimension')
axs[1, 0].set_ylabel('Average Distance to Nearest Neighbor')
# figure 2.8 right
axs[1, 1].plot(dimensions, g_bias_sq_vector, '.-b', label='Sq. bias')
axs[1, 1].plot(dimensions, g_variance_vector, linestyle='-', marker='.', color='mediumseagreen', label='Variance')
axs[1, 1].plot(dimensions, g_mse_vector, '.-r', label='MSE')
axs[1, 1].set_title("MSE vs. Dimension Fig 2.8")
axs[1, 1].set_xlabel('Dimension')
axs[1, 1].set_ylabel('Mse')
axs[1, 1].legend()

plt.show()
