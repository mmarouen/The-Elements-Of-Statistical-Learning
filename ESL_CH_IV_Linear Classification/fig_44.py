import os
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from _utils import cmap, get_lda_params, sphere_data

vowel_data = os.path.join('data', 'vowel.train.txt')

df = pd.read_csv(vowel_data, sep=',')

X = df[['x.1', 'x.2', 'x.3', 'x.4', 'x.5', 'x.6', 'x.7', 'x.8', 'x.9', 'x.10']].to_numpy()
y = df['y'].tolist()


classes, prior_counts, means, covariance, deltas = get_lda_params(X, y)
X_star = sphere_data(covariance=covariance, X=X)

fig, axs = plt.subplots(1, 2)

axs[0].scatter(X[:, 0], X[:, 1], edgecolors=[cmap[i] for i in y], facecolors='none')

axs[0].set_xlabel('Coordinate 1 for the training data')
axs[0].set_ylabel('Coordinate 2 for the training data')

axs[1].scatter(X_star[:, 0], X_star[:, 1], edgecolors=[cmap[i] for i in y], facecolors='none')
axs[1].set_xlabel('Coordinate 1 for the training data')
axs[1].set_ylabel('Coordinate 2 for the training data')

plt.show()
