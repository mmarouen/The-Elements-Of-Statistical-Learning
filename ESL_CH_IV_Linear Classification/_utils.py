import numpy as np

cmap = {
    0:  "#1f77b4",  # Blue
    1:  "#ff7f0e",  # Orange
    2:  "#2ca02c",  # Green
    3:  "#d62728",  # Red
    4:  "#9467bd",  # Purple
    5:  "#8c564b",  # Brown
    6:  "#e377c2",  # Pink
    7:  "#7f7f7f",  # Gray
    8:  "#bcbd22",  # Olive
    9:  "#17becf",  # Cyan
    10: "#393b79",  # Dark blue
    11: "#637939",  # Dark green
    12: "#8c6d31",  # Dark brown
    13: "#843c39",  # Dark red
    14: "#7b4173",  # Dark purple
    15: "#3182bd",  # Steel blue
    16: "#e6550d",  # Dark orange
    17: "#31a354",  # Forest green
    18: "#756bb1",  # Lavender
    19: "#636363",  # Dark gray
}

def get_lda_params(X, y):
    N = X.shape[0]
    p = X.shape[1]
    classes, y_idx, prior_counts = np.unique(y, return_inverse=True, return_counts=True)
    K = len(classes)
    class_matrices = {k: X[np.array(y) == k] for k in classes}
    means = {k: np.mean(mat, axis=0) for k, mat in class_matrices.items()}
    covariance = np.zeros((p, p))
    for k in class_matrices.keys():
        covariance += (class_matrices[k] - means[k]).T @ (class_matrices[k] - means[k])
    covariance /= (N - K)
    covariance_inv = np.linalg.inv(covariance)
    deltas = {
        k: lambda x, k=k: 
            x @ covariance_inv @ means[k].T - 0.5 * means[k] @ covariance_inv @ means[k].T + np.log(prior_counts[k] / N)
        for k in classes
    }
    return classes, prior_counts, means, covariance, deltas

def sphere_data(covariance, X):
    eigenvalues, eigenvectors = np.linalg.eig(covariance)
    return X @ eigenvectors @ np.diag(1./np.sqrt(eigenvalues))
