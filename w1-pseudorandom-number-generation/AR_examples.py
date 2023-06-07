"""Try to sample a Bernoulli(p=0.5) distribution
from a Binomial(n=4, p=0.2) distribution."""
import numpy as np
from scipy.stats import binom
import pandas as pd
import matplotlib.pyplot as plt

c = 1.22
n_samples = 100
Z = np.empty(n_samples)
n_tries = np.zeros(n_samples)

for ix in range(n_samples):
    tries = 0
    while True:
        tries += 1
        U = np.random.rand()
        Y = binom.rvs(n=4, p=0.2)
        p_y = 0.5 if Y in (0, 1) else 0
        q_y = binom.pmf(Y, n=4, p=0.2)
        if U <= (p_y / c * q_y):
            break
    Z[ix] = Y
    n_tries[ix] = tries


Z = pd.Series(Z).value_counts().sort_index()
plt.plot(Z.index, Z.values, '-o', label='AR Method')
plt.show()
print("Avg number of tries: ", n_tries.mean())  # NOQA: T201
