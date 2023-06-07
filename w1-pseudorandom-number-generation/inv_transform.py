"""Examples of generating a Bernoulli, a Binomial and a Geometric distribution
from a Uniform distribution, using the inverse transform sampling method."""

import numpy as np
from scipy.stats import binom, geom


def unif_bernoulli(n_samples, p):
    x = np.random.rand(n_samples)
    return (x < p).astype(int)


def unif_binomial(n_samples, n, p):
    x = np.random.rand(n_samples)
    cdf = binom.cdf(np.arange(n_samples), n, p)
    return np.digitize(x, cdf)


def unif_geom(n_samples, p):
    x = np.random.rand(n_samples)
    cdf = geom.cdf(np.arange(n_samples), p)
    return np.digitize(x, cdf)
