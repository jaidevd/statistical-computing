import numpy as np


def mixed_congruential(seed, n_samples, a=7**5,
                       m=(2**31 - 1), c=(2 ** 10 - 1)):
    """Uniform distribution ~U[0, 1] with the mixed congruential method."""
    x = np.empty(n_samples)
    x[0] = seed
    has_repeat = False
    for i in range(1, n_samples):
        val = (x[i - 1] * a + c) % m
        if val in x[:i]:
            has_repeat = True
            break
        x[i] = val
    if has_repeat:
        x = np.tile(x[:i], reps=(n_samples // i + 1,))[:n_samples]
    return x / m


def uniform_dist(seed, n_samples, A=0, B=1, **mc_params):  # NOQA: N803
    """A uniform distribution ~U[A, B]."""
    x = mixed_congruential(seed, n_samples, **mc_params)
    return (B - A) * x + A
