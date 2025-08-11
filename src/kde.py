from sklearn.neighbors import KernelDensity
import numpy as np


def compute_kde(l_x, g_x, b_lx, b_gx):
  """Returns density values"""
  l_kde = KernelDensity(kernel='gaussian', bandwidth=b_lx).fit(l_x)
  g_kde = KernelDensity(kernel='gaussian', bandwidth=b_gx).fit(g_x)
  return {'l_kde': l_kde, 'g_kde': g_kde}

  
def choose_next_hps(l_kde, g_kde, n_samples=1000):
    samples = l_kde.sample(n_samples)
    
    log_l = l_kde.score_samples(samples)
    log_g = g_kde.score_samples(samples)
    
    optimal_idx = np.argmax(np.exp(log_l - log_g))
    return samples[optimal_idx]


