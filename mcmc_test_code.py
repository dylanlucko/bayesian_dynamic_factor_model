#%%


#!/usr/bin/env python3
"""
heavy_mcmc_stress_test.py

A computationally intense stress test using both Metropolis–Hastings (MH)
and Hamiltonian Monte Carlo (HMC) on a high-dimensional multivariate normal.

Usage:
    python heavy_mcmc_stress_test.py --sampler hmc --dim 4000 --iterations 1000000 --leapfrog-steps 50
    python heavy_mcmc_stress_test.py --sampler mh  --dim 8000 --iterations 1000000 --proposal-std 0.1

Options:
    --sampler         Sampler type: 'mh' or 'hmc' (default: 'hmc')
    --dim             Dimension of the target distribution (default: 1000)
    --iterations      Total number of MCMC iterations (default: 1000000)
    --burnin          Burn-in iterations to discard (default: iterations // 4)
    --proposal-std    MH proposal standard deviation (default: 2.38 / sqrt(dim))
    --step-size       HMC leapfrog step size (default: 0.05)
    --leapfrog-steps  Number of leapfrog steps per HMC iteration (default: 25)
    --seed            Random seed for reproducibility (default: 42)
"""

import argparse
import time
import numpy as np


def target_log_pdf(x):
    """
    Log-density of a standard multivariate normal N(0, I).
    """
    return -0.5 * np.dot(x, x)


def grad_log_pdf(x):
    """
    Gradient of log-density: ∇ log p(x) = -x
    """
    return -x


def run_mh(dim, iterations, burnin, proposal_std, seed):
    np.random.seed(seed)
    x = np.zeros(dim)
    samples = []
    accept = 0
    start = time.time()
    for i in range(iterations):
        prop = x + np.random.normal(scale=proposal_std, size=dim)
        log_ratio = target_log_pdf(prop) - target_log_pdf(x)
        if np.log(np.random.rand()) < log_ratio:
            x = prop
            accept += 1
        if i >= burnin:
            samples.append(x.copy())
        if (i+1) % (iterations // 10) == 0:
            print(f"MH Iter {i+1}/{iterations} - elapsed {time.time()-start:.2f}s")
    total = time.time() - start
    return np.array(samples), total, accept / iterations


def run_hmc(dim, iterations, burnin, step_size, leapfrog_steps, seed):
    np.random.seed(seed)
    x = np.zeros(dim)
    samples = []
    accept = 0
    start = time.time()
    for i in range(iterations):
        p = np.random.randn(dim)
        current_H = -target_log_pdf(x) + 0.5 * np.dot(p, p)
        x_new = x.copy()
        p_new = p.copy()
        # leapfrog
        p_new += 0.5 * step_size * grad_log_pdf(x_new)
        for _ in range(leapfrog_steps):
            x_new += step_size * p_new
            p_new += step_size * grad_log_pdf(x_new)
        p_new += -0.5 * step_size * grad_log_pdf(x_new)
        proposed_H = -target_log_pdf(x_new) + 0.5 * np.dot(p_new, p_new)
        if np.log(np.random.rand()) < (current_H - proposed_H):
            x = x_new
            accept += 1
        if i >= burnin:
            samples.append(x.copy())
        if (i+1) % (iterations // 10) == 0:
            print(f"HMC Iter {i+1}/{iterations} - elapsed {time.time()-start:.2f}s")
    total = time.time() - start
    return np.array(samples), total, accept / iterations


def main():
    parser = argparse.ArgumentParser(description="Heavy MCMC stress test")
    parser.add_argument("--sampler", choices=["mh","hmc"], default="hmc",
                        help="Sampler type (mh or hmc)")
    parser.add_argument("--dim", type=int, default=4000,
                        help="Dimension of target distribution")
    parser.add_argument("--iterations", type=int, default=2000000,
                        help="Total number of MCMC iterations")
    parser.add_argument("--burnin", type=int, default=None,
                        help="Burn-in iterations (default: iterations//4)")
    parser.add_argument("--proposal-std", type=float, default=None,
                        help="MH proposal std (default: 2.38/sqrt(dim))")
    parser.add_argument("--step-size", type=float, default=0.05,
                        help="HMC leapfrog step size")
    parser.add_argument("--leapfrog-steps", type=int, default=25,
                        help="HMC leapfrog steps")
    parser.add_argument("--seed", type=int, default=42,
                        help="Random seed")
    args, _ = parser.parse_known_args()

    if args.burnin is None:
        burnin = args.iterations // 4
    else:
        burnin = args.burnin

    if args.sampler == "mh":
        proposal_std = args.proposal_std or (2.38 / np.sqrt(args.dim))
        print(f"Running MH: dim={args.dim}, iters={args.iterations}, burnin={burnin}, prop_std={proposal_std:.4f}")
        samples, total, acc = run_mh(args.dim, args.iterations, burnin, proposal_std, args.seed)
    else:
        print(f"Running HMC: dim={args.dim}, iters={args.iterations}, burnin={burnin}, step={args.step_size}, leapfrog={args.leapfrog_steps}")
        samples, total, acc = run_hmc(args.dim, args.iterations, burnin, args.step_size, args.leapfrog_steps, args.seed)

    print(f"\n--- Results ---")
    print(f"Total time: {total:.2f}s")
    print(f"Acceptance rate: {acc:.4f}")
    mean_est = np.mean(samples, axis=0)
    var_est = np.var(samples, axis=0)
    print(f"Mean(1–5): {mean_est[:5]}")
    print(f"Var (1–5): {var_est[:5]}")

if __name__ == "__main__":
    main()
# %%
