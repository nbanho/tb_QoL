import os
import pandas as pd
import jax
import jax.numpy as jnp
import numpyro
from numpyro import distributions as dist
from numpyro.infer import MCMC, NUTS
import numpy as np
import argparse

# Settings
numpyro.set_host_device_count(4)
n_chains = 4
n_smpl = 1000

# Parse command-line arguments
parser = argparse.ArgumentParser()
parser.add_argument("analysis", help="Type of analysis corresponding to the leading name of the directory containing for the data and sample files")
args = parser.parse_args()

# Iterate over all imputed datasets
dat_path = f'fitted-models/{args.analysis}-data'
num_files = len([f for f in os.listdir(dat_path) if os.path.isfile(os.path.join(dat_path, f))])

# Read the data
for i in range(1, num_files + 1):
    dat_file = f'fitted-models/{args.analysis}-data/imp-data-{i}.csv'
    df = pd.read_csv(dat_file)

    # Separate outcomes and predictors
    outcome_vars = ['phq9_score','sf12_ment','sf12_phys','smwt_dist','stst_nr','sgrq_tot_score','tb_symp_score']
    outcomes = df[outcome_vars]
    predictors = df.drop(columns=outcome_vars)

    # Prior standard deviations
    std_outcomes = outcomes.std(axis=0).to_numpy()
    std_predictors = predictors.std(axis=0).to_numpy()
    std_predictors[std_predictors == 0] = 1
    std_predictors[std_predictors < 0.5] = 0.5
    std_betas = 2.5 * std_outcomes[:, None] / std_predictors[None, :]

    # Define the model   
    def model(predictors, outcomes):
        predictors = jnp.array(predictors)
        outcomes = jnp.array(outcomes)

        # prior for intercepts
        intercepts = numpyro.sample('intercepts', dist.Normal(jnp.zeros(outcomes.shape[1]), 1.))

        # Priors for predictors
        betas = numpyro.sample('betas', dist.Normal(jnp.zeros((outcomes.shape[1], predictors.shape[1])), std_betas))

        # LKJ prior for the correlation matrix
        L_Omega = numpyro.sample('L_Omega', dist.LKJCholesky(outcomes.shape[1], concentration=1.))
        Sigma = numpyro.sample('Sigma', dist.HalfNormal(1.), sample_shape=(outcomes.shape[1],))
        scale_tril = jnp.matmul(jnp.sqrt(jnp.diag(Sigma)), L_Omega)  # Cholesky factor of the covariance matrix
        
        # Likelihood
        mu = jnp.einsum('ij,kj->ki', betas, predictors) + intercepts
        with numpyro.plate('data', len(outcomes)):
            numpyro.sample('obs', dist.MultivariateNormal(mu, scale_tril=scale_tril), obs=outcomes)

    # Run MCMC
    nuts_kernel = NUTS(model)
    mcmc = MCMC(nuts_kernel, num_warmup=n_smpl, num_samples=n_smpl, num_chains=n_chains, chain_method='parallel')
    mcmc.run(jax.random.PRNGKey(12345), predictors, outcomes)

    # Extract posterior samples
    posterior_samples = mcmc.get_samples()

    # Reshape betas and convert to DataFrame
    betas = posterior_samples['betas'].reshape(-1, predictors.shape[1])
    betas_df = pd.DataFrame(betas, columns=predictors.columns)

    # Add additional columns
    betas_df = betas_df.melt(var_name='predictor', value_name='beta')
    betas_df['draw'] = np.tile(np.repeat(np.arange(1, n_chains * n_smpl + 1), len(outcome_vars)), len(predictors.columns))
    betas_df['outcome'] = [outcome_vars[i] for i in (betas_df.index % len(outcome_vars)).tolist()]

    # Save to csv
    res_file = f'fitted-models/{args.analysis}-samples/imp-samples-{i}.csv'
    betas_df.to_csv(res_file, index=False)