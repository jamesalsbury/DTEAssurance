# Run paired replicates for one scenario, with checkpointing

Runs
[`single_paired_rep`](https://jamesalsbury.github.io/DTEAssurance/reference/single_paired_rep.md)
for `i = 1, ..., n_sims`, in chunks of `chunk_size` replicates (in
parallel via
[`parallel::mclapply`](https://rdrr.io/r/parallel/mclapply.html) when
`n_cores > 1`). After each chunk, all rows so far are written to
`checkpoint_file`; if that file exists at the start, the run resumes
from the replicates already completed.

## Usage

``` r
run_paired_scenario(
  n_sims,
  seed,
  ...,
  n_cores = 1,
  checkpoint_file = NULL,
  chunk_size = 100
)
```

## Arguments

- n_sims:

  Number of replicates.

- seed:

  Base seed (see
  [`single_paired_rep`](https://jamesalsbury.github.io/DTEAssurance/reference/single_paired_rep.md)).

- ...:

  Further arguments passed to
  [`single_paired_rep`](https://jamesalsbury.github.io/DTEAssurance/reference/single_paired_rep.md).

- n_cores:

  Number of cores (default 1).

- checkpoint_file:

  Optional path of an RDS checkpoint file.

- chunk_size:

  Number of replicates per chunk (default 100).

## Value

A list with `raw` (one row per replicate, ordered by `rep_id`) and
`settings` (all arguments, the package, R, rjags and JAGS versions, the
git commit of the working directory, the hostname, a timestamp and
`n_failed`). The number of failed replicates is reported with a message,
and a warning is given if more than 1\\

## See also

[`single_paired_rep`](https://jamesalsbury.github.io/DTEAssurance/reference/single_paired_rep.md),
[`summarize_grid_by_kappa`](https://jamesalsbury.github.io/DTEAssurance/reference/summarize_grid_by_kappa.md)
