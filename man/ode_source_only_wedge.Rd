% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ode_source_only.R
\name{ode_source_only_wedge}
\alias{ode_source_only_wedge}
\title{Birth, death, and migration ODE model.}
\usage{
ode_source_only_wedge(
  growth_rate,
  carrying_capacity,
  n0,
  migration_offset = 0,
  migration_intercept = 0,
  migration_baseline,
  delta_t,
  t_max = 25,
  ...
)
}
\arguments{
\item{migration_baseline}{Double, normalised internally by (n-1)}

\item{...}{additional arguments passed to the integrator or to the
    methods.}
}
\description{
Wedge and smooth refers to the presence of a non-differentiability in the state-dependent migration rate.
}
\details{
The migration mechanism is only aware of the conditions in \emph{source}. The
destination patch is evenly distributed amongst all present patches.
}
