% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ode_migration_only.R
\name{ode_migration_only}
\alias{ode_migration_only}
\title{Birth, death, and migration ODE model.}
\usage{
ode_migration_only(
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
This migration rate is just density-dependent. It makes no distinction on the state of the source nor the destination.
}
