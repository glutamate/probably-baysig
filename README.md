probably-baysig
===============

'probably-baysig' contains definitions and functions for probabilistic and
statistical inference.

* Math.Probably.Sampler defines the sampling function
  monad, as described by Sungwoo Park and implemented
  elsewhere (e.g. 'random-fu' and 'monte-carlo' packages)

* Math.Probably.PDF defines some common parametric
  log-probability density functions

* Math.Probably.FoldingStats defines statistics as folds
  that can be composed and calculated independently of the
  container of the underlying data.

* Strategy.\* implements various transition operators for
  Markov Chain Monte Carlo, including Metropolis-Hastings,
  Hamiltonian Monte Carlo, NUTS, and continuous/discrete
  slice samplers.

* Math.Probably.MCMC implements functions and combinators
  for running Markov chains and interleaving transition
  operators.

