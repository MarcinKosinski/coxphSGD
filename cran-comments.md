coxphSGD 0.2.0
==============

- 2017-07-04
- Version submitted to CRAN.
- Removed the `yihui/rlp` dependency and moved to regular `roxygen2` structure of the package's documentation.
- Removed dependencies (imports) of `assertthat`, `magrittr` and `reshape2` as it's better to have less dependencies in the
fastly evolving software development world (backward incompatibilities).
- Provided README.md
- Cleaned naming convention of functions and parameters.
- Created the package website.

coxphSGD 0.1.0
==============

- 2015-12-25 (released only on GitHub)
- Library was build with the experimental approach using `yihui/rlp` package
for creating package structures and documentation only in the vignette document.
- In the root directory of the package one could found the master thesis (in polish), 
entitled `Stochastic Gradient Descent Estimation in the Cox Proportional Hazards Model with
Applications to The Cancer Genome Atlas Data`. The package was the effect of tools written during
the study.
- Package was equipped with the [archivist](https://github.com/pbiecek/archivist) like
repository of R objects, where partial (and final results) of the analysis performed during
my master thesis were stored.

> The promotor of the master thesis was profesor Przemyslaw Biecek (Warsaw University of Technology), who provided many improvements and comments to the package and who had a great impact on my master thesis and the whole scientific career, which I deeply appreciate
