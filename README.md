# coevo_analysis_Rpackage

Codebase to facilitate coevolution method benchmarking. Mostly
a wrapper around the [`ROCR`](https://cran.r-project.org/web/packages/ROCR/index.html)
package.

Will be updated to include postprocessing of output from various coevolution
programs.

## Can do these things

- Get a few performance metrics
    - TPR, FPR, PPV
    - auROC, auPR, max-F<sub>1</sub>, max-φ
- Get TPR and PPV at controlled FPR
- Estimate *P<sub>normal</sub>* and *P<sub>empirical</sub>* as used in [Avila-Herrera
  & Pollard BMC Binf 2015](http://doi.org/10.1186/s12859-015-0677-y)
