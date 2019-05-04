# wavecojumps code 

The code has been developed in R version 3.5.0. and accompanies the paper

Barunik, J. and Fiser, P. (2019): *Co-jumping of Treasury Yield Curve Rates*, manuscript [available here for download](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3382841) (May 2019)

The code can also be used to estimate the Jump-Wavelet Two Scale Realized Variance (JWTSRV) and Jump-Wavelet Two Scale Realized Covariance (JWTSCV) estimators introduced in 

Baruník, J., and Vácha, L., (2018) [Do co-jumps impact correlations in currency markets?](https://ideas.repec.org/p/arx/papers/1602.05489.html) **Journal of Financial Markets**, 37, pp.97-119<br/>

Baruník, J., and Vácha, L., (2015) [Realized wavelet-based estimation of integrated variance and jumps in the presence of noise](https://ideas.repec.org/p/arx/papers/1202.1854.html) **Quantitative Finance**, 15 (6), pp. 959-973<br/>

# Example

For example see *OneDayEzample/OneDayExample.Rmd* file.

The main advantage of the wavelet-based estimation is the ability of wavelets to precisely separate the continuous price process into continuous and discontinuous parts even when data are covered with noise. For more details about the method see (Barunik and Vacha, 2018 JFM).
