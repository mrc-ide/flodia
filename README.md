# flodia <img src='man/figures/logo.png' align="right" height="138.5" />
<!-- badges: start -->
[![Project Status: Concept – Minimal or no implementation has been done yet, or the repository is only intended to be a limited example, demo, or proof-of-concept.](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
[![R build status](https://github.com/mrc-ide/flodia/workflows/R-CMD-check/badge.svg)](https://github.com/mrc-ide/flodia/actions)
[![codecov](https://codecov.io/gh/mrc-ide/gonovax/branch/master/graph/badge.svg?token=9u8S3v45AX)](https://codecov.io/gh/mrc-ide/gonovax)
[![CodeFactor](https://www.codefactor.io/repository/github/mrc-ide/flodia/badge)](https://www.codefactor.io/repository/github/mrc-ide/flodia)
<!-- badges: end -->

`flodia` provides tools for drawing compartmental model flow diagrams in base R.

## Installation

The package can be installed directly from GitHub with:

```r
remotes::install_github("mrc-ide/flodia", upgrade = FALSE)
```

## License

MIT © Imperial College of Science, Technology and Medicine

## Examples

### COVID-19 multistrain with vaccination
Sonabend R, Whittles LK, Imai N et al. Non-pharmaceutical interventions, vaccination, and the SARS-CoV-2 delta variant in England: a mathematical modelling study. The Lancet, 2021; 398, 1825-1835. [10.1016/S0140-6736(21)02276-5](https://doi.org/10.1016/S0140-6736(21)02276-5)

<img width="626" height="687" alt="sonabend_2021_flodia" src="https://github.com/user-attachments/assets/c0f98cf7-3441-4dbd-8aa7-9667ee6f81f2" />

### COVID-19 with age structure
Knock E, Whittles LK, Lees J et al. The 2020 SARS-CoV-2 epidemic in England: key epidemiological drivers and impact of interventions. Science Translational Medicine, 2021; 13(602). [10.1126/scitranslmed.abg4262](https://doi.org/10.1126/scitranslmed.abg4262) 

R package: [https://mrc-ide.github.io/sircovid/](https://mrc-ide.github.io/sircovid/)

<img width="1428" height="807" alt="image" src="https://github.com/user-attachments/assets/9daed958-5ece-44d2-94e8-174f836a92ee" />

### Gonorrhoea with activity-structure and vaccination 
Whittles LK, Didelot X, White PJ. Public health impact and cost-effectiveness of gonorrhoea vaccination: an integrated transmission-dynamic health-economic modelling analysis. The Lancet Infectious Diseases, 2022; 22(7), 1030-1041. [10.1016/S1473-3099(21)00744-1](https://doi.org/10.1016/S1473-3099(21)00744-1)

<img width="699" height="368" alt="Screenshot 2026-09-01 at 11 51 57" src="https://github.com/user-attachments/assets/9468dae7-4f56-4df5-9a42-34d20d20f8ee" />

