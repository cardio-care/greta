# GRETA

Analysis and figure scripts for **GRETA** (Genetic REsults DaTAbase), a relational
results database for whole-genome sequencing studies.

GRETA stores association results from the Hamburg City Health Study alongside
public reference resources, and makes them queryable through a graphical
interface by researchers without programming experience. This repository
contains the R code used to produce the figures and performance analyses
reported in the accompanying manuscript. It does **not** contain the database
itself, which is built on commercial software and holds controlled-access data.

## Repository contents

```
scripts/    R scripts for the manuscript figures and analyses
.here       anchor for here::here() path resolution
```

| Script | Purpose |
|---|---|
| `plot_warmup_effect.R` | Supplementary Figure 1: response times of the first replicate relative to steady state, across all step × method series |

## Requirements

R ≥ 4.1 (the native `|>` pipe is used) and the following packages:

```r
install.packages(c("here", "readxl", "dplyr", "tidyr",
                   "ggplot2", "scales", "patchwork"))
```

## Usage

Clone the repository and open it as an RStudio project, or set the working
directory to the repository root. Paths are resolved from the project root by
`here::here()`, so no path editing is required.

```r
source("scripts/plot_warmup_effect.R")
```

Each script expects its input file under `data/` and writes output to
`writing/floats/`. Both directories are excluded from version control.

## Data availability

The input data are not included in this repository. The response-time
measurements underlying the figures are available as additional files
accompanying the manuscript. The Hamburg City Health Study whole-genome
sequencing data underlying GRETA are not publicly available, due to data
protection regulations.

## Citation

If you use this code, please cite the accompanying manuscript:

> Riccio C, Dhabalia Ashok A, Guo L, Koliopanos G, Zeller T, Twerenbold R,
> Ziegler A. GRETA: a results database for whole-genome sequencing studies.
> *[Journal]*. [Year];[Volume]:[Pages]. doi:[DOI]

## Licence

MIT — see [LICENSE](LICENSE).

## Contact

Cardio-CARE AG, Medizincampus Davos, Davos, Switzerland.