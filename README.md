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
```

| Script | Purpose |
|---|---|
| `plot_response_times.R` | Figure 4: response times for each of the 17 workflow steps by access method (Web, CLI, GUI, SQL), showing the five replicates with their mean ± one standard deviation on a log scale; also writes the figure caption |
| `plot_warmup_effect.R` | Supplementary Figure 1: response times of the first replicate relative to steady state, across all step × method series |

## Requirements

R ≥ 4.1 and the following packages:

```r
utils::install.packages(c("here", "readxl", "dplyr", "tidyr",
                          "ggplot2", "scales", "patchwork"))
```

`grid`, also used by `plot_response_times.R`, ships with R and needs no
installation.

### Session information

The figures in the manuscript were produced with the following R session:

```text
> sessionInfo()
R version 4.5.1 (2025-06-13 ucrt)
Platform: x86_64-w64-mingw32/x64
Running under: Windows 11 x64 (build 26200)

Matrix products: default
  LAPACK version 3.12.1

locale:
[1] LC_COLLATE=German_Germany.utf8  LC_CTYPE=German_Germany.utf8    LC_MONETARY=German_Germany.utf8 LC_NUMERIC=C                    LC_TIME=German_Germany.utf8    

time zone: Europe/Zurich
tzcode source: internal

attached base packages:
[1] grid      stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1] patchwork_1.3.2 scales_1.4.0    ggplot2_4.0.0   tidyr_1.3.2     dplyr_1.1.4     readxl_1.4.5    here_1.0.2     

loaded via a namespace (and not attached):
 [1] vctrs_0.6.5        cli_3.6.5          rlang_1.1.6        purrr_1.2.2        generics_0.1.4     S7_0.2.0           glue_1.8.0         rprojroot_2.1.1    cellranger_1.1.0   tibble_3.3.0      
[11] lifecycle_1.0.4    compiler_4.5.1     RColorBrewer_1.1-3 pkgconfig_2.0.3    rstudioapi_0.17.1  farver_2.1.2       R6_2.6.1           tidyselect_1.2.1   pillar_1.11.1      magrittr_2.0.4    
[21] tools_4.5.1        withr_3.0.2        gtable_0.3.6
```

## Data availability

The response-time measurements underlying the figures are available as additional files
accompanying the manuscript.

## Citation

If you use this code, please cite the accompanying manuscript:

> Riccio C, Dhabalia Ashok A, Guo L, Koliopanos G, Zeller T, Twerenbold R,
> Ziegler A. GRETA: a results database for whole-genome sequencing studies.
> *[Journal]*. [Year];[Volume]:[Pages]. doi:[DOI]

## Licence

MIT — see [LICENSE](LICENSE).

## Contact

Cardio-CARE AG, Medizincampus Davos, Davos, Switzerland.
