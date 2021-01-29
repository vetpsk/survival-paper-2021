# Survival analysis of dairy cows in the Netherlands under altering agricultural policy

Version 0.1

Changes in agricultural policy such as milk-quota abolishment impact herd size and might influence culling decisions of dairy farmers. The purpose of this study was to develop a modelling strategy for studying the relevant cow-level risk factors for survival of Dutch dairy cows representing production, reproduction and metabolic health performances under perturbations due to agricultural policy changes. 


## Links

[Data edit](/src/data-edit.html) <br/>
[Main_analysis](/results/output/Main_analysis.md) <br/>
[Documentation folder](/docs/) <br/>

## Project organization

```
.
├── .gitignore
├── CITATION.md
├── LICENSE.md
├── README.md
├── requirements.txt
├── bin                <- Compiled and external code, ignored by git 
│   └── external       <- Any external source code, ignored by git 
├── config             <- Configuration files 
├── data               <- All project data, ignored by git
│   ├── models
│       ├── ModelOut   <- All created models in .Rdata format
│   ├── raw            <- The original, immutable data dump. 
│   └── inter          <- Intermediate data that has been transformed. 
├── docs               <- Documentation notebook for users 
│   ├── HPC            <- Documentation regarding HPC (high performance computation)
│   ├── manuscript     <- Manuscript source, e.g., LaTeX, pdf, docx, etc. 
│   └── reports        <- Other project reports and notebooks
├── results
│   ├── figures        <- Figures for the manuscript or reports 
│   └── output         <- Other output for the manuscript or reports (RMD, md, html, etc.)
│       └── Old_output
│       └── SlurmOut_copies
└── src                <- Source code for this project (.sh files, .R, etc.) 
    └── Old_analysis   <- Source code for older model tryouts
    └── data_edit      <- Source code for data filtering, editing, etc.
    └── Main~.analysis <- Main modelling code

```


## License

This project is licensed under the terms of the [MIT License](/LICENSE.md)

## Citation

Please [cite this project as described here](/CITATION.md).
