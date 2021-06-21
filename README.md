# Survival analysis of dairy cows in the Netherlands under altering agricultural policy

Version 0.1
[Published Article](https://www.sciencedirect.com/science/article/pii/S0167587721001422)</br>

Abstract: Culling of underperforming dairy cows by replacement heifers is a fundamental part of Dutch dairy farm management. Changes in national agricultural policies can influence farmers’ culling decisions. The objective of this study was to analyse the relevancy of cow-level risk factors for survival of Dutch dairy cows under perturbations due to national policy changes related to the -milk quota abolishment of 2015 and the phosphate regulations since 2017. For this purpose, an accelerated failure time model was fitted on-longitudinal dairy cows’ data at national level covering the period 2009−2019. The associated cow-level risk factors for culling such as lactation value (relative production level), parity number, rolling average of inseminations over all parities, very high fat-protein ratio (highFPR) and very low fat-protein ratio (lowFPR) in early lactation, test-day somatic cell count, were fitted in the model. Along with these, a factor representing three target policy periods, namely Milk Quota period (MQ), Post-Milk Quota period (PMQ) and Phosphate regulation period (PH) were fitted. The mean survival age for all producing cows was 441 weeks overall. The predicted median survival time for the policy periods MQ, PMQ and PH were 273 weeks, 271 weeks and 256 weeks, respectively. Risk factors such as lactation value, parity and highFPR, rolling average of inseminations over all parities were positively associated with survival time in all three policy periods. Risk factors such as test-day somatic cell count and lowFPR were negatively associated with survival time in all three policy periods. In conclusion, this study demonstrated the differences in survival of Dutch dairy cows in response to changing agricultural policy. The association of cow-level risk factors for culling was consistent across the three evaluated policy periods.


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
