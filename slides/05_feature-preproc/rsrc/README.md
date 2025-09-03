# R Scripts for Feature Preprocessing Slides

This folder contains R scripts extracted from the original R Markdown files in the `rmds` folder. The scripts have been converted to support the LaTeX presentation format.

## Files

### Individual Scripts
- `intro_fe.R` - Feature engineering introduction and importance comparison
- `target_transformation.R` - Target transformation effects (log transformation, distributions, benchmarks)
- `feature_transformations.R` - Feature scaling effects on k-NN performance
- `categorical_encoding_dummy.R` - One-hot vs dummy encoding comparison
- `target_encoding.R` - Target/impact encoding with regularization examples
- `ames_dataset.R` - Ames housing dataset overview and target distribution
- `data_leakage.R` - Train-test leakage demonstration

### Master Script
- `master_feature_preprocessing.R` - Combines all individual scripts and provides utility functions

## Generated Plots

The scripts generate the following PDF plots:
- `fe_importance_comparison.pdf` - Comparison of methods with/without feature engineering
- `target_distribution_comparison.pdf` - Original vs log-transformed target distributions
- `target_prediction_comparison.pdf` - Prediction scatter plots for original vs log scale
- `target_transformation_benchmark.pdf` - Benchmark comparison of transformation methods
- `feature_scaling_benchmark.pdf` - k-NN performance with different scaling methods
- `categorical_encoding_comparison.pdf` - Performance comparison of encoding methods
- `target_encoding_example.pdf` - Target encoding values for foundation feature
- `regularized_target_encoding.pdf` - Original vs regularized target encoding comparison
- `ames_target_distribution.pdf` - Histogram of Ames housing sale prices

## Usage

To run all scripts:
```r
source("master_feature_preprocessing.R")
```

To run individual scripts:
```r
source("intro_fe.R")
source("target_transformation.R") 
source("feature_transformations.R")
source("categorical_encoding_dummy.R")
source("target_encoding.R")
source("ames_dataset.R")
source("data_leakage.R")
```

## Dependencies

Required R packages:
- ggplot2
- dplyr
- knitr
- rprojroot
- readr
- magrittr
- tibble
- gridExtra
- tidyr

## Notes

- Scripts assume the working directory is set to the rsrc folder
- Data files should be available in the `../../../data/` directory  
- Generated plots are saved in the `../figure/` directory
- The `adjust_path()` function should be available in the environment
- Some scripts create synthetic benchmark data where original mlr-based results are not available
