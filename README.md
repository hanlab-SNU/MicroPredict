MicroPredict
===================

Introduction
------------
`MicroPredict` is a R package for imputing whole metagenomic shotgun sequencing abundance data using the 16S amplicon sequencing abundance data.

Installation
------------
Before download `MicroPredict` package, please install following three R packages first.

``` r
> install.packages("devtools")
> install.packages("data.table")
> install.packages("lme4")
```

Then, you can directly install the released version of `MicroPredict` package from Github with:

``` r
> devtools::install_github("hanlab-SNU/MicroPredict")
> library(MicroPredict)
```

Usage
------------
``` r
## Display example data for micropredict
> library(MicroPredict)
### input form data
> head(smpdata_input)
  Sample                  Species exp_16s cov1_cm cov2_tp
1  smp_1            Absiella_argi       0       C       1
2  smp_1        Absiella_dolichum       0       C       1
3  smp_1        Acinetobacter_sp.       0       C       1
4  smp_1   Acinetobacter_ursingii       0       C       1
5  smp_1 Actinomyces_graevenitzii       0       C       1
6  smp_1    Actinotignum_schaalii       0       C       1


### abd data
> head(smpdata)
                         smp_1 smp_2 smp_3 smp_4 smp_5 smp_6 smp_7 smp_8 smp_9
Absiella_argi                0 2.554     0     0     0     0     0     0     0
Absiella_dolichum            0 0.000     0     0     0     0     0     0     0
Acinetobacter_sp.            0 0.000     0     0     0     0     0     0     0
Acinetobacter_ursingii       0 0.000     0     0     0     0     0     0     0
Actinomyces_graevenitzii     0 0.000     0     0     0     0     0     0     0
Actinotignum_schaalii        0 0.000     0     0     0     0     0     0     0
                         smp_10
Absiella_argi                 0
Absiella_dolichum             0
Acinetobacter_sp.             0
Acinetobacter_ursingii        0
Actinomyces_graevenitzii      0
Actinotignum_schaalii         0

### meatadata
> head(metadata)
  Sample cov1_cm cov2_tp
1  smp_1       C       1
2  smp_2       M       2
3  smp_3       C       2
4  smp_4       C       5
5  smp_5       M       2
6  smp_6       M       2


## Perform MicroPredict
> predicted <- micropredict(X=smpdata_input, preprocess=TRUE, normalized=TRUE, imputation=TRUE)

## Perform MicroPredict without preprocess or normalized
> predicted <- micropredict(X=smpdata, meta=metadata, preprocess=FALSE, normalized=FALSE, imputation=TRUE)

## Perform MicroPredict without imputing abundances of WGS only species
> predicted <- micropredict(X=smpdata, meta=metadata, preprocess=TRUE, normalized=TRUE, imputation=FALSE)

## you can provide the input-form / matrix file path
> imputed <- micropredict(X="abd_filepath", meta="meta_filepath", preprocess=FALSE, normalized=FALSE, imputation=TRUE, sep=",") 
### The input and the predicted dataframes are all log-transformed abundances 

```


Citation
----------
If you use `MicroPredict`, please cite [Jang et al. MicroPredict: MicroPredict: Predicting species-level taxonomic abundance of whole-shotgun metagenomic data only using 16S amplicon sequencing data (under review) (2024)](www.)


Support
----------
This software was implemented by Chloe Soohyun Jang. Please contact soohyun9749@snu.ac.kr
