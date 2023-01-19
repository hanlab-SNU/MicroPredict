MicroImpute
===================

Introduction
------------
`MicroImpute` is a R package for imputing whole metagenomic shotgun sequencing abundance data using the 16S amplicon sequencing abundance data.

Installation
------------
Before download `MicroImpute` package, please install following three R packages first.

``` r
> install.packages("devtools")
> install.packages("data.table")
> install.packages("lme4")
```

Then, you can directly install the released version of `MicroImpute` package from Github with:

``` r
> devtools::install_github("hanlab-SNU/MicroImpute")
> library(MicroImpute)
```

Usage
------------
``` r
## Display example data for microimpute 
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


## Imputation
> imputed <- microimpute(X=smpdata_input, preprocess=TRUE, normalized=TRUE)

## or
> imputed <- microimpute(X=smpdata, meta=metadata, preprocess=FALSE, normalized=FALSE) 

## you can provide the input-form / matrix file path
> imputed <- microimpute(X="abd_filepath", meta="meta_filepath", preprocess=FALSE, normalized=FALSE, sep=",") 

```


Citation
----------
If you use `MicroImpute`, please cite [Jang et al. MicroImpute: Imputing the whole metagenomic shotgun sequencing data using the 16S amplicon sequencing data (under review) (2023)](www.)


Support
----------
This software was implemented by Chloe Soohyun Jang. Please contact soohyun9749@snu.ac.kr
