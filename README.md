
# NMstats

<!-- badges: start -->
<!-- badges: end -->

NMstats is an R package that includes functions that align to the Navidi/Monk Elementary and Essentials program.

## Installation

You can install the development version of NMstats from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("bjmonk/NMstats")
```
or with:

``` r
# install.packages("devtools")
devtools::install_url("https://barrymonk.com/NMstats_1.1.2.tar.gz")
```

## Usage

NMstats includes the following functions:


### `var.p`
The `var.p` function calculates the population variance of a data set.

### `sd.p`
The `sd.p` function calculates the population standard deviation of a data set.

### `combs`
The `combs` function calculates the number of ways to choose `r` items from a total of `n` items without replacement and where the order does not matter.

### `perms` 
The `perms` function calculates the number of ways to choose `r` items from a total of `n` items without replacement and where the order matters.

### `outlier_bounds` 
The `outlier_bounds` function computes the lower and upper outlier boundaries of a dataset using the IQR method.

### `data_rage`
The `data_range` function computes the range of a data set.

### `rel_hist`
The `rel_hist` function constructs a relative frequency histogram of given data values.

### `Z_Interval`
The `Z_Interval` function calculates the confidence interval for a population mean when the population standard deviation is known.

### `T_Interval`
The `T_Interval` function calculates the confidence interval for a population mean when the population standard deviation is not known.

### `One_Prop_Int`
The `One_Prop_Int` function calculates the confidence interval for a population proportion.

### `Z_Test`
The `Z_Test` function performs a hypothesis test about a population mean when the population standard deviation is known.

### `T_Test`
The `T_Test` function performs a hypothesis test about a population mean when the population standard deviation is not known.

### `One_Prop_Test`
The `One_Prop_Test` function performs a one-sample hypothesis test for a population proportion.

### `Two_Samp_T_Interval`
The `Two_Samp_T_Interval` function constructs a confidence interval for the difference between two population means (`mu_1 - mu_2`) given two independent samples. 

### `Two_Samp_T_Test`
The `Two_Samp_T_Test` function performs a hypothesis test about the difference between two population means (`mu_1 - mu_2`) given two independent samples.

### `Two_Prop_Int`
The `Two_Prop_Int` function calculates the confidence interval for the difference between two population proportions (`p_1 - p_2`).



## Examples

See [How To Use NMstats](https://barrymonk.com/how_to_use_NMstats.html)
