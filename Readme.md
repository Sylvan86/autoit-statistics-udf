This UDF provides functionality for statistical hypothesis testing, descriptive sample statistics, propability distributions and numerical mathematics:

*   probability distributions (probality density, cumulative density and quantile function) for:
    *   continuous uniform distribution
    *   gaussian normal distribution (also standard normal distribution) + random number generation
    *   F-distribution
    *   Student-t-distribution
    *   Beta-distribution
    *   Chi²-distribution
    *   Kolmogorov-Smirnov distribution
*   statistical hypothesis tests:
    *   one sample Kolmogorov-Smirnov-test
    *   Lilliefors-Test
    *   unpaired two sample student's t-test
    *   unpaired two sample Welch-test
    *   F-test for variance equality
    *   Wilcoxon-Mann-Whitney-Test
    *   combined test with condition check for these tests
*   descriptive statistics:
    *   arithmetic mean, modal value, median
    *   standard deviation, average deviation, standard error of mean, variance, skewness, curtosis
    *   minima, maxima, range, sum, length
    *   quantiles
    *   ranks
    *   histogram
*   numerical mathematics:
    *   single parameter function solver and root finding
    *   single parameter integration
*   basic funtions:
    *   Gamma-function (gamma, log-gamma, inverse gamma, incomplete gamma)
    *   Gauss error function, complementary Gauss error function, inverse complementary Gauss error function
    *   Beta-function (beta, incomplete beta, regularized incomplete beta, inversed regularized incomplete beta
    *   inverse of x \* log(x)

What can you do with it?  
Well, you could for example check if the AutoIt-Random function really returns evenly distributed values:

<details>
<summary>statistical test if AutoIt-Random produces uniformly distributed values</summary>

```AutoIt
#include "Stat.au3"

; create AutoIt sample data
    Local Const $N = 1e3
    Local $a_Sample[1e3]
    Local $lower = 0, $upper = 100
    For $i = 0 To $N -1
        $a_Sample[$i] = Random($lower, $upper)
    Next

; Test for uniform distribution:
    Local $a_Params = ["CallArgArray", 0, $lower, $upper]
    $a_Ret = _stat_test_KS($a_Sample, 0.05, _stat_uniform_cdf, $a_Params)
    _ArrayDisplay($a_Ret, "Is Random() uniformly distributed?")
    MsgBox(0, "Is Random() uniformly distributed?", ($a_Ret[5][1] ? "Random is uniformly distributed" : "Random is NOT uniformly distributed"))
```

</details>



Or you can check whether two samples have the same mean value (with normal distribution) or the same central tendency.  
The function automatically checks the respective prerequisites for the internal tests:

<details>
<summary>test if two samples have equal values on average</summary>

```AutoIt
#include "Stat.au3"

; Test whether two samples have the same mean values (with normal distribution) or central tendencies.
    Local $a_Sample1[1000], $a_Sample2[1000]
    For $i = 0 To 999
    ; generates normally distributed samples (for t- and Welch-test)
        $a_Sample1[$i] = _stat_norm_ran(100, 5)
        $a_Sample2[$i] = _stat_norm_ran(99.0, 4.5)
    ; generates non-normally distributed samples (for Wilcoxon blablabla test)
        ;  $a_Sample1[$i] = Random(0,100)
        ;  $a_Sample2[$i] = Random(10,90)
    Next

    $a_Results = _stat_test_SampleEquality($a_Sample1, $a_Sample2)
    _ArrayDisplay($a_Results, "test results")
    MsgBox(0, "", $a_Results[4][1] ? "Samples have equal values on average" : "Samples have different values on average")

; ========== Test pattern of _stat_test_SampleEquality =============
;   If both samples are normally distributed (Lilliefors test):
;       If variances are equal (F-test):
;            t-test
;       else:
;            Welch-Test
;   else:
;       Wilcoxon-Mann-Whitney-Test
```

</details>

