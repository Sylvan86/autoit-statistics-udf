#include-once
#include "Stat_Tests.au3"
#include <Array.au3>

; #INDEX# =======================================================================================================================
; Title .........: Stat
; AutoIt Version : 3.3.14.2
; Language ......: English/German
; Description ...: functionality for statistical hypothesis testing, descriptive sample statistics, propability distributions and
;                  numerical mathematics
; Author(s) .....: AspirinJunkie
; License .......: This work is free.
;                  You can redistribute it and/or modify it under the terms of the Do What The Fuck You Want To Public License, Version 2,
;                  as published by Sam Hocevar.
;                  See http://www.wtfpl.net/ for more details.
; ===============================================================================================================================

; #CURRENT# =====================================================================================================================
;
; · probability distributions (probality density, cumulative density and quantile function) for:
;     - continuous uniform distribution
;     - gaussian normal distribution (also standard normal distribution) + random number generation
;     - F-distribution
;     - Student-t-distribution
;     - Beta-distribution
;     - Chi²-distribution
;     - Kolmogorov-Smirnov distribution
;
; · statistical hypothesis tests:
;     - one sample Kolmogorov-Smirnov-test
;     - Lilliefors-Test
;     - unpaired two sample student's t-test
;     - unpaired two sample Welch-test
;     - F-test for variance equality
;     - Wilcoxon-Mann-Whitney-Test
;     - combined test with condition check for these tests
;
; · descriptive statistics:
;     - arithmetic mean, modal value, median
;     - standard deviation, average deviation, standard error of mean, variance, skewness, curtosis
;     - minima, maxima, range, sum, length
;     - quantiles
;     - ranks
;     - histogram
;
; · numerical mathematics:
;     - single parameter function solver and root finding
;     - single parameter integration
;
; · basic funtions:
;     - Gamma-function (gamma, log-gamma, inverse gamma, incomplete gamma)
;     - Gauss error function, complementary Gauss error function, inverse complementary Gauss error function
;     - Beta-function (beta, incomplete beta, regularized incomplete beta, inversed regularized incomplete beta
;     - inverse of x * log(x)
;
; ===============================================================================================================================




#Region Main-Function
; examples
If @ScriptName = "Stat.au3" Then

;~ ; Stichprobe erzeugen
;~ 	Local Const $N = 1e4
;~ 	Local $a_Sample[1e4]
;~ 	Local $lower = 0, $upper = 100
;~ 	For $i = 0 To $N -1
;~ 		$a_Sample[$i] = Random($lower, $upper)
;~ 	Next

;~ ; Auf Gleichverteilung testen:
;~ 	Local $a_Params = ["CallArgArray", 0, $lower, $upper]
;~ 	$a_Ret = _stat_test_KS($a_Sample, 0.05, _stat_uniform_cdf, $a_Params)
;~ 	_ArrayDisplay($a_Ret, "Ist Random() gleichverteilt?")
;~ 	MsgBox(0, "Ist Random() gleichverteilt?", ($a_Ret[5][1] ? "Random() ist gleichverteilt" : "Random ist nicht gleichverteilt"))



;~ 	; F-Test
;~ 	Local $a_FTest_1[] = [560, 530, 570, 490, 510, 550, 550, 530]
;~ 	Local $a_FTest_2[] = [600, 590, 590, 630, 610, 630]
;~ 	$a_FTest = _stat_test_F($a_FTest_1, $a_FTest_2)
;~ 	_ArrayDisplay($a_FTest, "Ergebnisse F-Test")


;~ 	; t-Test
;~ 	Local $a_FTest_1[] = [560, 530, 570, 490, 510, 550, 550, 530]
;~ 	Local $a_FTest_2[] = [600, 590, 590, 630, 610, 630]
;~ 	$a_T_Test = _stat_test_T_twoSample($a_FTest_1, $a_FTest_2, Default, Default, Default, Default, 0.05, 0, "two.sided")
;~ 	_ArrayDisplay($a_T_Test, "Zweistichproben-t-Test für unabhängige Stichproben")

;~ 	; Welch-Test
;~ 	Local $a_FTest_1[] = [560, 530, 570, 490, 510, 550, 550, 530]
;~ 	Local $a_FTest_2[] = [600, 590, 590, 630, 610, 630]
;~ 	$a_Welch_Test = _stat_test_Welch($a_FTest_1, $a_FTest_2, Default, Default, Default, Default, 0.05, 0, "less")
;~ 	_ArrayDisplay($a_Welch_Test, "Welch-Test")

	; Kolmogorov-Smirnoff-Test (bzw. Lilliefors-Test falls Parameter empirisch) auf Normalverteilung:
;~ 	Local $a_Test[] = [9.41, 9.92, 11.55, 11.6, 11.73, 12.0, 12.06, 13.02]
;~ 	$a_Ret = _stat_test_KS($a_Test)
;~ 	_ArrayDisplay($a_Ret, "Kolmogorov-Smirnoff-Test")

	; Wilcoxon-Mann-Whitney-Test
;~ 	Local $a_FTest_1[] = [115, 146, 132, 136, 120, 127, 134]
;~ 	Local $a_FTest_2[] = [150, 155, 135, 144, 160, 137]
;~ 	$a_Ret = _stat_test_wilcox($a_FTest_1, $a_FTest_2)
;~ 	_ArrayDisplay($a_Ret, "Wilcoxon-Mann-Whitney-Test")

;~ 	; Teste ob zwei Stichproben im Mittel gleiche Werte besitzen
;~ 	Local $a_Sample1[1000], $a_Sample2[1000]
;~ 	For $i = 0 To 999
;~ 		$a_Sample1[$i] = _stat_norm_ran(100, 5)
;~ 		$a_Sample2[$i] = _stat_norm_ran(99.0, 4.5)
;~ 		;$a_Sample1[$i] = Random(0,100)
;~ 		;$a_Sample2[$i] = Random(10,90)
;~ 	Next
;~ 	$a_Results = _stat_test_SampleEquality($a_Sample1, $a_Sample2)
;~ 	_ArrayDisplay($a_Results[6][1], "test results")



EndIf
#EndRegion Main-Function




