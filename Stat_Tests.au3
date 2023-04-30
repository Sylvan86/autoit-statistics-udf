#include-once
#include "Stat_Distributions.au3"
#include "Stat_ListFunctions.au3"
#include <Array.au3>

; main function
If @ScriptName = "Stat_Tests.au3" Then
;  ; F-Test
;  Local $a_FTest_1[] = [560, 530, 570, 490, 510, 550, 550, 530]
;  Local $a_FTest_2[] = [600, 590, 590, 630, 610, 630]
;  $a_FTest = _stat_test_F($a_FTest_1, $a_FTest_2)
;  _ArrayDisplay($a_FTest, "Ergebnisse F-Test")


; t-Test
Local $a_FTest_1[] = [560, 530, 570, 490, 510, 550, 550, 530]
Local $a_FTest_2[] = [600, 590, 590, 630, 610, 630]
$a_T_Test = _stat_test_T_twoSample($a_FTest_1, $a_FTest_2, Default, Default, Default, Default, 0.05, 0, "two.sided")
_ArrayDisplay($a_T_Test, "Zweistichproben-t-Test für unabhängige Stichproben")

; Welch-Test
Local $a_FTest_1[] = [560, 530, 570, 490, 510, 550, 550, 530]
Local $a_FTest_2[] = [600, 590, 590, 630, 610, 630]
$a_Welch_Test = _stat_test_Welch($a_FTest_1, $a_FTest_2, Default, Default, Default, Default, 0.05, 0, "less")
_ArrayDisplay($a_Welch_Test, "Welch-Test")

; Kolmogorov-Smirnoff-Test (bzw. Lilliefors-Test falls Parameter empirisch) auf Normalverteilung:
Local $a_Test[] = [9.41, 9.92, 11.55, 11.6, 11.73, 12.0, 12.06, 13.02]
$a_Ret = _stat_test_KS($a_Test)
_ArrayDisplay($a_Ret, "Kolmogorov-Smirnoff-Test")

;~ 	; Wilcoxon-Mann-Whitney-Test
Local $a_FTest_1[] = [115, 146, 132, 136, 120, 127, 134]
Local $a_FTest_2[] = [150, 155, 135, 144, 160, 137]
$a_Ret = _stat_test_wilcox($a_FTest_1, $a_FTest_2)
_ArrayDisplay($a_Ret, "Wilcoxon-Mann-Whitney-Test")

; Teste ob zwei Stichproben im Mittel gleiche Werte besitzen
Local $a_Sample1[1000], $a_Sample2[1000]
For $i = 0 To 999
	$a_Sample1[$i] = _stat_norm_ran(100, 5)
	$a_Sample2[$i] = _stat_norm_ran(99.0, 4.5)
	;$a_Sample1[$i] = Random(0,100)
	;$a_Sample2[$i] = Random(10,90)
Next
$a_Results = _stat_test_SampleEquality($a_Sample1, $a_Sample2)
_ArrayDisplay($a_Results, "test results")

__stat_EPS()
ConsoleWrite('@@ Debug(' & @ScriptLineNumber & ') : __stat_EPS() = ' & __stat_EPS() & @CRLF & '>Error code: ' & @error & @CRLF) ;### Debug Console

EndIf
#Region parametrische Tests


#Region Tests auf Verteilungen
; #FUNCTION# ======================================================================================
; Name ..........: _stat_test_KS_NV()
; Description ...: one sample Kolmogorov-Smirnov-test (or Lilliefors-test) for comparing a sample with a reference distribution
; Syntax ........: _stat_test_KS_NV($a_Samples[, $alpha = 0.05[, $cb_Dist = _stat_norm_cdf[, $a_DistProbs = Default[, $alternative = "two.sided"[, $b_Lilliefors = False]]]]])
; Parameters ....: $a_Samples    - zero-based 1D array with sample data
;                  $alpha        - [optional] probability of error (default:0.05)
;                  $cb_Dist      - [optional] reference distribution as a cdf-function variable (default:_stat_norm_cdf)
;                  $a_DistProbs  - [optional] parameter-array for the reference cdf-function (see Call() second parameter) (default:Default)
;                                  if Default: parameters of normal distribution are derived from the sample data and Lilliefors-test is used instead of ks-test
;                  $alternative  - [optional] H0 ("two.sided", "greater", "less") (default:"two.sided")
;                  $b_Lilliefors - [optional] if True: Lilliefors-test is used instead of ks-test (default:False)
; Return values .: Success: 2D array with result parameters: $Array[...][2] = [[name1, value1], ..., [nameX, valuex]]
;                  Failure: "" and set @error
; Author ........: AspirinJunkie
; Remarks .......: if parameters for the normal distribution are empirically derived from the sample then the p-value has to be calculated with the Lilliefors-test instead (see parameter $b_Lilliefors)
; Example .......: Yes
;                  Local $a_Test[] = [9.41, 9.92, 11.55, 11.6, 11.73, 12.0, 12.06, 13.02]
;                  $a_Ret = _stat_test_KS($a_Test)
;                  _ArrayDisplay($a_Ret, "Kolmogorov-Smirnoff-Test")
; =================================================================================================
Func _stat_test_KS($a_Samples, $alpha = 0.05, $cb_Dist = _stat_norm_cdf, $a_DistProbs = Default, $alternative = "two.sided", $b_Lilliefors = False)
	If Not IsArray($a_Samples) Then Return SetError(1, 0, "")
	_ArraySort($a_Samples)
	Local $F_n, $Phi, $doi, $dui, $dmax = 0
	Local $N = UBound($a_Samples)

	If $a_DistProbs <> Default And ((Not IsArray($a_DistProbs)) Or UBound($a_DistProbs) < 2 Or $a_DistProbs[0] <> "CallArgArray") Then Return SetError(2, 0, "")
	Local $a_DistOps = $a_DistProbs

	If FuncName($cb_Dist) = "_stat_norm_cdf" Then
		If $a_DistProbs = Default Then
			; hier Lilliefors-Test da Parameter empirisch ermittelt wurden (Verlust von Freiheitsgraden)
			$b_Lilliefors = True
			Local $a_Params = _stat_Sample_Parameter($a_Samples)
			Local $a_DistOps[4] = ["CallArgArray", 0, $a_Params[0], $a_Params[1]]
		EndIf
	Else
		If $a_DistProbs = Default Then Return SetError(3, 0, "")
	EndIf


	For $i = 0 To $N - 1
		$F_n = ($i + 1) * (1 / $N)
		$a_DistOps[1] = $a_Samples[$i]
		$Phi = Call($cb_Dist, $a_DistOps)
		$dui = ($i) * (1 / $N) - $Phi
		$doi = $F_n - $Phi

		If Abs($dui) > Abs($dmax) Then $dmax = $dui
		If Abs($doi) > Abs($dmax) Then $dmax = $doi
	Next

	If $b_Lilliefors Then
		; approximiere p-Wert für Lilliefors-Test
		Local $ddmax = Abs($dmax)
		If $N > 100 Then
			$ddmax *= ($N / 100) ^ 0.49
			$N = 100
		EndIf
		Local $p_Value = Exp(-7.01256 * $ddmax ^ 2 * ($N + 2.78019) + 2.99587 * $ddmax * Sqrt($N + 2.78019) - 0.122119 + 0.974598 / Sqrt($N) + 1.67997 / $N)

	Else
		Local $p_Value = _stat_ks_getProb($N, Abs($dmax)) * 2
	EndIf

	Local $a_Ret[8][2] = [ _
			["|D|", Abs($dmax)], _
			["n", $N], _
			["p-value", $p_Value], _
			['H' & ChrW(8320), "F_x = F_0"], _
			['H' & ChrW(8321), "F_x != F_0"], _
			["accept H" & ChrW(8320) & "?", $p_Value >= $alpha], _
			[ChrW(945), StringFormat("%g %%", $alpha * 100)], _
			["method", ($b_Lilliefors ? "Lilliefors-Test" : "Kolmogorov-Smirnov-test (one-sample)")] _
			]
	Return $a_Ret
EndFunc   ;==>_stat_test_KS
#EndRegion Tests auf Verteilungen


#Region Tests auf Mittelwerte
#Region T-Test
; #FUNCTION# ======================================================================================
; Name ..........: _stat_test_T_twoSample()
; Description ...: unpaired two sample student's t-test for comparing two empirical sample means
; Syntax ........: _stat_test_T_twoSample($x1, $x2[, $s1 = Default[, $s2 = Default[, $n1 = Default[, $n2 = Default[, $alpha = 0.05[, $omega0 = 0[, $alternative = "two.sided"]]]]]]])
; Parameters ....: $x1          - empirical mean of the first sample data
;                  |if Array: x1, s1 and n1 are derived from this sample data array
;                  $x2          - empirical mean of the second sample data
;                  |if Array: x2, s2 and n2 are derived from this sample data array
;                  $s1          - [optional] variance of the first sample (not standard deviation) (default:Default)
;                  $s2          - [optional] variance of the second sample (not standard deviation) (default:Default)
;                  $n1          - [optional] population of the first sample (default:Default)
;                  $n2          - [optional] population of the second sample (default:Default)
;                  $alpha       - [optional] probability of error (default:0.05)
;                  $omega0      - [optional] expected difference (x1-x2) to test for (equality of x1 and x2 means omega0 = 0) (default:0)
;                  $alternative - [optional] H0 ("two.sided", "greater", "less") (default:"two.sided")
; Return values .: Success: 2D array with result parameters: $Array[...][2] = [[name1, value1], ..., [nameX, valuex]]
;                  Failure: "" and set @error
; Author ........: AspirinJunkie
; Remarks .......: need equal variances for the two samples - if not: use Welch-test
; Example .......: Yes
;                  Local $a_FTest_1[] = [560, 530, 570, 490, 510, 550, 550, 530]
;                  Local $a_FTest_2[] = [600, 590, 590, 630, 610, 630]
;                  $a_T_Test = _stat_test_T_twoSample($a_FTest_1, $a_FTest_2, Default, Default, Default, Default, 0.05, 0, "two.sided")
;                  _ArrayDisplay($a_T_Test, "Zweistichproben-t-Test für unabhängige Stichproben")
; =================================================================================================
Func _stat_test_T_twoSample($x1, $x2, $s1 = Default, $s2 = Default, $n1 = Default, $n2 = Default, $alpha = 0.05, $omega0 = 0, $alternative = "two.sided")
	If IsArray($x1) Then
		$n1 = UBound($x1)
		Local $a_1 = _stat_Sample_Parameter($x1)
		$x1 = $a_1[0]
		$s1 = $a_1[2]
	EndIf
	If IsArray($x2) Then
		$n2 = UBound($x2)
		Local $a_1 = _stat_Sample_Parameter($x2)
		$x2 = $a_1[0]
		$s2 = $a_1[2]
	EndIf

	Local $FG = $n1 + $n2 - 2
	Local $t = ($x1 - $x2 - $omega0) / Sqrt((($n1 - 1) * $s1 + ($n2 - 1) * $s2) / $FG) * Sqrt($n1 * $n2 / ($n1 + $n2))
	Local $m_diff = $x1 - $x2
	Local $se = Sqrt((($s1 * ($n1 - 1) + $s2 * ($n2 - 1)) / $FG * (1 / $n1 + 1 / $n2)))

	Switch $alternative
		Case "greater"
			; H1: μ₁ > μ₂
			Local $error = _stat_t_icdf(1 - $alpha, $FG) * $se
			Local $t_u = $m_diff - $error
			Local $t_o = ChrW(8734)
			Local $p_Value = 1 - _stat_t_cdf($t, $FG)
		Case "less"
;~ 			; H1: μ₁ < μ₂
			Local $error = _stat_t_icdf(1 - $alpha, $FG) * $se
			Local $t_u = "-" & ChrW(8734)
			Local $t_o = $x1 - $x2 + $error
			Local $p_Value = _stat_t_cdf($t, $FG)
		Case Else ; "two.sided"
			; H1: μ₁ != μ₂
			Local $error = _stat_t_icdf(1 - $alpha / 2, $FG) * $se
			Local $t_u = $m_diff - $error
			Local $t_o = $x1 - $x2 + $error
			Local $p_Value = $t < 0 ? _stat_t_cdf($t, $FG) * 2 : (1 - _stat_t_cdf($t, $FG)) * 2 ; wie wahrscheinlich ist es eine solche Stichprobe zu ziehen unter der Annahme dass die Nullhypothese wahr ist
			; grob (aber ungenau) ~ Wahrscheinlichkeit dass Nullhypothese wahr ist
	EndSwitch

	Local $a_Ret[15][2] = [ _
			["t", $t], _
			["df", $FG], _
			["p-value", $p_Value], _
			["t_u", $t_u], _
			["t_o", $t_o], _
			['H' & ChrW(8320), ChrW(956) & ChrW(8321) & " - " & ChrW(956) & ChrW(8322) & ($alternative = "two.sided" ? " = " : $alternative = "greater" ? " <= " : " >= ") & $omega0], _
			['H' & ChrW(8321), ChrW(956) & ChrW(8321) & " - " & ChrW(956) & ChrW(8322) & ($alternative = "two.sided" ? " != " : $alternative = "greater" ? " > " : " < ") & $omega0], _
			["accept H" & ChrW(8320) & "?", $p_Value >= $alpha], _
			["mean" & ChrW(8321), $x1], _
			["mean" & ChrW(8322), $x2], _
			["s" & ChrW(8321) & "²", $s1], _
			["s" & ChrW(8322) & "²", $s2], _
			["n" & ChrW(8321), $n1], _
			["n" & ChrW(8322), $n2], _
			[ChrW(945), StringFormat("%g %%", $alpha * 100)] _
			]

	Return $a_Ret
EndFunc   ;==>_stat_test_T_twoSample
#EndRegion T-Test

#Region Welch-Test
; #FUNCTION# ======================================================================================
; Name ..........: _stat_test_Welch()
; Description ...: Welch-test for comparing two empirical sample means with different variances
; Syntax ........: _stat_test_Welch($x1, $x2[, $s1 = Default[, $s2 = Default[, $n1 = Default[, $n2 = Default[, $alpha = 0.05[, $omega0 = 0[, $alternative = "two.sided"]]]]]]])
; Parameters ....: $x1          - empirical mean of the first sample data
;                  |if Array: x1, s1 and n1 are derived from this sample data array
;                  $x2          - empirical mean of the second sample data
;                  |if Array: x2, s2 and n2 are derived from this sample data array
;                  $s1          - [optional] variance of the first sample (default:Default)
;                  $s2          - [optional] variance of the second sample (default:Default)
;                  $n1          - [optional] population of the first sample (default:Default)
;                  $n2          - [optional] population of the second sample (default:Default)
;                  $alpha       - [optional] probability of error (default:0.05)
;                  $omega0      - [optional] expected difference (x1-x2) to test for (equality of x1 and x2 means omega0 = 0) (default:0)
;                  $alternative - [optional] H0 ("two.sided", "greater", "less") (default:"two.sided")
; Return values .: Success: 2D array with result parameters: $Array[...][2] = [[name1, value1], ..., [nameX, valuex]]
;                  Failure: "" and set @error
; Author ........: AspirinJunkie
; Example .......: Yes
;                  Local $a_FTest_1[] = [560, 530, 570, 490, 510, 550, 550, 530]
;                  Local $a_FTest_2[] = [600, 590, 590, 630, 610, 630]
;                  $a_Welch_Test = _stat_test_Welch($a_FTest_1, $a_FTest_2, Default, Default, Default, Default, 0.05, 0, "less")
;                  _ArrayDisplay($a_Welch_Test, "Welch-Test")
; =================================================================================================
Func _stat_test_Welch($x1, $x2, $s1 = Default, $s2 = Default, $n1 = Default, $n2 = Default, $alpha = 0.05, $omega0 = 0, $alternative = "two.sided")
	If IsArray($x1) Then
		$n1 = UBound($x1)
		Local $a_1 = _stat_Sample_Parameter($x1)
		$x1 = $a_1[0]
		$s1 = $a_1[2]
	EndIf
	If IsArray($x2) Then
		$n2 = UBound($x2)
		Local $a_1 = _stat_Sample_Parameter($x2)
		$x2 = $a_1[0]
		$s2 = $a_1[2]
	EndIf

	Local $t = ($x1 - $x2 - $omega0) * ($s1 / $n1 + $s2 / $n2) ^ (-0.5)
	Local $nu = ($s1 / $n1 + $s2 / $n2) ^ 2 / ($s1 * $s1 * $n1 ^ (-2) / ($n1 - 1) + $s2 * $s2 * $n2 ^ (-2) / ($n2 - 1)) ; approximierter Freiheitsgrad
	Local $m_diff = $x1 - $x2
	Local $se = Sqrt($s1 / $n1 + $s2 / $n2)

	Switch $alternative
		Case "greater"
			; H1: μ₁ > μ₂
			Local $error = _stat_t_icdf(1 - $alpha, $nu) * $se
			Local $t_u = $m_diff - $error
			Local $t_o = ChrW(8734)
			Local $p_Value = 1 - _stat_t_cdf($t, $nu)
		Case "less"
;~ 			; H1: μ₁ < μ₂
			Local $error = _stat_t_icdf(1 - $alpha, $nu) * $se
			Local $t_u = "-" & ChrW(8734)
			Local $t_o = $x1 - $x2 + $error
			Local $p_Value = _stat_t_cdf($t, $nu)
		Case Else ; "two.sided"
			; H1: μ₁ != μ₂
			Local $error = _stat_t_icdf(1 - $alpha / 2, $nu) * $se
			Local $t_u = $m_diff - $error
			Local $t_o = $x1 - $x2 + $error
			Local $p_Value = $t < 0 ? _stat_t_cdf($t, $nu) * 2 : (1 - _stat_t_cdf($t, $nu)) * 2
	EndSwitch

	Local $a_Ret[15][2] = [ _
			["t", $t], _
			[ChrW(957) & " (~df)", $nu], _
			["p-value", $p_Value], _
			["t_u", $t_u], _
			["t_o", $t_o], _
			['H' & ChrW(8320), ChrW(956) & ChrW(8321) & " - " & ChrW(956) & ChrW(8322) & ($alternative = "two.sided" ? " = " : $alternative = "greater" ? " <= " : " >= ") & $omega0], _
			['H' & ChrW(8321), ChrW(956) & ChrW(8321) & " - " & ChrW(956) & ChrW(8322) & ($alternative = "two.sided" ? " != " : $alternative = "greater" ? " > " : " < ") & $omega0], _
			["accept H" & ChrW(8320) & "?", $p_Value >= $alpha], _
			["mean" & ChrW(8321), $x1], _
			["mean" & ChrW(8322), $x2], _
			["s" & ChrW(8321) & "²", $s1], _
			["s" & ChrW(8322) & "²", $s2], _
			["n" & ChrW(8321), $n1], _
			["n" & ChrW(8322), $n2], _
			[ChrW(945), StringFormat("%g %%", $alpha * 100)] _
			]

	Return $a_Ret
EndFunc   ;==>_stat_test_Welch
#EndRegion Welch-Test
#EndRegion Tests auf Mittelwerte


#Region Tests auf Varianzen
; #FUNCTION# ======================================================================================
; Name ..........: _stat_test_F()
; Description ...: F-test for test the equality of two empirical variances
; Syntax ........: _stat_test_F($s1, $s2[, $n1 = Default[, $n2 = Default, Const[ $alpha = 0.05, Const[ $alternative = "two.sided"]]]])
; Parameters ....: $s1                - variance of sample data 1 (not standard deviation!)
;                  |if Array: s1 and n1 are derived from this sample data array
;                  $s2                - variance of sample data 2 (not standard deviation!)
;                  |if Array: s2 and n2 are derived from this sample data array
;                  $n1                - [optional] sampling size 1 (default:Default)
;                  $n2                - [optional] sampling size 2 (default:Default)
;                  Const $alpha       - [optional] probability of error (default:0.05)
;                  Const $alternative - [optional] H0 ("two.sided", "greater", "less") (default:"two.sided")
; Return values .: Success: 2D array with result parameters: $Array[...][2] = [[name1, value1], ..., [nameX, valuex]]
;                  Failure: "" and set @error
; Author ........: AspirinJunkie
; Remarks .......: F-test is known to be extremely sensitive to non-normality alternatives:  Levene's test, Bartlett's test, Brown–Forsythe test
; Example .......: Yes
;                  Local $a_FTest_1[] = [560, 530, 570, 490, 510, 550, 550, 530]
;                  Local $a_FTest_2[] = [600, 590, 590, 630, 610, 630]
;                  $a_FTest = _stat_test_F($a_FTest_1, $a_FTest_2)
;                  _ArrayDisplay($a_FTest, "Ergebnisse F-Test")
; =================================================================================================
Func _stat_test_F($s1, $s2, $n1 = Default, $n2 = Default, Const $alpha = 0.05, Const $alternative = "two.sided")
	If IsArray($s1) Then
		$n1 = UBound($s1)
		$s1 = _stat_Sample_Parameter($s1)[2]
	EndIf
	If IsArray($s2) Then
		$n2 = UBound($s2)
		$s2 = _stat_Sample_Parameter($s2)[2]
	EndIf

	Local $df1 = $n1 - 1
	Local $df2 = $n2 - 1
	Local $F = $s1 / $s2 ; Verhältnis der Varianzen

	Switch $alternative
		Case "two.sided"
			; H1: s₁² != s₂²
			Local $F_krit_o = $F / _stat_f_icdf($alpha / 2, $df1, $df2)
			Local $F_krit_u = $F / _stat_f_icdf(1 - $alpha / 2, $df1, $df2)
			Local $p_Value = 2 * __stat_min(1 - _stat_f_cdf($F, $df1, $df2), _stat_f_cdf($F, $df1, $df2))

		Case "greater"
			; H1: s₁² > s₂²
			Local $F_krit_u = $F / _stat_f_icdf(1 - $alpha, $df1, $df2)
			Local $F_krit_o = ChrW(8734)
			Local $p_Value = 1 - _stat_f_cdf($F, $df1, $df2)
		Case "less"
			; H1: s₁² > s₂²
			Local $F_krit_u = 0
			Local $F_krit_o = $F / _stat_f_icdf($alpha, $df1, $df2)
			Local $p_Value = _stat_f_cdf($F, $df1, $df2)
	EndSwitch

	; Ergebnis aufbereiten
	Local $a_Ret[12][2] = [ _
			["F (ratio of variances)", $F], _
			["num df", $df1], _
			["denom df", $df2], _
			[StringFormat("%d%% interval lower", (1 - $alpha) * 100), $F_krit_u], _
			[StringFormat("%d%% interval upper", (1 - $alpha) * 100), $F_krit_o], _
			["p-value", $p_Value], _
			['H' & ChrW(8320), ChrW(963) & ChrW(8321) & ChrW(178) & ($alternative = "two.sided" ? " = " : $alternative = "greater" ? " <= " : " >= ") & ChrW(963) & ChrW(8322) & ChrW(178)], _
			['H' & ChrW(8321), ChrW(963) & ChrW(8321) & ChrW(178) & ($alternative = "two.sided" ? " != " : $alternative = "greater" ? " > " : " < ") & ChrW(963) & ChrW(8322) & ChrW(178)], _
			["accept H" & ChrW(8320) & "?", $p_Value >= $alpha], _
			["s" & ChrW(8321) & "²", $s1], _
			["s" & ChrW(8322) & "²", $s2], _
			[ChrW(945), StringFormat("%g %%", $alpha * 100)] _
			]

	Return $a_Ret
EndFunc   ;==>_stat_test_F
#EndRegion Tests auf Varianzen
#EndRegion parametrische Tests



#Region nichtparametrische Tests
; #FUNCTION# ======================================================================================
; Name ..........: _stat_test_wilcox()
; Description ...: nonparametric Wilcoxon-Mann-Whitney-Test for equality of two samples
; Syntax ........: _stat_test_wilcox($a_X, $a_Y[, $alpha = 0.05[, $alternative = "two.sided"]])
; Parameters ....: $a_X         - zero-based 1D array with sample data of the first sample
;                  $a_Y         - zero-based 1D array with sample data of the second sample
;                  $alpha       - [optional] propability of error (default:0.05)
;                  $alternative - [optional] H0 ("two.sided", "greater", "less") (default:"two.sided") (default:"two.sided")
; Return values .: Success: 2D array with result parameters: $Array[...][2] = [[name1, value1], ..., [nameX, valuex]]
;                  Failure: "" and set @error
; Author ........: AspirinJunkie
; Remarks .......: also known as U-Test, Wilcoxon rank-sum test, Mann–Whitney–Wilcoxon (MWW)
; Example .......: Yes
;                  Local $a_FTest_1[] = [115, 146, 132, 136, 120, 127, 134]
;                  Local $a_FTest_2[] = [150, 155, 135, 144, 160, 137]
;                  $a_Ret = _stat_test_wilcox($a_FTest_1, $a_FTest_2)
;                  _ArrayDisplay($a_Ret, "Wilcoxon-Mann-Whitney-Test")
; =================================================================================================
Func _stat_test_wilcox($a_X, $a_Y, $alpha = 0.05, $alternative = "two.sided")
	If Not (IsArray($a_X) And IsArray($a_Y)) Then Return SetError(1, 0, "")
	Local $nx = UBound($a_X), $ny = UBound($a_Y)
	Local $a_Data[$nx + $ny]
	For $i = 0 To $nx - 1
		$a_Data[$i] = $a_X[$i]
	Next
	For $i = $nx To $nx + $ny - 1
		$a_Data[$i] = $a_Y[$i - $nx]
	Next
	Local $a_ranks = __stat_rankdata($a_Data)

	; Rangsummen:
	Local $Rx = 0, $Ry = 0
	For $i = 0 To $nx - 1
		$Rx += $a_ranks[$i]
	Next
	For $i = $nx To $nx + $ny - 1
		$Ry += $a_ranks[$i]
	Next

	Local $Ux = $nx * $ny + ($nx * ($nx + 1)) / 2 - $Rx
	Local $Uy = $nx * $ny - $Ux
	Local $U = __stat_min($Ux, $Uy)
	Local $t = __stat_tiecorrect($a_ranks)
	Local $sd = Sqrt($t * $nx * $ny * ($nx + $ny + 1) / 12.0)
	Local $meanrank = $nx * $ny / 2

	Switch $alternative
		Case "two.sided"
			; H1: μ₁ != μ₂
			Local $p_Value = 2 * _stat_norm_cdf(Abs($U), $meanrank, $sd)
		Case "less"
			; H1: μ₁ > μ₂
			Local $p_Value = _stat_norm_cdf(Abs($Uy), $meanrank, $sd)
		Case "greater"
;~ 			; H1: μ₁ < μ₂
			Local $p_Value = _stat_norm_cdf(Abs($Ux), $meanrank, $sd)
	EndSwitch

	Local $a_Ret[8][2] = [ _
			["W", $U], _
			["p-value", $p_Value], _
			['H' & ChrW(8320), ChrW(956) & ChrW(8321) & ($alternative = "two.sided" ? " = " : $alternative = "greater" ? " >= " : " <= ") & ChrW(956) & ChrW(8322)], _
			['H' & ChrW(8321), ChrW(956) & ChrW(8321) & ($alternative = "two.sided" ? " != " : $alternative = "greater" ? " < " : " > ") & ChrW(956) & ChrW(8322)], _
			["accept H" & ChrW(8320) & "?", $p_Value >= $alpha], _
			["n" & ChrW(8321), $nx], _
			["n" & ChrW(8322), $ny], _
			[ChrW(945), StringFormat("%g %%", $alpha * 100)] _
			]

	Return $a_Ret
EndFunc   ;==>_stat_test_wilcox
#EndRegion nichtparametrische Tests


#Region zusammengesetzte Tests
; #FUNCTION# ======================================================================================
; Name ..........: _stat_test_SampleEquality()
; Description ...: combined test for equality of two sample means. Checks stepwise the conditions of sensible tests and choose respectively the best one
; Syntax ........: _stat_test_SampleEquality($a_1, $a_2[, $alpha = 0.05[, $alternative = "two.sided"]])
; Parameters ....: $a_1         - zero-based 1D-array with first sample data
;                  $a_2         - zero-based 1D-array with second sample data
;                  $alpha       - [optional] probability of error (default:0.05)
;                  $alternative - [optional] H0 ("two.sided", "greater", "less") (default:"two.sided")
; Return values .: Success: 2D array with result parameters: $Array[...][2] = [[name1, value1], ..., [nameX, valuex]]
;                  Failure: "" and set @error
; Author ........: AspirinJunkie
; Remarks .......: Use t-test -> welch-test -> U-Test if conditions for them are given
; Example .......: Yes
;                  Local $a_Sample1[100], $a_Sample2[100]
;                  For $i = 0 To 99
;                      $a_Sample1[$i] = _stat_norm_ran(100, 5)
;                      $a_Sample2[$i] = _stat_norm_ran(95, 4.5)
;                  Next
;                  $a_Results = _stat_test_SampleEquality($a_Sample1, $a_Sample2)
;                  _ArrayDisplay($a_Results, "test results")
; =================================================================================================
Func _stat_test_SampleEquality($a_1, $a_2, $alpha = 0.05, $alternative = "two.sided")
	If Not IsArray($a_1) Or Not IsArray($a_2) Then Return SetError(1, 0, "")

	; derive parameters
	Local $a_Params1 = _stat_Sample_Parameter($a_1), $a_Params2 = _stat_Sample_Parameter($a_2)
	Local $x1 = $a_Params1[0], $s1 = $a_Params1[2], $n1 = UBound($a_1)
	Local $x2 = $a_Params2[0], $s2 = $a_Params2[2], $n2 = UBound($a_2)

	; test for normality first:
	Local $a_Norm1 = _stat_test_KS($a_1, $alpha)
	Local $a_Norm2 = _stat_test_KS($a_1, $alpha)

	Local $a_Return[7][2] = [["Test", 0], ['H' & ChrW(8320), 0], ['H' & ChrW(8321), 0], ["p-value", 0], ["accept H" & ChrW(8320) & "?", 0], [ChrW(945), StringFormat("%g %%", $alpha * 100)], ["complete test results", 0]]

	If $a_Norm1[5][1] And $a_Norm1[5][1] Then ; both samples are normal distributed
		Local $a_Var = _stat_test_F($s1, $s2, $n1, $n2, $alpha)

		If $a_Var[8][1] Then ; equal variances -> t-test
			Local $a_Ret = _stat_test_T_twoSample($x1, $x2, $s1, $s2, $n1, $n2, $alpha, 0, $alternative)
			$a_Return[0][1] = "two sample unpaired t-test"
			$a_Return[1][1] = $a_Ret[5][1]
			$a_Return[2][1] = $a_Ret[6][1]
			$a_Return[3][1] = $a_Ret[2][1]
			$a_Return[4][1] = $a_Ret[7][1]
			$a_Return[6][1] = $a_Ret

		Else ; unequal variances -> Welch-Test
			Local $a_Ret = _stat_test_Welch($x1, $x2, $s1, $s2, $n1, $n2, $alpha, 0, $alternative)
			$a_Return[0][1] = "two sample unpaired Welch-test"
			$a_Return[1][1] = $a_Ret[5][1]
			$a_Return[2][1] = $a_Ret[6][1]
			$a_Return[3][1] = $a_Ret[2][1]
			$a_Return[4][1] = $a_Ret[7][1]
			$a_Return[6][1] = $a_Ret
		EndIf
	Else
		$a_Ret = _stat_test_wilcox($a_1, $a_2, $alpha)
		$a_Return[0][1] = "Wilcoxon-Mann-Whitney-Test"
		$a_Return[1][1] = $a_Ret[2][1]
		$a_Return[2][1] = $a_Ret[3][1]
		$a_Return[3][1] = $a_Ret[1][1]
		$a_Return[4][1] = $a_Ret[4][1]
		$a_Return[6][1] = $a_Ret
	EndIf
	Return $a_Return
EndFunc   ;==>_stat_test_SampleEquality
#EndRegion zusammengesetzte Tests
