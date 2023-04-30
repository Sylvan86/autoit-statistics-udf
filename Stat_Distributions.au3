#include-once
#include "Stat_Basics.au3"
#include "Stat_ListFunctions.au3"

; Main-function
If @ScriptName = "Stat_Distributions.au3" Then
;~ 	ConsoleWrite(_stat_norm_pdf(0, 0, 2) & @CRLF)
;~ 	ConsoleWrite(_stat_norm_cdf(1) & @CRLF)
;~ 	ConsoleWrite(_stat_norm_icdf(0.841344746068543) & @CRLF & @CRLF)

;~ 	ConsoleWrite(_stat_f_pdf(1, 7, 5) & @CRLF)
;~ 	ConsoleWrite(_stat_f_cdf(2.11633663366337, 7, 5) & @CRLF)
;~ 	ConsoleWrite(_stat_f_icdf(0.920529921102706, 7, 5) & @CRLF & @CRLF)

;~ 	ConsoleWrite(_stat_t_pdf(1, 10) & @CRLF)
;~ 	ConsoleWrite(_stat_t_cdf(0.7, 10) & @CRLF)
;~ 	ConsoleWrite(_stat_t_icdf(0.750056214913558, 10) & @CRLF & @CRLF)

	ConsoleWrite(_stat_chi2_pdf(2, 4) & @CRLF)
	ConsoleWrite(_stat_chi2_cdf(2, 4) & @CRLF)
	ConsoleWrite(_stat_chi2_icdf(0.264241117657115, 4) & @CRLF & @CRLF)

;~ 	ConsoleWrite(_stat_beta_pdf(0.2, 0.5, 0.5) & @CRLF)
;~ 	ConsoleWrite(_stat_beta_cdf(0.8, 0.5, 0.5) & @CRLF)
;~ 	ConsoleWrite(_stat_beta_icdf(0.704832764699133, 0.5, 0.5) & @CRLF)

;~ 	ConsoleWrite(_stat_uniform_pdf(1, 0, 3) & @CRLF)
;~ 	ConsoleWrite(_stat_uniform_cdf(2, 0, 3) & @CRLF)
;~ 	ConsoleWrite(_stat_uniform_icdf(0.6666666666666666666, 0, 3) & @CRLF & @CRLF)

;~ 	Local $N = 1e5
;~ 	Local $aSample[$N]
;~ 	For $i = 0 To $N-1
;~ 		$aSample[$i] = _stat_norm_ran(10,2)
;~ 	Next
;~ 	$a_Parameters = _stat_Sample_Parameter($aSample, True)
;~ 	_ArrayDisplay($a_Parameters)
EndIf


#Region Gleich-Verteilung
; #FUNCTION# ======================================================================================
; Name ..........: _stat_uniform_pdf()
; Description ...: propability density function for the continuous uniform distribution
; Syntax ........: _stat_uniform_pdf(Const $x, Const $a, Const $b)
; Parameters ....: Const $x -
;                  Const $a - lower bound of the distribution
;                  Const $b - upper bound of the distribution
; Return values .: Success
;                  Failure
; Author ........: AspirinJunkie
; =================================================================================================
Func _stat_uniform_pdf(Const $x, Const $a, Const $b)
	Return $x < $a Or $x > $b ? 0 : 1 / $b - $a
EndFunc   ;==>_stat_uniform_pdf

; #FUNCTION# ======================================================================================
; Name ..........: _stat_uniform_cdf()
; Description ...: cumulative density function for the continuous uniform distribution
; Syntax ........: _stat_uniform_cdf(Const $x, Const $a, Const $b)
; Parameters ....: Const $x -
;                  Const $a - lower bound of the distribution
;                  Const $b - upper bound of the distribution
; Return values .: Success
;                  Failure
; Author ........: AspirinJunkie
; =================================================================================================
Func _stat_uniform_cdf(Const $x, Const $a, Const $b)
	Return $x < $a ? 0 : ($x > $b ? 1 : ($x - $a) / ($b - $a))
EndFunc   ;==>_stat_uniform_cdf

; #FUNCTION# ======================================================================================
; Name ..........: _stat_uniform_icdf()
; Description ...: quantile function (inverse cdf) for the continuous uniform distribution
; Syntax ........: _stat_uniform_icdf(Const $p, Const $a, Const $b)
; Parameters ....: Const $p -
;                  Const $a - lower bound of the distribution
;                  Const $b - upper bound of the distribution
; Return values .: Success
;                  Failure
; Author ........: AspirinJunkie
; =================================================================================================
Func _stat_uniform_icdf(Const $p, Const $a, Const $b)
	If $p < 0 Or $p > 1 Then Return SetError(1, 0, "")
	Return $p * ($b - $a) + $a
EndFunc   ;==>_stat_uniform_icdf
#EndRegion Gleich-Verteilung

#Region Normal-Verteilung
; #FUNCTION# ======================================================================================
; Name ..........: _stat_norm_pdf()
; Description ...: propability density function for the gaussian normal distribution
; Syntax ........: _stat_norm_pdf($x[, $mu = 0[, $sigma = 1]])
; Parameters ....: $x     -
;                  $mu    - [optional]  (default:0)
;                  $sigma - [optional]  (default:1)
; Return values .: Success
;                  Failure
; Author ........: AspirinJunkie
; Remarks .......: default values for mu and sigma lead to "standard normal distribution"
; =================================================================================================
Func _stat_norm_pdf($x, $mu = 0, $sigma = 1)
	If $sigma <= 0 Then Return SetError(1, 0, "")
	Return (0.398942280401432678 / $sigma) * Exp(-0.5 * (($x - $mu) / $sigma) ^ 2) ; 0.398942280401432678 / $sigma = 1 / ($sigma * Sqrt(2 * $PI))
EndFunc   ;==>_stat_norm_pdf

; #FUNCTION# ======================================================================================
; Name ..........: _stat_norm_cdf()
; Description ...: Cumulative density function for the gaussian normal distribution
; Syntax ........: _stat_norm_cdf($x[, $mu = 0[, $sigma = 1]])
; Parameters ....: $x     -
;                  $mu    - [optional]  (default:0)
;                  $sigma - [optional]  (default:1)
; Return values .: Success
;                  Failure
; Author ........: AspirinJunkie
; Remarks .......: default values for mu and sigma lead to "standard normal distribution"
; =================================================================================================
Func _stat_norm_cdf($x, $mu = 0, $sigma = 1)
	Return 0.5 * _stat_erfc(-0.707106781186547524 * ($x - $mu) / $sigma)
EndFunc   ;==>_stat_norm_cdf

; #FUNCTION# ======================================================================================
; Name ..........: _stat_norm_icdf()
; Description ...: quantile function (inverse cdf) for the gaussian normal distribution
; Syntax ........: _stat_norm_icdf($p[, $mu = 0[, $sigma = 1[, $bSet = False]]])
; Parameters ....: $p     -
;                  $mu    - [optional]  (default:0)
;                  $sigma - [optional]  (default:1)
;                  $bSet  - [optional]  (default:False)
; Return values .: Success
;                  Failure
; Author ........: AspirinJunkie
; Remarks .......: default values for mu and sigma lead to "standard normal distribution"
; =================================================================================================
Func _stat_norm_icdf($p, $mu = 0, $sigma = 1, $bSet = False)
	If $p <= 0 Or $p >= 1 Then Return SetError(1, 0, "")
	Return -1.41421356237309505 * $sigma * _stat_inverfc(2 * $p) + $mu
EndFunc   ;==>_stat_norm_icdf

; #FUNCTION# ======================================================================================
; Name ..........: _stat_norm_ran()
; Description ...: generate a normal distributed random variable
; Syntax ........: _stat_norm_ran(Const[ $mu = 0, Const[ $sigma = 1]])
; Parameters ....: Const $mu    - [optional]  (default:0)
;                  Const $sigma - [optional] standard deviation of the normal distribution (default:1)
; Return values .: Success: a normal distributed random value
;                  Failure
; Author ........: AspirinJunkie
; =================================================================================================
Func _stat_norm_ran(Const $mu = 0, Const $sigma = 1)
	Local Static $stored = 0
	Local $v1, $v2, $rsq, $fac

	If $stored = 0 Then
		Do
			$v1 = 2 * Random(-1, 1) - 1
			$v2 = 2 * Random(-1, 1) - 1
			$rsq = $v1 * $v1 + $v2 * $v2
		Until $rsq < 1.0 And $rsq <> 0
		$fac = Sqrt(-2*Log($rsq)/$rsq)
		$stored = $v1*$fac
		Return $mu + $sigma*$v2*$fac
	Else
		$fac = $stored
		$stored = 0
		Return $mu + $sigma * $fac
	EndIf
EndFunc
#EndRegion Normal-Verteilung

#Region F-Verteilung
; #FUNCTION# ======================================================================================
; Name ..........: _stat_f_pdf()
; Description ...: propability density function for the f-distribution
; Syntax ........: _stat_f_pdf(Const $x, Const[ $m = 2, Const[ $n = 2]])
; Parameters ....: Const $x -
;                  Const $m - [optional] degrees of freedom in the numerator (default:2)
;                  Const $n - [optional] degrees of freedom in the denominator (default:2)
; Return values .: Success
;                  Failure
; Author ........: AspirinJunkie
; =================================================================================================
Func _stat_f_pdf(Const $x, Const $m = 2, Const $n = 2)
	If $x < 0 Then Return 0
	Return $x < 0 ? 0 : _
			($m ^ ($m / 2)) * ($n ^ ($n / 2)) * _
			(_stat_Gamma($m / 2 + $n / 2) / _
			(_stat_Gamma($m / 2) * _stat_Gamma($n / 2))) * _
			($x ^ ($m / 2 - 1)) / _
			(($m * $x + $n) ^ (($m + $n) / 2))
EndFunc   ;==>_stat_f_pdf

; #FUNCTION# ======================================================================================
; Name ..........: _stat_f_cdf()
; Description ...: cumulative density function for the f-distribution
; Syntax ........: _stat_f_cdf($x[, $m = 2[, $n = 2]])
; Parameters ....: $x -
;                  $m - [optional] degrees of freedom in the numerator (default:2)
;                  $n - [optional] degrees of freedom in the denominator (default:2)
; Return values .: Success
;                  Failure
; Author ........: AspirinJunkie
; =================================================================================================
Func _stat_f_cdf($x, $m = 2, $n = 2)
	If $x < 0 Then Return SetError(1, 0, "")
	Return _stat_betaRI($m * $x / ($n + $m * $x), 0.5 * $m, 0.5 * $n)
EndFunc   ;==>_stat_f_cdf

; #FUNCTION# ======================================================================================
; Name ..........: _stat_f_icdf()
; Description ...: quantile function (Inverse cdf) for the f-distribution
; Syntax ........: _stat_f_icdf($p[, $m = 2[, $n = 2]])
; Parameters ....: $p -
;                  $m - [optional]  (default:2)
;                  $n - [optional]  (default:2)
; Return values .: Success
;                  Failure
; Author ........: AspirinJunkie
; =================================================================================================
Func _stat_f_icdf($p, $m = 2, $n = 2)
	If $p <= 0 Or $p >= 1 Then Return SetError(1, 0, "")
	Local $x = _stat_betaRI_inv($p, 0.5 * $m, 0.5 * $n)
	Return $n * $x / ($m * (1 - $x))
EndFunc   ;==>_stat_f_icdf
#EndRegion F-Verteilung

#Region Student-T-Verteilung
; #FUNCTION# ======================================================================================
; Name ..........: _stat_t_pdf()
; Description ...: propability density function for the student-t distribution
; Syntax ........: _stat_t_pdf($t, Const[ $n = 10[, $m = 0[, $sig = 1]]])
; Parameters ....: $t       -
;                  Const $n - [optional]  (default:10)
;                  $m       - [optional]  (default:0)
;                  $sig     - [optional]  (default:1)
; Return values .: Success
;                  Failure
; Author ........: AspirinJunkie
; =================================================================================================
Func _stat_t_pdf($t, Const $n = 10, $m = 0, $sig = 1)
	If $sig <= 0 Or $n <= 0 Then Return SetError(1, 0, "")
	Local $np = 0.5 * ($n + 1)
	Local $fac = _stat_GammaLn($np) - _stat_GammaLn(0.5 * $n)

	Return Exp(-$np * Log(1 + ((($t - $m) / $sig) ^ 2) / $n) + $fac) / (Sqrt($PI * $n) * $sig)
EndFunc   ;==>_stat_t_pdf

; #FUNCTION# ======================================================================================
; Name ..........: _stat_t_cdf()
; Description ...: Cumulative density function for the student-t distribution
; Syntax ........: _stat_t_cdf($t, Const[ $n = 10[, $m = 0[, $sig = 1]]])
; Parameters ....: $t       -
;                  Const $n - [optional]  (default:10)
;                  $m       - [optional]  (default:0)
;                  $sig     - [optional]  (default:1)
; Return values .: Success
;                  Failure
; Author ........: AspirinJunkie
; =================================================================================================
Func _stat_t_cdf($t, Const $n = 10, $m = 0, $sig = 1)
	Local $p = 0.5 * _stat_betaRI($n / ($n + ((($t - $m) / $sig) ^ 2)), 0.5 * $n, 0.5)
	Return $t >= $m ? 1 - $p : $p
EndFunc   ;==>_stat_t_cdf

; #FUNCTION# ======================================================================================
; Name ..........: _stat_t_icdf()
; Description ...: quantile function (inverse cdf) for the student-t distribution
; Syntax ........: _stat_t_icdf($p, $n[, $m = 0[, $sig = 1]])
; Parameters ....: $p   -
;                  $n   -
;                  $m   - [optional]  (default:0)
;                  $sig - [optional]  (default:1)
; Return values .: Success
;                  Failure
; Author ........: AspirinJunkie
; =================================================================================================
Func _stat_t_icdf($p, $n, $m = 0, $sig = 1)
	Local $x = _stat_betaRI_inv(2 * __stat_min($p, 1 - $p), 0.5 * $n, 0.5)
	$x = $sig * Sqrt($n * (1 - $x) / $x)
	Return $p >= 0.5 ? $m + $x : $m - $x
EndFunc   ;==>_stat_t_icdf
#EndRegion Student-T-Verteilung

#Region Beta-Verteilung
; #FUNCTION# ======================================================================================
; Name ..........: _stat_beta_pdf()
; Description ...: propability density function for the beta distribution
; Syntax ........: _stat_beta_pdf($x[, $alpha = 1[, $beta = 1]])
; Parameters ....: $x     -
;                  $alpha - [optional]  (default:1)
;                  $beta  - [optional]  (default:1)
; Return values .: Success
;                  Failure
; Author ........: AspirinJunkie
; =================================================================================================
Func _stat_beta_pdf($x, $alpha = 1, $beta = 1)
	Return ($x ^ ($alpha - 1)) * ((1 - $x) ^ ($beta - 1)) / (_stat_Gamma($alpha) * _stat_Gamma($beta) / _stat_Gamma($alpha + $beta))
EndFunc   ;==>_stat_beta_pdf

; #FUNCTION# ======================================================================================
; Name ..........: _stat_beta_cdf()
; Description ...: Cumulative density function for the beta distribution
; Syntax ........: _stat_beta_cdf($x[, $alpha = 1[, $beta = 1]])
; Parameters ....: $x     -
;                  $alpha - [optional]  (default:1)
;                  $beta  - [optional]  (default:1)
; Return values .: Success
;                  Failure
; Author ........: AspirinJunkie
; =================================================================================================
Func _stat_beta_cdf($x, $alpha = 1, $beta = 1)
	If $x <= 0 Then Return 0
	If $x >= 1 Then Return 1
	Return _stat_betaRI($x, $alpha, $beta)
EndFunc   ;==>_stat_beta_cdf

; #FUNCTION# ======================================================================================
; Name ..........: _stat_beta_icdf()
; Description ...: quantile function (Inverse cdf) for the beta distribution
; Syntax ........: _stat_beta_icdf($p[, $a = 2[, $b = 2]])
; Parameters ....: $p -
;                  $a - [optional]  (default:2)
;                  $b - [optional]  (default:2)
; Return values .: Success
;                  Failure
; Author ........: AspirinJunkie
; =================================================================================================
Func _stat_beta_icdf($p, $a = 2, $b = 2)
	Return _stat_betaRI_inv($p, $a, $b)
EndFunc   ;==>_stat_beta_icdf
#EndRegion Beta-Verteilung

#Region Chi-Quadrat-Verteilung
; #FUNCTION# ======================================================================================
; Name ..........: _stat_chi2_pdf()
; Description ...: propability density function for the Chi²-distribution
; Syntax ........: _stat_chi2_pdf($x[, $n = 2])
; Parameters ....: $x -
;                  $n - [optional]  (default:2)
; Return values .: Success
;                  Failure
; Author ........: AspirinJunkie
; =================================================================================================
Func _stat_chi2_pdf($x, $n = 2)
	If $x <= 0 Then Return 0
	Local Const $n2 = 0.5 * $n
	Return ($x ^ ($n2 - 1) * Exp(-0.5 * $x)) / _
			2.0 ^ $n2 * _stat_Gamma($n2)
EndFunc   ;==>_stat_chi2_pdf

; #FUNCTION# ======================================================================================
; Name ..........: _stat_chi2_cdf()
; Description ...: cumulative density function for the Chi²-distribution
; Syntax ........: _stat_chi2_cdf($x[, $n = 2])
; Parameters ....: $x -
;                  $n - [optional]  (default:2)
; Return values .: Success
;                  Failure
; Author ........: AspirinJunkie
; =================================================================================================
Func _stat_chi2_cdf($x, $n = 2)
	If $x <= 0 Then Return SetError(1, 0, "")
	Return _stat_gammp(0.5 * $n, 0.5 * $x)

;~ 	Local $a_Params[3] = ["CallArgArray", $x, $n]
;~ 	Return _stat_Int_Romberg(_stat_chi2_pdf, 0.0, $x, $a_Params, 1)
EndFunc   ;==>_stat_chi2_cdf

; #FUNCTION# ======================================================================================
; Name ..........: _stat_chi2_icdf()
; Description ...: quantile function (Inverse cdf) for the Chi²-distribution
; Syntax ........: _stat_chi2_icdf($p, $n)
; Parameters ....: $p -
;                  $n -
; Return values .: Success
;                  Failure
; Author ........: AspirinJunkie
; =================================================================================================
Func _stat_chi2_icdf($p, $n)
	If $p < 0 Or $p >= 1 Then Return SetError(1, 0, "")
	Return 2 * _stat_invgammp($p, 0.5 * $n)

;~ 	Local $a_Params[3] = ["CallArgArray", $t, $n]
;~ 	Return _stat_findFuncVal(_stat_chi2_cdf, 0, 20 * $n, $t, $a_Params, 1)
EndFunc   ;==>_stat_chi2_icdf
#EndRegion Chi-Quadrat-Verteilung

#Region Kolmogorov-Smirnov-Verteilung
; #FUNCTION# ======================================================================================
; Name ..........: _stat_ks_getProb()
; Description ...: returns the propability for a quantile of the kolmogorov-smirnov distribution
; Syntax ........: _stat_ks_getProb($n, $e)
; Parameters ....: $n - sample size
;                  $e -
; Return values .: Success
;                  Failure
; Author ........: AspirinJunkie
; Remarks .......: used for evaluate the p-value for the ks-test
;                  source of the algorithm: cephes module for Python
; =================================================================================================
Func _stat_ks_getProb($n, $e)
	Local Static $MAXLOG = Log(2 ^ 127)

	Local $nn = Floor($n * (1 - $e))
	Local $p = 0, $c, $evn

	If $n < 1013 Then
		$c = 1
		For $v = 0 To $nn
			$evn = $e + $v / $n
			$p += $c * ($evn ^ ($v - 1)) * ((1 - $evn) ^ ($n - $v))
			$c *= ($n - $v) / ($v + 1)
		Next
	Else
		Local $lgamnp1 = _stat_GammaLn($n + 1), $omevn, $t
		For $v = 0 To $nn
			$evn = $e + $v / $n
			$omevn = 1 - $evn
			If Abs($omevn) > 0 Then
				$t = $lgamnp1 - _stat_GammaLn($v + 1) - _stat_GammaLn($n - $v + 1) + ($v - 1) * Log($evn) + ($n - $v) * Log($omevn)
				If $t > -$MAXLOG Then $p += Exp($t)
			EndIf
		Next
	EndIf
	Return $p * $e
EndFunc   ;==>_stat_ks_getProb

; #FUNCTION# ======================================================================================
; Name ..........: _stat_ks_pks()
; Description ...: cumulative density function for the kolmogorov-smirnov distribution
; Syntax ........: _stat_ks_pks(Const $z)
; Parameters ....: Const $z -
; Return values .: Success
;                  Failure
; Author ........: AspirinJunkie
; Remarks .......: Source of algorithm = Numerical Receipes - The Art of Scientific Computing 3rd ed (Cambridge 2007)
; =================================================================================================
Func _stat_ks_pks(Const $z)
	If $z < 0 Then Return SetError(1, 0, "")
	If $z = 0 Then Return 0

	If $z < 1.18 Then
		Local $y = Exp(-1.23370055013616983 / ($z * $z))
		Return 2.25675833419102515 * Sqrt(-Log($y)) * ($y + $y ^ 9 + $y ^ 25 + $y ^ 49) ;
	Else
		Local $x = Exp(-2 * ($z * $z))
		Return 1 - 2 * ($x - $x ^ 4 + $x ^ 9)
	EndIf
EndFunc   ;==>_stat_ks_pks

; #FUNCTION# ======================================================================================
; Name ..........: _stat_ks_qks()
; Description ...: complementary cumulative density function [Q_KS(z) = 1-P_KS(z)] for the kolmogorov-smirnov distribution
; Syntax ........: _stat_ks_qks(Const $z[, $m = 10])
; Parameters ....: Const $z -
;                  $m       - [optional]  (default:10)
; Return values .: Success
;                  Failure
; Author ........: AspirinJunkie
; Remarks .......: Source of algorithm = Numerical Receipes - The Art of Scientific Computing 3rd ed (Cambridge 2007)
; =================================================================================================
Func _stat_ks_qks(Const $z, $m = 10)
;~ 	Local $sum = 0
;~ 	For $k = 1 To $m
;~ 		$sum += Exp(-((2*$k-1)^2)*($PI^2)/(8*$z^2))
;~ 	Next
;~ 	Return 1-((Sqrt(2*$PI)/$z) * $sum)

	If $z < 0 Then Return SetError(1, 0, "")
	If $z = 0 Then Return 1

	If $z < 1.18 Then Return 1 - _stat_ks_pks($z)

	Local $x = Exp(-2 * ($z * $z))
	Return 2 * ($x - $x ^ 4 + $x ^ 9)

EndFunc   ;==>_stat_ks_qks

; #FUNCTION# ======================================================================================
; Name ..........: _stat_ks_invpks()
; Description ...: inverse of the cdf [P_KS^-1(P)] for the kolmogorov-smirnov distribution
; Syntax ........: _stat_ks_invpks(Const $p)
; Parameters ....: Const $p -
; Return values .: Success
;                  Failure
; Author ........: AspirinJunkie
; Remarks .......: Source of algorithm = Numerical Receipes - The Art of Scientific Computing 3rd ed (Cambridge 2007)
; =================================================================================================
Func _stat_ks_invqks(Const $q)
;~ 	Local $a_Params[2] = ["CallArgArray", $q]
;~ 	Return _stat_findFuncVal(_stat_ks_qks, 0, 1, $q, $a_Params, 1)

	If $q <= 0 Or $q > 1 Then Return SetError(1, 0, "")
	If $q = 1 Then Return 0


	If $q > 0.3 Then
		Local $f = -0.392699081698724155 * ((1 - $q) ^ 2)
		Local $y = __invxlogx($f)
		Local $yp, $logy, $ff, $u
		Do
			$yp = $y
			$logy = Log($y)
			$ff = $f / ((1 + $y ^ 4 + $y ^ 12) ^ 2)
			$u = ($y * $logy - $ff) / (1 + $logy)
			$t = $u / __stat_max(0.5, 1 - 0.5 * $u / ($y * (1 + $logy)))
			$y = $y - $t
		Until Abs($t / $y) <= 1E-15
		Return 1.57079632679489662 / Sqrt(-Log($y))
	Else
		Local $x = 0.03, $xp
		Do
			$xp = $x
			$x = 0.5 * $q + $x ^ 4 - $x ^ 9
			If $x > 0.06 Then $x += $x ^ 16 - $x ^ 25
		Until Abs(($xp - $x) / $x) <= 1E-15
		Return Sqrt(-0.5 * Log($x))
	EndIf
EndFunc   ;==>_stat_ks_invqks

; #FUNCTION# ======================================================================================
; Name ..........: _stat_ks_invqks()
; Description ...: complementary inverse of the cdf [Q_KS^-1(Q)] for the kolmogorov-smirnov distribution
; Syntax ........: _stat_ks_invqks(Const $q)
; Parameters ....: Const $q -
; Return values .: Success
;                  Failure
; Author ........: AspirinJunkie
; Remarks .......: Source of algorithm = Numerical Receipes - The Art of Scientific Computing 3rd ed (Cambridge 2007)
; =================================================================================================
Func _stat_ks_invpks(Const $p)
	Return _stat_ks_invqks(1 - $p)
EndFunc   ;==>_stat_ks_invpks
#EndRegion Kolmogorov-Smirnov-Verteilung
