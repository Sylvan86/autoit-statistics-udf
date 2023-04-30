#include-once
#include "Stat_Helpers.au3"
#include <Array.au3>

#Region global constants
If Not IsDeclared("PI") Then Global Const $PI = ACos(-1)
Global Const $STAT_EPS = __stat_EPS() ; machine accuracy
Global Const $STAT_FPMIN = 2.22507E-308 / $STAT_EPS ; close to smallest represantable floating point number
#EndRegion global constants

#Region load Standard-C-Library
For $i = 200 To 90 Step -10
	Global $h_DLL_MSVCR = DllOpen("msvcr" & $i & ".dll")
	If $h_DLL_MSVCR <> -1 Then ExitLoop
Next
If $h_DLL_MSVCR = -1 Then Exit MsgBox(48, "Fehler", "Kein Zugriff auf msvcrt.dll!")
#EndRegion load Standard-C-Library



If @ScriptName = "Stat_Basics.au3" Then
	ConsoleWrite(_stat_gammp(2, 2) & @CRLF)
	ConsoleWrite(_stat_invgammp(0.593994150290162, 2) & @CRLF)


	Local $a_Test[] = [1,  9,  4,  7,  2,  3,  5, 10, 11,  6,  8, 12]
	ConsoleWrite(__stat_tiecorrect($a_Test) & @CRLF)

	Local $a_Test[]=[0, 2, 3, 2]
	$a_Ret = __stat_rankdata($a_Test)
	_ArrayDisplay($a_Ret)
EndIf





; #FUNCTION# ======================================================================================
; Name ..........: __stat_rankdata()
; Description ...: calculate the rank for every element in a array
; Syntax ........: __stat_rankdata($a_Data)
; Parameters ....: $a_Data -
; Return values .: Success
;                  Failure
; Author ........: AspirinJunkie
; =================================================================================================
Func __stat_rankdata($a_Data)
	Local $o_Count = ObjCreate("Scripting.Dictionary"), $o_Rank = ObjCreate("Scripting.Dictionary")
	Local $iC
	Local $a_Sorted = $a_Data
	_ArraySort($a_Sorted)

	For $i In $a_Sorted
		$o_Count($i) += 1
	Next

	Local $i = 0
	Do
		$iC = $o_Count($a_Sorted[$i])
		$o_Rank($a_Sorted[$i]) = $i + $iC*($IC+1)/2/$iC
		$i += $iC
	Until $i >= UBound($a_Sorted)
	Local $a_Ret[UBound($a_Data)]
	For $i = 0 To UBound($a_Data) -1
		$a_Ret[$i] = $o_Rank($a_Data[$i])
	Next
	Return $a_Ret
EndFunc   ;==>__stat_rankdata



; #FUNCTION# ======================================================================================
; Name ..........: _stat_symround()
; Description ...: symmetric/half-even/geodetic/congruent rounding
;                  in normal round() values of x.5 are rounding to next higher integer. this leads to unsymmetric distribution.
;                  A solution is to round to the next even integer.
; Syntax ........: _stat_symround($fVal)
; Parameters ....: $fVal - the floating point value
; Return values .: Success
;                  Failure
; Author ........: AspirinJunkie
; =================================================================================================
Func _stat_symround($fVal)
	Return Mod(Abs($fVal), 2) = 0.5 ? Floor($fVal) : Round($fVal)
EndFunc


; #FUNCTION# ======================================================================================
; Name ..........: __stat_tiecorrect()
; Description ...: Tie correction factor for ties in the Mann-Whitney U and Kruskal-Wallis H tests
; Syntax ........: __stat_tiecorrect($a_Data)
; Parameters ....: $a_Data -
; Return values .: Success
;                  Failure
; Author ........: AspirinJunkie
; =================================================================================================
Func __stat_tiecorrect($a_Data)
	Local $n = UBound($a_Data)
	_ArraySort($a_Data)

	Local $idx, $a_tied[UBound($a_Data) - 1]
	For $i = 1 To UBound($a_Data) - 1
		$a_tied[$i - 1] = ($a_Data[$i] <> $a_Data[$i - 1])
	Next
	ReDim $a_tied[UBound($a_tied) + 1]
	$a_tied[UBound($a_tied) - 1] = True

	_ArrayInsert($a_tied, 0, True)
	Local $idx[UBound($a_tied)] = [0], $iC = 0
	For $i = 0 To UBound($a_tied) - 1
		If $a_tied[$i] Then
			$idx[$iC] = $i
			$iC += 1
		EndIf
	Next
	ReDim $idx[$iC]

	Local $cntSum = 0, $dDiff
	For $i = 1 To UBound($idx) - 1
		$dDiff = $idx[$i] - $idx[$i - 1]
		$cntSum += $dDiff ^ 3 - $dDiff
	Next

	If $cntSum = 0 Then Return 1
	Return $n < 2 ? 1 : 1.0 - $cntSum / ($n ^ 3 - $n)
EndFunc   ;==>__stat_tiecorrect



; #FUNCTION# ======================================================================================
; Name ..........: _stat_invgammp()
; Description ...: inverse gamma function
; Syntax ........: _stat_invgammp($p, $a)
; Parameters ....: $p -
;                  $a -
; Return values .: Success: x such that P(a,x) = p for an argument p between 0 and 1
;                  Failure
; Author ........: AspirinJunkie
; =================================================================================================
Func _stat_invgammp($p, $a)
	Local $x, $err, $t, $u, $pp, $lna1, $afac, $a1 = $a - 1
	Local $EPS = 1E-8
	Local $gln = _stat_GammaLn($a)

	If $a <= 0 Then Return SetError(1, 0, "")
	If $p >= 1 Then Return __stat_max(100, $a + 100 * Sqrt($a))
	If $p <= 0 Then Return 0

	If $a > 1 Then
		$lna = Log($a1)
		$afac = Exp($a1 * ($lna1 - 1) - $gln)
		$pp = ($p < 0.5) ? $p : 1 - $p
		$t = Sqrt(-2 * Log($pp))
		$x = (2.30753 + $t * 0.27061) / (1 + $t * (0.99229 + $t * 0.04481)) - $t
		If $p < 0.5 Then $x = -$x
		$x = __stat_max(1E-3, $a * ((1 - 1 / (9 * $a) - $x / (3 * Sqrt($a))) ^ 3))
	Else
		$t = 1 - $a * (0.253 + $a * 0.12)
		$x = $p < $t ? ($p / $t) ^ (1 / $a) : 1 - Log(1 - ($p - $t) / (1 - $t))
	EndIf

	For $j = 0 To 11
		If $x <= 0 Then Return 0
		$err = _stat_gammp($a, $x) - $p
		$t = ($a > 1) ? $afac * Exp(-($x - $a1) + $a1 * (Log($x) - $lna1)) : Exp(-$x + $a1 * Log($x) - $gln)
		$u = $err / $t
		$t = $u / (1 - 0.5 * __stat_min(1, $u * (($a - 1) / $x - 1)))
		$x -= $t
		If $x <= 0 Then $x = 0.5 * ($x + $t)
		If Abs($t) < $EPS * $x Then ExitLoop
	Next
	Return $x
EndFunc   ;==>_stat_invgammp


; #FUNCTION# ======================================================================================
; Name ..........: _stat_gammp()
; Description ...: Returns the incomplete gamma function P(a;x)
; Syntax ........: _stat_gammp($a, $x)
; Parameters ....: $a -
;                  $x -
; Return values .: Success
;                  Failure
; Author ........: AspirinJunkie
; =================================================================================================
Func _stat_gammp($a, $x)
	Local Const $ASWITCH = 100
	If $x < 0 Or $a <= 0 Then Return SetError(1, 0, "")
	If $x = 0 Then
		Return 0
	ElseIf Int($a) >= $ASWITCH Then
		Return __stat_gammp_approx($a, $x, 1)
	ElseIf $x < $a + 1 Then
		Return __stat_gser($a, $x)
	Else
		Return 1 - __stat_gcf($a, $x)
	EndIf
EndFunc   ;==>_stat_gammp

; #FUNCTION# ======================================================================================
; Name ..........: _stat_gammq()
; Description ...: Returns the incomplete gamma function Q(a,x) = 1 - P(a,x)
; Syntax ........: _stat_gammq($a, $x)
; Parameters ....: $a -
;                  $x -
; Return values .: Success
;                  Failure
; Author ........: AspirinJunkie
; =================================================================================================
Func _stat_gammq($a, $x)
	Local Const $ASWITCH = 100
	If $x < 0 Or $a <= 0 Then Return SetError(1, 0, "")
	If $x = 0 Then
		Return 0
	ElseIf Int($a) >= $ASWITCH Then
		Return __stat_gammp_approx($a, $x, 0)
	ElseIf $x < $a + 1 Then
		Return 1 - __stat_gser($a, $x)
	Else
		Return __stat_gcf($a, $x)
	EndIf
EndFunc   ;==>_stat_gammq

; #FUNCTION# ======================================================================================
; Name ..........: __stat_gcf()
; Description ...: Returns the incomplete gamma function Q(a;x)
; Syntax ........: __stat_gcf($a, $x, Const[ $EPS = $STAT_EPS, Const[ $FPMIN = $STAT_FPMIN]])
; Parameters ....: $a           -
;                  $x           -
;                  Const $EPS   - [optional]  (default:$STAT_EPS)
;                  Const $FPMIN - [optional]  (default:$STAT_FPMIN)
; Return values .: Success
;                  Failure
; Author ........: AspirinJunkie
; Remarks .......: evaluated by its continued fraction representation
; =================================================================================================
Func __stat_gcf($a, $x, Const $EPS = $STAT_EPS, Const $FPMIN = $STAT_FPMIN)
	Local $an, $del
	Local $gln = _stat_GammaLn($a), $b = $x + 1 - $a, $c = 1 / $FPMIN, $d = 1 / $b, $h = $d

	For $i = 1 To 1e5
		$an = -$i * ($i - $a)
		$b += 2
		$d = $an * $d + $b

		If Abs($d) < $FPMIN Then $d = $FPMIN
		$c = $b + $an / $c
		If Abs($c) < $FPMIN Then $c = $FPMIN
		$d = 1 / $d
		$del = $d * $c
		$h *= $del
		If Abs($del - 1) <= $EPS Then ExitLoop
	Next
	Return Exp(-$x + $a * Log($x) - $gln) * $h

EndFunc   ;==>__stat_gcf

; #FUNCTION# ======================================================================================
; Name ..........: __stat_gser()
; Description ...: incomplete gamma function P(a;x)
; Syntax ........: __stat_gser($a, $x, Const[ $EPS = $STAT_EPS])
; Parameters ....: $a         -
;                  $x         -
;                  Const $EPS - [optional]  (default:$STAT_EPS)
; Return values .: Success
;                  Failure
; Author ........: AspirinJunkie
; Remarks .......: evaluated by its series representation
; =================================================================================================
Func __stat_gser($a, $x, Const $EPS = $STAT_EPS)
	Local $sum, $del, $ap
	Local $gln = _stat_GammaLn($a)

	$ap = $a
	$sum = 1 / $a
	$del = $sum
	Do
		$ap += 1
		$del *= $x / $ap
		$sum += $del
		If Abs($del) < Abs($sum) * $EPS Then Return $sum * Exp(-$x + $a * Log($x) - $gln)
	Until 0
EndFunc   ;==>__stat_gser


; #FUNCTION# ======================================================================================
; Name ..........: __stat_gammp_approx()
; Description ...: incomplete gamma function
; Syntax ........: __stat_gammp_approx($a, $x[, $psig = 1])
; Parameters ....: $a    -
;                  $x    -
;                  $psig - [optional]  (default:1)
; Return values .: Success: P(a;x) or Q(a;x), when psig is 1 or 0, respectively.
;                  Failure
; Author ........: AspirinJunkie
; Remarks .......: approximated by quadrature
; =================================================================================================
Func __stat_gammp_approx($a, $x, $psig = 1)
	Local Static $y[18] = [0.0021695375159141994, 0.011413521097787704, 0.027972308950302116, 0.051727015600492421, 0.082502225484340941, 0.12007019910960293, 0.16415283300752470, 0.21442376986779355, 0.27051082840644336, 0.33199876341447887, 0.39843234186401943, 0.46931971407375483, 0.54413605556657973, 0.62232745288031077, 0.70331500465597174, 0.78649910768313447, 0.87126389619061517, 0.95698180152629142]
	Local Static $w[18] = [0.0055657196642445571, 0.012915947284065419, 0.020181515297735382, 0.027298621498568734, 0.034213810770299537, 0.040875750923643261, 0.047235083490265582, 0.053244713977759692, 0.058860144245324798, 0.064039797355015485, 0.068745323835736408, 0.072941885005653087, 0.076598410645870640, 0.079687828912071670, 0.082187266704339706, 0.084078218979661945, 0.085346685739338721, 0.085983275670394821]
	Local $j, $xu, $t, $sum = 0, $ans
	Local $a1 = $a - 1, $lna1 = $a1 >= 0 ? Log($a1) : 0, $sqrta1 = Sqrt($a1)

	Local $gln = _stat_GammaLn($a)

	$xu = $x > $a1 ? __stat_max($a1 + 11.5 * $sqrta1, $x + 6 * $sqrta1) : __stat_max(0, __stat_min($a1 - 7.5 * $sqrta1, $x - 5 * $sqrta1))

	For $j = 0 To 17
		$t = $x + ($xu - $x) * $y[$j]
		$sum += $w[$j] * Exp(-($t - $a1) + $a1 * (Log($t) - $lna1))
	Next
	$ans = $sum * ($xu - $x) * Exp($a1 * ($lna1 - 1) - $gln)
	Return $psig = 1 ? ($ans > 0 ? 1 - $ans : -$ans) : ($ans >= 0 ? $ans : 1 + $ans)
EndFunc   ;==>__stat_gammp_approx




; #FUNCTION#=================================================================================
; Name...........: _stat_findFuncVal
; Description ...: Berechnet die Nullstelle in einem Intervall numerisch
;                   Methode: Modifiziertes Anderson-Björck-King Verfahren
; Syntax.........: _stat_findFuncVal(Const $cb_F, $a, $b, $eps)
; Return values .: X-Wert der Nullstelle
;                   @Extended = Anzahl Iterationen
; Parameters.....:       $cb_F: Funktionsvariable oder Funktionsstring einer 1D-Funktion f(x)
;                        $a:        linke Suchbereichsgrenze
;                        $b:        rechte Suchbereichsgrenze
;                        $eps:      der Grenzwert ab dem der Funktionswert f(0) als 0 angesehen wird
; Remarks .......: modifiziertes Anderson-Björk-King Verfahren
; Requirements ..: Funktion float _Fx( string F(x), float x )
;                   Global $FLT_EPSILON = float
; Author ........: AspirinJunkie
;============================================================================================
Func _stat_findFuncVal(Const $cb_F, $x1, $x2, Const $fVal = 0.0, Const $a_Params = Default, Const $i_ind = 1, $EPS_x = 1E-10, $EPS_y = $EPS_x)
	Local $f1 = __f($cb_F, $a_Params, $x1, $i_ind) - $fVal
	Local $f2 = __f($cb_F, $a_Params, $x2, $i_ind) - $fVal
	Local $x3, $f3, $i = 0, $f32

	#cs Implementierung von https://link.springer.com/content/pdf/bbm%3A978-3-642-05175-3%2F1.pdf
		Do
		$x3 = $x2 - ($x2 - $x1) * $f2 / ($f2 - $f1)
		$f3 = __f($cb_F, $a_Params, $x3, $i_ind) - $fVal

		If $f2 * $f3 < 0 Then
		$x1 = $x2
		$x2 = $x3
		$f1 = $f2
		$f2 = $f3
		Else
		$x2 = $x3
		$f32 = 1 - $f3/$f2
		$f1 *= $f32 <= 0 ? 0.5 : $f32
		$f2 = $f3
		EndIf
		Until Abs($x1 - $x2) < $EPS_x Or Abs($f3) < $EPS_y
	#ce

	;Implementierung von Buch: Numerik-Algorithmen: Verfahren, Beispiele, Anwendungen
	Do
;~ 	; Sekante berechnen
		$x3 = $x2 - ($x2 - $x1) * $f2 / ($f2 - $f1)
		$f3 = __f($cb_F, $a_Params, $x3, $i_ind) - $fVal
		If $f3 * $f2 < 0.0 Then
			$x1 = $x2
			$f1 = $f2
			$x2 = $x3
			$f2 = $f3
		Else
			$f32 = 1 - $f3 / $f2
			$f1 *= $f32 <= 0 ? 0.5 : $f32
			$x2 = $x3
			$f2 = $f3
		EndIf
		$i += 1
	Until Abs($x1 - $x2) < $EPS_x Or Abs($f3) < $EPS_y

	Return SetExtended($i, $x3)
	; es gibt noch eine weitere Implementierung: https://www.mat.tuhh.de/lehre/material/num_meth/Kapitel7_ho.pdf
EndFunc   ;==>_stat_findFuncVal


; #FUNCTION#=================================================================================
; Name...........: _stat_Int_Romberg
; Description ...: Berechnet das bestimmte Integral eindimensionalen Funktion mit
;                  einer festzulegenden relativen Genauigkeit
; Syntax.........: _stat_Int_Romberg(Const $a, Const $b, Const $EPS = 0.0001)
; Parameters ....: $cb_F   - Funktionsvariable oder Funktionsstring einer 1D-Funktion f(x)
;                  $a      - Untere Integrationsgrenze
;                  $b      - obere Integrationsgrenze
;                  $EPS    - zu erreichender relativer Integrationsfehler
;                      Hinweis: != absoluter Fehler, rel. Fehler = Abs(1-(berechneter Integralwert / wahrer Integralwert)
;                  $kMax   - Maximale Anzahl an Halbierungsintervallen
; Return values .: Integral in den Grenzen $a-$b, @extended = Anzahl benötigter Iterationen
;                   @error = 1: Maximale Iterationsanzahl erreicht
; Remarks .......: Romberg-Integrationsalgorithmus
; Related .......: __F()
; Author ........: AspirinJunkie
;============================================================================================
Func _stat_Int_Romberg(Const $cb_F, Const $a, Const $b, Const $a_Params, Const $i_ind = 1, Const $EPS = 0.00001, Const $kMax = 15)
	Local $a_R1[$kMax + 1], $a_R2[$kMax + 1]
	Local $h = $b - $a, $n = 1, $sumf, $f

	If Not IsFunc($cb_F) Then Return SetError(1, 0, "")
	If (Not IsArray($a_Params)) Or UBound($a_Params, 0) <> 1 Or UBound($a_Params, 1) < 2 Or $i_ind >= UBound($a_Params, 1) Or $a_Params[0] <> "CallArgArray" Then Return SetError(2, 0, "")

	$a_R1[0] = 0.5 * $h * (__f($cb_F, $a_Params, $a, $i_ind) + __f($cb_F, $a_Params, $b, $i_ind)) ; 1/2 * h + (f(A) + f(B))

	For $k = 1 To $kMax ; Schrittweise halbieren
		$sumf = 0
		For $i = 1 To $n
			$sumf += __f($cb_F, $a_Params, $a + ($i - 0.5) * $h, $i_ind)
		Next
		$a_R2[0] = 0.5 * ($a_R1[0] + $h * $sumf) ; Trapezoid-Formel
		$f = 1.0
		For $j = 1 To $k ; Quadratur-Ordnung erhöhen
			$f *= 4
			$a_R2[$j] = ($f * $a_R2[$j - 1] - $a_R1[$j - 1]) / ($f - 1) ; Neue Approximation
		Next
		If $k > 1 Then ; Check auf Konvergenz
			If (Abs($a_R2[$k] - $a_R1[$k - 1]) <= ($EPS * Abs($a_R2[$k]))) Then ExitLoop
			If (Abs($a_R2[$k]) <= $EPS) And (Abs($a_R2[$k]) <= Abs($a_R2[$k] - $a_R1[$k - 1])) Then ExitLoop
		EndIf
		$h *= 0.5 ; Halbiere Schrittweite
		$n *= 2

		$a_R1 = $a_R2
	Next

	If $k >= $kMax Then Return SetError(1, $k, $a_R2[$k - 1])
	Return SetExtended($k, $a_R2[$k])
EndFunc   ;==>_stat_Int_Romberg


; #FUNCTION# ======================================================================================
; Name ..........: _stat_Gamma()
; Description ...: Gamma function
; Syntax ........: _stat_Gamma(Const $z)
; Parameters ....: Const $z -
; Author ........: AspirinJunkie
; =================================================================================================
Func _stat_Gamma(Const $z)
	Return DllCall($h_DLL_MSVCR, "double:cdecl", "tgamma", "double", $z)[0]
EndFunc   ;==>_stat_Gamma

; #FUNCTION# ======================================================================================
; Name ..........: _stat_GammaLn()
; Description ...: natural logarithm of the absolute value of the gamma function
; Syntax ........: _stat_GammaLn(Const $z)
; Parameters ....: Const $z -
; Author ........: AspirinJunkie
; =================================================================================================
Func _stat_GammaLn(Const $z)
	Return DllCall($h_DLL_MSVCR, "double:cdecl", "lgamma", "double", $z)[0]
EndFunc   ;==>_stat_GammaLn

; #FUNCTION# ======================================================================================
; Name ..........: _stat_erf()
; Description ...: Gauss error function
; Syntax ........: _stat_erf(Const $z)
; Parameters ....: Const $z -
; Author ........: AspirinJunkie
; =================================================================================================
Func _stat_erf(Const $z)
	Return DllCall($h_DLL_MSVCR, "double:cdecl", "erf", "double", $z)[0]
EndFunc   ;==>_stat_erf

; #FUNCTION# ======================================================================================
; Name ..........: _stat_erfc()
; Description ...: complementary Gauss error function
; Syntax ........: _stat_erfc(Const $z)
; Parameters ....: Const $z -
; Author ........: AspirinJunkie
; =================================================================================================
Func _stat_erfc(Const $z)
	Return DllCall($h_DLL_MSVCR, "double:cdecl", "erfc", "double", $z)[0]
EndFunc   ;==>_stat_erfc


; #FUNCTION# ======================================================================================
; Name ..........: _stat_inverfc()
; Description ...: inverse complementary Gauss error function
; Syntax ........: _stat_inverfc(Const $p)
; Parameters ....: Const $p -
; Author ........: AspirinJunkie
; =================================================================================================
Func _stat_inverfc(Const $p)
	Local $x, $err, $t
	If $p >= 2 Then Return -100
	If $p <= 0 Then Return 100

	Local $pp = $p < 1 ? $p : 2 - $p
	$t = Sqrt(-2 * Log($pp / 2))
	$x = -0.70711 * ((2.30753 + $t * 0.27061) / (1 + $t * (0.99229 + $t * 0.04481)) - $t)

	For $j = 0 To 1
		$err = _stat_erfc($x) - $pp
		$x += $err / (1.12837916709551257 * Exp(-($x * $x)) - $x * $err)
	Next
	Return $p < 1 ? $x : -$x
EndFunc   ;==>_stat_inverfc


; #FUNCTION# ======================================================================================
; Name ..........: _stat_beta()
; Description ...: Returns beta function (not beta distribution!)
; Syntax ........: _stat_beta($a, $b)
; Author ........: AspirinJunkie
; Related .......: _stat_Gamma
; =================================================================================================
Func _stat_beta($a, $b)
;~ 	Return Exp(_stat_GammaLn($x) + _stat_GammaLn($y) - _stat_GammaLn($x + $y))
	Return _stat_Gamma($a) * _stat_Gamma($b) / _stat_Gamma($a + $b)
EndFunc   ;==>_stat_beta


; #FUNCTION# ======================================================================================
; Name ..........: _stat_betaI()
; Description ...: Returns incomplete beta function Bx(a; b) (not regularized!)
; Syntax ........: _stat_betaI($x, $a, $b)
; Author ........: AspirinJunkie
; Related .......: _stat_beta, _stat_betaRI
; =================================================================================================
Func _stat_betaI($x, $a, $b)
	Return _stat_betaRI($x, $a, $b) * _stat_beta($a, $b)
EndFunc   ;==>_stat_betaI


; #FUNCTION# ======================================================================================
; Name ..........: _stat_betaRI()
; Description ...: Returns incomplete beta function Ix(a; b) for positive a and b, and x between 0 and 1  (regularized incomplete beta function)
; Syntax ........: _stat_betaI($x, $a, $b)
; Author ........: translation to AutoIt: AspirinJunkie
; Related .......: __stat_betacf, __stat_betaRI_approx, _stat_GammaLn
; Remarks .......: Source of algorithm = Numerical Receipes - The Art of Scientific Computing 3rd ed (Cambridge 2007)
; =================================================================================================
Func _stat_betaRI($x, $a, $b)
	Local Const $SWITCH = 3000

	If $a <= 0 Or $b <= 0 Then Return SetError(1, 0, -1)
	If $x < 0 Or $x > 1 Then Return SetError(2, 0, -1)
	If $x = 0 Or $x = 1 Then Return $x
	If $a > $SWITCH And $b > $SWITCH Then Return __stat_betaRI_approx($x, $a, $b)

	Local $bt = Exp(_stat_GammaLn($a + $b) - _stat_GammaLn($a) - _stat_GammaLn($b) + $a * Log($x) + $b * Log(1 - $x))

	If $x < ($a + 1) / ($a + $b + 2) Then Return $bt * __stat_betacf($x, $a, $b) / $a
	Return 1 - $bt * __stat_betacf(1 - $x, $b, $a) / $b
EndFunc   ;==>_stat_betaRI


; #FUNCTION# ======================================================================================
; Name ..........: __stat_betaI_approx()
; Description ...: Incomplete beta by quadrature. Returns I_x(a; b)
; Syntax ........: __stat_betaI_approx($x, $a, $b)
; Author ........: translation to AutoIt: AspirinJunkie
; Remarks .......: Source of algorithm = Numerical Receipes - The Art of Scientific Computing 3rd ed (Cambridge 2007)
; Related .......: __stat_max, __stat_min, _stat_GammaLn
; =================================================================================================
Func __stat_betaRI_approx($x, $a, $b)
	Local Static $y[18] = [0.0021695375159141994, 0.011413521097787704, 0.027972308950302116, 0.051727015600492421, 0.082502225484340941, 0.12007019910960293, 0.16415283300752470, 0.21442376986779355, 0.27051082840644336, 0.33199876341447887, 0.39843234186401943, 0.46931971407375483, 0.54413605556657973, 0.62232745288031077, 0.70331500465597174, 0.78649910768313447, 0.87126389619061517, 0.95698180152629142]
	Local Static $w[18] = [0.0055657196642445571, 0.012915947284065419, 0.020181515297735382, 0.027298621498568734, 0.034213810770299537, 0.040875750923643261, 0.047235083490265582, 0.053244713977759692, 0.058860144245324798, 0.064039797355015485, 0.068745323835736408, 0.072941885005653087, 0.076598410645870640, 0.079687828912071670, 0.082187266704339706, 0.084078218979661945, 0.085346685739338721, 0.085983275670394821]

	Local $j, $xu, $t, $sum, $ans
	Local $a1 = $a - 1.0, $b1 = $b - 1.0, $mu = $a / ($a + $b)
	Local $lnmu = Log($mu), $lnmuc = Log(1.0 - $mu)

	$t = Sqrt($a * $b / (($a + $b) ^ 2 * ($a + $b + 1.0)))

	If ($x > $a / ($a + $b)) Then
		If $x >= 1 Then Return 1
		$xu = __stat_min(1, __stat_max($mu + 10 * $t, $x + 5 * $t))
	Else
		If $x < 0 Then Return 0
		$xu = __stat_max(0, __stat_min($mu - 10 * $t, $x - 5 * $t))
	EndIf

	$sum = 0
	For $j = 0 To 17
		$t = $x + ($xu - $x) * $y[$j]
		$sum += $w[$j] * Exp($a1 * (Log($t) - $lnmu) + $b1 * (Log(1 - $t) - $lnmuc))
	Next

	$ans = $sum * ($xu - $x) * Exp($a1 * $lnmu - _stat_GammaLn($a) + $b1 * $lnmuc - _stat_GammaLn($b) + _stat_GammaLn($a + $b))
	Return $ans > 0 ? 1 - $ans : -$ans


EndFunc   ;==>__stat_betaRI_approx


; #FUNCTION# ======================================================================================
; Name ..........: __stat_betacf()
; Description ...: Evaluates continued fraction for incomplete beta function by modified Lentz’s method
; Syntax ........: __stat_betacf($x, $a, $b, Const[ $EPS = $STAT_EPS, Const[ $FPMIN = $STAT_FPMIN]])
; Author ........: translation to AutoIt: AspirinJunkie
; Remarks .......: Source of algorithm = Numerical Receipes - The Art of Scientific Computing 3rd ed (Cambridge 2007)
; =================================================================================================
Func __stat_betacf($x, $a, $b, Const $EPS = $STAT_EPS, Const $FPMIN = $STAT_FPMIN)
	Local $m, $m2, $aa, $del
	Local $qab = $a + $b, $qap = $a + 1.0, $qam = $a - 1.0, $c = 1, $d = 1 - $qab * $x / $qap
	If Abs($d) < $FPMIN Then $d = $FPMIN
	$d = 1 / $d
	Local $h = $d

	For $m = 1 To 9999
		$m2 = 2 * $m
		$aa = $m * ($b - $m) * $x / (($qam + $m2) * ($a + $m2))
		$d = 1 + $aa * $d
		If Abs($d) < $FPMIN Then $d = $FPMIN
		$c = 1 + $aa / $c
		If Abs($c) < $FPMIN Then $c = $FPMIN
		$d = 1 / $d
		$h *= $d * $c
		$aa = -($a + $m) * ($qab + $m) * $x / (($a + $m2) * ($qap + $m2))
		$d = 1 + $aa * $d
		If Abs($d) < $FPMIN Then $d = $FPMIN
		$c = 1 + $aa / $c
		If Abs($c) < $FPMIN Then $c = $FPMIN
		$d = 1 / $d
		$del = $d * $c
		$h *= $del
		If (Abs($del - 1) <= $EPS) Then ExitLoop
	Next
	Return $h
EndFunc   ;==>__stat_betacf


; #FUNCTION# ======================================================================================
; Name ..........: _stat_betaRI_inv()
; Description ...: Inverse of incomplete beta function. Returns x such that Ix.a; b/ D p for argument p between 0 and 1 (regularized incomplete beta function)
; Syntax ........: _stat_betaRI_inv($p, $a, $b)
; Author ........: translation to AutoIt: AspirinJunkie
; Related .......: _stat_GammaLn, __stat_min, _stat_betaRI
; Remarks .......: Source of algorithm = Numerical Receipes - The Art of Scientific Computing 3rd ed (Cambridge 2007)
; =================================================================================================
Func _stat_betaRI_inv($p, $a, $b)
	Local Const $EPS = 1E-8
	Local $pp, $t, $u, $err, $x, $al, $h, $w, $afac, $a1 = $a - 1, $b1 = $b - 1, $j

	If $p <= 0 Then
		Return 0
	ElseIf $p >= 1 Then
		Return 1
	ElseIf $a >= 1 And $b >= 1 Then

		$pp = $p < 0.5 ? $p : 1 - $p
		$t = Sqrt(-2 * Log($pp))
		$x = (2.30753 + $t * 0.27061) / (1 + $t * (0.99229 + $t * 0.04481)) - $t
		If $p < 0.5 Then $x = -$x
		$al = ($x * $x - 3) / 6
		$h = 2 / (1 / (2 * $a - 1) + 1 / (2 * $b - 1))
		$w = ($x * Sqrt($al + $h) / $h) - (1 / (2 * $b - 1) - 1 / (2 * $a - 1)) * ($al + 5 / 6 - 2 / (3 * $h))
		$x = $a / ($a + $b * Exp(2 * $w))
	Else
		Local $lna = Log($a / ($a + $b)), $lnb = Log($b / ($a + $b))
		$t = Exp($a * $lna) / $a
		$u = Exp($b * $lnb) / $b
		$w = $t + $u
		If $p < ($t / $w) Then
			$x = ($a * $w * $p) ^ (1 / $a)
		Else
			$x = 1 - (($b * $w * (1 - $p)) ^ (1 / $b))
		EndIf
	EndIf

	$afac = -_stat_GammaLn($a) - _stat_GammaLn($b) + _stat_GammaLn($a + $b)
	For $j = 0 To 9
		If $x = 0 Or $x = 1 Then Return $x
		$err = _stat_betaRI($x, $a, $b) - $p
		$t = Exp($a1 * Log($x) + $b1 * Log(1 - $x) + $afac)
		$u = $err / $t
		$t = $u / (1 - 0.5 * __stat_min(1, $u * ($a1 / $x - $b1 / (1 - $x))))
		$x -= $t
		If $x <= 0 Then $x = 0.5 * ($x + $t)
		If $x >= 1 Then $x = 0.5 * ($x + $t + 1)
		If (Abs($t) < $EPS * $x) And $j > 0 Then ExitLoop
	Next
	Return $x
EndFunc   ;==>_stat_betaRI_inv




; #FUNCTION# ======================================================================================
; Name ..........: __invxlogx()
; Description ...: Inverse of the Function x log(x)
; Syntax ........: __invxlogx($y)
; Parameters ....: $y - y
; Author ........: AspirinJunkie
; =================================================================================================
Func __invxlogx($y)
	Local Const $ooe = 0.367879441171442322
	Local $t, $u, $to = 0

	If $y >= 0 Or $y <= -$ooe Then Return SetError(1, 0, "")
	$u = $y < -0.2 ? Log($ooe - Sqrt(2 * $ooe * ($y + $ooe))) : -10
	Do
		$t = (Log($y / $u) - $u) * ($u / (1 + $u))
		$u += $t
		If $t < 1E-8 And Abs($t + $to) < 0.01 * Abs($t) Then ExitLoop
		$to = $t
	Until Abs($t / $u) <= 1E-15
	Return Exp($u)
EndFunc   ;==>__invxlogx




