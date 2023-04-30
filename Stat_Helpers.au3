#include-once

; #FUNCTION# ======================================================================================
; Name ..........: __f()
; Description ...: runs a funcion with call and change one of these parameters
; Syntax ........: __f($cb_F, $a_Params, $x, $i_ind)
; Parameters ....: $cb_F     - function
;                  $a_Params - parameter-array for Call()
;                  $x        - the new value
;                  $i_ind    - the index of the value
; Remarks .......: used in _stat_Int_Romberg and _stat_findFuncVal
; =================================================================================================
Func __f($cb_F, $a_Params, $x, $i_ind)
	$a_Params[$i_ind] = $x
	Return Call($cb_F, $a_Params)
EndFunc   ;==>__f

; #FUNCTION# ======================================================================================
; Name ..........: __stat_max()
; Description ...: returns the higher value of the two parameters
; Syntax ........: __stat_max($a, $b)
; Author ........: AspirinJunkie
; =================================================================================================
Func __stat_max(Const $a, Const $b)
	Return $a >= $b ? $a : $b
EndFunc   ;==>__stat_max

; #FUNCTION# ======================================================================================
; Name ..........: __stat_min()
; Description ...: returns the lower value of the two parameters
; Syntax ........: __stat_min($a, $b)
; Author ........: AspirinJunkie
; =================================================================================================
Func __stat_min(Const $a, Const $b)
	Return $a <= $b ? $a : $b
EndFunc   ;==>__stat_min

; #FUNCTION# ======================================================================================
; Name ..........: __stat_EPS()
; Description ...: calculate the machine accuracy (floating point rounding error)
; Syntax ........: __stat_EPS()
; Return values .: Epsilon
; Author ........: AspirinJunkie
; =================================================================================================
Func __stat_EPS()
	Local $x = 1
	While True
		$x /= 2
		If 1 + $x <= 1 Then ExitLoop
	WEnd
	Return 2 * $x
EndFunc   ;==>__stat_EPS