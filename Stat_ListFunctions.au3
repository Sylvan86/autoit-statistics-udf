#include-once
#include <Array.au3>


; main function
If @ScriptName = "Stat_ListFunctions.au3" Then
	Local $a_Test[] = [200, 545, 290, 165, 190, 355, 200, 185, 290, 205, 290, 175]

	ConsoleWrite(_stat_Median($a_Test) & @CRLF)

;~ 	$a_Hist = _stat_Sample_Modal($a_Test)
;~ 	ConsoleWrite(@extended & @CRLF)
;~ 	_ArrayDisplay($a_Hist)

	;  $a_Parameters = _stat_Sample_Parameter($a_Test, True)
	;  _ArrayDisplay($a_Parameters)


	$m = _stat_MedianOfFive($a_Test[0], $a_Test[1], $a_Test[2], $a_Test[3], $a_Test[4])
	ConsoleWrite($m & @CRLF)
EndIf





; #FUNCTION# ======================================================================================
; Name ..........: _stat_MedianOfFive()
; Description ...: return the median of 5 input variables with maximum of 6 comparisons
; Syntax ........: _stat_MedianOfFive($a, $b, $c, $d, $e)
; Parameters ....: $a - $e: input values
; Return values .: Success: the median value of these five input values
;                  Failure
; Author ........: AspirinJunkie
; =================================================================================================
Func _stat_MedianOfFive($a, $b, $c, $d, $e)
    return $b < $a ? $d < $c ? $b < $d ? $a < $e ? $a < $d ? $e < $d ? $e : $d _
                                                 : $c < $a ? $c : $a _
                                         : $e < $d ? $a < $d ? $a : $d _
                                                 : $c < $e ? $c : $e _
                                 : $c < $e ? $b < $c ? $a < $c ? $a : $c _
                                                 : $e < $b ? $e : $b _
                                         : $b < $e ? $a < $e ? $a : $e _
                                                 : $c < $b ? $c : $b _
                         : $b < $c ? $a < $e ? $a < $c ? $e < $c ? $e : $c _
                                                 : $d < $a ? $d : $a _
                                         : $e < $c ? $a < $c ? $a : $c _
                                                 : $d < $e ? $d : $e _
                                 : $d < $e ? $b < $d ? $a < $d ? $a : $d _
                                                 : $e < $b ? $e : $b _
                                         : $b < $e ? $a < $e ? $a : $e _
                                                 : $d < $b ? $d : $b _
                 : $d < $c ? $a < $d ? $b < $e ? $b < $d ? $e < $d ? $e : $d _
                                                 : $c < $b ? $c : $b _
                                         : $e < $d ? $b < $d ? $b : $d _
                                                 : $c < $e ? $c : $e _
                                 : $c < $e ? $a < $c ? $b < $c ? $b : $c _
                                                 : $e < $a ? $e : $a _
                                         : $a < $e ? $b < $e ? $b : $e _
                                                 : $c < $a ? $c : $a _
                         : $a < $c ? $b < $e ? $b < $c ? $e < $c ? $e : $c _
                                                 : $d < $b ? $d : $b _
                                         : $e < $c ? $b < $c ? $b : $c _
                                                 : $d < $e ? $d : $e _
                                 : $d < $e ? $a < $d ? $b < $d ? $b : $d _
                                                 : $e < $a ? $e : $a _
                                         : $a < $e ? $b < $e ? $b : $e _
                                                 : $d < $a ? $d : $a
EndFunc








; #FUNCTION# ======================================================================================
; Name ..........: _stat_Median()
; Description ...: return the median of a dataset
; Syntax ........: _stat_Median($a_Sample)
; Parameters ....: $a_Sample -
; Return values .: Success
;                  Failure
; Author ........: AspirinJunkie
; Remarks .......: fast evaluation without sorting the array (quick-select algorithm)
; =================================================================================================
Func _stat_Median($a_Sample)
	Local $n = UBound($a_Sample)

	If Mod($n, 2) Then ; odd
		Return __stat_NthBiggestElement($a_Sample, ($n-1)/2)
	Else ; even
		Return 0.5* (__stat_NthBiggestElement($a_Sample, ($n/2)-1) + __stat_NthBiggestElement($a_Sample, ($n/2)))
	EndIf
EndFunc



; #FUNCTION# ======================================================================================
; Name ..........: _stat_Sample_Parameter()
; Description ...: calculate various statistical values of an dataset
; Syntax ........: _stat_Sample_Parameter($a_Sample[, $b_present = False])
; Parameters ....: $a_Sample  -
;                  $b_present - [optional]  (default:False)
; Return values .: Success: $b_present =
;                           False: human readable array with some additional values (+ modal value and median)
;                           True: [mean, standard deviation, variance, skewness, curtosis, minima, maxima, range, sum, average deviation, standard error of mean]
;                  Failure
; Author ........: AspirinJunkie
; =================================================================================================
Func _stat_Sample_Parameter($a_Sample, $b_present = False)
	; Mittelwert, Standardabweichung, Varianz, Schiefe, Wölbung, Minimum, Maximum, Spanne, Summe, mittlere absolute Abweichung, Standardfehler des Mittelwertes
	Local Enum $mean, $sdev, $var, $skew, $curt, $min, $max, $range, $sum, $adev, $sdevMean
	Local $a_Ret[11]
	Local $n = UBound($a_Sample)
	Local $ep = 0, $s = 0, $p

	$a_Ret[$max] = $a_Sample[0]
	$a_Ret[$min] = $a_Sample[0]
	For $j = 0 To $n - 1
		If $a_Sample[$j] > $a_Ret[$max] Then $a_Ret[$max] = $a_Sample[$j]
		If $a_Sample[$j] < $a_Ret[$min] Then $a_Ret[$min] = $a_Sample[$j]
		$s += $a_Sample[$j]
	Next
	$a_Ret[$sum] = $s
	$a_Ret[$range] = $a_Ret[$max] - $a_Ret[$min]
	$a_Ret[$mean] = $s / $n

	For $j = 0 To $n - 1
		$s = $a_Sample[$j] - $a_Ret[$mean]
		$a_Ret[$adev] += Abs($s)
		$ep += $s
		$p = $s * $s
		$a_Ret[$var] += $p
		$p *= $s
		$a_Ret[$skew] += $p
		$p *= $s
		$a_Ret[$curt] += $p
	Next
	$a_Ret[$adev] /= $n
	$a_Ret[$var] = ($a_Ret[$var] - $ep * $ep / $n) / ($n - 1)
	$a_Ret[$sdev] = Sqrt($a_Ret[$var])
	If $a_Ret[$var] <> 0 Then
		$a_Ret[$skew] /= ($n * $a_Ret[$var] * $a_Ret[$sdev])
		$a_Ret[$curt] = $a_Ret[$curt] / ($n * ($a_Ret[$var] ^ 2)) - 3
	EndIf

	$a_Ret[$sdevMean] = $a_Ret[$sdev] / Sqrt($n)

	If Not $b_present Then Return $a_Ret

	Local $a_Modal = _stat_Sample_Modal($a_Sample)
	Local $d_Modal = @extended

	Local $a_Ret2[14][2] = [ _
		["mean", $a_Ret[$mean]], _
		["standard deviation (L2)", $a_Ret[$sdev]], _
		["mean absolute deviation (L1)", $a_Ret[$adev]], _
		["variance", $a_Ret[$var]], _
		["skewness", $a_Ret[$skew]], _
		["curtosis", $a_Ret[$curt]], _
		["minimum", $a_Ret[$min]], _
		["maximum", $a_Ret[$max]], _
		["range", $a_Ret[$range]], _
		["sum", $a_Ret[$sum]], _
		["standard error of mean value", $a_Ret[$sdevMean]], _
		["modal value (" & $d_Modal & ")", $a_Modal[0] & (UBound($a_Modal) > 1 ? ", [...]" : "")], _
		["median", _stat_Median($a_Sample)], _
		["length", $n] _
	]
	Return $a_Ret2
EndFunc   ;==>_stat_Sample_Parameter

; #FUNCTION# ======================================================================================
; Name ..........: _stat_Sample_Modal()
; Description ...: return the modal value/values of an array
; Syntax ........: _stat_Sample_Modal(ByRef $a_List)
; Parameters ....: ByRef $a_List -
; Return values .: Success
;                  Failure
; Author ........: AspirinJunkie
; =================================================================================================
Func _stat_Sample_Modal(ByRef $a_List)
	Local $a_Hist = _stat_Sample_Hist($a_List)
	Local $n = @extended
	Local $a_Ret[UBound($a_Hist)] = [$a_Hist[0][0]]
	For $i = 1 To UBound($a_Hist) - 1
		If $a_Hist[$i][1] <> $n Then ExitLoop
		$a_Ret[$i] = $a_Hist[$i][0]
	Next
	ReDim $a_Ret[$i]
	Return SetExtended($n, $a_Ret)
EndFunc   ;==>__stat_Sample_Modal

; #FUNCTION# ======================================================================================
; Name ..........: _stat_Sample_Hist()
; Description ...: create a histogram (unique elements + counts) for an array
; Syntax ........: _stat_Sample_Hist(ByRef $a_List)
; Parameters ....: ByRef $a_List -
; Return values .: Success
;                  Failure
; Author ........: AspirinJunkie
; =================================================================================================
Func _stat_Sample_Hist(ByRef $a_List)
	Local $n = 1, $o_Dic = ObjCreate("Scripting.Dictionary")
	For $i = 0 To UBound($a_List) - 1
		$o_Dic($a_List[$i]) += 1
		If $o_Dic($a_List[$i]) > $n Then $n = $o_Dic($a_List[$i])
	Next
	Local $a_Ret[$o_Dic.Count()][2]
	Local $j = 0
	For $v In $o_Dic.Keys
		$a_Ret[$j][0] = $v
		$a_Ret[$j][1] = $o_Dic($v)
		$j += 1
	Next
	_ArraySort($a_Ret, 1, 0, 0, 1)
	Return SetExtended($n, $a_Ret)
EndFunc   ;==>__stat_Sample_Hist



; #FUNCTION# ======================================================================================
; Name ..........: __stat_NthBiggestElement
; Description ...: determine the nth biggest element in an unsorted array without sorting it (faster)
;                  one possible application is the fast calculation of the median-value
; Syntax ........: __stat_NthBiggestElement(ByRef $a_A, $d_Nth, $i_Min, $i_Max)
; Parameters ....: $a_A           - the array
;                  $d_Nth         - the theoretical position of the wanted value if the array is sorted
;                  $i_Min         - the start index for the partitioning range in the array
;                  $i_Max         - the end index for the partitioning range in the array
; Return values .: the value of the nth biggest value
; Author ........: AspirinJunkie
; =================================================================================================
Func __stat_NthBiggestElement(ByRef $a_A, $d_Nth = (UBound($a_A) = 1) ? 0 : Floor((UBound($a_A) - 1) / 2), $i_Min = 0, $i_Max = UBound($a_A) - 1)
	Do
		Local $iMiddle = Floor(($i_Max + $i_Min) / 2)
		Local $A = $a_A[$i_Min], $b = $a_A[$i_Max], $c = $a_A[$iMiddle]

		; calculate the pivot element by median(Array[min], Array[middle], Array[max])
		Local $p_Value = $A > $b ? $A > $c ? $c > $b ? $c : $b : $A : $A > $c ? $A : $c > $b ? $b : $c ; = Median(a,b,c)
		Local $p_Index = $p_Value = $A ? $i_Min : $p_Value = $b ? $i_Max : $iMiddle ; = Index(p_Value)

		; move the pivot-element to the end of the array
		If $p_Index < $i_Max Then
			$a_A[$p_Index] = $a_A[$i_Max]
			$a_A[$i_Max] = $p_Value
		EndIf

		Local $i_PivotPos = __stat_partHoare($a_A, $i_Min, $i_Max, $p_Value)

		If $i_PivotPos = $d_Nth Then
			Return $a_A[$i_PivotPos]
		ElseIf $i_PivotPos > $d_Nth Then
			$i_Max = $i_PivotPos - 1
		Else
			$i_Min = $i_PivotPos + 1
		EndIf
	Until 0
EndFunc   ;==>__stat_NthBiggestElement


; #FUNCTION# ======================================================================================
; Name ..........: __stat_partHoare
; Description ...: helper function for partitioning inside the quicksort-function
;                  there exists several algorithms for this.
; Syntax ........: __stat_partHoare(ByRef $a_Array, Const $i_Min, Const $i_Max, Const $p_Value)
; Parameters ....: $a_Array       - the array
;                  $i_Min         - the start index for the partitioning range in the array
;                  $i_Max         - the end index for the partitioning range in the array
;                  $p_Value       - the value of the pivot-element
; Return values .: the position of the pivot-element
; Author ........: AspirinJunkie
; =================================================================================================
Func __stat_partHoare(ByRef $a_Array, Const $i_Min, Const $i_Max, Const $p_Value)
	; divide the array in two separate lists in dependency of the pivot-element
	; there are several algorithms to reach this (here used: "Quickselect / Hoare's selection algorithm" - see "Lomuto's algorithm")
	Local $i = $i_Min - 1
	Local $j = $i_Max + 1
	Local $t

	Do
		; start from right and go left until the next element which is smaller than pivot:
		Do
			$j -= 1
		Until $a_Array[$j] < $p_Value Or $j = $i_Min
		; start from left and go right until the next element which is greater than pivot:
		Do
			$i += 1
		Until $a_Array[$i] > $p_Value Or $i = $i_Max

		; swap if elements are on the wrong side of the lists
		If $i < $j Then
			$t = $a_Array[$j]
			$a_Array[$j] = $a_Array[$i]
			$a_Array[$i] = $t
		EndIf
	Until $i >= $j

	; swap with pivot-element if pivot is at the wrong list-side:
	If $a_Array[$i] > $p_Value Then
		$a_Array[$i_Max] = $a_Array[$i]
		$a_Array[$i] = $p_Value
	EndIf


	Return $i
EndFunc   ;==>__stat_partHoare
