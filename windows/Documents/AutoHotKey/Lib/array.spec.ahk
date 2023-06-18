#SingleInstance, Force
SetWorkingDir, %A_ScriptDir%

#Include <Yunit\Yunit>
#Include <Yunit\Stdout>

#Include %A_ScriptDir%\array.ahk

Tester := Yunit.Use(YunitStdout).Test(TestArrayUtil, TestArrayMatcher)

class TestArrayUtil {
    equal() {
        arr := [1, 2, 3, 4, 5]

        Yunit.assert(ArrayUtil.equal([], []) == True)
        Yunit.assert(ArrayUtil.equal(arr, [1, 2, 3, 4, 5]) == True)
        Yunit.assert(ArrayUtil.equal(arr, [2, 2, 3, 4, 5]) != True)
        Yunit.assert(ArrayUtil.equal(arr, [1, 2, 3, 4, 4]) != True)
        Yunit.assert(ArrayUtil.equal(arr, [1, 2, 3, 4]) != True)
    }
    slice() {
        arr := [1, 2, 3, 4, 5]

        Yunit.assert(ArrayUtil.equal(ArrayUtil.slice(arr, 2), [2, 3, 4, 5]) == True)
        Yunit.assert(ArrayUtil.equal(ArrayUtil.slice(arr, 2, 5), [2, 3, 4, 5]) == True)
        Yunit.assert(ArrayUtil.equal(ArrayUtil.slice(arr, 2, 6), [2, 3, 4, 5]) == True)
        Yunit.assert(ArrayUtil.equal(ArrayUtil.slice(arr, 1, 4), [1, 2, 3, 4]) == True)
        Yunit.assert(ArrayUtil.equal(ArrayUtil.slice(arr, 0, 4), [1, 2, 3, 4]) == True)
        Yunit.assert(ArrayUtil.equal(ArrayUtil.slice(arr, -1, 4), [1, 2, 3, 4]) == True)
        Yunit.assert(ArrayUtil.equal(ArrayUtil.slice(arr, 2, -2), [2, 3]) == True)
    }
    join() {
        arr := [1, 2, 3, 4, 5]

        result := ArrayUtil.join(arr, "|")
        answer := "1|2|3|4|5"
        Yunit.assert(result == answer)

        result := ArrayUtil.join(arr, ",")
        answer := "1,2,3,4,5"
        Yunit.assert(result == answer)
    }
    in() {
        arr := [1, 2, 3, 4, 5]

        Yunit.assert(ArrayUtil.in(arr, 1) == True)
        Yunit.assert(ArrayUtil.in(arr, 0) != True)
    }
    contains() {
        arr := [1, 2, 3, 4, 5]

        Yunit.assert(ArrayUtil.contains(arr, 1) == True)
        Yunit.assert(ArrayUtil.contains(arr, 0) != True)
    }
}

class TestArrayMatcher {
    in() {
        arr := [1, 2, 3, 4, 5]
        matcher := new ArrayMatcher(arr*)

        Yunit.assert(matcher.in(1) == True)
        Yunit.assert(matcher.in(5) == True)
        Yunit.assert(matcher.in(0) != True)
        Yunit.assert(matcher.in(6) != True)
        Yunit.assert(matcher.in("a1") != True)
    }
    contains() {
        arr := [1, 2, 3, 4, 5]
        matcher := new ArrayMatcher(arr*)

        Yunit.assert(matcher.contains(1) == True)
        Yunit.assert(matcher.contains(5) == True)
        Yunit.assert(matcher.contains(0) != True)
        Yunit.assert(matcher.contains(6) != True)
        Yunit.assert(matcher.contains("a1") == True)
        Yunit.assert(matcher.contains("5a") == True)
        Yunit.assert(matcher.contains("a3a") == True)
        Yunit.assert(matcher.contains("a0") != True)
    }
}
