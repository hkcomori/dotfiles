class ArrayUtil {
    equal(arr1, arr2) {
        if (arr1.Length != arr2.Length)
            return False
        Loop arr1.Length {
            if (arr1[A_Index] != arr2[A_Index])
                return False
        }
        return True
    }
    ; Get sliced array (arr[start:end])
    slice(arr, start, end := "") {
        if(end == "" || end > arr.MaxIndex())
            end := arr.MaxIndex()
        if(end < 0)
            end := arr.MaxIndex() + end
        if(start<arr.MinIndex())
            start := arr.MinIndex()
        len := arr.Length
        if((len <= 0) || (start is not integer) || (end is not integer))
            return []

        ret := []
        c := end - start
        loop % c + 1
            ret.push(arr[A_Index + start - 1])
        return ret
    }
    ; Get string by concatenating all of items in arr, separated by sep
    join(arr, sep) {
        joined := ""
        for _, item in arr
            joined .= item . sep
        joined := RTrim(joined, sep)
        return joined
    }
    ; Check if arr has item
    in(arr, item) {
        matcher := ArrayMatcher(arr*)
        return matcher.in(item)
    }
    ; Check if arr has items containing str
    contains(arr, str) {
        matcher := ArrayMatcher(arr*)
        return matcher.contains(str)
    }
}

class ArrayMatcher {
    __New(args*) {
        this.pattern := ArrayUtil.join(args, "|")
    }
    ; Check if array has item
    in(item) {
        Return (item ~= "i)\A(" . this.pattern . ")\z")
    }
    ; Check if array has items containing str
    contains(str) {
        Return ((str ~= "i)" . this.pattern) != 0)
    }
}
