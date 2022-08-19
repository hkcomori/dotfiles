class ArrayUtil {
    ; Get sliced array (arr[start:end])
    slice(arr, start, end := "") {
        len := arr.Length()
        if(end == "" || end > len)
            end := len
        if(end < 0)
            end := len + end
        if(start<arr.MinIndex())
            start := arr.MinIndex()
        if(len<=0 || len < start || start > end)
            return []

        ret := []
        c := end - start - 1
        loop % c
            ret.push(arr[A_Index + start])
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
        matcher := new ArrayMatcher(arr*)
        return matcher.in(item)
    }
    ; Check if arr has items containing str
    contains(arr, str) {
        matcher := new ArrayMatcher(arr*)
        return matcher.contains(item)
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
        Return (str ~= "i)" . this.pattern)
    }
}
