;--------------------------------------------------------------------------------
; Key
;--------------------------------------------------------------------------------
class KeyUtil {
    trim_modifier(key) {
        if (key.Length == 1) {
            return key
        } Else {
            return LTrim(key, "^!+#")
        }
    }
}
