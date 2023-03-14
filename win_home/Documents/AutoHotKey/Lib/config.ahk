class ConfigUtil {
    static read(filepath, defaults := "") {
        if (defaults == "") {
            defaults := {}
        }
        sections := defaults.Clone()
        for section, sectionData in sections.OwnProps() {
            for key, defaultValue in sectionData.OwnProps() {
                readed := ConfigValue(IniRead(filepath, section, key, defaultValue))
                sections.%section%.%key% := readed.value
            }
        }
        return sections
    }
}

class ConfigValue {
    __New(value) {
        switch (value) {
            case "True", "true":
                this.value := true
            case "False", "false":
                this.value := false
            default:
                this.value := value
        }
    }
}
