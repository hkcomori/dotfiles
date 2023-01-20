class ConfigUtil {
    static read(filepath, defaults := "") {
        if (defaults == "") {
            defaults := {}
        }
        sections := defaults.Clone()
        for section, sectionData in sections.OwnProps() {
            for key, defaultValue in sectionData.OwnProps() {
                value := IniRead(filepath, section, key, defaultValue)
                sections[section][key] := value
            }
        }
        return sections
    }
}
