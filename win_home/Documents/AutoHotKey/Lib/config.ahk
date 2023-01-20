class ConfigUtil {
    static read(filepath, defaults := "") {
        if (defaults == "") {
            defaults := {}
        }
        sections := defaults.Clone()
        for section, sectionData in sections {
            for key, defaultValue in sectionData {
                IniRead, value, % filepath, % section, % key, % defaultValue
                sections[section][key] := value
            }
        }
        return sections
    }
}
