package br.com.acbr.lib.comum.ini;

import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.util.HashMap;

public class ACBrIniSection extends HashMap<String, String> {

    private ACBrIniFile parent;
    private String name;

    public ACBrIniSection(String name) {
        this(null, name);
    }

    public ACBrIniSection(ACBrIniFile parent, String name) {
        this.parent = parent;
        this.name = name;
    }

    public ACBrIniFile getParent() {
        return parent;
    }

    public void setParent(ACBrIniFile parent) {
        this.parent = parent;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    protected void save(Writer stream) throws IOException {
        stream.write("[" + name + "]\n");
        for (Entry<String, String> iniData : this.entrySet()) {
            stream.write(iniData.getKey() + "=" + iniData.getValue() + "\n");
        }
        stream.write("\n");
    }

    public <TType> TType getValue(String key, TType defaultValue) {
        if (key == null || key.trim().isEmpty()) {
            return defaultValue;
        }

        TType result;
        try {
            if (!containsKey(key)) {
                return defaultValue;
            }
            result = IniValueWrapper.unwrap(this.get(key), defaultValue);
        } catch (Exception e) {
            result = defaultValue;
        }
        return result;
    }

    public <TType> void setValue(String key, TType value) {
        put(key, IniValueWrapper.wrap(value));
    }

    @Override
    public String toString() {
        StringWriter writer = new StringWriter();
        try {
            save(writer);
        } catch (IOException e) {
            e.printStackTrace();
        }
        return writer.toString();
    }
}
