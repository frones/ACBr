package br.com.acbr.lib.comum.ini;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;

public class IniUtil {
    public static <T> void writeToIni(ACBrIniFile iniData, T obj, String sectionName) {
        if (obj == null) return;

        ACBrIniSection section = iniData.contains(sectionName)
                ? iniData.getSection(sectionName)
                : iniData.addNew(sectionName);

        writeToIni(section, obj);
    }

    public static <T> void writeToIni(ACBrIniSection section, T obj) {
        if (obj == null) return;

        Class<?> clazz = obj.getClass();
        for (Field field : clazz.getDeclaredFields()) {
            if (Modifier.isStatic(field.getModifiers()) || Modifier.isTransient(field.getModifiers())) continue;

            field.setAccessible(true);
            try {
                Object value = field.get(obj);
                if (value != null) {
                    section.setValue(field.getName(), value.toString());
                }
            } catch (IllegalAccessException e) {
                e.printStackTrace();
            }
        }
    }

    public static <T> T readFromIni(ACBrIniFile iniData, Class<T> clazz, String sectionName) {
        if (!iniData.contains(sectionName)) return null;

        ACBrIniSection section = iniData.getSection(sectionName);
        return readFromIni(section, clazz);
    }

    public static <T> T readFromIni(ACBrIniSection section, Class<T> clazz) {
        try {
            T obj = clazz.getDeclaredConstructor().newInstance();

            for (Field field : clazz.getDeclaredFields()) {
                if (Modifier.isStatic(field.getModifiers()) || Modifier.isTransient(field.getModifiers())) continue;

                field.setAccessible(true);
                String value = section.getValue(field.getName(), "");
                if (!value.isEmpty()) {
                    setFieldValue(field, obj, value);
                }
            }

            return obj;
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
    }

    private static void setFieldValue(Field field, Object obj, String value) {
        try {
            Class<?> fieldType = field.getType();
            if (fieldType == int.class || fieldType == Integer.class) {
                field.set(obj, Integer.parseInt(value));
            } else if (fieldType == long.class || fieldType == Long.class) {
                field.set(obj, Long.parseLong(value));
            } else if (fieldType == float.class || fieldType == Float.class) {
                field.set(obj, Float.parseFloat(value));
            } else if (fieldType == double.class || fieldType == Double.class) {
                field.set(obj, Double.parseDouble(value));
            } else if (fieldType == boolean.class || fieldType == Boolean.class) {
                field.set(obj, Boolean.parseBoolean(value));
            } else {
                field.set(obj, value);
            }
        } catch (IllegalAccessException e) {
            e.printStackTrace();
        }
    }
}
