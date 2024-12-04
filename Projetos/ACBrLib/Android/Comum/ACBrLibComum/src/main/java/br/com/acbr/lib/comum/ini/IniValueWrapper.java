package br.com.acbr.lib.comum.ini;

import android.os.Build;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.text.NumberFormat;
import java.text.SimpleDateFormat;
import java.util.Base64;
import java.util.Date;
import java.util.Locale;

public class IniValueWrapper {

    private static final NumberFormat numberFormat = NumberFormat.getInstance(new Locale("pt", "BR"));
    private static final SimpleDateFormat dateFormat = new SimpleDateFormat("dd/MM/yyyy");
    private static final SimpleDateFormat dateTimeFormat = new SimpleDateFormat("dd/MM/yyyy HH:mm:ss");

    public static <T> String wrap(Object value) {
        if (value == null) return null;

        if (value instanceof Double || value instanceof Float || value instanceof Number) {
            return numberFormat.format(value);
        } else if (value instanceof Date) {
            Date date = (Date) value;
            return dateTimeFormat.format(date);
        } else if (value instanceof Boolean) {
            return ((Boolean) value) ? "1" : "0";
        } else if (value instanceof Enum) {
            return ((Enum<?>) value).ordinal() + "";
        } else if (value instanceof String[]) {
            return String.join("|", (String[]) value);
        } else if (value instanceof InputStream) {
            return streamToBase64((InputStream) value);
        } else {
            return value.toString();
        }
    }

    public static <T> T unwrap(String value, T defaultValue) {
        return unwrap(value, (Class<T>) defaultValue.getClass(), defaultValue);
    }

    @SuppressWarnings("unchecked")
    public static <T> T unwrap(String value, Class<T> type, T defaultValue) {
        try {
            if (type == Integer.class) {
                return (T) Integer.valueOf(value);
            } else if (type == Double.class) {
                return (T) Double.valueOf(value);
            } else if (type == Boolean.class) {
                return (T) Boolean.valueOf(value);
            }
        } catch (Exception e) {
            return defaultValue;
        }
        return defaultValue;
    }

    private static String streamToBase64(InputStream inputStream) {
        try {
            ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
            byte[] buffer = new byte[1024];
            int bytesRead;

            while ((bytesRead = inputStream.read(buffer)) != -1) {
                byteArrayOutputStream.write(buffer, 0, bytesRead);
            }
            return android.util.Base64.encodeToString(byteArrayOutputStream.toByteArray(), android.util.Base64.DEFAULT);

        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
    }

    private static InputStream base64ToStream(String base64String) {
        byte[] decodedBytes = null;
        if (android.os.Build.VERSION.SDK_INT >= android.os.Build.VERSION_CODES.O) {
            decodedBytes = Base64.getDecoder().decode(base64String);
        }
        return new ByteArrayInputStream(decodedBytes);
    }

    public static boolean canWrapUnwrap(Class<?> type) {
        return type == String.class
                || type == char.class || type == Character.class
                || type == byte.class || type == Byte.class
                || type == short.class || type == Short.class
                || type == int.class || type == Integer.class
                || type == long.class || type == Long.class
                || type == float.class || type == Float.class
                || type == double.class || type == Double.class
                || type == boolean.class || type == Boolean.class
                || type == Date.class || type == InputStream.class
                || type.isEnum();
    }
}
