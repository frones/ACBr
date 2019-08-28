package com.acbr.mail;



import java.util.HashMap;
import java.util.Map;

public enum MessPriority {
    MP_unknown(0),
    MP_low(1),
    MP_normal(2),
    MP_high(3);
    
    private static final Map<Integer, MessPriority> map;
    private final int enumValue;

    static {
        map = new HashMap<>();
        for (MessPriority value : MessPriority.values()) {
            map.put(value.asInt(), value);
        }
    }

    public static MessPriority valueOf(int value) {
        return map.get(value);
    }

    private MessPriority(int id) {
        this.enumValue = id;
    }

    public int asInt() {
        return enumValue;
    }
}
