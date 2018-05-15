package com.acbr.posprinter;

import java.util.HashMap;
import java.util.Map;

public enum SerialStopBytes {
    One(0),
    OnePointFive(1),
    Two(2);

    private static final Map<Integer, SerialStopBytes> map;
    private final int enumValue;

    static {
        map = new HashMap<>();
        for (SerialStopBytes value : SerialStopBytes.values()) {
            map.put(value.asInt(), value);
        }
    }

    public static SerialStopBytes valueOf(int value) {
        return map.get(value);
    }

    private SerialStopBytes(int id) {
        this.enumValue = id;
    }

    public int asInt() {
        return enumValue;
    }
}
