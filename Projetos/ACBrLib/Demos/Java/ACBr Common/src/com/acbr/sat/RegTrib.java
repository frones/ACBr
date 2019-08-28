package com.acbr.sat;

import java.util.HashMap;
import java.util.Map;

public enum RegTrib {
    RTSimplesNacional(0),
    RTRegimeNormal(1);
    
    private static final Map<Integer, RegTrib> map;
    private final int enumValue;

    static {
        map = new HashMap<>();
        for (RegTrib value : RegTrib.values()) {
            map.put(value.asInt(), value);
        }
    }

    public static RegTrib valueOf(int value) {
        return map.get(value);
    }

    private RegTrib(int id) {
        this.enumValue = id;
    }

    public int asInt() {
        return enumValue;
    }
}
