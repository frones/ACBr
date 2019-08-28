package com.acbr.sat;

import java.util.HashMap;
import java.util.Map;

public enum indRatISSQN {
    irSim(0),
    irNao(1);
    
    private static final Map<Integer, indRatISSQN> map;
    private final int enumValue;

    static {
        map = new HashMap<>();
        for (indRatISSQN value : indRatISSQN.values()) {
            map.put(value.asInt(), value);
        }
    }

    public static indRatISSQN valueOf(int value) {
        return map.get(value);
    }

    private indRatISSQN(int id) {
        this.enumValue = id;
    }

    public int asInt() {
        return enumValue;
    }
}
