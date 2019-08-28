package com.acbr.sat;

import java.util.HashMap;
import java.util.Map;

public enum AutoSimNao {
    rAuto(0),
    rSim(1),
    rNao(2);
    
    private static final Map<Integer, AutoSimNao> map;
    private final int enumValue;

    static {
        map = new HashMap<>();
        for (AutoSimNao value : AutoSimNao.values()) {
            map.put(value.asInt(), value);
        }
    }

    public static AutoSimNao valueOf(int value) {
        return map.get(value);
    }

    private AutoSimNao(int id) {
        this.enumValue = id;
    }

    public int asInt() {
        return enumValue;
    }
}
