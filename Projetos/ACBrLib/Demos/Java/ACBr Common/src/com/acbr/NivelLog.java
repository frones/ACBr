package com.acbr;

import java.util.HashMap;
import java.util.Map;

public enum NivelLog {
    logNenhum(0),
    logSimples(1),
    logNormal(2),
    logCompleto(3),
    logParanoico(4);
    
    private static final Map<Integer, NivelLog> map;
    private final int enumValue;

    static {
        map = new HashMap<>();
        for (NivelLog value : NivelLog.values()) {
            map.put(value.asInt(), value);
        }
    }

    public static NivelLog valueOf(int value) {
        return map.get(value);
    }

    private NivelLog(int id) {
        this.enumValue = id;
    }

    public int asInt() {
        return enumValue;
    }
}
