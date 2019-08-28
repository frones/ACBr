package com.acbr.dfe;

import java.util.HashMap;
import java.util.Map;

public enum TipoRelatorioBobina {
    tpFortes(0),
    tpEscPos(1);
    
    private static final Map<Integer, TipoRelatorioBobina> map;
    private final int enumValue;

    static {
        map = new HashMap<>();
        for (TipoRelatorioBobina value : TipoRelatorioBobina.values()) {
            map.put(value.asInt(), value);
        }
    }

    public static TipoRelatorioBobina valueOf(int value) {
        return map.get(value);
    }

    private TipoRelatorioBobina(int id) {
        this.enumValue = id;
    }

    public int asInt() {
        return enumValue;
    }
}
