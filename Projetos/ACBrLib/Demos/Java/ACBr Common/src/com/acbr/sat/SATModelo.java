package com.acbr.sat;

import java.util.HashMap;
import java.util.Map;

public enum SATModelo {
    satNenhum(0),
    satDinamico_cdecl(1),
    satDinamico_stdcall(2),
    mfe_Integrador_XML(3);
    
    private static final Map<Integer, SATModelo> map;
    private final int enumValue;

    static {
        map = new HashMap<>();
        for (SATModelo value : SATModelo.values()) {
            map.put(value.asInt(), value);
        }
    }
    
    public static SATModelo valueOf(int value) {
        return map.get(value);
    }

    private SATModelo(int id) {
        this.enumValue = id;
    }

    public int asInt() {
        return enumValue;
    }
}
