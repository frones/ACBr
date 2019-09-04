package com.acbr.boleto;

import java.util.HashMap;
import java.util.Map;

public enum TipoDocumento {
    Tradicional(0),
    Escritural(1);
    
    private static final Map<Integer, TipoDocumento> map;
    private final int enumValue;

    static {
        map = new HashMap<>();
        for (TipoDocumento value : TipoDocumento.values()) {
            map.put(value.asInt(), value);
        }
    }
    
    public static TipoDocumento valueOf(int value) {
        return map.get(value);
    }

    private TipoDocumento(int id) {
        this.enumValue = id;
    }

    public int asInt() {
        return enumValue;
    }
}