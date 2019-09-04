package com.acbr.boleto;

import java.util.HashMap;
import java.util.Map;

public enum CNABBoleto {
    CNAB240(0),
    CNAB400(1);   
    
    private static final Map<Integer, CNABBoleto> map;
    private final int enumValue;

    static {
        map = new HashMap<>();
        for (CNABBoleto value : CNABBoleto.values()) {
            map.put(value.asInt(), value);
        }
    }
    
    public static CNABBoleto valueOf(int value) {
        return map.get(value);
    }

    private CNABBoleto(int id) {
        this.enumValue = id;
    }

    public int asInt() {
        return enumValue;
    }
}
