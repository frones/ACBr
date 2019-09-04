package com.acbr.boleto;

import java.util.HashMap;
import java.util.Map;

public enum TipoCarteiraBoleto {
    tctSimples(0), 
    tctRegistrada(1), 
    tctEletronica(2);
    
    private static final Map<Integer, TipoCarteiraBoleto> map;
    private final int enumValue;

    static {
        map = new HashMap<>();
        for (TipoCarteiraBoleto value : TipoCarteiraBoleto.values()) {
            map.put(value.asInt(), value);
        }
    }
    
    public static TipoCarteiraBoleto valueOf(int value) {
        return map.get(value);
    }

    private TipoCarteiraBoleto(int id) {
        this.enumValue = id;
    }

    public int asInt() {
        return enumValue;
    }
}
