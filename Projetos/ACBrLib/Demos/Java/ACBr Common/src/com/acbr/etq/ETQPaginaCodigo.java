package com.acbr.etq;

import java.util.HashMap;
import java.util.Map;

public enum ETQPaginaCodigo {

    pceNone(0),
    pce437(1),
    pce850(2),
    pce852(3),
    pce860(4),
    pce1250(5),
    pce1252(6);
    
    private static final Map<Integer, ETQPaginaCodigo> map;
    private final int enumValue; 
    
    static {
        map = new HashMap<>();
        for (ETQPaginaCodigo value : ETQPaginaCodigo.values()) {
            map.put(value.asInt(), value);
        }
    }
    
    public static ETQPaginaCodigo valueOf(int value) {
        return map.get(value);
    }
    
    private ETQPaginaCodigo(int id) {
        this.enumValue = id;
    }
    
    public int asInt() {
        return enumValue;
    }
}
