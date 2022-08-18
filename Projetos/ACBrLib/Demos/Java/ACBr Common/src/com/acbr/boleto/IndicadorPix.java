package com.acbr.boleto;

import java.util.HashMap;
import java.util.Map;

public enum IndicadorPix {
    Nao(0),
    Sim(1);
    
    private static final Map <Integer, IndicadorPix> map;
    private final int enumValue;
    
    static{
        map = new HashMap<>();
        for (IndicadorPix value : IndicadorPix.values()) {
            map.put(value.asInt(), value);
        }
    }
    
    public static IndicadorPix valueOf(int value) {
        return map.get(value);
    }
    
    private IndicadorPix(int id) {
        this.enumValue = id;
    }
    
    public int asInt(){
        return enumValue;
    }
}
