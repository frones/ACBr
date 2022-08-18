package com.acbr.boleto;

import java.util.HashMap;
import java.util.Map;

public enum LogRegistro {
    Nao(0),
    Sim(1);
    
    private static final Map<Integer, LogRegistro> map;
    private final int enumValue;
    
    static {
        map = new HashMap<>();
        for (LogRegistro value : LogRegistro.values()) {
            map.put (value.asInt(), value);
        }
    }
    
    public static LogRegistro valueOf(int value){
        return map.get(value);
    }
    
    private LogRegistro (int id) {
        this.enumValue = id;
    }
    
    public int asInt() {
        return enumValue;
    }
}
