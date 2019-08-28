package com.acbr.serial;

import java.util.HashMap;
import java.util.Map;

public enum  SerialHandShake {
    Nenhum(0),
    XON_XOFF(1),
    RTS_CTS(2),
    DTR_DSR(3);
    
    private static final Map<Integer, SerialHandShake> map;
    private final int enumValue; 
    
    static {
        map = new HashMap<>();
        for (SerialHandShake value : SerialHandShake.values()) {
            map.put(value.asInt(), value);
        }
    }
    
    public static SerialHandShake valueOf(int value) {
        return map.get(value);
    }
    
    private SerialHandShake(int id) {
        this.enumValue = id;
    }
    
    public int asInt() {
        return enumValue;
    }
}
