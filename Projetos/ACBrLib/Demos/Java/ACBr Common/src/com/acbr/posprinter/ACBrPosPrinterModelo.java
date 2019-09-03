package com.acbr.posprinter;

import java.util.HashMap;
import java.util.Map;

public enum  ACBrPosPrinterModelo {
    Texto(0),
    EscPosEpson(1),
    EscBematech(2),
    EscDaruma(3),
    EscVox(4),
    EscDiebold(5),
    EscEpsonP2(6),
    CustomPos(7),
    EscPosStar(8),
	EscZJiang(9);
    
    private static final Map<Integer, ACBrPosPrinterModelo> map;
    private final int enumValue; 
    
    static {
        map = new HashMap<>();
        for (ACBrPosPrinterModelo value : ACBrPosPrinterModelo.values()) {
            map.put(value.asInt(), value);
        }
    }    
       
    public static ACBrPosPrinterModelo valueOf(int value) {
        return map.get(value);
    }
    
    private ACBrPosPrinterModelo(int id) {
        this.enumValue = id;
    }
    
    public int asInt() {
        return enumValue;
    }    
}