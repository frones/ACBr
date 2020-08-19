package com.acbr.gnre;

import java.util.HashMap;
import java.util.Map;

public enum TamanhoPapel {
    
    tpA4(0),
    tpA4_2vias(1),
    tpA5(2);
    
    private static final Map<Integer, TamanhoPapel>map;
    private final int enumValue;
    
    static{
        map = new HashMap<>();
        for (TamanhoPapel value : TamanhoPapel.values()){
            map.put(value.asInt(), value);
        }
    }
    
    public static TamanhoPapel valueOf(int value){
        return map.get(value);
    }
    
    private TamanhoPapel(int id){
        this.enumValue = id;
    }
    
    public int asInt(){
        return enumValue;
    }
    
}
