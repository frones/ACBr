package com.acbr.gnre;

import java.util.HashMap;
import java.util.Map;

public enum VersaoDF {
    
    ve100(0);
    
    private static final Map<Integer, VersaoDF>map;
    private final int enumValue;
    
    static{
        map = new HashMap<>();
        for (VersaoDF value : VersaoDF.values()){
            map.put(value.asInt(), value);
        }
    }
    
    public static VersaoDF valueOf(int value){
        return map.get(value);
    }
    
    private VersaoDF(int id){
        this.enumValue = id;
    }
    
    public int asInt(){
        return enumValue;
    }
    
}
