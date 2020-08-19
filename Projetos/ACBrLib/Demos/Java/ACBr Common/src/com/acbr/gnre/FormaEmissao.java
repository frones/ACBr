package com.acbr.gnre;

import java.util.HashMap;
import java.util.Map;

public enum FormaEmissao {
    
    teNormal(0),
    teContingencia(1);
    
    private static final Map<Integer, FormaEmissao>map;
    private final int enumValue;
    
    static{
        map = new HashMap<>();
        for (FormaEmissao value : FormaEmissao.values()){
            map.put(value.asInt(), value);
        }
    }
    
    public static FormaEmissao valueOf(int value){
        return map.get(value);
    }
    
    private FormaEmissao(int id){
        this.enumValue = id;
    }
    
    public int asInt(){
        return enumValue;
    }
    
}
