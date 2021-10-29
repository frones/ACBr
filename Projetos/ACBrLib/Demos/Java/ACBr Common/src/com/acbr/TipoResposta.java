/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.acbr;

import java.util.HashMap;
import java.util.Map;

/**
 *
 * @author rften
 */
public enum TipoResposta {
    fmtINI(0), 
    fmtXml(1),
    fmtJSON(2);
    
    
    private static final Map<Integer, TipoResposta> intMap;
    private final int enumValue; 
    
    static {
        intMap = new HashMap<>();
        for (TipoResposta value : TipoResposta.values()) {
            intMap.put(value.asInt(), value);
        }
    }
    
    public static TipoResposta valueOf(int value) {
        return intMap.get(value);
    }
        
    private TipoResposta(int id) {
        this.enumValue = id;
    }
    
    public int asInt() {
        return enumValue;
    }
}

