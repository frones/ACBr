/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.acbr.etq;

import java.util.HashMap;
import java.util.Map;

/**
 *
 * @author rften
 */
public enum  ETQDPI {
    dpi203(0),
    dpi300(1),
    dpi600(2);
    
    private static final Map<Integer, ETQDPI> map;
    private final int enumValue; 
    
    static {
        map = new HashMap<>();
        for (ETQDPI value : ETQDPI.values()) {
            map.put(value.asInt(), value);
        }
    }
    
    public static ETQDPI valueOf(int value) {
        return map.get(value);
    }
    
    private ETQDPI(int id) {
        this.enumValue = id;
    }
    
    public int asInt() {
        return enumValue;
    }
}
