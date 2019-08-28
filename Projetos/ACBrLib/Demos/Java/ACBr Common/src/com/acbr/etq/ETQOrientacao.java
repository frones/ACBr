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
public enum  ETQOrientacao {
    orNormal(0),
    or270(1),
    or180(2),
    or90(3);
    
    private static final Map<Integer, ETQOrientacao> map;
    private final int enumValue; 
    
    static {
        map = new HashMap<>();
        for (ETQOrientacao value : ETQOrientacao.values()) {
            map.put(value.asInt(), value);
        }
    }
    
    public static ETQOrientacao valueOf(int value) {
        return map.get(value);
    }
    
    private ETQOrientacao(int id) {
        this.enumValue = id;
    }
    
    public int asInt() {
        return enumValue;
    }
}
