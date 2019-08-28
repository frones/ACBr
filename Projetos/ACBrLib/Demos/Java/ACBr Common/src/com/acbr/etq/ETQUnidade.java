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
public enum  ETQUnidade {
    etqMilimetros(0),
    etqPolegadas(1),
    etqDots(2),
    etqDecimoDeMilimetros(3);
    
    private static final Map<Integer, ETQUnidade> map;
    private final int enumValue; 
    
    static {
        map = new HashMap<>();
        for (ETQUnidade value : ETQUnidade.values()) {
            map.put(value.asInt(), value);
        }
    }
    
    public static ETQUnidade valueOf(int value) {
        return map.get(value);
    }
    
    private ETQUnidade(int id) {
        this.enumValue = id;
    }
    
    public int asInt() {
        return enumValue;
    }
}
