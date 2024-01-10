/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.acbr.pixcd;

import com.acbr.pixcd.*;
import java.util.HashMap;
import java.util.Map;

public enum Ambiente {
    ambTeste(0),
    ambProducao(1),
    ambPreProducao(2);
    
    private static final Map<Integer, Ambiente> map;
    private final int enumValue;
    
    static {
        map = new HashMap<>();
        for (Ambiente value : Ambiente.values()) {
            map.put(value.asInt(), value);
        }
    }
    
    public static Ambiente valueOf(int value){
        return map.get(value);
    }
    
    private Ambiente(int id){
        this.enumValue = id;
    }
    
    public int asInt() {
        return enumValue;
    }
}
