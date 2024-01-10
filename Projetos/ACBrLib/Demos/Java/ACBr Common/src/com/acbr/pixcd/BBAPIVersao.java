/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.acbr.pixcd;

import com.acbr.pixcd.*;
import java.util.HashMap;
import java.util.Map;

public enum BBAPIVersao {
    apiVersao1(0),
    apiVersao2(1);
    
    private static final Map<Integer, BBAPIVersao> map;
    private final int enumValue;
    
    static {
        map = new HashMap<>();
        for (BBAPIVersao value : BBAPIVersao.values()) {
            map.put(value.asInt(), value);
        }
    }
    
    public static BBAPIVersao valueOf(int value){
        return map.get(value);
    }
    
    private BBAPIVersao(int id){
        this.enumValue = id;
    }
    
    public int asInt() {
        return enumValue;
    }
}
