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
public enum  TipoCodBarra {
    barEAN13(0),
    barEAN8(1),
    barSTANDARD(2),
    barINTERLEAVED(3),
    barCODE128(4),
    barCODE39(5),
    barCODE93(6),
    barUPCA(7),
    barCODABAR(8),
    barMSI(9),
    barCODE11(10);
    
    private static final Map<Integer, TipoCodBarra> map;
    private final int enumValue; 
    
    static {
        map = new HashMap<>();
        for (TipoCodBarra value : TipoCodBarra.values()) {
            map.put(value.asInt(), value);
        }
    }
    
    public static TipoCodBarra valueOf(int value) {
        return map.get(value);
    }
    
    private TipoCodBarra(int id) {
        this.enumValue = id;
    }
    
    public int asInt() {
        return enumValue;
    }
}
