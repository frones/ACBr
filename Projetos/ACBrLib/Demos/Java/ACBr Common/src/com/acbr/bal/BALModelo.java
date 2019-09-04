/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.acbr.bal;

import java.util.HashMap;
import java.util.Map;

/**
 *
 * @author rften
 */
public enum  BALModelo {
    balNenhum(0), 
    balFilizola(1),
    balToledo(2),
    balToledo2090(3),
    balToledo2180(4),
    balUrano(5),
    balLucasTec(6),
    balMagna(7),
    balDigitron(8),
    balMagellan(9),
    balUranoPOP(10),
    balLider(11),
    balRinnert(12),
    balMuller(13),
    balSaturno(14),
    balAFTS(15),
    balGenerica(16),
    balLibratek(17),
    balMicheletti(18),
    balAlfa(19),
    balToledo9091_8530_8540(20),
    balWeightechWT1000(21), 
    balMarelCG62XL(22);
    
    private static final Map<Integer, BALModelo> map;
    private final int enumValue; 
    
    static {
        map = new HashMap<>();
        for (BALModelo value : BALModelo.values()) {
            map.put(value.asInt(), value);
        }
    }
    
    public static BALModelo valueOf(int value) {
        return map.get(value);
    }
    
    private BALModelo(int id) {
        this.enumValue = id;
    }
    
    public int asInt() {
        return enumValue;
    }
}
