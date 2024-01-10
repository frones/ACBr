/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.acbr.pixcd;

import com.acbr.pixcd.*;
import java.util.HashMap;
import java.util.Map;

public enum NivelLogPSP {
        logPSPNenhum (0),
        logPSPBaixo (1),
        logPSPNormal (2),
        logPSPAlto  (3),
        logPSPMuitoAlto (4);
    
    private static final Map<Integer, NivelLogPSP> map;
    private final int enumValue;
    
    static {
        map = new HashMap<>();
        for (NivelLogPSP value : NivelLogPSP.values()) {
            map.put(value.asInt(), value);
        }
    }
    
    public static NivelLogPSP valueOf(int value){
        return map.get(value);
    }
    
    private NivelLogPSP(int id){
        this.enumValue = id;
    }
    
    public int asInt() {
        return enumValue;
    }
}
