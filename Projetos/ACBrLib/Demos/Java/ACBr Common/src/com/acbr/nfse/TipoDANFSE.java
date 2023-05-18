/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.acbr.nfse;

import java.util.HashMap;
import java.util.Map;

public enum TipoDANFSE {
    Retrato(0),
    Paisagem(1);
    
    private static final Map<Integer, TipoDANFSE> map;
    private final int enumValue;
    
    static {
        map = new HashMap<>();
        for (TipoDANFSE value : TipoDANFSE.values()) {
            map.put(value.asInt(), value);
        }
    }
    
    public static TipoDANFSE valueOf(int value){
        return map.get(value);
    }
    
    private TipoDANFSE(int id){
        this.enumValue = id;
    }
    
    public int asInt() {
        return enumValue;
    }
}
