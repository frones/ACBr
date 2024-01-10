/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.acbr.pixcd;

import com.acbr.pixcd.*;
import java.util.HashMap;
import java.util.Map;

public enum TipoChave {
        pspBradesco (0),
        pspItau (1),
        pspBancoBrasil (2),
        pspSantander (3),
        pspShipay  (4),
        pspSicredi (5),
        pspSicoob (6),
        pspPagSeguro (7),
        pspGerenciaNet (8),
        pspPixPDV (9),
        pspInter (10),
        pspAilos (11),
        pspMatera (12),
        pspCielo (13),
        pspMercadoPago (14);
    
    private static final Map<Integer, TipoChave> map;
    private final int enumValue;
    
    static {
        map = new HashMap<>();
        for (TipoChave value : TipoChave.values()) {
            map.put(value.asInt(), value);
        }
    }
    
    public static TipoChave valueOf(int value){
        return map.get(value);
    }
    
    private TipoChave(int id){
        this.enumValue = id;
    }
    
    public int asInt() {
        return enumValue;
    }
}
