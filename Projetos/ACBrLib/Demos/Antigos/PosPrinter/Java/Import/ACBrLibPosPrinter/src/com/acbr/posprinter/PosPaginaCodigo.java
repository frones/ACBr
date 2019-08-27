/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.acbr.posprinter;

import java.util.HashMap;
import java.util.Map;

/**
 *
 * @author rften
 */
public enum PosPaginaCodigo {
    None(0),
    pc437(1),
    pc850(2),
    pc852(3),
    pc860(4),
    pcUTF8(5),
    pc1252(6);
    
    private static final Map<Integer, PosPaginaCodigo> map;
    private final int enumValue;

    static {
        map = new HashMap<>();
        for (PosPaginaCodigo value : PosPaginaCodigo.values()) {
            map.put(value.asInt(), value);
        }
    }

    public static PosPaginaCodigo valueOf(int value) {
        return map.get(value);
    }

    private PosPaginaCodigo(int id) {
        this.enumValue = id;
    }

    public int asInt() {
        return enumValue;
    }
}
