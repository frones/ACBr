/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.acbr.cte;

import java.util.HashMap;
import java.util.Map;

public enum ModeloCTe {
    moCTe(0),
    moCTeOS(1);
    
    private static final Map<Integer, ModeloCTe> map;
    private final int enumValue;

    static {
        map = new HashMap<>();
        for (ModeloCTe value : ModeloCTe.values()) {
            map.put(value.asInt(), value);
        }
    }

    public static ModeloCTe valueOf(int value) {
        return map.get(value);
    }

    private ModeloCTe(int id) {
        this.enumValue = id;
    }

    public int asInt() {
        return enumValue;
    }
}
