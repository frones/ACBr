package com.acbr.dfe;

import java.util.HashMap;
import java.util.Map;

public enum ModeloDFe {
    moNFe(0),
    moNFCe(1);
    
    private static final Map<Integer, ModeloDFe> map;
    private final int enumValue;

    static {
        map = new HashMap<>();
        for (ModeloDFe value : ModeloDFe.values()) {
            map.put(value.asInt(), value);
        }
    }

    public static ModeloDFe valueOf(int value) {
        return map.get(value);
    }

    private ModeloDFe(int id) {
        this.enumValue = id;
    }

    public int asInt() {
        return enumValue;
    }
}
