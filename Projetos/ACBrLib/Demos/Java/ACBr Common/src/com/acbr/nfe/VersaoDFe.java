package com.acbr.nfe;

import java.util.HashMap;
import java.util.Map;

public enum VersaoDFe {
    ve200(0),
    ve300(1),
    ve310(2),
    ve400(3);
    
    private static final Map<Integer, VersaoDFe> map;
    private final int enumValue;

    static {
        map = new HashMap<>();
        for (VersaoDFe value : VersaoDFe.values()) {
            map.put(value.asInt(), value);
        }
    }

    public static VersaoDFe valueOf(int value) {
        return map.get(value);
    }

    private VersaoDFe(int id) {
        this.enumValue = id;
    }

    public int asInt() {
        return enumValue;
    }
}
