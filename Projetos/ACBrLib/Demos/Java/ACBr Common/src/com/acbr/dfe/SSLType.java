package com.acbr.dfe;

import java.util.HashMap;
import java.util.Map;

public enum SSLType {
    LT_all(0),
    LT_SSLv2(1),
    LT_SSLv3(2),
    LT_TLSv1(3),
    LT_TLSv1_1(4),
    LT_TLSv1_2(5),
    LT_SSHv2(6);
        
    private static final Map<Integer, SSLType> map;
    private final int enumValue;

    static {
        map = new HashMap<>();
        for (SSLType value : SSLType.values()) {
            map.put(value.asInt(), value);
        }
    }

    public static SSLType valueOf(int value) {
        return map.get(value);
    }

    private SSLType(int id) {
        this.enumValue = id;
    }

    public int asInt() {
        return enumValue;
    }
}
