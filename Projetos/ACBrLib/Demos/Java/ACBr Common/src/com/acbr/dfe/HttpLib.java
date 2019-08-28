package com.acbr.dfe;

import java.util.HashMap;
import java.util.Map;

public enum HttpLib {
    httpNone(0),
    httpWinINet(1),
    httpWinHttp(2),
    httpOpenSSL(3),
    httpIndy(4);
        
    private static final Map<Integer, HttpLib> map;
    private final int enumValue;

    static {
        map = new HashMap<>();
        for (HttpLib value : HttpLib.values()) {
            map.put(value.asInt(), value);
        }
    }

    public static HttpLib valueOf(int value) {
        return map.get(value);
    }

    private HttpLib(int id) {
        this.enumValue = id;
    }

    public int asInt() {
        return enumValue;
    }
}
