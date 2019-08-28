package com.acbr.dfe;

import java.util.HashMap;
import java.util.Map;

public enum SSLLib {
    libNone(0),
    libOpenSSL(1),
    libCapicom(2),
    libCapicomDelphiSoap(3),
    libWinCrypt(4),
    libCustom(5);
        
    private static final Map<Integer, SSLLib> map;
    private final int enumValue;

    static {
        map = new HashMap<>();
        for (SSLLib value : SSLLib.values()) {
            map.put(value.asInt(), value);
        }
    }

    public static SSLLib valueOf(int value) {
        return map.get(value);
    }

    private SSLLib(int id) {
        this.enumValue = id;
    }

    public int asInt() {
        return enumValue;
    }
}
