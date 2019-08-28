package com.acbr.dfe;

import java.util.HashMap;
import java.util.Map;

public enum CryptLib {
    cryNone(0),
    cryOpenSSL(1),
    cryCapicom(2),
    cryWinCrypt(3);
        
    private static final Map<Integer, CryptLib> map;
    private final int enumValue;

    static {
        map = new HashMap<>();
        for (CryptLib value : CryptLib.values()) {
            map.put(value.asInt(), value);
        }
    }

    public static CryptLib valueOf(int value) {
        return map.get(value);
    }

    private CryptLib(int id) {
        this.enumValue = id;
    }

    public int asInt() {
        return enumValue;
    }
}
