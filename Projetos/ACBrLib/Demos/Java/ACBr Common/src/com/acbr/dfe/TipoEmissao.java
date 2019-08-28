package com.acbr.dfe;

import java.util.HashMap;
import java.util.Map;

public enum TipoEmissao {
    teNormal(0),
    teContingencia(1),
    teSCAN(2),
    teDPEC(3),
    teFSDA(4),
    teSVCAN(5),
    teSVCRS(6),
    teSVCSP(7),
    teOffLine(8);
    
    private static final Map<Integer, TipoEmissao> map;
    private final int enumValue;

    static {
        map = new HashMap<>();
        for (TipoEmissao value : TipoEmissao.values()) {
            map.put(value.asInt(), value);
        }
    }

    public static TipoEmissao valueOf(int value) {
        return map.get(value);
    }

    private TipoEmissao(int id) {
        this.enumValue = id;
    }

    public int asInt() {
        return enumValue;
    }
}
