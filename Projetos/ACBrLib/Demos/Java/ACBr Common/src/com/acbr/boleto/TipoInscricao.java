package com.acbr.boleto;

import java.util.HashMap;
import java.util.Map;

public enum TipoInscricao {
    pFisica(0),
    pJuridica(1);
    
    private static final Map<Integer, TipoInscricao> map;
    private final int enumValue;

    static {
        map = new HashMap<>();
        for (TipoInscricao value : TipoInscricao.values()) {
            map.put(value.asInt(), value);
        }
    }
    
    public static TipoInscricao valueOf(int value) {
        return map.get(value);
    }

    private TipoInscricao(int id) {
        this.enumValue = id;
    }

    public int asInt() {
        return enumValue;
    }
}