package com.acbr.reinf;

import java.util.HashMap;
import java.util.Map;

public enum TipoContribuinte {
    
    tePessoaJuridica(0),
    teOrgaoPublico(1),
    tePessoaFisica(2);
    
    private static final Map<Integer, TipoContribuinte> map;
    private final int enumValue;

    static {
        map = new HashMap<>();
        for (TipoContribuinte value : TipoContribuinte.values()) {
            map.put(value.asInt(), value);
        }
    }

    public static TipoContribuinte valueOf(int value) {
        return map.get(value);
    }

    private TipoContribuinte(int id) {
        this.enumValue = id;
    }

    public int asInt() {
        return enumValue;
    }
    
}
