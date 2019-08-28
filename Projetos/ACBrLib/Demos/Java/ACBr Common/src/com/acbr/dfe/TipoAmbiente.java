package com.acbr.dfe;

import java.util.HashMap;
import java.util.Map;

public enum TipoAmbiente {
    taProducao(0),
    taHomologacao(1);
    
    private static final Map<Integer, TipoAmbiente> map;
    private final int enumValue;

    static {
        map = new HashMap<>();
        for (TipoAmbiente value : TipoAmbiente.values()) {
            map.put(value.asInt(), value);
        }
    }

    public static TipoAmbiente valueOf(int value) {
        return map.get(value);
    }

    private TipoAmbiente(int id) {
        this.enumValue = id;
    }

    public int asInt() {
        return enumValue;
    }
}
