package com.acbr.sat;

import java.util.HashMap;
import java.util.Map;

public enum SATExtratoFiltro {
    fiNenhum(0),
    fiPDF(1),
    fiHTML(2);
    
    private static final Map<Integer, SATExtratoFiltro> map;
    private final int enumValue;

    static {
        map = new HashMap<>();
        for (SATExtratoFiltro value : SATExtratoFiltro.values()) {
            map.put(value.asInt(), value);
        }
    }

    public static SATExtratoFiltro valueOf(int value) {
        return map.get(value);
    }

    private SATExtratoFiltro(int id) {
        this.enumValue = id;
    }

    public int asInt() {
        return enumValue;
    }
}
