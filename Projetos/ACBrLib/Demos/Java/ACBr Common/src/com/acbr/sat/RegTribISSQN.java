package com.acbr.sat;

import java.util.HashMap;
import java.util.Map;

public enum RegTribISSQN {
    RTISSMicroempresaMunicipal(0),
    RTISSEstimativa(1),
    RTISSSociedadeProfissionais(2),
    RTISSCooperativa(3),
    RTISSMEI(4),
    RTISSMEEPP(5),
    RTISSNenhum(6);
    
    private static final Map<Integer, RegTribISSQN> map;
    private final int enumValue;

    static {
        map = new HashMap<>();
        for (RegTribISSQN value : RegTribISSQN.values()) {
            map.put(value.asInt(), value);
        }
    }

    public static RegTribISSQN valueOf(int value) {
        return map.get(value);
    }

    private RegTribISSQN(int id) {
        this.enumValue = id;
    }

    public int asInt() {
        return enumValue;
    }
}
