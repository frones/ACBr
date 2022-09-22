package com.acbr.esocial;

import java.util.HashMap;
import java.util.Map;

public enum TipoEmpregador {
    
    tePessoaJuridica(0),
    teOrgaoPublico(1),
    tePessoaFisica(2),
    teOrgaoPublicoExecutivoFederal(3),
    teOrgaoPublicoLegislativoFederal(4),
    teOrgaoPublicoJudiciarioFederal(5),
    teOrgaoPublicoAutonomoFederal(6);
    
    
    private static final Map<Integer, TipoEmpregador> map;
    private final int enumValue;

    static {
        map = new HashMap<>();
        for (TipoEmpregador value : TipoEmpregador.values()) {
            map.put(value.asInt(), value);
        }
    }

    public static TipoEmpregador valueOf(int value) {
        return map.get(value);
    }

    private TipoEmpregador(int id) {
        this.enumValue = id;
    }

    public int asInt() {
        return enumValue;
    }
    
}
