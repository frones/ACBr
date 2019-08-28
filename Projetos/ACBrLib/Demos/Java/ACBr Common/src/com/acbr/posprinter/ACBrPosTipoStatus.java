package com.acbr.posprinter;

import java.util.EnumSet;
import java.util.Set;

public enum ACBrPosTipoStatus {
    None(0),
    Erro(1),
    NaoSerial(1 << 1),
    PoucoPapel(1 << 2),
    SemPapel(1 << 3),
    GavetaAberta(1 << 4),
    Imprimindo(1 << 5),
    OffLine(1 << 6),
    TampaAberta(1 << 7),
    ErroLeitura(1 << 8);

    private final long statusValue;

    ACBrPosTipoStatus(long statusValue) {
        this.statusValue = statusValue;
    }

    public long asLong(){
        return statusValue;
    }

    public static EnumSet<ACBrPosTipoStatus> valueOf(long statusValue) {
        EnumSet statusFlags = EnumSet.noneOf(ACBrPosTipoStatus.class);
        for (ACBrPosTipoStatus statusFlag : ACBrPosTipoStatus.values()) {
            long flagValue = statusFlag.asLong();
            if ( (flagValue&statusValue ) == flagValue ) {
                statusFlags.add(statusFlag);
            }
        }
        return statusFlags;
    }

    public static long valueOf(Set<ACBrPosTipoStatus> flags) {
        long value=0;
        for (ACBrPosTipoStatus statusFlag : ACBrPosTipoStatus.values()) {
                value|= statusFlag.asLong();
        }
        return value;
    }
}
