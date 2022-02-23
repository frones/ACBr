package com.acbr.boleto;

import java.util.HashMap;
import java.util.Map;

public enum BancoBoleto {
    cobNenhum(0),
    cobBancoDoBrasil(1),
    cobSantander(2),
    cobCaixaEconomica(3),
    cobCaixaSicob(4),
    cobBradesco(5),
    cobItau(6),
    cobBancoMercantil(7),
    cobSicred(8),
    cobBancoob(9),
    cobBanrisul(10),
    cobBanestes(11),
    cobHSBC(12),
    cobBancoDoNordeste(13),
    cobBRB(14),
    cobBicBanco(15),
    cobBradescoSICOOB(16),
    cobBancoSafra(17),
    cobSafraBradesco(18),
    cobBancoCECRED(19),
    cobBancoDaAmazonia(20),
    cobBancoDoBrasilSICOOB(21),
    cobUniprime(22),
    cobUnicredRS(23),
    cobBanese(24),
    cobCrediSIS(25),
    cobUnicredES(26),
    cobBancoCresolSCRS(27),
    cobCitiBank(28),
    cobBancoABCBrasil(29),
    cobDaycoval(30),
    cobUniprimeNortePR(31),
    cobBancoPine(32),
    cobBancoPineBradesco(33),
    cobUnicredSC(34),
    cobBancoAlfa(35),
    cobBancoDoBrasilAPI(36),
    cobBancoDoBrasilWS(37),
    cobBancoCresol(38),
    cobMoneyPlus(39),
    cobBancoC6(40),
    cobBancoRendimento(41),
    cobBancoInter(42),
    cobBancoSofisaSantander(43),
    cobBS2(44);
    
    private static final Map<Integer, BancoBoleto> map;
    private final int enumValue;

    static {
        map = new HashMap<>();
        for (BancoBoleto value : BancoBoleto.values()) {
            map.put(value.asInt(), value);
        }
    }
    
    public static BancoBoleto valueOf(int value) {
        return map.get(value);
    }

    private BancoBoleto(int id) {
        this.enumValue = id;
    }

    public int asInt() {
        return enumValue;
    }
}