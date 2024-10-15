package com.acbr.cep.acbrlibcep.demo;

import br.com.acbr.lib.cep.ACBrLibCep;

public class ACBrLibHelper {

    private static ACBrLibCep ACBrCEP;

    public static ACBrLibCep getInstance(String fileConfig) {
        if (ACBrCEP == null) {
            ACBrCEP = new ACBrLibCep(fileConfig, "");
        }
        return ACBrCEP;
    }

}
