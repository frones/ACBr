package com.acbr.consultacnpj.acbrlibconsultacnpj.demo;

import br.com.acbr.lib.consultacnpj.ACBrLibConsultaCNPJ;

public class ACBrLibHelper {

    private static ACBrLibConsultaCNPJ ACBrConsultaCNPJ;

    public static ACBrLibConsultaCNPJ getInstance(String fileConfig) {
        if (ACBrConsultaCNPJ == null) {
            ACBrConsultaCNPJ = new ACBrLibConsultaCNPJ(fileConfig, "");
        }
        return ACBrConsultaCNPJ;
    }
}
