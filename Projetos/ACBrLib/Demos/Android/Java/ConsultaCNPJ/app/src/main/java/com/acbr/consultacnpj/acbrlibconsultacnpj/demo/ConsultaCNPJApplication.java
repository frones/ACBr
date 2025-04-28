package com.acbr.consultacnpj.acbrlibconsultacnpj.demo;

import android.app.Application;
import java.io.File;
import br.com.acbr.lib.consultacnpj.ACBrLibConsultaCNPJ;

public class ConsultaCNPJApplication extends Application {

    private File appDir;
    private ACBrLibConsultaCNPJ ACBrConsultaCNPJ;
    public final String ARQCONFIG_PADRAO = "ACBrLib.ini";
    public final String LOG_PATH_PADRAO = "logs";
    private String arqConfigPath;

    private String[] treeDirectory = {
            LOG_PATH_PADRAO
    };

    private String logPath;

    @Override
    public void onCreate(){
        super.onCreate();

        appDir = getExternalFilesDir(null);
        arqConfigPath = appDir.getAbsolutePath() + "/" + ARQCONFIG_PADRAO;
        logPath = appDir.getAbsolutePath() + "/" + LOG_PATH_PADRAO;
        ACBrConsultaCNPJ = ACBrLibHelper.getInstance(arqConfigPath);
        initAppDir();
    }

    private void initAppDir() {
        initRootDirectory();
    }

    private void initRootDirectory() {
        if (!appDir.exists())
            appDir.mkdir();

        for (String currentDir : treeDirectory) {
            File f = new File(appDir, currentDir);
            if (!f.exists()) {
                f.mkdirs();
            }
        }
    }

    public File getAppDir() {
        return appDir;
    }

    public String getArqConfigPath() {
        return arqConfigPath;
    }

    public ACBrLibConsultaCNPJ getAcBrLibConsultaCNPJ() {
        return ACBrConsultaCNPJ;
    }

    public String getLogPath() {
        return logPath;
    }

}
