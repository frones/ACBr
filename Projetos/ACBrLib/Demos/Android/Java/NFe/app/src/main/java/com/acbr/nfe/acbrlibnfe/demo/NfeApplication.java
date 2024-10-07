package com.acbr.nfe.acbrlibnfe.demo;

import android.app.Application;
import android.content.pm.PackageManager;
import android.content.res.AssetManager;
import android.os.Build;
import android.util.Log;

import java.io.File;
import java.io.IOException;

import com.acbr.nfe.acbrlibnfe.demo.ACBrLibHelper;
import br.com.acbr.lib.comum.helper.AssetsCopyHelper;
import br.com.acbr.lib.comum.helper.FileUtils;

import br.com.acbr.lib.nfe.ACBrLibNFe;

public class NfeApplication extends Application {

    private File appDir;
    private ACBrLibNFe ACBrNFe;
    public final String ARQCONFIG_PADRAO = "ACBrLib.ini";
    public final String SCHEMAS_PADRAO = "Schemas";
    public final String PATH_SALVAR_PADRAO = "xmls";
    public final String PDF_PATH_PADRAO = "pdf";
    public final String LOG_PATH_PADRAO = "logs";
    public final String PFX_PADRAO = "cert.pfx";
    private String arqConfigPath;
    private AssetManager assetManager;
    private String pfxPath;
    private String schemasPath;
    private String schemasZip;

    private String[] treeDirectory = {
            PATH_SALVAR_PADRAO,
            PDF_PATH_PADRAO,
            LOG_PATH_PADRAO
    };

    private String pathSalvar;
    private String logPath;
    private String pdfPath;

    @Override
    public void onCreate() {
        super.onCreate();

        appDir = getExternalFilesDir(null);
        arqConfigPath = appDir.getAbsolutePath() + "/" + ARQCONFIG_PADRAO;
        pfxPath = appDir.getAbsolutePath() + "/" + PFX_PADRAO;
        schemasPath = appDir.getAbsolutePath() + "/" + SCHEMAS_PADRAO + "/" + "NFe";
        schemasZip = appDir.getAbsolutePath() + "/schemas.zip";
        pathSalvar = appDir.getAbsolutePath() + "/" + PATH_SALVAR_PADRAO;
        pdfPath = appDir.getAbsolutePath() + "/" + PDF_PATH_PADRAO;
        logPath = appDir.getAbsolutePath() + "/" + LOG_PATH_PADRAO;
        assetManager = getAssets();
        ACBrNFe = ACBrLibHelper.getInstance(arqConfigPath);
        initAppDir();
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

    private void initAppDir() {
        initRootDirectory();
        try {
            AssetsCopyHelper assetsHelper = new AssetsCopyHelper(this);
            FileUtils fileUtils = new FileUtils();
            assetsHelper.copyAllAssetsIfNotExist(appDir);
            fileUtils.unzip(schemasZip, appDir.getAbsolutePath());
        } catch (IOException e) {
            Log.e("NFeApplication", e.getMessage());
            e.printStackTrace();
        }
    }

    public File getAppDir() {
        return appDir;
    }

    public String getArqConfigPath() {
        return arqConfigPath;
    }

    public ACBrLibNFe getAcBrLibNFe() {
        return ACBrNFe;
    }

    public String getPfxPath() {
        return pfxPath;
    }

    public String getSchemasPath() {
        return schemasPath;
    }

    public String getPdfPath() {
        return pdfPath;
    }

    public String getLogPath() {
        return logPath;
    }

    public String getPathSalvar() {
        return pathSalvar;
    }
}