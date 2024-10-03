package br.com.acbr.lib.comum.helper;

import android.content.Context;
import android.content.res.AssetManager;
import android.util.Log;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;


/**
 * AssetsCopyHelper é uma classe que auxilia a cópia de arquivos da pasta assets (do projeto) para o getExternalFilesDir
 */
public class AssetsCopyHelper {
    private AssetManager assetManager;
    private Context contex;

    /**
     *
     * @param context
     */
    public AssetsCopyHelper(Context context) {
        assetManager = context.getAssets();
        this.contex = context;
    }


    private boolean isAssetIgnored(String s) {
        return s.equals("images") || s.equals("webkit");
    }

    /**
     * Método usado para copiar arquivos de assets para appDir
     *   @param appDir é um diretório que o App tenha acesso, preferencialmente  um subdiretório getExternalFilesDir(null) ou getAppDir()
     *   @throws IOException
     */
    public void copyAllAssetsIfNotExist(File appDir) throws IOException {

        if ( appDir == null) {
            throw new IOException("appDir não pode ser nulo");
        }
        String[] files = assetManager.list("");// Lista todos os arquivos na pasta 'assets
        if (files != null) {
            for (String fileName : files) {
                if (isAssetIgnored(fileName)) continue;
                File outFile = new File(appDir, fileName);  // Usa o AppDir já inicializado

                // Verifica se o arquivo já existe, e se não, copia
                if (!outFile.exists()) {
                    copyAssetFile(fileName, outFile);
                    Log.d("FileCopied", "Copied: " + fileName);
                } else {
                    Log.d("FileExists", "Already exists: " + fileName);
                }
            }
        }
    }

    private void copyAssetFile(String assetFileName, File outFile) throws IOException {
        InputStream inputStream = assetManager.open(assetFileName);
        FileOutputStream outputStream = new FileOutputStream(outFile);

        byte[] buffer = new byte[4096];
        int length;
        while ((length = inputStream.read(buffer)) > 0) {
            outputStream.write(buffer, 0, length);
        }

        outputStream.close();
        inputStream.close();
    }

}