package br.com.acbr.lib.comum.helper;

import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.IOException;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import java.io.BufferedOutputStream;
import java.io.FileInputStream;


/**
 * É uma classe que auxilia a descompactação de arquivos compactados com formato ZIP
 */
public class FileUtils {

    /**
     *  Metódo usado para descompactar um arquivo  ZIP
     * @param zipFilePath caminho do arquivo zip
     * @param destinationDir destino de descompactaçãpo
     */
    public void unzip(String zipFilePath, String destinationDir) throws IOException {
        File destDir = new File(destinationDir);
        if (!destDir.exists()) {
            destDir.mkdirs();
        }

        try (ZipInputStream zipIn = new ZipInputStream(new FileInputStream(zipFilePath))) {
            ZipEntry entry = zipIn.getNextEntry();

            // Itera por cada entrada (arquivo ou pasta) no arquivo zip
            while (entry != null) {
                String filePath = destinationDir + File.separator + entry.getName();

                if (!entry.isDirectory()) {
                    // Se a entrada for um arquivo, extraia-o
                    extractFile(zipIn, filePath);
                } else {
                    // Se a entrada for um diretório, crie-o
                    File dir = new File(filePath);
                    dir.mkdirs();
                }

                // Move para a próxima entrada
                zipIn.closeEntry();
                entry = zipIn.getNextEntry();
            }
        }
    }

    private void extractFile(ZipInputStream zipIn, String filePath) throws IOException {
        try (BufferedOutputStream bos = new BufferedOutputStream(new FileOutputStream(filePath))) {
            byte[] buffer = new byte[1024];
            int read;
            while ((read = zipIn.read(buffer)) != -1) {
                bos.write(buffer, 0, read);
            }
        }
    }
}
