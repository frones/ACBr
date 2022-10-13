package com.acbr.gtin;

import com.acbr.ACBrLibBase;
import com.acbr.ACBrSessao;
import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Platform;
import com.sun.jna.ptr.IntByReference;
import java.io.File;
import java.nio.ByteBuffer;
import java.nio.file.Paths;

public final class ACBrGTIN extends ACBrLibBase {

    private interface ACBrGTINLib extends Library {

        static String JNA_LIBRARY_NAME = LibraryLoader.getLibraryName();
        public final static ACBrGTINLib INSTANCE = LibraryLoader.getInstance();

        class LibraryLoader {

            private static String library = "";
            private static ACBrGTINLib instance = null;

            private static String getLibraryName() {
                if (library.isEmpty()) {
                    if (Platform.isWindows()) {
                        library = Platform.is64Bit() ? "ACBrGTIN64" : "ACBrGTIN32";
                    } else {
                        library = Platform.is64Bit() ? "acbrgtin64" : "acbrgtin32";
                    }
                }
                return library;
            }

            public static ACBrGTINLib getInstance() {
                if (instance == null) {
                    instance = (ACBrGTINLib) Native.synchronizedLibrary(
                            (Library) Native.load(JNA_LIBRARY_NAME, ACBrGTINLib.class));
                }

                return instance;
            }
        }

        int GTIN_Inicializar(String eArqConfig, String eChaveCrypt);

        int GTIN_Finalizar();

        int GTIN_Nome(ByteBuffer buffer, IntByReference bufferSize);

        int GTIN_Versao(ByteBuffer buffer, IntByReference bufferSize);

        int GTIN_UltimoRetorno(ByteBuffer buffer, IntByReference bufferSize);

        int GTIN_ConfigImportar(String eArqConfig);

        int GTIN_ConfigExportar(ByteBuffer buffer, IntByReference bufferSize);

        int GTIN_ConfigLer(String eArqConfig);

        int GTIN_ConfigGravar(String eArqConfig);

        int GTIN_ConfigLerValor(String eSessao, String eChave, ByteBuffer buffer, IntByReference bufferSize);

        int GTIN_ConfigGravarValor(String eSessao, String eChave, String valor);
        
        int GTIN_Consultar(String aGTIN, ByteBuffer buffer, IntByReference bufferSize);
                
    }

    public ACBrGTIN() throws Exception {
        File iniFile = Paths.get(System.getProperty("user.dir"), "ACBrLib.ini").toFile();
        if (!iniFile.exists()) {
            iniFile.createNewFile();
        }

        int ret = ACBrGTINLib.INSTANCE.GTIN_Inicializar(toUTF8(iniFile.getAbsolutePath()), toUTF8(""));
        checkResult(ret);
    }

    public ACBrGTIN(String eArqConfig, String eChaveCrypt) throws Exception {
        int ret = ACBrGTINLib.INSTANCE.GTIN_Inicializar(toUTF8(eArqConfig), toUTF8(eChaveCrypt));
        checkResult(ret);
    }

    @Override
    protected void dispose() throws Exception {
        int ret = ACBrGTINLib.INSTANCE.GTIN_Finalizar();
        checkResult(ret);
    }

    public String nome() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrGTINLib.INSTANCE.GTIN_Nome(buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());
    }

    public String versao() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrGTINLib.INSTANCE.GTIN_Versao(buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());
    }

    public void configLer() throws Exception {
        configLer("");
    }

    public void configLer(String eArqConfig) throws Exception {
        int ret = ACBrGTINLib.INSTANCE.GTIN_ConfigLer(toUTF8(eArqConfig));
        checkResult(ret);
    }

    public void configGravar() throws Exception {
        configGravar("");
    }

    public void configGravar(String eArqConfig) throws Exception {
        int ret = ACBrGTINLib.INSTANCE.GTIN_ConfigGravar(toUTF8(eArqConfig));
        checkResult(ret);
    }

    @Override
    public String configLerValor(ACBrSessao eSessao, String eChave) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrGTINLib.INSTANCE.GTIN_ConfigLerValor(toUTF8(eSessao.name()), toUTF8(eChave), buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);
    }

    @Override
    public void configGravarValor(ACBrSessao eSessao, String eChave, Object value) throws Exception {
        int ret = ACBrGTINLib.INSTANCE.GTIN_ConfigGravarValor(toUTF8(eSessao.name()), toUTF8(eChave), toUTF8(value.toString()));
        checkResult(ret);
    }


    public void ConfigImportar(String eArqConfig) throws Exception {

        int ret = ACBrGTINLib.INSTANCE.GTIN_ConfigImportar(eArqConfig);
        checkResult(ret);

    }

    public String ConfigExportar() throws Exception {

        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrGTINLib.INSTANCE.GTIN_ConfigExportar(buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);

    }
    
    public String consultar(String aGTIN) throws Exception {
        
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrGTINLib.INSTANCE.GTIN_Consultar(aGTIN, buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }
    
    @Override
    protected void UltimoRetorno(ByteBuffer buffer, IntByReference bufferLen) {
        ACBrGTINLib.INSTANCE.GTIN_UltimoRetorno(buffer, bufferLen);
    }
}
