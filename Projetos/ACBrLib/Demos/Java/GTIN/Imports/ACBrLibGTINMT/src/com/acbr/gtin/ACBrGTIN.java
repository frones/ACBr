package com.acbr.gtin;

import com.acbr.ACBrLibBase;
import com.acbr.ACBrSessao;
import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Platform;
import com.sun.jna.Pointer;
import com.sun.jna.ptr.IntByReference;
import com.sun.jna.ptr.PointerByReference;
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

        int GTIN_Inicializar(PointerByReference libHandler, String eArqConfig, String eChaveCrypt);

        int GTIN_Finalizar(Pointer libHandler);

        int GTIN_Nome(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize);

        int GTIN_Versao(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize);

        int GTIN_UltimoRetorno(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize);

        int GTIN_ConfigImportar(Pointer libHandler, String eArqConfig);

        int GTIN_ConfigExportar(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize);

        int GTIN_ConfigLer(Pointer libHandler, String eArqConfig);

        int GTIN_ConfigGravar(Pointer libHandler, String eArqConfig);

        int GTIN_ConfigLerValor(Pointer libHandler, String eSessao, String eChave, ByteBuffer buffer, IntByReference bufferSize);

        int GTIN_ConfigGravarValor(Pointer libHandler, String eSessao, String eChave, String valor);
        
        int GTIN_Consultar(Pointer libHandler, String aGTIN, ByteBuffer buffer, IntByReference bufferSize);
                
    }

    public ACBrGTIN() throws Exception {
        File iniFile = Paths.get(System.getProperty("user.dir"), "ACBrLib.ini").toFile();
        if (!iniFile.exists()) {
            iniFile.createNewFile();
        }
        
        PointerByReference handle = new PointerByReference();
        int ret = ACBrGTINLib.INSTANCE.GTIN_Inicializar(handle, toUTF8( iniFile.getAbsolutePath()), toUTF8(""));
        checkResult(ret);
        setHandle(handle.getValue());
    }

    public ACBrGTIN(String eArqConfig, String eChaveCrypt) throws Exception {
        PointerByReference handle = new PointerByReference();
        int ret = ACBrGTINLib.INSTANCE.GTIN_Inicializar(handle, toUTF8(eArqConfig), toUTF8(eChaveCrypt));
        checkResult(ret);
        setHandle(handle.getValue());
    }

    @Override
    protected void dispose() throws Exception {
        int ret = ACBrGTINLib.INSTANCE.GTIN_Finalizar(getHandle());
        checkResult(ret);
    }

    public String nome() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrGTINLib.INSTANCE.GTIN_Nome(getHandle(), buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());
    }

    public String versao() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrGTINLib.INSTANCE.GTIN_Versao(getHandle(), buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());
    }

    public void configLer() throws Exception {
        configLer("");
    }

    public void configLer(String eArqConfig) throws Exception {
        int ret = ACBrGTINLib.INSTANCE.GTIN_ConfigLer(getHandle(), toUTF8(eArqConfig));
        checkResult(ret);
    }

    public void configGravar() throws Exception {
        configGravar("");
    }

    public void configGravar(String eArqConfig) throws Exception {
        int ret = ACBrGTINLib.INSTANCE.GTIN_ConfigGravar(getHandle(), toUTF8(eArqConfig));
        checkResult(ret);
    }

    @Override
    public String configLerValor(ACBrSessao eSessao, String eChave) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrGTINLib.INSTANCE.GTIN_ConfigLerValor(getHandle(), toUTF8(eSessao.name()), toUTF8(eChave), buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);
    }

    @Override
    public void configGravarValor(ACBrSessao eSessao, String eChave, Object value) throws Exception {
        int ret = ACBrGTINLib.INSTANCE.GTIN_ConfigGravarValor(getHandle(), toUTF8(eSessao.name()), toUTF8(eChave), toUTF8(value.toString()));
        checkResult(ret);
    }


    public void ConfigImportar(String eArqConfig) throws Exception {

        int ret = ACBrGTINLib.INSTANCE.GTIN_ConfigImportar(getHandle(), eArqConfig);
        checkResult(ret);

    }

    public String ConfigExportar() throws Exception {

        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrGTINLib.INSTANCE.GTIN_ConfigExportar(getHandle(), buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);

    }
    
    public String consultar(String aGTIN) throws Exception {
        
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrGTINLib.INSTANCE.GTIN_Consultar(getHandle(), aGTIN, buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }
    
    @Override
    protected void UltimoRetorno(ByteBuffer buffer, IntByReference bufferLen) {
        ACBrGTINLib.INSTANCE.GTIN_UltimoRetorno(getHandle(), buffer, bufferLen);
    }
}
