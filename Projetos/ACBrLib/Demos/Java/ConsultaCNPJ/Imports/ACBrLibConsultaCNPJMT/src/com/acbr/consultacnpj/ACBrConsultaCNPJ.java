package com.acbr.consultacnpj;

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

public final class ACBrConsultaCNPJ extends ACBrLibBase {

    private interface ACBrConsultaCNPJLib extends Library {

        static String JNA_LIBRARY_NAME = LibraryLoader.getLibraryName();
        public final static ACBrConsultaCNPJLib INSTANCE = LibraryLoader.getInstance();

        class LibraryLoader {

            private static String library = "";
            private static ACBrConsultaCNPJLib instance = null;

            private static String getLibraryName() {
                if (library.isEmpty()) {
                    if (Platform.isWindows()) {
                        library = Platform.is64Bit() ? "ACBrConsultaCNPJ64" : "ACBrConsultaCNPJ32";
                    } else {
                        library = Platform.is64Bit() ? "acbrconsultacnpj64" : "acbrconsultacnpj32";
                    }
                }
                return library;
            }

            public static ACBrConsultaCNPJLib getInstance() {
                if (instance == null) {
                    instance = (ACBrConsultaCNPJLib) Native.synchronizedLibrary(
                            (Library) Native.load(JNA_LIBRARY_NAME, ACBrConsultaCNPJLib.class));
                }

                return instance;
            }
        }

        int CNPJ_Inicializar(PointerByReference libHandler, String eArqConfig, String eChaveCrypt);

        int CNPJ_Finalizar(Pointer libHandler);

        int CNPJ_Nome(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize);

        int CNPJ_Versao(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize);

        int CNPJ_UltimoRetorno(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize);

        int CNPJ_ConfigImportar(Pointer libHandler, String eArqConfig);

        int CNPJ_ConfigExportar(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize);

        int CNPJ_ConfigLer(Pointer libHandler, String eArqConfig);

        int CNPJ_ConfigGravar(Pointer libHandler, String eArqConfig);

        int CNPJ_ConfigLerValor(Pointer libHandler, String eSessao, String eChave, ByteBuffer buffer, IntByReference bufferSize);

        int CNPJ_ConfigGravarValor(Pointer libHandler, String eSessao, String eChave, String valor);
        
        int CNPJ_ConsultarCaptcha(Pointer libHandler, String ePathDownload, ByteBuffer buffer, IntByReference bufferSize);
        
        int CNPJ_Consultar(Pointer libHanPointer, String eCNPJ, ByteBuffer buffer, IntByReference bufferSize);
                
    }

    public ACBrConsultaCNPJ() throws Exception {
        File iniFile = Paths.get(System.getProperty("user.dir"), "ACBrLib.ini").toFile();
        if (!iniFile.exists()) {
            iniFile.createNewFile();
        }
        
        PointerByReference handle = new PointerByReference();
        int ret = ACBrConsultaCNPJLib.INSTANCE.CNPJ_Inicializar(handle, toUTF8( iniFile.getAbsolutePath()), toUTF8(""));
        checkResult(ret);
        setHandle(handle.getValue());
    }

    public ACBrConsultaCNPJ(String eArqConfig, String eChaveCrypt) throws Exception {
        PointerByReference handle = new PointerByReference();
        int ret = ACBrConsultaCNPJLib.INSTANCE.CNPJ_Inicializar(handle, toUTF8(eArqConfig), toUTF8(eChaveCrypt));
        checkResult(ret);
        setHandle(handle.getValue());
    }

    @Override
    protected void dispose() throws Exception {
        int ret = ACBrConsultaCNPJLib.INSTANCE.CNPJ_Finalizar(getHandle());
        checkResult(ret);
    }

    public String nome() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrConsultaCNPJLib.INSTANCE.CNPJ_Nome(getHandle(), buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());
    }

    public String versao() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrConsultaCNPJLib.INSTANCE.CNPJ_Versao(getHandle(), buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());
    }

    public void configLer() throws Exception {
        configLer("");
    }

    public void configLer(String eArqConfig) throws Exception {
        int ret = ACBrConsultaCNPJLib.INSTANCE.CNPJ_ConfigLer(getHandle(), toUTF8(eArqConfig));
        checkResult(ret);
    }

    public void configGravar() throws Exception {
        configGravar("");
    }

    public void configGravar(String eArqConfig) throws Exception {
        int ret = ACBrConsultaCNPJLib.INSTANCE.CNPJ_ConfigGravar(getHandle(), toUTF8(eArqConfig));
        checkResult(ret);
    }

    @Override
    public String configLerValor(ACBrSessao eSessao, String eChave) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrConsultaCNPJLib.INSTANCE.CNPJ_ConfigLerValor(getHandle(), toUTF8(eSessao.name()), toUTF8(eChave), buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);
    }

    @Override
    public void configGravarValor(ACBrSessao eSessao, String eChave, Object value) throws Exception {
        int ret = ACBrConsultaCNPJLib.INSTANCE.CNPJ_ConfigGravarValor(getHandle(), toUTF8(eSessao.name()), toUTF8(eChave), toUTF8(value.toString()));
        checkResult(ret);
    }


    public void ConfigImportar(String eArqConfig) throws Exception {

        int ret = ACBrConsultaCNPJLib.INSTANCE.CNPJ_ConfigImportar(getHandle(), eArqConfig);
        checkResult(ret);

    }

    public String ConfigExportar() throws Exception {

        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrConsultaCNPJLib.INSTANCE.CNPJ_ConfigExportar(getHandle(), buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);

    }
    
    public String consultarCaptcha(String ePathDownload) throws Exception {
        
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrConsultaCNPJLib.INSTANCE.CNPJ_ConsultarCaptcha(getHandle(), ePathDownload, buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }
    
    public String Consultar(String eCNPJ) throws Exception {
     
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrConsultaCNPJLib.INSTANCE.CNPJ_Consultar(getHandle(), eCNPJ, buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }
    
    @Override
    protected void UltimoRetorno(ByteBuffer buffer, IntByReference bufferLen) {
        ACBrConsultaCNPJLib.INSTANCE.CNPJ_UltimoRetorno(getHandle(), buffer, bufferLen);
    }
}
