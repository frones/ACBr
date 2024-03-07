package com.acbr.consultacnpj;

import com.acbr.ACBrLibBase;
import com.acbr.ACBrSessao;
import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Platform;
import com.sun.jna.ptr.IntByReference;
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

        int CNPJ_Inicializar(String eArqConfig, String eChaveCrypt);

        int CNPJ_Finalizar();

        int CNPJ_Nome(ByteBuffer buffer, IntByReference bufferSize);

        int CNPJ_Versao(ByteBuffer buffer, IntByReference bufferSize);

        int CNPJ_UltimoRetorno(ByteBuffer buffer, IntByReference bufferSize);

        int CNPJ_ConfigImportar(String eArqConfig);

        int CNPJ_ConfigExportar(ByteBuffer buffer, IntByReference bufferSize);

        int CNPJ_ConfigLer(String eArqConfig);

        int CNPJ_ConfigGravar(String eArqConfig);

        int CNPJ_ConfigLerValor(String eSessao, String eChave, ByteBuffer buffer, IntByReference bufferSize);

        int CNPJ_ConfigGravarValor(String eSessao, String eChave, String valor);
        
        int CNPJ_ConsultarCaptcha(String ePathDownload, ByteBuffer buffer, IntByReference bufferSize);
        
        int CNPJ_Consultar(String eCNPJ, ByteBuffer buffer, IntByReference bufferSize);
                
    }

    public ACBrConsultaCNPJ() throws Exception {
        File iniFile = Paths.get(System.getProperty("user.dir"), "ACBrLib.ini").toFile();
        if (!iniFile.exists()) {
            iniFile.createNewFile();
        }

        int ret = ACBrConsultaCNPJLib.INSTANCE.CNPJ_Inicializar(toUTF8(iniFile.getAbsolutePath()), toUTF8(""));
        checkResult(ret);
    }

    public ACBrConsultaCNPJ(String eArqConfig, String eChaveCrypt) throws Exception {
        int ret = ACBrConsultaCNPJLib.INSTANCE.CNPJ_Inicializar(toUTF8(eArqConfig), toUTF8(eChaveCrypt));
        checkResult(ret);
    }

    @Override
    protected void dispose() throws Exception {
        int ret = ACBrConsultaCNPJLib.INSTANCE.CNPJ_Finalizar();
        checkResult(ret);
    }

    public String nome() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrConsultaCNPJLib.INSTANCE.CNPJ_Nome(buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());
    }

    public String versao() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrConsultaCNPJLib.INSTANCE.CNPJ_Versao(buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());
    }

    public void configLer() throws Exception {
        configLer("");
    }

    public void configLer(String eArqConfig) throws Exception {
        int ret = ACBrConsultaCNPJLib.INSTANCE.CNPJ_ConfigLer(toUTF8(eArqConfig));
        checkResult(ret);
    }

    public void configGravar() throws Exception {
        configGravar("");
    }

    public void configGravar(String eArqConfig) throws Exception {
        int ret = ACBrConsultaCNPJLib.INSTANCE.CNPJ_ConfigGravar(toUTF8(eArqConfig));
        checkResult(ret);
    }

    @Override
    public String configLerValor(ACBrSessao eSessao, String eChave) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrConsultaCNPJLib.INSTANCE.CNPJ_ConfigLerValor(toUTF8(eSessao.name()), toUTF8(eChave), buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);
    }

    @Override
    public void configGravarValor(ACBrSessao eSessao, String eChave, Object value) throws Exception {
        int ret = ACBrConsultaCNPJLib.INSTANCE.CNPJ_ConfigGravarValor(toUTF8(eSessao.name()), toUTF8(eChave), toUTF8(value.toString()));
        checkResult(ret);
    }


    public void ConfigImportar(String eArqConfig) throws Exception {

        int ret = ACBrConsultaCNPJLib.INSTANCE.CNPJ_ConfigImportar(eArqConfig);
        checkResult(ret);

    }

    public String ConfigExportar() throws Exception {

        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrConsultaCNPJLib.INSTANCE.CNPJ_ConfigExportar(buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);

    }
    
    public String consultarCaptcha(String ePathDownload) throws Exception {
        
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrConsultaCNPJLib.INSTANCE.CNPJ_ConsultarCaptcha(ePathDownload, buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }
    
    public String Consultar(String eCNPJ) throws Exception{
        
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrConsultaCNPJLib.INSTANCE.CNPJ_Consultar(eCNPJ, buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }
    
    @Override
    protected void UltimoRetorno(ByteBuffer buffer, IntByReference bufferLen) {
        ACBrConsultaCNPJLib.INSTANCE.CNPJ_UltimoRetorno(buffer, bufferLen);
    }
}
