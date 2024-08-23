package com.acbr.cep;

import com.acbr.ACBrLibBase;
import com.acbr.ACBrSessao;
import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Platform;
import com.sun.jna.ptr.IntByReference;
import java.io.File;
import java.nio.ByteBuffer;
import java.nio.file.Paths;

public final class ACBrCEP extends ACBrLibBase {

    private interface ACBrCEPLib extends Library {

        static String JNA_LIBRARY_NAME = LibraryLoader.getLibraryName();
        public final static ACBrCEPLib INSTANCE = LibraryLoader.getInstance();

        class LibraryLoader {

            private static String library = "";
            private static ACBrCEPLib instance = null;

            public static String getLibraryName() {
                if (library.isEmpty()) {
                    if (Platform.isWindows()) {
                        library = Platform.is64Bit() ? "ACBrCEP64" : "ACBrCEP32";
                    } else {
                        library = Platform.is64Bit() ? "acbrcep64" : "acbrcep32";
                    }
                }
                return library;
            }

            public static ACBrCEPLib getInstance() {
                if (instance == null) {
                    instance = (ACBrCEPLib) Native.synchronizedLibrary((Library) Native.load(JNA_LIBRARY_NAME, ACBrCEPLib.class));
                }
                return instance;
            }
        }

        int CEP_Inicializar(String eArqConfig, String eChaveCrypt);

        int CEP_Finalizar();

        int CEP_Nome(ByteBuffer buffer, IntByReference bufferSize);

        int CEP_Versao(ByteBuffer buffer, IntByReference bufferSize);

        int CEP_UltimoRetorno(ByteBuffer buffer, IntByReference bufferSize);

        int CEP_ConfigImportar(String eArqConfig);

        int CEP_ConfigExportar(ByteBuffer buffer, IntByReference bufferSize);

        int CEP_ConfigLer(String eArqConfig);

        int CEP_ConfigGravar(String eArqConfig);

        int CEP_ConfigLerValor(String eSessao, String eChave, ByteBuffer buffer, IntByReference bufferSize);

        int CEP_ConfigGravarValor(String eSessao, String eChave, String valor);

        int CEP_BuscarPorCEP(String eCEP, ByteBuffer buffer, IntByReference bufferSize);

        int CEP_BuscarPorLogradouro(String eCidade, String eTipo_Logradouro, String eLogradouro, String eUF, String eBairro, ByteBuffer buffer, IntByReference bufferSize);

    }

    public ACBrCEP() throws Exception {
        File iniFile = Paths.get(System.getProperty("user.dir"), "ACBrLib.ini").toFile();

        if (!iniFile.exists()) {
            iniFile.createNewFile();
        }

        int ret = ACBrCEPLib.INSTANCE.CEP_Inicializar(toUTF8(iniFile.getAbsolutePath()), toUTF8(""));
        checkResult(ret);
    }

    public ACBrCEP(String eArqConfig, String eChaveCrypt) throws Exception {
        int ret = ACBrCEPLib.INSTANCE.CEP_Inicializar(toUTF8(eArqConfig), toUTF8(eChaveCrypt));
        checkResult(ret);
    }

    @Override
    protected void dispose() throws Exception {
        int ret = ACBrCEPLib.INSTANCE.CEP_Finalizar();
        checkResult(ret);
    }

    public String nome() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrCEPLib.INSTANCE.CEP_Nome(buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());
    }

    public String versao() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrCEPLib.INSTANCE.CEP_Versao(buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());
    }

    public void configLer() throws Exception {
        configLer("");
    }

    public void configLer(String eArqConfig) throws Exception {
        int ret = ACBrCEPLib.INSTANCE.CEP_ConfigLer(toUTF8(eArqConfig));
        checkResult(ret);
    }

    public void configGravar() throws Exception {
        configGravar("");
    }

    public void configGravar(String eArqConfig) throws Exception {
        int ret = ACBrCEPLib.INSTANCE.CEP_ConfigGravar(toUTF8(eArqConfig));
        checkResult(ret);
    }

    @Override
    public String configLerValor(ACBrSessao eSessao, String eChave) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrCEPLib.INSTANCE.CEP_ConfigLerValor(toUTF8(eSessao.name()), toUTF8(eChave), buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);
    }

    @Override
    public void configGravarValor(ACBrSessao eSessao, String eChave, Object value) throws Exception {
        int ret = ACBrCEPLib.INSTANCE.CEP_ConfigGravarValor(toUTF8(eSessao.name()), toUTF8(eChave), toUTF8(value.toString()));
        checkResult(ret);
    }

    public String buscarPorCEP(String eCEP) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrCEPLib.INSTANCE.CEP_BuscarPorCEP(toUTF8(eCEP), buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);
    }

    public String buscarPorLogradouro(String eCidade, String eTipo_Logradouro, String eLogradouro, String eUF, String eBairro) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrCEPLib.INSTANCE.CEP_BuscarPorLogradouro(toUTF8(eCidade), toUTF8(eTipo_Logradouro), toUTF8(eLogradouro), toUTF8(eUF), toUTF8(eBairro), buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);
    }

    public void ConfigImportar(String eArqConfig) throws Exception {
        int ret = ACBrCEPLib.INSTANCE.CEP_ConfigImportar(eArqConfig);
        checkResult(ret);
    }

    public String ConfigExportar() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrCEPLib.INSTANCE.CEP_ConfigExportar(buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());
    }

    @Override
    protected void UltimoRetorno(ByteBuffer buffer, IntByReference bufferLen) {
        ACBrCEPLib.INSTANCE.CEP_UltimoRetorno(buffer, bufferLen);
    }
}
