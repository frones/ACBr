package com.acbr.ncm;

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

public final class ACBrNCM extends ACBrLibBase {

    private interface ACBrNCMLib extends Library {

        static String JNA_LIBRARY_NAME = LibraryLoader.getLibraryName();
        public final static ACBrNCMLib INSTANCE = LibraryLoader.getInstance();

        class LibraryLoader {

            private static String library = "";
            private static ACBrNCMLib instance = null;

            private static String getLibraryName() {
                if (library.isEmpty()) {
                    if (Platform.isWindows()) {
                        library = Platform.is64Bit() ? "ACBrNCMs64" : "ACBrNCMs32";
                    } else {
                        library = Platform.is64Bit() ? "acbrncms64" : "acbrncms32";
                    }
                }
                return library;
            }

            public static ACBrNCMLib getInstance() {
                if (instance == null) {
                    instance = (ACBrNCMLib) Native.synchronizedLibrary(
                            (Library) Native.load(JNA_LIBRARY_NAME, ACBrNCMLib.class));
                }

                return instance;
            }
        }

        int NCM_Inicializar(PointerByReference libHandler, String eArqConfig, String eChaveCrypt);

        int NCM_Finalizar(Pointer libHandler);

        int NCM_Nome(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize);

        int NCM_Versao(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize);

        int NCM_UltimoRetorno(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize);

        int NCM_ConfigImportar(Pointer libHandler, String eArqConfig);

        int NCM_ConfigExportar(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize);

        int NCM_ConfigLer(Pointer libHandler, String eArqConfig);

        int NCM_ConfigGravar(Pointer libHandler, String eArqConfig);

        int NCM_ConfigLerValor(Pointer libHandler, String eSessao, String eChave, ByteBuffer buffer, IntByReference bufferSize);

        int NCM_ConfigGravarValor(Pointer libHandler, String eSessao, String eChave, String valor);
        
        int NCM_DescricaoNCM(Pointer libHandler, String cNCM, ByteBuffer buffer, IntByReference bufferSize);
        
        int NCM_Validar(Pointer libHandler, String cNCM, ByteBuffer buffer, IntByReference bufferSize);
        
        int NCM_BaixarLista(Pointer libHandler, String cNomeArquivo, ByteBuffer buffer, IntByReference bufferSize);
        
        int NCM_ObterNCMs(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize);
        
        int NCM_BuscarPorCodigo(Pointer libHandler, String cNCM, ByteBuffer buffer, IntByReference bufferSize);
        
        int NCM_BuscarPorDescricao(Pointer libHandler, String cDesc, int nTipo, ByteBuffer buffer, IntByReference bufferSize);
                
    }

    public ACBrNCM() throws Exception {
        File iniFile = Paths.get(System.getProperty("user.dir"), "ACBrLib.ini").toFile();
        if (!iniFile.exists()) {
            iniFile.createNewFile();
        }
        
        PointerByReference handle = new PointerByReference();
        int ret = ACBrNCMLib.INSTANCE.NCM_Inicializar(handle, toUTF8(iniFile.getAbsolutePath()), toUTF8(""));
        checkResult(ret);
    }

    public ACBrNCM(String eArqConfig, String eChaveCrypt) throws Exception {
        PointerByReference handle = new PointerByReference();
        int ret = ACBrNCMLib.INSTANCE.NCM_Inicializar(handle, toUTF8(eArqConfig), toUTF8(eChaveCrypt));
        checkResult(ret);
    }

    @Override
    protected void dispose() throws Exception {
        int ret = ACBrNCMLib.INSTANCE.NCM_Finalizar(getHandle());
        checkResult(ret);
    }

    public String nome() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrNCMLib.INSTANCE.NCM_Nome(getHandle(), buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());
    }

    public String versao() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrNCMLib.INSTANCE.NCM_Versao(getHandle(), buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());
    }

    public void configLer() throws Exception {
        configLer("");
    }

    public void configLer(String eArqConfig) throws Exception {
        int ret = ACBrNCMLib.INSTANCE.NCM_ConfigLer(getHandle(), toUTF8(eArqConfig));
        checkResult(ret);
    }

    public void configGravar() throws Exception {
        configGravar("");
    }

    public void configGravar(String eArqConfig) throws Exception {
        int ret = ACBrNCMLib.INSTANCE.NCM_ConfigGravar(getHandle(), toUTF8(eArqConfig));
        checkResult(ret);
    }

    @Override
    public String configLerValor(ACBrSessao eSessao, String eChave) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrNCMLib.INSTANCE.NCM_ConfigLerValor(getHandle(), toUTF8(eSessao.name()), toUTF8(eChave), buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);
    }

    @Override
    public void configGravarValor(ACBrSessao eSessao, String eChave, Object value) throws Exception {
        int ret = ACBrNCMLib.INSTANCE.NCM_ConfigGravarValor(getHandle(), toUTF8(eSessao.name()), toUTF8(eChave), toUTF8(value.toString()));
        checkResult(ret);
    }


    public void ConfigImportar(String eArqConfig) throws Exception {

        int ret = ACBrNCMLib.INSTANCE.NCM_ConfigImportar(getHandle(), eArqConfig);
        checkResult(ret);

    }

    public String ConfigExportar() throws Exception {

        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrNCMLib.INSTANCE.NCM_ConfigExportar(getHandle(), buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);

    }
    
    public String descricaoNCM(String cNCM) throws Exception {
        
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrNCMLib.INSTANCE.NCM_DescricaoNCM(getHandle(), cNCM, buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }
    
    public String validar(String cNCM) throws Exception {
        
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrNCMLib.INSTANCE.NCM_Validar(getHandle(), cNCM, buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }
    
    public String baixarLista(String cNomeArquivo) throws Exception {
        
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrNCMLib.INSTANCE.NCM_BaixarLista(getHandle(), cNomeArquivo, buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }
    
    public String obterNCMs() throws Exception{
        
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrNCMLib.INSTANCE.NCM_ObterNCMs(getHandle(), buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
        
    }
    
    public String buscarPorCodigo(String cNCM) throws Exception {
     
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrNCMLib.INSTANCE.NCM_BuscarPorCodigo(getHandle(), cNCM, buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }
    
    public String buscarPorDescricao(String cDesc, int nTipo) throws Exception {
        
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrNCMLib.INSTANCE.NCM_BuscarPorDescricao(getHandle(), cDesc, nTipo, buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }
    
    @Override
    protected void UltimoRetorno(ByteBuffer buffer, IntByReference bufferLen) {
        ACBrNCMLib.INSTANCE.NCM_UltimoRetorno(getHandle(), buffer, bufferLen);
    }
}
