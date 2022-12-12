package com.acbr.ncm;

import com.acbr.ACBrLibBase;
import com.acbr.ACBrSessao;
import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Platform;
import com.sun.jna.ptr.IntByReference;
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

        int NCM_Inicializar(String eArqConfig, String eChaveCrypt);

        int NCM_Finalizar();

        int NCM_Nome(ByteBuffer buffer, IntByReference bufferSize);

        int NCM_Versao(ByteBuffer buffer, IntByReference bufferSize);

        int NCM_UltimoRetorno(ByteBuffer buffer, IntByReference bufferSize);

        int NCM_ConfigImportar(String eArqConfig);

        int NCM_ConfigExportar(ByteBuffer buffer, IntByReference bufferSize);

        int NCM_ConfigLer(String eArqConfig);

        int NCM_ConfigGravar(String eArqConfig);

        int NCM_ConfigLerValor(String eSessao, String eChave, ByteBuffer buffer, IntByReference bufferSize);

        int NCM_ConfigGravarValor(String eSessao, String eChave, String valor);
        
        int NCM_DescricaoNCM(String cNCM, ByteBuffer buffer, IntByReference bufferSize);
        
        int NCM_Validar(String cNCM, ByteBuffer buffer, IntByReference bufferSize);
        
        int NCM_BaixarLista(String cNomeArquivo, ByteBuffer buffer, IntByReference bufferSize);
        
        int NCM_ObterNCMs(ByteBuffer buffer, IntByReference bufferSize);
        
        int NCM_BuscarPorCodigo(String cNCM, ByteBuffer buffer, IntByReference bufferSize);
        
        int NCM_BuscarPorDescricao(String cDesc, int nTipo, ByteBuffer buffer, IntByReference bufferSize);
                
    }

    public ACBrNCM() throws Exception {
        File iniFile = Paths.get(System.getProperty("user.dir"), "ACBrLib.ini").toFile();
        if (!iniFile.exists()) {
            iniFile.createNewFile();
        }

        int ret = ACBrNCMLib.INSTANCE.NCM_Inicializar(toUTF8(iniFile.getAbsolutePath()), toUTF8(""));
        checkResult(ret);
    }

    public ACBrNCM(String eArqConfig, String eChaveCrypt) throws Exception {
        int ret = ACBrNCMLib.INSTANCE.NCM_Inicializar(toUTF8(eArqConfig), toUTF8(eChaveCrypt));
        checkResult(ret);
    }

    @Override
    protected void dispose() throws Exception {
        int ret = ACBrNCMLib.INSTANCE.NCM_Finalizar();
        checkResult(ret);
    }

    public String nome() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrNCMLib.INSTANCE.NCM_Nome(buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());
    }

    public String versao() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrNCMLib.INSTANCE.NCM_Versao(buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());
    }

    public void configLer() throws Exception {
        configLer("");
    }

    public void configLer(String eArqConfig) throws Exception {
        int ret = ACBrNCMLib.INSTANCE.NCM_ConfigLer(toUTF8(eArqConfig));
        checkResult(ret);
    }

    public void configGravar() throws Exception {
        configGravar("");
    }

    public void configGravar(String eArqConfig) throws Exception {
        int ret = ACBrNCMLib.INSTANCE.NCM_ConfigGravar(toUTF8(eArqConfig));
        checkResult(ret);
    }

    @Override
    public String configLerValor(ACBrSessao eSessao, String eChave) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrNCMLib.INSTANCE.NCM_ConfigLerValor(toUTF8(eSessao.name()), toUTF8(eChave), buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);
    }

    @Override
    public void configGravarValor(ACBrSessao eSessao, String eChave, Object value) throws Exception {
        int ret = ACBrNCMLib.INSTANCE.NCM_ConfigGravarValor(toUTF8(eSessao.name()), toUTF8(eChave), toUTF8(value.toString()));
        checkResult(ret);
    }


    public void ConfigImportar(String eArqConfig) throws Exception {

        int ret = ACBrNCMLib.INSTANCE.NCM_ConfigImportar(eArqConfig);
        checkResult(ret);

    }

    public String ConfigExportar() throws Exception {

        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrNCMLib.INSTANCE.NCM_ConfigExportar(buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);

    }
    
    public String descricaoNCM(String cNCM) throws Exception {
        
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrNCMLib.INSTANCE.NCM_DescricaoNCM(cNCM, buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }
    
    public String validar(String cNCM) throws Exception {
        
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrNCMLib.INSTANCE.NCM_Validar(cNCM, buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }
    
    public String baixarLista(String cNomeArquivo) throws Exception {
        
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrNCMLib.INSTANCE.NCM_BaixarLista(cNomeArquivo, buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }
    
    public String obterNCMs() throws Exception{
        
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrNCMLib.INSTANCE.NCM_ObterNCMs(buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
        
    }
    
    public String buscarPorCodigo(String cNCM) throws Exception {
     
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrNCMLib.INSTANCE.NCM_BuscarPorCodigo(cNCM, buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }
    
    public String buscarPorDescricao(String cDesc, int nTipo) throws Exception {
        
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrNCMLib.INSTANCE.NCM_BuscarPorDescricao(cDesc, nTipo, buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }
    
    @Override
    protected void UltimoRetorno(ByteBuffer buffer, IntByReference bufferLen) {
        ACBrNCMLib.INSTANCE.NCM_UltimoRetorno(buffer, bufferLen);
    }
}
