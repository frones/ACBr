package com.acbr.bal;

import com.acbr.ACBrLibBase;
import com.acbr.ACBrSessao;
import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Platform;
import com.sun.jna.ptr.DoubleByReference;
import com.sun.jna.ptr.FloatByReference;
import com.sun.jna.ptr.IntByReference;
import java.io.File;
import java.nio.ByteBuffer;
import java.nio.file.Paths;

public final class ACBrBAL extends ACBrLibBase {
    private interface ACBrBALLib extends Library {
        static String JNA_LIBRARY_NAME = LibraryLoader.getLibraryName();
        public final static ACBrBALLib INSTANCE = LibraryLoader.getInstance();
        
        class LibraryLoader {
            private static String library = "";
            private static ACBrBALLib instance = null;
                        
            private static String getLibraryName() {
                if ( library.isEmpty() ) {
                    library = Platform.is64Bit() ? "ACBrBAL64" : "ACBrBAL32";
                }  
                return library;
            }
            
            public static ACBrBALLib getInstance() {
                if ( instance == null ) {
                    instance = (ACBrBALLib) Native.synchronizedLibrary(
                            (Library) Native.loadLibrary(JNA_LIBRARY_NAME, ACBrBALLib.class));
                }
                
                return instance;
            }                             
        }
    
        int BAL_Inicializar(String eArqConfig, String eChaveCrypt);

        int BAL_Finalizar();

        int BAL_Nome(ByteBuffer buffer, IntByReference bufferSize);

        int BAL_Versao(ByteBuffer buffer, IntByReference bufferSize);

        int BAL_UltimoRetorno(ByteBuffer buffer, IntByReference bufferSize);

        int BAL_ConfigLer(String eArqConfig);

        int BAL_ConfigGravar(String eArqConfig);

        int BAL_ConfigLerValor(String eSessao, String eChave, ByteBuffer buffer, IntByReference bufferSize);

        int BAL_ConfigGravarValor(String eSessao, String eChave, String valor);

        int BAL_Ativar();

        int BAL_Desativar();

        int BAL_LePeso(int MillisecTimeOut, DoubleByReference Peso);

        int BAL_SolicitarPeso();

        int BAL_UltimoPesoLido(DoubleByReference Peso);

        int BAL_InterpretarRespostaPeso(ByteBuffer eResposta, FloatByReference Peso);                   
    }

    public ACBrBAL() throws Exception{
        File iniFile = Paths.get(System.getProperty("user.dir"), "ACBrLib.ini").toFile();
        if ( !iniFile.exists() ) {
            iniFile.createNewFile();
        }
        
        int ret = ACBrBALLib.INSTANCE.BAL_Inicializar(toUTF8(iniFile.getAbsolutePath()), toUTF8(""));
        checkResult(ret);
    }
    
    public ACBrBAL(String eArqConfig, String eChaveCrypt) throws Exception{
        int ret = ACBrBALLib.INSTANCE.BAL_Inicializar(toUTF8(eArqConfig), toUTF8(eChaveCrypt));
        checkResult(ret);
    }
    
    @Override
    protected void finalize() throws Throwable {
        try {
            int ret = ACBrBALLib.INSTANCE.BAL_Finalizar();
            checkResult(ret);
        } catch (Exception e) {
            super.finalize();
        }
    }
    
    public String nome() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrBALLib.INSTANCE.BAL_Nome(buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());
    }

    public String versao() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrBALLib.INSTANCE.BAL_Versao(buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());
    }

    public void configLer() throws Exception {
        configLer("");
    }

    public void configLer(String eArqConfig) throws Exception {
        int ret = ACBrBALLib.INSTANCE.BAL_ConfigLer(toUTF8(eArqConfig));
        checkResult(ret);
    }

    public void configGravar() throws Exception {
        configGravar("");
    }

    public void configGravar(String eArqConfig) throws Exception {
        int ret = ACBrBALLib.INSTANCE.BAL_ConfigGravar(toUTF8(eArqConfig));
        checkResult(ret);
    }

    public String configLerValor(ACBrSessao eSessao, String eChave) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrBALLib.INSTANCE.BAL_ConfigLerValor(toUTF8(eSessao.name()), toUTF8(eChave), buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }

    public void configGravarValor(ACBrSessao eSessao, String eChave, Object value) throws Exception {
        int ret = ACBrBALLib.INSTANCE.BAL_ConfigGravarValor(toUTF8(eSessao.name()), toUTF8(eChave), toUTF8(value.toString()));
        checkResult(ret);
    }
    
    public void ativar() throws Exception {
        int ret = ACBrBALLib.INSTANCE.BAL_Ativar();
        checkResult(ret);
    }
    
    public void desativar() throws Exception {
        int ret = ACBrBALLib.INSTANCE.BAL_Desativar();
        checkResult(ret);
    }
    
    public double lePeso(int MillisecTimeOut) throws Exception {
        DoubleByReference peso = new DoubleByReference(STR_BUFFER_LEN);

        int ret = ACBrBALLib.INSTANCE.BAL_LePeso(MillisecTimeOut, peso);
        checkResult(ret);       
        
        System.out.println(peso.getValue());
        
        return peso.getValue();
    }

    public void SolicitarPeso() throws Exception {
        int ret = ACBrBALLib.INSTANCE.BAL_SolicitarPeso();
        checkResult(ret);
    }

    public double ultimoPesoLido() throws Exception {
        DoubleByReference peso = new DoubleByReference(STR_BUFFER_LEN);

        int ret = ACBrBALLib.INSTANCE.BAL_UltimoPesoLido(peso);
        checkResult(ret);
        
        return peso.getValue();
    }

    public float interpretarRespostaPeso(ByteBuffer eResposta, double var) throws Exception {
        FloatByReference peso = new FloatByReference(STR_BUFFER_LEN);

        int ret = ACBrBALLib.INSTANCE.BAL_InterpretarRespostaPeso(eResposta, peso);
        checkResult(ret);
        
        return peso.getValue();
    } 
    
    @Override
    protected void UltimoRetorno(ByteBuffer buffer, IntByReference bufferLen) {
        ACBrBALLib.INSTANCE.BAL_UltimoRetorno(buffer, bufferLen);
    }
       
}
