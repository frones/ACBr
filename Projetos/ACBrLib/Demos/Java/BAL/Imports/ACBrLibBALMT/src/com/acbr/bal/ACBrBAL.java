package com.acbr.bal;

import com.acbr.ACBrLibBase;
import com.acbr.ACBrSessao;
import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Platform;
import com.sun.jna.Pointer;
import com.sun.jna.ptr.DoubleByReference;
import com.sun.jna.ptr.FloatByReference;
import com.sun.jna.ptr.IntByReference;
import com.sun.jna.ptr.PointerByReference;
import java.io.File;
import java.nio.ByteBuffer;
import java.nio.file.Paths;

public final class ACBrBAL extends ACBrLibBase implements AutoCloseable {
    
    private interface ACBrBALLib extends Library {
        
        static String JNA_LIBRARY_NAME = LibraryLoader.getLibraryName();
        public final static ACBrBALLib INSTANCE = LibraryLoader.getInstance();
        
        class LibraryLoader {
            
            private static String library = "";
            private static ACBrBALLib instance = null;
                        
            private static String getLibraryName() {
                if ( library.isEmpty() ) {
                    if(Platform.isWindows()){
                        library = Platform.is64Bit() ? "ACBrBAL64" : "ACBrBAL32";                        
                    }else{
                        library = Platform.is64Bit() ? "acbrbal64" : "acbrbal32";
                    }
                    
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
    
        int BAL_Inicializar(PointerByReference libHandler, String eArqConfig, String eChaveCrypt);

        int BAL_Finalizar(Pointer libHandler);

        int BAL_Nome(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize);

        int BAL_Versao(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize);

        int BAL_UltimoRetorno(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize);

        int BAL_ConfigLer(Pointer libHandler, String eArqConfig);

        int BAL_ConfigGravar(Pointer libHandler, String eArqConfig);

        int BAL_ConfigLerValor(Pointer libHandler, String eSessao, String eChave, ByteBuffer buffer, IntByReference bufferSize);

        int BAL_ConfigGravarValor(Pointer libHandler, String eSessao, String eChave, String valor);

        int BAL_Ativar(Pointer libHandler);

        int BAL_Desativar(Pointer libHandler);

        int BAL_LePeso(Pointer libHandler, int MillisecTimeOut, DoubleByReference Peso);

        int BAL_SolicitarPeso(Pointer libHandler);

        int BAL_UltimoPesoLido(Pointer libHandler, DoubleByReference Peso);

        int BAL_ConfigImportar(Pointer libHandler, String eArqConfig);
        
		int BAL_ConfigExportar(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize);
        
        int BAL_InterpretarRespostaPeso(Pointer libHandler, ByteBuffer eResposta, FloatByReference Peso);                   
    }

    public ACBrBAL() throws Exception{
        File iniFile = Paths.get(System.getProperty("user.dir"), "ACBrLib.ini").toFile();
        if ( !iniFile.exists() ) {
            iniFile.createNewFile();
        }
        
        PointerByReference handle = new PointerByReference();
        int ret = ACBrBALLib.INSTANCE.BAL_Inicializar(handle, toUTF8(iniFile.getAbsolutePath()), toUTF8(""));
        checkResult(ret);
		setHandle(handle.getValue());
    }
    
    public ACBrBAL(String eArqConfig, String eChaveCrypt) throws Exception{
        PointerByReference handle = new PointerByReference();
        int ret = ACBrBALLib.INSTANCE.BAL_Inicializar(handle, toUTF8(eArqConfig), toUTF8(eChaveCrypt));
        checkResult(ret);
		setHandle(handle.getValue());
    }
    
    @Override
    public void close() throws Exception {
        int ret = ACBrBALLib.INSTANCE.BAL_Finalizar(getHandle());
        checkResult(ret);
    }
    
    @Override
    protected void finalize() throws Throwable {
        try {
            int ret = ACBrBALLib.INSTANCE.BAL_Finalizar(getHandle());
            checkResult(ret);
        } catch (Exception e) {
            super.finalize();
        }
    }
    
    public String nome() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrBALLib.INSTANCE.BAL_Nome(getHandle(), buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());
    }

    public String versao() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrBALLib.INSTANCE.BAL_Versao(getHandle(), buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());
    }

    public void configLer() throws Exception {
        configLer("");
    }

    public void configLer(String eArqConfig) throws Exception {
        int ret = ACBrBALLib.INSTANCE.BAL_ConfigLer(getHandle(), toUTF8(eArqConfig));
        checkResult(ret);
    }

    public void configGravar() throws Exception {
        configGravar("");
    }

    public void configGravar(String eArqConfig) throws Exception {
        int ret = ACBrBALLib.INSTANCE.BAL_ConfigGravar(getHandle(), toUTF8(eArqConfig));
        checkResult(ret);
    }

    @Override
    public String configLerValor(ACBrSessao eSessao, String eChave) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrBALLib.INSTANCE.BAL_ConfigLerValor(getHandle(), toUTF8(eSessao.name()), toUTF8(eChave), buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }

    @Override
    public void configGravarValor(ACBrSessao eSessao, String eChave, Object value) throws Exception {
        int ret = ACBrBALLib.INSTANCE.BAL_ConfigGravarValor(getHandle(), toUTF8(eSessao.name()), toUTF8(eChave), toUTF8(value.toString()));
        checkResult(ret);
    }
    
    public void ativar() throws Exception {
        int ret = ACBrBALLib.INSTANCE.BAL_Ativar(getHandle());
        checkResult(ret);
    }
    
    public void desativar() throws Exception {
        int ret = ACBrBALLib.INSTANCE.BAL_Desativar(getHandle());
        checkResult(ret);
    }
    
    public double lePeso(int MillisecTimeOut) throws Exception {
        DoubleByReference peso = new DoubleByReference(STR_BUFFER_LEN);

        int ret = ACBrBALLib.INSTANCE.BAL_LePeso(getHandle(), MillisecTimeOut, peso);
        checkResult(ret);       
        
        System.out.println(peso.getValue());
        
        return peso.getValue();
    }

    public void SolicitarPeso() throws Exception {
        int ret = ACBrBALLib.INSTANCE.BAL_SolicitarPeso(getHandle());
        checkResult(ret);
    }

    public double ultimoPesoLido() throws Exception {
        DoubleByReference peso = new DoubleByReference(STR_BUFFER_LEN);

        int ret = ACBrBALLib.INSTANCE.BAL_UltimoPesoLido(getHandle(), peso);
        checkResult(ret);
        
        return peso.getValue();
    }

    public float interpretarRespostaPeso(ByteBuffer eResposta, double var) throws Exception {
        FloatByReference peso = new FloatByReference(STR_BUFFER_LEN);

        int ret = ACBrBALLib.INSTANCE.BAL_InterpretarRespostaPeso(getHandle(), eResposta, peso);
        checkResult(ret);
        
        return peso.getValue();
    } 
    
    public void ConfigImportar(String eArqConfig) throws Exception {
        
        int ret = ACBrBALLib.INSTANCE.BAL_ConfigImportar(getHandle(), eArqConfig);
        checkResult(ret);
        
    }
    
    public String ConfigExportar() throws Exception {
		
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrBALLib.INSTANCE.BAL_ConfigExportar(getHandle(), buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());
		
    }
    
    @Override
    protected void UltimoRetorno(ByteBuffer buffer, IntByReference bufferLen) {
        ACBrBALLib.INSTANCE.BAL_UltimoRetorno(getHandle(), buffer, bufferLen);
    }       
}
