package com.acbr.abecspinpad;

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

public final class ACBrAbecsPinpad extends ACBrLibBase {

    public interface ACBrAbecsPinpadLib extends Library {

        public static String JNA_LIBRARY_NAME = LibraryLoader.getLibraryName();
        public static ACBrAbecsPinpadLib INSTANCE = LibraryLoader.getInstance();

        class LibraryLoader {

            private static String library = "";
            private static ACBrAbecsPinpadLib instance = null;

            public static String getLibraryName() {
                if (library.isEmpty()) {
                    if (Platform.isWindows()) {
                        library = Platform.is64Bit() ? "ACBrAbecsPinpad64" : "ACBrAbecsPinpad32";
                    } else {
                        library = Platform.is64Bit() ? "acbrabecspinpad64" : "acbrabecspinpad32";
                    }
                }
                return library;
            }

            public static ACBrAbecsPinpadLib getInstance() {
                if (instance == null) {
                    instance = (ACBrAbecsPinpadLib) Native.synchronizedLibrary((Library) Native.load(JNA_LIBRARY_NAME, ACBrAbecsPinpadLib.class));
                }
                return instance;
            }
        }

        int AbecsPinpad_Inicializar(PointerByReference libHandler, String eArqConfig, String eChaveCrypt);

        int AbecsPinpad_Finalizar(Pointer libHandler);

        int AbecsPinpad_Nome(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize);

        int AbecsPinpad_Versao(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize);

        int AbecsPinpad_UltimoRetorno(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize);

        int AbecsPinpad_ConfigImportar(Pointer libHandler, String eArqConfig);

        int AbecsPinpad_ConfigExportar(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize);

        int AbecsPinpad_ConfigLer(Pointer libHandler, String eArqConfig);

        int AbecsPinpad_ConfigGravar(Pointer libHandler, String eArqConfig);

        int AbecsPinpad_ConfigLerValor(Pointer libHandler, String eSessao, String eChave, ByteBuffer buffer, IntByReference bufferSize);

        int AbecsPinpad_ConfigGravarValor(Pointer libHandler, String eSessao, String eChave, String valor);

        int AbecsPinpad_Ativar(Pointer libHandler);

        int AbecsPinpad_Desativar(Pointer libHandler);
        
        int AbecsPinpad_OPN(Pointer libHandler);
        
        int AbecsPinpad_CLO(Pointer libHandler, String sMensagem);
        
        int AbecsPinpad_CLX(Pointer libHandler, String sMensagemOuNomeImagem);
        
        int AbecsPinpad_GIX(Pointer libHandler, String PP_DATA, ByteBuffer buffer, IntByReference bufferSize);
          
        int AbecsPinpad_GIN(Pointer libHandler, int GIN_ACQIDX, ByteBuffer buffer, IntByReference bufferSize);
          
        int AbecsPinpad_PinPadCapabilities(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize);
          
        int AbecsPinpad_DSP(Pointer libHandler, String sMensagem);
          
        int AbecsPinpad_DEX(Pointer libHandler, String sMensagem);
          
        int AbecsPinpad_GKY(Pointer libHandler);
          
        int AbecsPinpad_RMC(Pointer libHandler, String sMensagemRMC);
          
        int AbecsPinpad_GCD(Pointer libHandler, int aMSGIDX, int aTimeOut, ByteBuffer buffer, IntByReference bufferSize);
          
        int AbecsPinpad_CEX(Pointer libHandler, boolean VerifyKey, boolean VerifyMagnetic, boolean VerifyICCInsertion, boolean VerifyICCRemoval, boolean VerifyCTLSPresence, int aTimeOut, ByteBuffer buffer, IntByReference bufferSize);
          
        int AbecsPinpad_MNU(Pointer libHandler, String sMNUOPT, String sDSPMSG, int aTimeOut, ByteBuffer buffer, IntByReference bufferSize);

        int AbecsPinpad_LoadMedia(Pointer libHandler, String sCaminhoImagem, int aTipoImagem, ByteBuffer buffer, IntByReference bufferSize);
          
        int AbecsPinpad_LMF(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize);
          
        int AbecsPinpad_DSI(Pointer libHandler, String sNomeArquivo);
          
        int AbecsPinpad_DMF(Pointer libHandler, String sNomeArquivo);
    }

    public ACBrAbecsPinpad() throws Exception {
        File iniFile = Paths.get(System.getProperty("user.dir"), "ACBrLib.ini").toFile();
        if (!iniFile.exists()) {
            iniFile.createNewFile();
        }

        PointerByReference handle = new PointerByReference();
        int ret = ACBrAbecsPinpadLib.INSTANCE.AbecsPinpad_Inicializar(handle, toUTF8(iniFile.getAbsolutePath()), toUTF8(""));
        checkResult(ret);
        setHandle(handle.getValue());
    }

    public ACBrAbecsPinpad(String eArqConfig, String eChaveCrypt) throws Exception {
        PointerByReference handle = new PointerByReference();
        int ret = ACBrAbecsPinpadLib.INSTANCE.AbecsPinpad_Inicializar(handle, toUTF8(eArqConfig), toUTF8(eChaveCrypt));
        checkResult(ret);
        setHandle(handle.getValue());
    }

    @Override
    protected void dispose() throws Exception {
        int ret = ACBrAbecsPinpadLib.INSTANCE.AbecsPinpad_Finalizar(getHandle());
        checkResult(ret);
    }

    public String nome() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrAbecsPinpadLib.INSTANCE.AbecsPinpad_Nome(getHandle(), buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());
    }

    public String versao() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrAbecsPinpadLib.INSTANCE.AbecsPinpad_Versao(getHandle(), buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());
    }
    
    public void ConfigImportar(String eArqConfig) throws Exception {

        int ret = ACBrAbecsPinpadLib.INSTANCE.AbecsPinpad_ConfigImportar(getHandle(), eArqConfig);
        checkResult(ret);
    }
    
        public String ConfigExportar() throws Exception {

        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrAbecsPinpadLib.INSTANCE.AbecsPinpad_ConfigExportar(getHandle(), buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());
    }

    @Override
    protected void UltimoRetorno(ByteBuffer buffer, IntByReference bufferLen) {
        ACBrAbecsPinpadLib.INSTANCE.AbecsPinpad_UltimoRetorno(getHandle(), buffer, bufferLen);
    }
    
    public void configLer() throws Exception {
        configLer("");
    }

    public void configLer(String eArqConfig) throws Exception {
        int ret = ACBrAbecsPinpadLib.INSTANCE.AbecsPinpad_ConfigLer(getHandle(), toUTF8(eArqConfig));
        checkResult(ret);
    }

    public void configGravar() throws Exception {
        configGravar("");
    }

    public void configGravar(String eArqConfig) throws Exception {
        int ret = ACBrAbecsPinpadLib.INSTANCE.AbecsPinpad_ConfigGravar(getHandle(), toUTF8(eArqConfig));
        checkResult(ret);
    }

    @Override
    public String configLerValor(ACBrSessao eSessao, String eChave) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrAbecsPinpadLib.INSTANCE.AbecsPinpad_ConfigLerValor(getHandle(), toUTF8(eSessao.name()), toUTF8(eChave), buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);
    }

    @Override
    public void configGravarValor(ACBrSessao eSessao, String eChave, Object value) throws Exception {
        int ret = ACBrAbecsPinpadLib.INSTANCE.AbecsPinpad_ConfigGravarValor(getHandle(), toUTF8(eSessao.name()), toUTF8(eChave), toUTF8(value.toString()));
        checkResult(ret);
    }

    public void ativar() throws Exception {
        int ret = ACBrAbecsPinpadLib.INSTANCE.AbecsPinpad_Ativar(getHandle());
        checkResult(ret);
    }

    public void desativar() throws Exception {
        int ret = ACBrAbecsPinpadLib.INSTANCE.AbecsPinpad_Desativar(getHandle());
        checkResult(ret);
    }

   public void OPN() throws Exception {
       int ret = ACBrAbecsPinpadLib.INSTANCE.AbecsPinpad_OPN(getHandle());
       checkResult(ret);
   }
   
   public void CLO(String sMensagem) throws Exception {
       int ret = ACBrAbecsPinpadLib.INSTANCE.AbecsPinpad_CLO(getHandle(), sMensagem);
       checkResult(ret);
   }
   
   public void CLX(String sMensagemOuNomeImagem) throws Exception {
       int ret = ACBrAbecsPinpadLib.INSTANCE.AbecsPinpad_CLX(getHandle(), sMensagemOuNomeImagem);
       checkResult(ret);
   }
   
   public String GIX(String PP_DATA) throws Exception {
       ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
       IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
       
       int ret = ACBrAbecsPinpadLib.INSTANCE.AbecsPinpad_GIX(getHandle(), PP_DATA, buffer, bufferLen);
       checkResult(ret);
       
       return processResult(buffer, bufferLen);
   }
   
   public String GIN(int GIN_ACQIDX)throws Exception {
       ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
       IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
       
       int ret = ACBrAbecsPinpadLib.INSTANCE.AbecsPinpad_GIN(getHandle(), GIN_ACQIDX, buffer, bufferLen);
       checkResult(ret);
       
       return processResult(buffer, bufferLen);
   }
   
   public String PinPadCapabilities() throws Exception{
       ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
       IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
       
       int ret = ACBrAbecsPinpadLib.INSTANCE.AbecsPinpad_PinPadCapabilities(getHandle(), buffer, bufferLen);
       checkResult(ret);
       
       return processResult(buffer, bufferLen);
   }
   
   public void DSP(String sMensagem) throws Exception {
       int ret = ACBrAbecsPinpadLib.INSTANCE.AbecsPinpad_DSP(getHandle(), sMensagem);
       checkResult(ret);
   }
   
   public void DEX(String sMensagem) throws Exception {
       int ret = ACBrAbecsPinpadLib.INSTANCE.AbecsPinpad_DEX(getHandle(), sMensagem);
       checkResult(ret);
   }
   
   public void GKY() throws Exception {
       int ret = ACBrAbecsPinpadLib.INSTANCE.AbecsPinpad_GKY(getHandle());
       checkResult(ret);
   }
   
   public void RMC(String sMensagemRMC) throws Exception {
       int ret = ACBrAbecsPinpadLib.INSTANCE.AbecsPinpad_RMC(getHandle(), sMensagemRMC);
       checkResult(ret);
   }
   
   public String GCD(int aMSGIDX, int aTimeOut) throws Exception {
       ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
       IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
       
       int ret = ACBrAbecsPinpadLib.INSTANCE.AbecsPinpad_GCD(getHandle(), aMSGIDX, aTimeOut, buffer, bufferLen);
       checkResult(ret);
       
       return processResult(buffer, bufferLen);
   }
   
   public String CEX(boolean VerifyKey, boolean VerifyMagnetic, boolean VerifyICCInsertion, boolean VerifyICCRemoval, boolean VerifyCTLSPresence, int aTimeOut) throws Exception {
       ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
       IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
       
       int ret = ACBrAbecsPinpadLib.INSTANCE.AbecsPinpad_CEX(getHandle(), VerifyKey, VerifyMagnetic, VerifyICCInsertion, VerifyICCRemoval, VerifyCTLSPresence, aTimeOut, buffer, bufferLen);
       checkResult(ret);
       
       return processResult(buffer, bufferLen);
   }
   
   public String MNU(String sMNUOPT, String sDSPMSG, int aTimeOut) throws Exception {
       ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
       IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
       
       int ret = ACBrAbecsPinpadLib.INSTANCE.AbecsPinpad_MNU(getHandle(), sMNUOPT, sDSPMSG, aTimeOut, buffer, bufferLen);
       checkResult(ret);
       
       return processResult(buffer, bufferLen);
   }
   
   public String LoadMedia(String sCaminhoImagem, int aTipoImagem) throws Exception {
       ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
       IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
       
       int ret = ACBrAbecsPinpadLib.INSTANCE.AbecsPinpad_LoadMedia(getHandle(), sCaminhoImagem, aTipoImagem, buffer, bufferLen);
       checkResult(ret);
       
       return processResult(buffer, bufferLen);
   }
   
   public String LMF() throws Exception {
       ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
       IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
       
       int ret = ACBrAbecsPinpadLib.INSTANCE.AbecsPinpad_LMF(getHandle(), buffer, bufferLen);
       checkResult(ret);
       
       return processResult(buffer, bufferLen);
   }
   
   public void DSI(String sNomeArquivo) throws Exception {
       int ret = ACBrAbecsPinpadLib.INSTANCE.AbecsPinpad_DSI(getHandle(), sNomeArquivo);
       checkResult(ret);
   }
   
   public void DMF(String sNomeArquivo) throws Exception {
       int ret = ACBrAbecsPinpadLib.INSTANCE.AbecsPinpad_DMF(getHandle(), sNomeArquivo);
       checkResult(ret);
   }
}
