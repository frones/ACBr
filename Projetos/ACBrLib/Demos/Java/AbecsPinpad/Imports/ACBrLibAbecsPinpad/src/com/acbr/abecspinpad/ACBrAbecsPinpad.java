package com.acbr.abecspinpad;

import com.acbr.ACBrLibBase;
import com.acbr.ACBrSessao;
import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Platform;
import com.sun.jna.ptr.IntByReference;
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

        int AbecsPinpad_Inicializar(String eArqConfig, String eChaveCrypt);

        int AbecsPinpad_Finalizar();

        int AbecsPinpad_Nome(ByteBuffer buffer, IntByReference bufferSize);

        int AbecsPinpad_Versao(ByteBuffer buffer, IntByReference bufferSize);

        int AbecsPinpad_UltimoRetorno(ByteBuffer buffer, IntByReference bufferSize);

        int AbecsPinpad_ConfigImportar(String eArqConfig);

        int AbecsPinpad_ConfigExportar(ByteBuffer buffer, IntByReference bufferSize);

        int AbecsPinpad_ConfigLer(String eArqConfig);

        int AbecsPinpad_ConfigGravar(String eArqConfig);

        int AbecsPinpad_ConfigLerValor(String eSessao, String eChave, ByteBuffer buffer, IntByReference bufferSize);

        int AbecsPinpad_ConfigGravarValor(String eSessao, String eChave, String valor);

        int AbecsPinpad_Ativar();

        int AbecsPinpad_Desativar();
        
        int AbecsPinpad_OPN();
        
        int AbecsPinpad_CLO(String sMensagem);
        
        int AbecsPinpad_CLX(String sMensagemOuNomeImagem);
        
        int AbecsPinpad_GIX(String PP_DATA, ByteBuffer buffer, IntByReference bufferSize);
          
        int AbecsPinpad_GIN(int GIN_ACQIDX, ByteBuffer buffer, IntByReference bufferSize);
          
        int AbecsPinpad_PinPadCapabilities(ByteBuffer buffer, IntByReference bufferSize);
          
        int AbecsPinpad_DSP(String sMensagem);
          
        int AbecsPinpad_DEX(String sMensagem);
          
        int AbecsPinpad_GKY();
          
        int AbecsPinpad_RMC(String sMensagemRMC);
          
        int AbecsPinpad_GCD(int aMSGIDX, int aTimeOut, ByteBuffer buffer, IntByReference bufferSize);
          
        int AbecsPinpad_CEX(boolean VerifyKey, boolean VerifyMagnetic, boolean VerifyICCInsertion, boolean VerifyICCRemoval, boolean VerifyCTLSPresence, int aTimeOut, ByteBuffer buffer, IntByReference bufferSize);
          
        int AbecsPinpad_MNU(String sMNUOPT, String sDSPMSG, int aTimeOut, ByteBuffer buffer, IntByReference bufferSize);

        int AbecsPinpad_LoadMedia(String sCaminhoImagem, int aTipoImagem, ByteBuffer buffer, IntByReference bufferSize);
          
        int AbecsPinpad_LMF(ByteBuffer buffer, IntByReference bufferSize);
          
        int AbecsPinpad_DSI(String sNomeArquivo);
          
        int AbecsPinpad_DMF(String sNomeArquivo);
    }

    public ACBrAbecsPinpad() throws Exception {
        File iniFile = Paths.get(System.getProperty("user.dir"), "ACBrLib.ini").toFile();
        if (!iniFile.exists()) {
            iniFile.createNewFile();
        }

        int ret = ACBrAbecsPinpadLib.INSTANCE.AbecsPinpad_Inicializar(toUTF8(iniFile.getAbsolutePath()), toUTF8(""));
        checkResult(ret);
    }

    public ACBrAbecsPinpad(String eArqConfig, String eChaveCrypt) throws Exception {
        int ret = ACBrAbecsPinpadLib.INSTANCE.AbecsPinpad_Inicializar(toUTF8(eArqConfig), toUTF8(eChaveCrypt));
        checkResult(ret);
    }

    @Override
    protected void dispose() throws Exception {
        int ret = ACBrAbecsPinpadLib.INSTANCE.AbecsPinpad_Finalizar();
        checkResult(ret);
    }

    public String nome() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrAbecsPinpadLib.INSTANCE.AbecsPinpad_Nome(buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());
    }

    public String versao() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrAbecsPinpadLib.INSTANCE.AbecsPinpad_Versao(buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());
    }
    
    public void ConfigImportar(String eArqConfig) throws Exception {

        int ret = ACBrAbecsPinpadLib.INSTANCE.AbecsPinpad_ConfigImportar(eArqConfig);
        checkResult(ret);
    }
    
        public String ConfigExportar() throws Exception {

        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrAbecsPinpadLib.INSTANCE.AbecsPinpad_ConfigExportar(buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());
    }

    @Override
    protected void UltimoRetorno(ByteBuffer buffer, IntByReference bufferLen) {
        ACBrAbecsPinpadLib.INSTANCE.AbecsPinpad_UltimoRetorno(buffer, bufferLen);
    }
    
    public void configLer() throws Exception {
        configLer("");
    }

    public void configLer(String eArqConfig) throws Exception {
        int ret = ACBrAbecsPinpadLib.INSTANCE.AbecsPinpad_ConfigLer(toUTF8(eArqConfig));
        checkResult(ret);
    }

    public void configGravar() throws Exception {
        configGravar("");
    }

    public void configGravar(String eArqConfig) throws Exception {
        int ret = ACBrAbecsPinpadLib.INSTANCE.AbecsPinpad_ConfigGravar(toUTF8(eArqConfig));
        checkResult(ret);
    }

    @Override
    public String configLerValor(ACBrSessao eSessao, String eChave) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrAbecsPinpadLib.INSTANCE.AbecsPinpad_ConfigLerValor(toUTF8(eSessao.name()), toUTF8(eChave), buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);
    }

    @Override
    public void configGravarValor(ACBrSessao eSessao, String eChave, Object value) throws Exception {
        int ret = ACBrAbecsPinpadLib.INSTANCE.AbecsPinpad_ConfigGravarValor(toUTF8(eSessao.name()), toUTF8(eChave), toUTF8(value.toString()));
        checkResult(ret);
    }

    public void ativar() throws Exception {
        int ret = ACBrAbecsPinpadLib.INSTANCE.AbecsPinpad_Ativar();
        checkResult(ret);
    }

    public void desativar() throws Exception {
        int ret = ACBrAbecsPinpadLib.INSTANCE.AbecsPinpad_Desativar();
        checkResult(ret);
    }

   public void OPN() throws Exception {
       int ret = ACBrAbecsPinpadLib.INSTANCE.AbecsPinpad_OPN();
       checkResult(ret);
   }
   
   public void CLO(String sMensagem) throws Exception {
       int ret = ACBrAbecsPinpadLib.INSTANCE.AbecsPinpad_CLO(sMensagem);
       checkResult(ret);
   }
   
   public void CLX(String sMensagemOuNomeImagem) throws Exception {
       int ret = ACBrAbecsPinpadLib.INSTANCE.AbecsPinpad_CLX(sMensagemOuNomeImagem);
       checkResult(ret);
   }
   
   public String GIX(String PP_DATA) throws Exception {
       ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
       IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
       
       int ret = ACBrAbecsPinpadLib.INSTANCE.AbecsPinpad_GIX(PP_DATA, buffer, bufferLen);
       checkResult(ret);
       
       return processResult(buffer, bufferLen);
   }
   
   public String GIN(int GIN_ACQIDX)throws Exception {
       ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
       IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
       
       int ret = ACBrAbecsPinpadLib.INSTANCE.AbecsPinpad_GIN(GIN_ACQIDX, buffer, bufferLen);
       checkResult(ret);
       
       return processResult(buffer, bufferLen);
   }
   
   public String PinPadCapabilities() throws Exception{
       ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
       IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
       
       int ret = ACBrAbecsPinpadLib.INSTANCE.AbecsPinpad_PinPadCapabilities(buffer, bufferLen);
       checkResult(ret);
       
       return processResult(buffer, bufferLen);
   }
   
   public void DSP(String sMensagem) throws Exception {
       int ret = ACBrAbecsPinpadLib.INSTANCE.AbecsPinpad_DSP(sMensagem);
       checkResult(ret);
   }
   
   public void DEX(String sMensagem) throws Exception {
       int ret = ACBrAbecsPinpadLib.INSTANCE.AbecsPinpad_DEX(sMensagem);
       checkResult(ret);
   }
   
   public void GKY() throws Exception {
       int ret = ACBrAbecsPinpadLib.INSTANCE.AbecsPinpad_GKY();
       checkResult(ret);
   }
   
   public void RMC(String sMensagemRMC) throws Exception {
       int ret = ACBrAbecsPinpadLib.INSTANCE.AbecsPinpad_RMC(sMensagemRMC);
       checkResult(ret);
   }
   
   public String GCD(int aMSGIDX, int aTimeOut) throws Exception {
       ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
       IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
       
       int ret = ACBrAbecsPinpadLib.INSTANCE.AbecsPinpad_GCD(aMSGIDX, aTimeOut, buffer, bufferLen);
       checkResult(ret);
       
       return processResult(buffer, bufferLen);
   }
   
   public String CEX(boolean VerifyKey, boolean VerifyMagnetic, boolean VerifyICCInsertion, boolean VerifyICCRemoval, boolean VerifyCTLSPresence, int aTimeOut) throws Exception {
       ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
       IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
       
       int ret = ACBrAbecsPinpadLib.INSTANCE.AbecsPinpad_CEX(VerifyKey, VerifyMagnetic, VerifyICCInsertion, VerifyICCRemoval, VerifyCTLSPresence, aTimeOut, buffer, bufferLen);
       checkResult(ret);
       
       return processResult(buffer, bufferLen);
   }
   
   public String MNU(String sMNUOPT, String sDSPMSG, int aTimeOut) throws Exception {
       ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
       IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
       
       int ret = ACBrAbecsPinpadLib.INSTANCE.AbecsPinpad_MNU(sMNUOPT, sDSPMSG, aTimeOut, buffer, bufferLen);
       checkResult(ret);
       
       return processResult(buffer, bufferLen);
   }
   
   public String LoadMedia(String sCaminhoImagem, int aTipoImagem) throws Exception {
       ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
       IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
       
       int ret = ACBrAbecsPinpadLib.INSTANCE.AbecsPinpad_LoadMedia(sCaminhoImagem, aTipoImagem, buffer, bufferLen);
       checkResult(ret);
       
       return processResult(buffer, bufferLen);
   }
   
   public String LMF() throws Exception {
       ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
       IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
       
       int ret = ACBrAbecsPinpadLib.INSTANCE.AbecsPinpad_LMF(buffer, bufferLen);
       checkResult(ret);
       
       return processResult(buffer, bufferLen);
   }
   
   public void DSI(String sNomeArquivo) throws Exception {
       int ret = ACBrAbecsPinpadLib.INSTANCE.AbecsPinpad_DSI(sNomeArquivo);
       checkResult(ret);
   }
   
   public void DMF(String sNomeArquivo) throws Exception {
       int ret = ACBrAbecsPinpadLib.INSTANCE.AbecsPinpad_DMF(sNomeArquivo);
       checkResult(ret);
   }
}
