package com.acbr.mail;

import com.acbr.ACBrLibBase;
import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Platform;
import com.sun.jna.ptr.IntByReference;
import java.io.File;
import com.acbr.ACBrSessao;
import com.sun.jna.Pointer;
import com.sun.jna.ptr.PointerByReference;
import java.nio.ByteBuffer;
import java.nio.file.Paths;

public final class ACBrMail extends ACBrLibBase implements AutoCloseable  {    
       
    private interface ACBrMailLib extends Library {
        
        static String JNA_LIBRARY_NAME = LibraryLoader.getLibraryName();
        public final static ACBrMailLib INSTANCE = LibraryLoader.getInstance();
        
        class LibraryLoader {
            private static String library = "";
            private static ACBrMailLib instance = null;
            
            public static String getLibraryName() {
                if (library.isEmpty()) {
                    library = Platform.is64Bit() ? "ACBrMail64" : "ACBrMail32";
                }
                
                return library;
            }
            
            public static ACBrMailLib getInstance() {
                if (instance == null) {
                    instance = (ACBrMailLib) Native.synchronizedLibrary((Library) Native.loadLibrary(JNA_LIBRARY_NAME, ACBrMailLib.class));
                }
                
                return instance;
            }
        }
        
        int MAIL_Inicializar(PointerByReference libHandle, String eArqConfig, String eChaveCrypt);
        
        int MAIL_Finalizar(Pointer libHandler);
        
        int MAIL_Nome(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize);
        
        int MAIL_Versao(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize);
        
        int MAIL_UltimoRetorno(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize);
        
        int MAIL_ConfigImportar(Pointer libHandler, String eArqConfig);
        
	    int MAIL_ConfigExportar(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize);
        
        int MAIL_ConfigLer(Pointer libHandler, String eArqConfig);
        
        int MAIL_ConfigGravar(Pointer libHandler, String eArqConfig);
        
        int MAIL_ConfigLerValor(Pointer libHandler, String eSessao, String eChave, ByteBuffer buffer, IntByReference bufferSize);
        
        int MAIL_ConfigGravarValor(Pointer libHandler, String eSessao, String eChave, String valor);
          
        int MAIL_SetSubject(Pointer libHandler, String eSubject);  
        
        int MAIL_AddAddress(Pointer libHandler, String eEmail, String eName);
        
        int MAIL_AddReplyTo(Pointer libHandler, String eEmail, String eName);
        
        int MAIL_AddCC(Pointer libHandler, String eEmail, String eName);
        
        int MAIL_AddBCC(Pointer libHandler, String eEmail);
        
        int MAIL_ClearAttachment(Pointer libHandler);
        
        int MAIL_AddAttachment(Pointer libHandler, String eFileName, String eDescription, int aDisposition);
        
        int MAIL_AddBody(Pointer libHandler, String eBody);
        
        int MAIL_AddAltBody(Pointer libHandler, String eAltBody);
        
        int MAIL_SaveToFile(Pointer libHandler, String eFileName);
        
        int MAIL_Clear(Pointer libHandler);
        
        int MAIL_Send(Pointer libHandler);
    }
 
    public ACBrMail() throws Exception {
        File iniFile = Paths.get(System.getProperty("user.dir"), "ACBrLib.ini").toFile();
        if (!iniFile.exists()) {
            iniFile.createNewFile();
        }
        
        PointerByReference handle = new PointerByReference();
        int ret = ACBrMailLib.INSTANCE.MAIL_Inicializar(handle, toUTF8(iniFile.getAbsolutePath()), toUTF8(""));
        checkResult(ret);
    }

    public ACBrMail(String eArqConfig, String eChaveCrypt) throws Exception {
        PointerByReference handle = new PointerByReference();
        int ret = ACBrMailLib.INSTANCE.MAIL_Inicializar(handle, toUTF8(eArqConfig), toUTF8(eChaveCrypt));
        checkResult(ret);
    }
    
    @Override
    public void close() throws Exception {
        int ret = ACBrMailLib.INSTANCE.MAIL_Finalizar(getHandle());
        checkResult(ret);
    }
    
    @Override
    protected void finalize() throws Throwable {
        try {
            int ret = ACBrMailLib.INSTANCE.MAIL_Finalizar(getHandle());
            checkResult(ret);
        } finally {
            super.finalize();
        }
    }
    
    public String nome() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrMailLib.INSTANCE.MAIL_Nome(getHandle(), buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());
    }

    public String versao() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrMailLib.INSTANCE.MAIL_Versao(getHandle(), buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());
    }

    public void configLer() throws Exception {
        configLer("");
    }

    public void configLer(String eArqConfig) throws Exception {
        int ret = ACBrMailLib.INSTANCE.MAIL_ConfigLer(getHandle(), toUTF8(eArqConfig));
        checkResult(ret);
    }

    public void configGravar() throws Exception {
        configGravar("");
    }

    public void configGravar(String eArqConfig) throws Exception {
        int ret = ACBrMailLib.INSTANCE.MAIL_ConfigGravar(getHandle(), toUTF8(eArqConfig));
        checkResult(ret);
    }

    public String configLerValor(ACBrSessao eSessao, String eChave) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrMailLib.INSTANCE.MAIL_ConfigLerValor(getHandle(), toUTF8(eSessao.name()), toUTF8(eChave), buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }

    public void configGravarValor(ACBrSessao eSessao, String eChave, Object value) throws Exception {
        int ret = ACBrMailLib.INSTANCE.MAIL_ConfigGravarValor(getHandle(), toUTF8(eSessao.name()), toUTF8(eChave), toUTF8(value.toString()));
        checkResult(ret);
    }
    
    public void setSubject(String eMail) throws Exception {
        int ret = ACBrMailLib.INSTANCE.MAIL_SetSubject(getHandle(), toUTF8(eMail));
        checkResult(ret);
    }
    
    public void addAddress(String eMail, String eName) throws Exception {
        int ret = ACBrMailLib.INSTANCE.MAIL_AddAddress(getHandle(), toUTF8(eMail), toUTF8(eName));
        checkResult(ret);
    }
    
    public void addReplyTo(String eMail, String eName) throws Exception {
        int ret = ACBrMailLib.INSTANCE.MAIL_AddReplyTo(getHandle(), toUTF8(eMail), toUTF8(eName));
        checkResult(ret);
    }
    
    public void addCC(String eMail, String eName) throws Exception {
        int ret = ACBrMailLib.INSTANCE.MAIL_AddCC(getHandle(), toUTF8(eMail), toUTF8(eName));
        checkResult(ret);
    }
    
    public void addBCC(String eMail) throws Exception {
        int ret = ACBrMailLib.INSTANCE.MAIL_AddBCC(getHandle(), toUTF8(eMail));
        checkResult(ret);
    }
    
    public void clearAttachment() throws Exception {
        int ret = ACBrMailLib.INSTANCE.MAIL_ClearAttachment(getHandle());
        checkResult(ret);
    }
    
    public void addAttachment(String eFileName, String eDescription, MailAttachmentDisposition aDisposition) throws Exception {
        int ret = ACBrMailLib.INSTANCE.MAIL_AddAttachment(getHandle(), toUTF8(eFileName), 
                toUTF8(eDescription), aDisposition.asInt());
        checkResult(ret);
    }
    
    public void addBody(String eBody) throws Exception {
        int ret = ACBrMailLib.INSTANCE.MAIL_AddBody(getHandle(), toUTF8(eBody));
        checkResult(ret);
    }
    
    public void addAltBody(String eAltBody) throws Exception {
        int ret = ACBrMailLib.INSTANCE.MAIL_AddAltBody(getHandle(), toUTF8(eAltBody));
        checkResult(ret);
    }
    
    public void saveToFile(String eFileName) throws Exception {
        int ret = ACBrMailLib.INSTANCE.MAIL_SaveToFile(getHandle(), toUTF8(eFileName));
        checkResult(ret);
    }
    
    public void clear() throws Exception {
        int ret = ACBrMailLib.INSTANCE.MAIL_Clear(getHandle());
        checkResult(ret);
    }
    
    public void send() throws Exception {
        int ret = ACBrMailLib.INSTANCE.MAIL_Send(getHandle());
        checkResult(ret);
    }
    
    public void ConfigImportar(String eArqConfig) throws Exception {
        
        int ret = ACBrMailLib.INSTANCE.MAIL_ConfigImportar(getHandle(), eArqConfig);
        checkResult(ret);
        
    }
    
    public String ConfigExportar() throws Exception {
		
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrMailLib.INSTANCE.MAIL_ConfigExportar(getHandle(), buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());
		
    }
    
    @Override
    protected void UltimoRetorno(ByteBuffer buffer, IntByReference bufferLen) {
        ACBrMailLib.INSTANCE.MAIL_UltimoRetorno(getHandle(), buffer, bufferLen);
    }
}
