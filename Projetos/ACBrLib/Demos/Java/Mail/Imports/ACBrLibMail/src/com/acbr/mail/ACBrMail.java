package com.acbr.mail;

import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Platform;
import com.sun.jna.ptr.IntByReference;
import java.io.File;
import com.acbr.ACBrSessao;
import java.nio.ByteBuffer;
import java.nio.charset.Charset;
import java.nio.file.Paths;

public final class ACBrMail  {    
       
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
        
        int MAIL_Inicializar(String eArqConfig, String eChaveCrypt);
        
        int MAIL_Finalizar();
        
        int MAIL_Nome(ByteBuffer buffer, IntByReference bufferSize);
        
        int MAIL_Versao(ByteBuffer buffer, IntByReference bufferSize);
        
        int MAIL_UltimoRetorno(ByteBuffer buffer, IntByReference bufferSize);
        
        int MAIL_ConfigLer(String eArqConfig);
        
        int MAIL_ConfigGravar(String eArqConfig);
        
        int MAIL_ConfigLerValor(String eSessao, String eChave, ByteBuffer buffer, IntByReference bufferSize);
        
        int MAIL_ConfigGravarValor(String eSessao, String eChave, String valor);
          
        int MAIL_SetSubject(String eSubject);  
        
        int MAIL_AddAddress(String eEmail, String eName);
        
        int MAIL_AddReplyTo(String eEmail, String eName);
        
        int MAIL_AddCC(String eEmail, String eName);
        
        int MAIL_AddBCC(String eEmail);
        
        int MAIL_ClearAttachment();
        
        int MAIL_AddAttachment(String eFileName, String eDescription, int aDisposition);
        
        int MAIL_AddBody(String eBody);
        
        int MAIL_AddAltBody(String eAltBody);
        
        int MAIL_SaveToFile(String eFileName);
        
        int MAIL_Clear();
        
        int MAIL_Send();
    }

    private static Charset UTF8 = Charset.forName("UTF-8");
    private static final int STR_BUFFER_LEN = 256;
    
    public ACBrMail() throws Exception {
        File iniFile = Paths.get(System.getProperty("user.dir"), "ACBrLib.ini").toFile();
        if (!iniFile.exists()) {
            iniFile.createNewFile();
        }

        int ret = ACBrMailLib.INSTANCE.MAIL_Inicializar(toUTF8(iniFile.getAbsolutePath()), toUTF8(""));
        checkResult(ret);
    }

    public ACBrMail(String eArqConfig, String eChaveCrypt) throws Exception {
        int ret = ACBrMailLib.INSTANCE.MAIL_Inicializar(toUTF8(eArqConfig), toUTF8(eChaveCrypt));
        checkResult(ret);
    }
    
    @Override
    protected void finalize() throws Throwable {
        try {
            int ret = ACBrMailLib.INSTANCE.MAIL_Finalizar();
            checkResult(ret);
        } finally {
            super.finalize();
        }
    }
    
    public String nome() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrMailLib.INSTANCE.MAIL_Nome(buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());
    }

    public String versao() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrMailLib.INSTANCE.MAIL_Versao(buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());
    }

    public void configLer() throws Exception {
        configLer("");
    }

    public void configLer(String eArqConfig) throws Exception {
        int ret = ACBrMailLib.INSTANCE.MAIL_ConfigLer(toUTF8(eArqConfig));
        checkResult(ret);
    }

    public void configGravar() throws Exception {
        configGravar("");
    }

    public void configGravar(String eArqConfig) throws Exception {
        int ret = ACBrMailLib.INSTANCE.MAIL_ConfigGravar(toUTF8(eArqConfig));
        checkResult(ret);
    }

    public String configLerValor(ACBrSessao eSessao, String eChave) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrMailLib.INSTANCE.MAIL_ConfigLerValor(toUTF8(eSessao.name()), toUTF8(eChave), buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }

    public void configGravarValor(ACBrSessao eSessao, String eChave, Object value) throws Exception {
        int ret = ACBrMailLib.INSTANCE.MAIL_ConfigGravarValor(toUTF8(eSessao.name()), toUTF8(eChave), toUTF8(value.toString()));
        checkResult(ret);
    }
    
    public void setSubject(String eMail) throws Exception {
        int ret = ACBrMailLib.INSTANCE.MAIL_SetSubject(toUTF8(eMail));
        checkResult(ret);
    }
    
    public void addAddress(String eMail, String eName) throws Exception {
        int ret = ACBrMailLib.INSTANCE.MAIL_AddAddress(toUTF8(eMail), toUTF8(eName));
        checkResult(ret);
    }
    
    public void addReplyTo(String eMail, String eName) throws Exception {
        int ret = ACBrMailLib.INSTANCE.MAIL_AddReplyTo(toUTF8(eMail), toUTF8(eName));
        checkResult(ret);
    }
    
    public void addCC(String eMail, String eName) throws Exception {
        int ret = ACBrMailLib.INSTANCE.MAIL_AddCC(toUTF8(eMail), toUTF8(eName));
        checkResult(ret);
    }
    
    public void addBCC(String eMail) throws Exception {
        int ret = ACBrMailLib.INSTANCE.MAIL_AddBCC(toUTF8(eMail));
        checkResult(ret);
    }
    
    public void clearAttachment() throws Exception {
        int ret = ACBrMailLib.INSTANCE.MAIL_ClearAttachment();
        checkResult(ret);
    }
    
    public void addAttachment(String eFileName, String eDescription, MailAttachmentDisposition aDisposition) throws Exception {
        int ret = ACBrMailLib.INSTANCE.MAIL_AddAttachment(toUTF8(eFileName), 
                toUTF8(eDescription), aDisposition.asInt());
        checkResult(ret);
    }
    
    public void addBody(String eBody) throws Exception {
        int ret = ACBrMailLib.INSTANCE.MAIL_AddBody(toUTF8(eBody));
        checkResult(ret);
    }
    
    public void addAltBody(String eAltBody) throws Exception {
        int ret = ACBrMailLib.INSTANCE.MAIL_AddAltBody(toUTF8(eAltBody));
        checkResult(ret);
    }
    
    public void saveToFile(String eFileName) throws Exception {
        int ret = ACBrMailLib.INSTANCE.MAIL_SaveToFile(toUTF8(eFileName));
        checkResult(ret);
    }
    
    public void clear() throws Exception {
        int ret = ACBrMailLib.INSTANCE.MAIL_Clear();
        checkResult(ret);
    }
    
    public void send() throws Exception {
        int ret = ACBrMailLib.INSTANCE.MAIL_Send();
        checkResult(ret);
    }
    
    private static String toUTF8(String value) {
        return new String(value.getBytes(UTF8));
    }

    private static String fromUTF8(ByteBuffer buffer, int len) {
        return new String(buffer.array(), 0, len, UTF8);
    }

    private static void checkResult(int result) throws Exception {
        if (result == 0) {
            return;
        }

        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        ACBrMailLib.INSTANCE.MAIL_UltimoRetorno(buffer, bufferLen);
        throw new Exception(processResult(buffer, bufferLen));
    }
    
    private static String processResult(ByteBuffer buffer, IntByReference bufferLen){
        if (bufferLen.getValue() <= STR_BUFFER_LEN) {
            return fromUTF8(buffer, bufferLen.getValue());
        }

        if (bufferLen.getValue() > STR_BUFFER_LEN) {
            buffer = ByteBuffer.allocate(bufferLen.getValue());
            ACBrMailLib.INSTANCE.MAIL_UltimoRetorno(buffer, bufferLen);
        }

        return fromUTF8(buffer, bufferLen.getValue());
    }
}
