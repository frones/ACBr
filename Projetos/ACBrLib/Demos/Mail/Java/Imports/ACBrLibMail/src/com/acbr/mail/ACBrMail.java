package com.acbr.mail;

import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Platform;
import com.sun.jna.ptr.IntByReference;
import java.nio.ByteBuffer;
import java.nio.charset.Charset;

public interface ACBrMail extends Library {

    static Charset UTF8 = Charset.forName("UTF-8");
    static String JNA_LIBRARY_NAME = LibraryLoader.getLibraryName();
    public final static ACBrMail INSTANCE = LibraryLoader.getInstance();

    class LibraryLoader {

        private static String library = "";
        private static ACBrMail instance = null;

        public static String getLibraryName() {
            if (library.isEmpty()) {
                library = Platform.is64Bit() ? "ACBrMail64" : "ACBrMail32";
            }
            return library;
        }

        public static ACBrMail getInstance() {
            if (instance == null) {
                instance = (ACBrMail) Native.synchronizedLibrary((Library) Native.loadLibrary(JNA_LIBRARY_NAME, ACBrMail.class));
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
    
    int MAIL_Send(Boolean UseThreadNow);

    public static String toUTF8(String value) {
        return new String(value.getBytes(UTF8));
    }

    public static String fromUTF8(ByteBuffer buffer, int len) {
        return new String(buffer.array(), 0, len, UTF8);
    }

    public static void checkResult(int result) throws Exception {
        if (result == 0) {
            return;
        }

        ByteBuffer buffer = ByteBuffer.allocate(256);
        IntByReference bufferLen = new IntByReference(256);

        ACBrMail.INSTANCE.MAIL_UltimoRetorno(buffer, bufferLen);

        if (bufferLen.getValue() > 256) {
            buffer = ByteBuffer.allocate(bufferLen.getValue());
            ACBrMail.INSTANCE.MAIL_UltimoRetorno(buffer, bufferLen);
        }

        throw new Exception(fromUTF8(buffer, bufferLen.getValue()));
    }
}
