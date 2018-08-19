package com.acbr.mail;

import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Platform;
import com.sun.jna.ptr.IntByReference;
import java.nio.ByteBuffer;
import java.nio.charset.Charset;

public interface ACBrMail extends Library {

    public final static Charset UTF8 = Charset.forName("UTF-8");
    public final static String JNA_LIBRARY_NAME = LibraryLoader.getLibraryName();
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
