package com.acbr.posprinter;

import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Platform;
import com.sun.jna.ptr.IntByReference;
import java.nio.ByteBuffer;
import java.nio.charset.Charset;

public interface ACBrPosPrinter extends Library {

    public final static Charset UTF8 = Charset.forName("UTF-8");
    public static String JNA_LIBRARY_NAME = LibraryLoader.getLibraryName();
    public static ACBrPosPrinter INSTANCE = LibraryLoader.getInstance();

    public int POS_LerStatusImpressora(int i, ByteBuffer buffer, IntByReference bufferLen);

    class LibraryLoader {

        private static String library = "";
        private static ACBrPosPrinter instance = null;

        public static String getLibraryName() {
            if (library.isEmpty()) {
                library = Platform.is64Bit() ? "ACBrPosPrinter64" : "ACBrPosPrinter32";
            }
            return library;
        }

        public static ACBrPosPrinter getInstance() {
            if (instance == null) {
                instance = (ACBrPosPrinter) Native.synchronizedLibrary((Library) Native.loadLibrary(JNA_LIBRARY_NAME, ACBrPosPrinter.class));
            }
            return instance;
        }
    }

    int POS_Inicializar(String eArqConfig, String eChaveCrypt);

    int POS_Finalizar();

    int POS_Nome(ByteBuffer buffer, IntByReference bufferSize);

    int POS_Versao(ByteBuffer buffer, IntByReference bufferSize);

    int POS_UltimoRetorno(ByteBuffer buffer, IntByReference bufferSize);

    int POS_ConfigLer(String eArqConfig);

    int POS_ConfigGravar(String eArqConfig);

    int POS_ConfigLerValor(String eSessao, String eChave, ByteBuffer buffer, IntByReference bufferSize);

    int POS_ConfigGravarValor(String eSessao, String eChave, String valor);

    int POS_Ativar();

    int POS_Desativar();

    int POS_Imprimir(String aString, boolean pulaLinha, boolean decodificarTags, boolean codificarPagina, int copias);

    int POS_ImprimirLinha(String aString);

    int POS_ImprimirCmd(String aString);

    int POS_ImprimirTags();

    int POS_TxRx(String aString, byte bytesToRead, int aTimeOut, boolean waitForTerminator, ByteBuffer buffer, IntByReference bufferSize);

    int POS_Zerar();

    int POS_InicializarPos();

    int POS_Reset();

    int POS_PularLinhas(int numLinhas);

    int POS_CortarPapel(boolean parcial);

    int POS_AbrirGaveta();

    int POS_LerInfoImpressora(ByteBuffer buffer, IntByReference bufferSize);

    int POS_LerStatusImpressora(int tentativas, IntByReference status);

    int POS_RetornarTags(ByteBuffer buffer, IntByReference bufferSize, boolean incluiAjuda);

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

        ACBrPosPrinter.INSTANCE.POS_UltimoRetorno(buffer, bufferLen);

        if (bufferLen.getValue() > 256) {
            buffer = ByteBuffer.allocate(bufferLen.getValue());
            ACBrPosPrinter.INSTANCE.POS_UltimoRetorno(buffer, bufferLen);
        }

        throw new Exception(fromUTF8(buffer, bufferLen.getValue()));
    }

}
