package com.acbr.etq;

import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Platform;
import com.sun.jna.ptr.IntByReference;
import java.nio.ByteBuffer;
import java.nio.charset.Charset;

public interface ACBrETQ extends Library {

    static Charset UTF8 = Charset.forName("UTF-8");
    static String JNA_LIBRARY_NAME = LibraryLoader.getLibraryName();
    public final static ACBrETQ INSTANCE = LibraryLoader.getInstance();

    static class LibraryLoader {

        private static String library = "";
        private static ACBrETQ instance = null;

        public static String getLibraryName() {
            if (library.isEmpty()) {
                library = Platform.is64Bit() ? "ACBrETQ64" : "ACBrETQ32";
            }
            return library;
        }

        public static ACBrETQ getInstance() {
            if (instance == null) {
                instance = (ACBrETQ) Native.synchronizedLibrary((Library) Native.loadLibrary(JNA_LIBRARY_NAME, ACBrETQ.class));
            }
            return instance;
        }
    }

    int ETQ_Inicializar(String eArqConfig, String eChaveCrypt);

    int ETQ_Finalizar();

    int ETQ_Nome(ByteBuffer buffer, IntByReference bufferSize);

    int ETQ_Versao(ByteBuffer buffer, IntByReference bufferSize);

    int ETQ_UltimoRetorno(ByteBuffer buffer, IntByReference bufferSize);

    int ETQ_ConfigLer(String eArqConfig);

    int ETQ_ConfigGravar(String eArqConfig);

    int ETQ_ConfigLerValor(String eSessao, String eChave, ByteBuffer buffer, IntByReference bufferSize);

    int ETQ_ConfigGravarValor(String eSessao, String eChave, String valor);

    int ETQ_Ativar();

    int ETQ_Desativar();

    int ETQ_IniciarEtiqueta();

    int ETQ_FinalizarEtiqueta(int ACopias, int AAvancoEtq);

    int ETQ_CarregarImagem(String eArquivoImagem, String eNomeImagem, boolean Flipped);

    int ETQ_Imprimir(int ACopias, int AAvancoEtq);

    int ETQ_ImprimirTexto(int Orientacao, int Fonte, int MultiplicadorH, int MultiplicadorV,
            int Vertical, int Horizontal, String eTexto, int SubFonte, boolean ImprimirReverso);

    int ETQ_ImprimirTextoStr(int Orientacao, String Fonte, int MultiplicadorH, int MultiplicadorV,
            int Vertical, int Horizontal, String eTexto, int SubFonte, boolean ImprimirReverso);

    int ETQ_ImprimirBarras(int Orientacao, int TipoBarras, int LarguraBarraLarga, int LarguraBarraFina,
            int Vertical, int Horizontal, String eTexto, int AlturaCodBarras, int ExibeCodigo);

    int ETQ_ImprimirLinha(int Vertical, int Horizontal, int Largura, int Altura);

    int ETQ_ImprimirCaixa(int Vertical, int Horizontal, int Largura, int Altura, int EspessuraVertical,
            int EspessuraHorizontal);

    int ETQ_ImprimirImagem(int MultiplicadorImagem, int Vertical, int Horizontal, String eNomeImagem);

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

        ACBrETQ.INSTANCE.ETQ_UltimoRetorno(buffer, bufferLen);

        if (bufferLen.getValue() > 256) {
            buffer = ByteBuffer.allocate(bufferLen.getValue());
            ACBrETQ.INSTANCE.ETQ_UltimoRetorno(buffer, bufferLen);
        }

        throw new Exception(fromUTF8(buffer, bufferLen.getValue()));
    }
}
