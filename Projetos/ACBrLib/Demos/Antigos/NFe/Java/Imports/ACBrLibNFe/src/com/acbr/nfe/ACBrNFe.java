package com.acbr.nfe;

import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Platform;
import com.sun.jna.ptr.IntByReference;
import java.nio.ByteBuffer;
import java.nio.charset.Charset;

public interface ACBrNFe extends Library {

    static final Charset UTF8 = Charset.forName("UTF-8");
    static final int STR_BUFFER_LEN = 256;
    static String JNA_LIBRARY_NAME = LibraryLoader.getLibraryName();
    public final static ACBrNFe INSTANCE = LibraryLoader.getInstance();

    class LibraryLoader {

        private static String library = "";
        private static ACBrNFe instance = null;

        private static String getLibraryName() {
            if (library.isEmpty()) {
                library = Platform.is64Bit() ? "ACBrNFe64" : "ACBrNFe32";
            }
            return library;
        }

        public static ACBrNFe getInstance() {
            if (instance == null) {
                instance = (ACBrNFe) Native.synchronizedLibrary(
                        (Library) Native.loadLibrary(JNA_LIBRARY_NAME, ACBrNFe.class));
            }
            return instance;
        }
    }

    int NFE_Inicializar(String eArqConfig, String eChaveCrypt);

    int NFE_Finalizar();

    int NFE_Nome(ByteBuffer buffer, IntByReference bufferSize);

    int NFE_Versao(ByteBuffer buffer, IntByReference bufferSize);

    int NFE_UltimoRetorno(ByteBuffer buffer, IntByReference bufferSize);

    int NFE_ConfigLer(String eArqConfig);

    int NFE_ConfigGravar(String eArqConfig);

    int NFE_ConfigLerValor(String eSessao, String eChave, ByteBuffer buffer, IntByReference bufferSize);

    int NFE_ConfigGravarValor(String eSessao, String eChave, String valor);

    int NFE_CarregarXML(String eArquivoOuXML);

    int NFE_CarregarINI(String eArquivoOuINI);

    int NFE_LimparLista();

    int NFE_CarregarEventoXML(String eArquivoOuXml);

    int NFE_CarregarEventoINI(String eArquivoOuIni);

    int NFE_LimparListaEventos();

    int NFE_Assinar();

    int NFE_Validar();

    int NFE_ValidarRegrasdeNegocios(ByteBuffer buffer, IntByReference bufferSize);

    int NFE_VerificarAssinatura(ByteBuffer buffer, IntByReference bufferSize);

    int NFE_StatusServico(ByteBuffer buffer, IntByReference bufferSize);

    int NFE_Consultar(String eChaveOuNFe, ByteBuffer buffer, IntByReference bufferSize);

    int NFE_Inutilizar(String ACNPJ, String AJustificativa, int Ano, int Modelo, int Serie,
            int NumeroInicial, int NumeroFinal, ByteBuffer buffer, IntByReference bufferSize);

    int NFE_Enviar(int ALote, boolean Imprimir, boolean sincrono, boolean zipado, ByteBuffer buffer, IntByReference bufferSize);

    int NFE_ConsultarRecibo(String aRecibo, ByteBuffer buffer, IntByReference bufferSize);
    
    int NFE_Cancelar(String eChave, String eJustificativa, String eCNPJ, int ALote,
            ByteBuffer buffer, IntByReference bufferSize);

    int NFE_EnviarEvento(int idLote, ByteBuffer buffer, IntByReference bufferSize);

    int NFE_DistribuicaoDFePorUltNSU(int AcUFAutor, String eCNPJCPF, ByteBuffer buffer,
            IntByReference bufferSize);

    int NFE_DistribuicaoDFePorNSU(int AcUFAutor, String eCNPJCPF, String eNSU,
            ByteBuffer buffer, IntByReference bufferSize);

    int NFE_DistribuicaoDFePorChave(int AcUFAutor, String eCNPJCPF, String echNFe,
            ByteBuffer buffer, IntByReference bufferSize);

    int NFE_EnviarEmail(String ePara, String eChaveNFe, boolean AEnviaPDF, String eAssunto,
            String eCC, String eAnexos, String eMensagem);

    int NFE_EnviarEmailEvento(String ePara, String eChaveEvento, String eChaveNFe,
            boolean AEnviaPDF, String eAssunto, String eCC, String eAnexos, String eMensagem);

    int NFE_Imprimir(String cImpressora, int nNumCopias, String cProtocolo,
            String bMostrarPreview, String cMarcaDagua, String bViaConsumidor, String bSimplificado);

    int NFE_ImprimirPDF();

    int NFE_ImprimirEvento(String eChaveNFe, String eChaveEvento);

    int NFE_ImprimirEventoPDF(String eChaveNFe, String eChaveEvento);

    int NFE_ImprimirInutilizacao(String eChave);

    int NFE_ImprimirInutilizacaoPDF(String eChave);
    
    public static int parseInt(ByteBuffer buffer, IntByReference len) {
        return parseInt(buffer, len, 0);
    }
    
    public static int parseInt(ByteBuffer buffer, IntByReference len, int defaultVal) {
        String s = fromUTF8(buffer, len);
        return s.matches("-?\\d+") ? Integer.parseInt(s) : defaultVal;   
    }
    
    public static boolean parseBoolean(ByteBuffer buffer, IntByReference len) {
        String s = fromUTF8(buffer, len);
        return s.equals("1");
    }
    
    public static String parseString(ByteBuffer buffer, IntByReference len){
        return fromUTF8(buffer, len);
    }
    
    public static String toUTF8(Boolean value) {
        return toUTF8(value ? "1" : "0");
    }
    
    public static String toUTF8(int value) {
        return toUTF8(Integer.toString(value));
    }
    
    public static String toUTF8(char[] value) {
        return toUTF8(new String(value));
    }
    
    public static String toUTF8(String value) {
        return new String(value.getBytes(UTF8));
    }

    public static String fromUTF8(ByteBuffer buffer, IntByReference len) {
        return new String(buffer.array(), 0, len.getValue(), UTF8);
    }
    
    public static void checkResult(int result) throws Exception {
        if (result == 0) {
            return;
        }

        ByteBuffer buffer = ByteBuffer.allocate(256);
        IntByReference bufferLen = new IntByReference(256);

        ACBrNFe.INSTANCE.NFE_UltimoRetorno(buffer, bufferLen);

        if (bufferLen.getValue() > 256) {
            buffer = ByteBuffer.allocate(bufferLen.getValue());
            ACBrNFe.INSTANCE.NFE_UltimoRetorno(buffer, bufferLen);
        }

        throw new Exception(fromUTF8(buffer, bufferLen));
    }
}
