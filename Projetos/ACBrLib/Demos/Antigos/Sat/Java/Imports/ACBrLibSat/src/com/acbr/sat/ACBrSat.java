package com.acbr.sat;

import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Platform;
import com.sun.jna.ptr.IntByReference;
import java.nio.ByteBuffer;
import java.nio.charset.Charset;

public interface ACBrSat extends Library {

    static Charset UTF8 = Charset.forName("UTF-8");
    static String JNA_LIBRARY_NAME = LibraryLoader.getLibraryName();
    public final static ACBrSat INSTANCE = LibraryLoader.getInstance();

    class LibraryLoader {

        private static String library = "";
        private static ACBrSat instance = null;

        public static String getLibraryName() {
            if (library.isEmpty()) {
                library = Platform.is64Bit() ? "ACBrSAT64" : "ACBrSAT32";
            }
            return library;
        }

        public static ACBrSat getInstance() {
            if (instance == null) {
                instance = (ACBrSat) Native.synchronizedLibrary((Library) Native.loadLibrary(JNA_LIBRARY_NAME, ACBrSat.class));
            }
            return instance;
        }
    }

    int SAT_Inicializar(String eArqConfig, String eChaveCrypt);

    int SAT_Finalizar();

    int SAT_Nome(ByteBuffer buffer, IntByReference bufferSize);

    int SAT_Versao(ByteBuffer buffer, IntByReference bufferSize);

    int SAT_UltimoRetorno(ByteBuffer buffer, IntByReference bufferSize);

    int SAT_ConfigLer(String eArqConfig);

    int SAT_ConfigGravar(String eArqConfig);

    int SAT_ConfigLerValor(String eSessao, String eChave, ByteBuffer buffer, IntByReference bufferSize);

    int SAT_ConfigGravarValor(String eSessao, String eChave, String valor);

    int SAT_InicializarSAT();

    int SAT_DesInicializar();

    int SAT_AssociarAssinatura(String CNPJValue, String assinaturaCNPJs, ByteBuffer buffer, IntByReference bufferSize);

    int SAT_BloquearSAT(ByteBuffer buffer, IntByReference bufferSize);

    int SAT_DesbloquearSAT(ByteBuffer buffer, IntByReference bufferSize);

    int SAT_TrocarCodigoDeAtivacao(String codigoDeAtivacaoOuEmergencia, int opcao, String novoCodigo, ByteBuffer buffer, IntByReference bufferSize);

    int SAT_ConsultarSAT(ByteBuffer buffer, IntByReference bufferSize);

    int SAT_ConsultarStatusOperacional(ByteBuffer buffer, IntByReference bufferSize);

    int SAT_ConsultarNumeroSessao(int cNumeroDeSessao, ByteBuffer buffer, IntByReference bufferSize);

    int SAT_AtualizarSoftwareSAT(ByteBuffer buffer, IntByReference bufferSize);

    int SAT_ComunicarCertificadoICPBRASIL(String certificado, ByteBuffer buffer, IntByReference bufferSize);

    int SAT_ExtrairLogs(String eArquivo);

    int SAT_TesteFimAFim(String eArquivoXmlVenda, ByteBuffer buffer, IntByReference bufferSize);

    int SAT_CriarCFe(String eArquivoIni, ByteBuffer buffer, IntByReference bufferSize);

    int SAT_CriarEnviarCFe(String eArquivoIni, ByteBuffer buffer, IntByReference bufferSize);

    int SAT_EnviarCFe(String eArquivoXml, ByteBuffer buffer, IntByReference bufferSize);

    int SAT_CancelarCFe(String eArquivoXml, ByteBuffer buffer, IntByReference bufferSize);

    int SAT_ImprimirExtratoVenda(String eArquivoXml, String eNomeImpressora);

    int SAT_ImprimirExtratoResumido(String eArquivoXml, String eNomeImpressora);

    int SAT_GerarPDFExtratoVenda(String eArquivoXml, ByteBuffer buffer, IntByReference bufferSize);

    int SAT_GerarImpressaoFiscalMFe(String eArquivoXml, String eNomeArquivo, ByteBuffer buffer, IntByReference bufferSize);

    int SAT_EnviarEmail(String eArquivoXml, String ePara, String eAssunto, String eNomeArquivo,
            String sMensagem, String sCC, String eAnexos);

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

        ACBrSat.INSTANCE.SAT_UltimoRetorno(buffer, bufferLen);

        if (bufferLen.getValue() > 256) {
            buffer = ByteBuffer.allocate(bufferLen.getValue());
            ACBrSat.INSTANCE.SAT_UltimoRetorno(buffer, bufferLen);
        }

        throw new Exception(fromUTF8(buffer, bufferLen.getValue()));
    }
}
