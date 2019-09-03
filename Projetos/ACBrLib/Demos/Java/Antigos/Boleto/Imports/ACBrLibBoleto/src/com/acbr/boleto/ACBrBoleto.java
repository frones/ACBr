package com.acbr.boleto;

import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Platform;
import com.sun.jna.ptr.IntByReference;
import java.nio.ByteBuffer;
import java.nio.charset.Charset;

public interface ACBrBoleto extends Library {
    
    static Charset UTF8 = Charset.forName("UTF-8");
    static String JNA_LIBRARY_NAME = LibraryLoader.getLibraryName();
    public final static ACBrBoleto INSTANCE = LibraryLoader.getInstance();
    
   static class LibraryLoader {

        private static String library = "";
        private static ACBrBoleto instance = null;

        public static String getLibraryName() {
            if (library.isEmpty()) {
                library = Platform.is64Bit() ? "ACBrBoleto64" : "ACBrBoleto32";
            }
            return library;
        }

        public static ACBrBoleto getInstance() {
            if (instance == null) {
                instance = (ACBrBoleto) Native.synchronizedLibrary((Library) Native.loadLibrary(JNA_LIBRARY_NAME, ACBrBoleto.class));
            }
            return instance;
        }
    }
   
    int Boleto_Inicializar(String eArqConfig, String eChaveCrypt);

    int Boleto_Finalizar();

    int Boleto_Nome(ByteBuffer buffer, IntByReference bufferSize);

    int Boleto_Versao(ByteBuffer buffer, IntByReference bufferSize);

    int Boleto_UltimoRetorno(ByteBuffer buffer, IntByReference bufferSize);

    int Boleto_ConfigLer(String eArqConfig);

    int Boleto_ConfigGravar(String eArqConfig);

    int Boleto_ConfigLerValor(String eSessao, String eChave, ByteBuffer buffer, IntByReference bufferSize);

    int Boleto_ConfigGravarValor(String eSessao, String eChave, String valor);
    
    int Boleto_ConfigurarDados(String eArquivoIni, ByteBuffer buffer, IntByReference bufferSize );
    
    int Boleto_IncluirTitulos(String eArquivoIni, String eTpSaida, ByteBuffer buffer, IntByReference bufferSize);
    
    int Boleto_LimparLista(); 
    
    int Boleto_TotalTitulosLista(ByteBuffer buffer, IntByReference bufferSize);
    
    int Boleto_Imprimir(String eNomeImpressora);
    
    int Boleto_GerarPDF();
    
    int Boleto_GerarHTML();
    
    int Boleto_GerarRemessa(String eDir, int eNumArquivo, String eNomeArquivo);
    
    int Boleto_LerRetorno(String eDir, String eNomeArq);
    
    int Boleto_EnviarEmail(String ePara, String eAssunto, String eMensagem, String eCC);
    
    int Boleto_SetDiretorioArquivo(String eDir, String eArq, ByteBuffer buffer, IntByReference bufferSize);
    
    int Boleto_ListaBancos(ByteBuffer buffer, IntByReference bufferSize);
    
    int Boleto_ListaCaractTitulo(ByteBuffer buffer, IntByReference bufferSize);
    
    int Boleto_ListaOcorrencias(ByteBuffer buffer, IntByReference bufferSize);
    
    int Boleto_ListaOcorrenciasEX(ByteBuffer buffer, IntByReference bufferSize);
    
    int Boleto_TamNossoNumero(String eCarteira, String enossoNumero, String eConvenio, ByteBuffer buffer, IntByReference bufferSize );
   
    int Boleto_CodigosMoraAceitos(ByteBuffer buffer, IntByReference bufferSize);
    
    int Boleto_SelecionaBanco(String eCodBanco, ByteBuffer buffer, IntByReference bufferSize);
    
    int Boleto_MontarNossoNumero(int eIndice, ByteBuffer buffer, IntByReference bufferSize);
    
    int Boleto_RetornaLinhaDigitavel(int eIndice, ByteBuffer buffer, IntByReference bufferSize);
    
    int Boleto_RetornaCodigoBarras(int eIndice, ByteBuffer buffer, IntByReference bufferSize);
   
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

        ACBrBoleto.INSTANCE.Boleto_UltimoRetorno(buffer, bufferLen);

        if (bufferLen.getValue() > 256) {
            buffer = ByteBuffer.allocate(bufferLen.getValue());
            ACBrBoleto.INSTANCE.Boleto_UltimoRetorno(buffer, bufferLen);
        }

        throw new Exception(fromUTF8(buffer, bufferLen.getValue()));
    }
}
