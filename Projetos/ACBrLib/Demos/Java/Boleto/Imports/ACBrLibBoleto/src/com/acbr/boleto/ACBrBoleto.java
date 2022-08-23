package com.acbr.boleto;

import com.acbr.ACBrLibBase;
import com.acbr.ACBrSessao;
import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Platform;
import com.sun.jna.ptr.IntByReference;
import java.io.File;
import java.nio.ByteBuffer;
import java.nio.file.Paths;

public final class ACBrBoleto extends ACBrLibBase {   
    
    private interface ACBrBoletoLib extends Library {
        static String JNA_LIBRARY_NAME = LibraryLoader.getLibraryName();
        public final static ACBrBoletoLib INSTANCE = LibraryLoader.getInstance();       

        class LibraryLoader {
            private static String library = "";
            private static ACBrBoletoLib instance = null;

            public static String getLibraryName() {
                if (library.isEmpty()) {
                    if(Platform.isWindows()){
                        library = Platform.is64Bit() ? "ACBrBoleto64" : "ACBrBoleto32";                        
                    }else{
                        library = Platform.is64Bit() ? "acbrboleto64" : "acbrboleto32";
                    }
                }
                return library;
            }           

            public static ACBrBoletoLib getInstance() {
                if (instance == null) {
                    instance = (ACBrBoletoLib) Native.synchronizedLibrary((Library) Native.load(JNA_LIBRARY_NAME, ACBrBoletoLib.class));
                }
                return instance;
            }
        }

        int Boleto_Inicializar(String eArqConfig, String eChaveCrypt);

        int Boleto_Finalizar();

        int Boleto_Nome(ByteBuffer buffer, IntByReference bufferSize);

        int Boleto_Versao(ByteBuffer buffer, IntByReference bufferSize);

        int Boleto_UltimoRetorno(ByteBuffer buffer, IntByReference bufferSize);

        int Boleto_ConfigImportar(String eArqConfig);
        
        int Boleto_ConfigExportar(ByteBuffer buffer, IntByReference bufferSize);
        
        int Boleto_ConfigLer(String eArqConfig);

        int Boleto_ConfigGravar(String eArqConfig);

        int Boleto_ConfigLerValor(String eSessao, String eChave, ByteBuffer buffer, IntByReference bufferSize);

        int Boleto_ConfigGravarValor(String eSessao, String eChave, String valor);

        int Boleto_ConfigurarDados(String eArquivoIni);

        int Boleto_IncluirTitulos(String eArquivoIni, String eTpSaida);

        int Boleto_LimparLista(); 

        int Boleto_TotalTitulosLista();

        int Boleto_Imprimir(String eNomeImpressora);
        
        int Boleto_ImprimirBoleto(int eIndice, String eNomeImpressora);

        int Boleto_GerarPDF();

        int Boleto_GerarHTML();

        int Boleto_GerarRemessa(String eDir, int eNumArquivo, String eNomeArquivo);

        int Boleto_LerRetorno(String eDir, String eNomeArq);
        
        int Boleto_ObterRetorno(String eDir, String eNomeArq, ByteBuffer buffer, IntByReference bufferSize);

        int Boleto_EnviarEmail(String ePara, String eAssunto, String eMensagem, String eCC);

        int Boleto_EnviarEmailBoleto(int eIndice, String ePara, String eAssunto, String eMensagem, String eCC);
        
        int Boleto_SetDiretorioArquivo(String eDir, String eArq);

        int Boleto_ListaBancos(ByteBuffer buffer, IntByReference bufferSize);

        int Boleto_ListaCaractTitulo(ByteBuffer buffer, IntByReference bufferSize);

        int Boleto_ListaOcorrencias(ByteBuffer buffer, IntByReference bufferSize);

        int Boleto_ListaOcorrenciasEX(ByteBuffer buffer, IntByReference bufferSize);

        int Boleto_TamNossoNumero(String eCarteira, String enossoNumero, String eConvenio);

        int Boleto_CodigosMoraAceitos(ByteBuffer buffer, IntByReference bufferSize);

        int Boleto_SelecionaBanco(String eCodBanco);

        int Boleto_MontarNossoNumero(int eIndice, ByteBuffer buffer, IntByReference bufferSize);

        int Boleto_RetornaLinhaDigitavel(int eIndice, ByteBuffer buffer, IntByReference bufferSize);

        int Boleto_RetornaCodigoBarras(int eIndice, ByteBuffer buffer, IntByReference bufferSize);
        
        int Boleto_EnviarBoleto(int eCodigoOperacao, ByteBuffer buffer, IntByReference bufferSize);
        
        int Boleto_ConsultarTitulosPorPeriodo(String eArquivoIni, ByteBuffer buffer, IntByReference bufferSize);
    }
    
    public ACBrBoleto() throws Exception {
        File iniFile = Paths.get(System.getProperty("user.dir"), "ACBrLib.ini").toFile();
        if (!iniFile.exists()) {
            iniFile.createNewFile();
        }

        int ret = ACBrBoletoLib.INSTANCE.Boleto_Inicializar(toUTF8(iniFile.getAbsolutePath()), toUTF8(""));
        checkResult(ret);
    }

    public ACBrBoleto(String eArqConfig, String eChaveCrypt) throws Exception {
        int ret = ACBrBoletoLib.INSTANCE.Boleto_Inicializar(toUTF8(eArqConfig), toUTF8(eChaveCrypt));
        checkResult(ret);
    }
    
    @Override
    protected void dispose() throws Exception {
        int ret = ACBrBoletoLib.INSTANCE.Boleto_Finalizar();
        checkResult(ret);
    }
    
    public String nome() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrBoletoLib.INSTANCE.Boleto_Nome(buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);
    }

    public String versao() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrBoletoLib.INSTANCE.Boleto_Versao(buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);
    }
    
     public void configLer() throws Exception {
         configLer("");
     }
    
    public void configLer(String eArqConfig) throws Exception {
        int ret = ACBrBoletoLib.INSTANCE.Boleto_ConfigLer(toUTF8(eArqConfig));
        checkResult(ret);
    }
    
    public void configGravar() throws Exception {
        configGravar("");
    }
    
    public void configGravar(String eArqConfig) throws Exception {
        int ret = ACBrBoletoLib.INSTANCE.Boleto_ConfigGravar(toUTF8(eArqConfig));
        checkResult(ret);
    }

    @Override
    public String configLerValor(ACBrSessao eSessao, String eChave) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrBoletoLib.INSTANCE.Boleto_ConfigLerValor(toUTF8(eSessao.name()), toUTF8(eChave), buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }

    @Override
    public void configGravarValor(ACBrSessao eSessao, String eChave, Object value) throws Exception {
        int ret = ACBrBoletoLib.INSTANCE.Boleto_ConfigGravarValor(toUTF8(eSessao.name()), toUTF8(eChave), toUTF8(value.toString()));
        checkResult(ret);
    }
    
    public void importarConfig(String eArqConfig) throws Exception {        
        int ret = ACBrBoletoLib.INSTANCE.Boleto_ConfigImportar(eArqConfig);
        checkResult(ret);        
    }
    
    public String exportarConfig() throws Exception {		
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrBoletoLib.INSTANCE.Boleto_ConfigExportar(buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());		
    }
    
    public void ConfigurarDados(String eArquivoIni) throws Exception {
        int ret = ACBrBoletoLib.INSTANCE.Boleto_ConfigurarDados(toUTF8(eArquivoIni));
        checkResult(ret);
    }
    
    public void IncluirTitulos(String eArquivoIni, String eTpSaida) throws Exception {
        int ret = ACBrBoletoLib.INSTANCE.Boleto_IncluirTitulos(toUTF8(eArquivoIni), toUTF8(eTpSaida));
        checkResult(ret);
    }
    
    public void LimparLista() throws Exception {
        int ret = ACBrBoletoLib.INSTANCE.Boleto_LimparLista();
        checkResult(ret);
    }
    
    public int TotalTitulosLista() throws Exception {
        int ret = ACBrBoletoLib.INSTANCE.Boleto_TotalTitulosLista();
        checkResult(ret);
        
        return ret;
    }
    
    public void Imprimir() throws Exception {
        Imprimir("");
    }
    
    public void Imprimir(String eNomeImpressora) throws Exception {
        int ret = ACBrBoletoLib.INSTANCE.Boleto_Imprimir(eNomeImpressora);
        checkResult(ret);
    }
    
    public void ImprimirBoleto(int Indice) throws Exception {
        ImprimirBoleto(Indice, "");
    }
    
    public void ImprimirBoleto(int Indice, String eNomeImpressora) throws Exception {
        int ret = ACBrBoletoLib.INSTANCE.Boleto_ImprimirBoleto(Indice, eNomeImpressora);
        checkResult(ret);
    }
    
    public void GerarPDF() throws Exception {
        int ret = ACBrBoletoLib.INSTANCE.Boleto_GerarPDF();
        checkResult(ret);
    }

    public void GerarHTML() throws Exception {
        int ret = ACBrBoletoLib.INSTANCE.Boleto_GerarHTML();
        checkResult(ret);
    }

    public void GerarRemessa(String eDir, int eNumArquivo, String eNomeArquivo) throws Exception {
        int ret = ACBrBoletoLib.INSTANCE.Boleto_GerarRemessa(eDir, eNumArquivo, eNomeArquivo);
        checkResult(ret);
    }
    
    public void LerRetorno(String eDir, String eNomeArq) throws Exception {
        int ret = ACBrBoletoLib.INSTANCE.Boleto_LerRetorno(eDir, eNomeArq);
        checkResult(ret);
    }

    public void EnviarEmail(String ePara, String eAssunto, String eMensagem, String eCC) throws Exception {
        int ret = ACBrBoletoLib.INSTANCE.Boleto_EnviarEmail(ePara, eAssunto, eMensagem, eCC);
        checkResult(ret);
    }
    
    public void EnviarEmailBoleto(int eIndice, String ePara, String eAssunto, String eMensagem, String eCC) throws Exception {
        int ret = ACBrBoletoLib.INSTANCE.Boleto_EnviarEmailBoleto(eIndice, ePara, eAssunto, eMensagem, eCC);
        checkResult(ret);
    }
    
    public void SetDiretorioArquivo(String eDir, String eArq) throws Exception {
        int ret = ACBrBoletoLib.INSTANCE.Boleto_SetDiretorioArquivo(eDir, eArq);
        checkResult(ret);
    }

    public String[] ListaBancos() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrBoletoLib.INSTANCE.Boleto_ListaBancos(buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen).split("|");
    }
    
    public String[] ListaCaractTitulo() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrBoletoLib.INSTANCE.Boleto_ListaCaractTitulo(buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen).split("|");
    }
    
    public String[] ListaOcorrencias() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrBoletoLib.INSTANCE.Boleto_ListaOcorrencias(buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen).split("|");
    }

    public String[] ListaOcorrenciasEX() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrBoletoLib.INSTANCE.Boleto_ListaOcorrenciasEX(buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen).split("|");
    }
    
    public int TamNossoNumero(String eCarteira, String enossoNumero, String eConvenio) throws Exception {
        int ret = ACBrBoletoLib.INSTANCE.Boleto_TamNossoNumero(eCarteira, enossoNumero, eConvenio);
        checkResult(ret);
        
        return ret;
    }
    
    public String CodigosMoraAceitos() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrBoletoLib.INSTANCE.Boleto_CodigosMoraAceitos(buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }
    
    public void SelecionaBanco(String eCodBanco) throws Exception {
        int ret = ACBrBoletoLib.INSTANCE.Boleto_SelecionaBanco(eCodBanco);
        checkResult(ret);
    }

    public String MontarNossoNumero(int eIndice) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrBoletoLib.INSTANCE.Boleto_MontarNossoNumero(eIndice, buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }

    public String RetornaLinhaDigitavel(int eIndice) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrBoletoLib.INSTANCE.Boleto_RetornaLinhaDigitavel(eIndice, buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }

    public String RetornaCodigoBarras(int eIndice) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrBoletoLib.INSTANCE.Boleto_RetornaCodigoBarras(eIndice, buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }

    public String EnviarBoleto(int eCodigoOperacao) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrBoletoLib.INSTANCE.Boleto_EnviarBoleto(eCodigoOperacao, buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }
    
    public String ConsultarTitulosPorPeriodo(String eArquivoIni) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrBoletoLib.INSTANCE.Boleto_ConsultarTitulosPorPeriodo(eArquivoIni, buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }
    
    @Override
    protected void UltimoRetorno(ByteBuffer buffer, IntByReference bufferLen) {
        ACBrBoletoLib.INSTANCE.Boleto_UltimoRetorno(buffer, bufferLen);
    }
}
