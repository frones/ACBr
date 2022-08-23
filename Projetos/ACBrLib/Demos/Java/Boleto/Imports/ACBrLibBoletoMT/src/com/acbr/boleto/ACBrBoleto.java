package com.acbr.boleto;

import com.acbr.ACBrLibBase;
import com.acbr.ACBrSessao;
import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Platform;
import com.sun.jna.Pointer;
import com.sun.jna.ptr.IntByReference;
import com.sun.jna.ptr.PointerByReference;
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

        int Boleto_Inicializar(PointerByReference libHandler, String eArqConfig, String eChaveCrypt);

        int Boleto_Finalizar(Pointer libHandler);

        int Boleto_Nome(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize);

        int Boleto_Versao(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize);

        int Boleto_UltimoRetorno(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize);

        int Boleto_ConfigImportar(Pointer libHandler, String eArqConfig);
        
	int Boleto_ConfigExportar(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize);
        
        int Boleto_ConfigLer(Pointer libHandler, String eArqConfig);

        int Boleto_ConfigGravar(Pointer libHandler, String eArqConfig);

        int Boleto_ConfigLerValor(Pointer libHandler, String eSessao, String eChave, ByteBuffer buffer, IntByReference bufferSize);

        int Boleto_ConfigGravarValor(Pointer libHandler, String eSessao, String eChave, String valor);

        int Boleto_ConfigurarDados(Pointer libHandler, String eArquivoIni);

        int Boleto_IncluirTitulos(Pointer libHandler, String eArquivoIni, String eTpSaida);

        int Boleto_LimparLista(Pointer libHandler); 

        int Boleto_TotalTitulosLista(Pointer libHandler);

        int Boleto_Imprimir(Pointer libHandler, String eNomeImpressora);
        
        int Boleto_ImprimirBoleto(Pointer libHandler, int eIndice, String eNomeImpressora);

        int Boleto_GerarPDF(Pointer libHandler);

        int Boleto_GerarHTML(Pointer libHandler);

        int Boleto_GerarRemessa(Pointer libHandler, String eDir, int eNumArquivo, String eNomeArquivo);

        int Boleto_LerRetorno(Pointer libHandler, String eDir, String eNomeArq);
        
        int Boleto_ObterRetorno(Pointer libHandler, String eDir, String eNomeArq, ByteBuffer buffer, IntByReference bufferSize);

        int Boleto_EnviarEmail(Pointer libHandler, String ePara, String eAssunto, String eMensagem, String eCC);

        int Boleto_EnviarEmailBoleto(Pointer libHandler, int eIndice, String ePara, String eAssunto, String eMensagem, String eCC);
        
        int Boleto_SetDiretorioArquivo(Pointer libHandler, String eDir, String eArq);

        int Boleto_ListaBancos(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize);

        int Boleto_ListaCaractTitulo(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize);

        int Boleto_ListaOcorrencias(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize);

        int Boleto_ListaOcorrenciasEX(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize);

        int Boleto_TamNossoNumero(Pointer libHandler, String eCarteira, String enossoNumero, String eConvenio);

        int Boleto_CodigosMoraAceitos(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize);

        int Boleto_SelecionaBanco(Pointer libHandler, String eCodBanco);

        int Boleto_MontarNossoNumero(Pointer libHandler, int eIndice, ByteBuffer buffer, IntByReference bufferSize);

        int Boleto_RetornaLinhaDigitavel(Pointer libHandler, int eIndice, ByteBuffer buffer, IntByReference bufferSize);

        int Boleto_RetornaCodigoBarras(Pointer libHandler, int eIndice, ByteBuffer buffer, IntByReference bufferSize);
        
        int Boleto_EnviarBoleto(Pointer libHandler, int eCodigoOperacao, ByteBuffer buffer, IntByReference bufferSize);
        
        int Boleto_ConsultarTitulosPorPeriodo(Pointer libHandler, String eArquivoIni, ByteBuffer buffer, IntByReference bufferSize);
        
    }
    
    public ACBrBoleto() throws Exception {
        File iniFile = Paths.get(System.getProperty("user.dir"), "ACBrLib.ini").toFile();
        if (!iniFile.exists()) {
            iniFile.createNewFile();
        }

        PointerByReference handle = new PointerByReference();
        int ret = ACBrBoletoLib.INSTANCE.Boleto_Inicializar(handle, toUTF8(iniFile.getAbsolutePath()), toUTF8(""));
        checkResult(ret);
		setHandle(handle.getValue());
    }

    public ACBrBoleto(String eArqConfig, String eChaveCrypt) throws Exception {
        PointerByReference handle = new PointerByReference();
        int ret = ACBrBoletoLib.INSTANCE.Boleto_Inicializar(handle, toUTF8(eArqConfig), toUTF8(eChaveCrypt));
        checkResult(ret);
		setHandle(handle.getValue());
    }
    
    @Override
    protected void dispose() throws Exception {
        int ret = ACBrBoletoLib.INSTANCE.Boleto_Finalizar(getHandle());
        checkResult(ret);
    }
    
    public String nome() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrBoletoLib.INSTANCE.Boleto_Nome(getHandle(), buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);
    }

    public String versao() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrBoletoLib.INSTANCE.Boleto_Versao(getHandle(), buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);
    }

    public void configLer() throws Exception {
        configLer("");
    }

    public void configLer(String eArqConfig) throws Exception {
        int ret = ACBrBoletoLib.INSTANCE.Boleto_ConfigLer(getHandle(), toUTF8(eArqConfig));
        checkResult(ret);
    }

    public void configGravar() throws Exception {
        configGravar("");
    }

    public void configGravar(String eArqConfig) throws Exception {
        int ret = ACBrBoletoLib.INSTANCE.Boleto_ConfigGravar(getHandle(), toUTF8(eArqConfig));
        checkResult(ret);
    }

    @Override
    public String configLerValor(ACBrSessao eSessao, String eChave) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrBoletoLib.INSTANCE.Boleto_ConfigLerValor(getHandle(), toUTF8(eSessao.name()), toUTF8(eChave), buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }

    @Override
    public void configGravarValor(ACBrSessao eSessao, String eChave, Object value) throws Exception {
        int ret = ACBrBoletoLib.INSTANCE.Boleto_ConfigGravarValor(getHandle(), toUTF8(eSessao.name()), toUTF8(eChave), toUTF8(value.toString()));
        checkResult(ret);
    }
    
    public void ConfigImportar(String eArqConfig) throws Exception {
        
        int ret = ACBrBoletoLib.INSTANCE.Boleto_ConfigImportar(getHandle(), eArqConfig);
        checkResult(ret);
        
    }
    
    public String ConfigExportar() throws Exception {		
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrBoletoLib.INSTANCE.Boleto_ConfigExportar(getHandle(), buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);		
    }
    
    public void ConfigurarDados(String eArquivoIni) throws Exception {

        int ret = ACBrBoletoLib.INSTANCE.Boleto_ConfigurarDados(getHandle(), toUTF8(eArquivoIni));
        checkResult(ret);
    }
    
    public void IncluirTitulos(String eArquivoIni, String eTpSaida) throws Exception {
        int ret = ACBrBoletoLib.INSTANCE.Boleto_IncluirTitulos(getHandle(), toUTF8(eArquivoIni), toUTF8(eTpSaida));
        checkResult(ret);
    }
    
    public void LimparLista() throws Exception {
        int ret = ACBrBoletoLib.INSTANCE.Boleto_LimparLista(getHandle());
        checkResult(ret);
    }
    
    public int TotalTitulosLista() throws Exception {

        int ret = ACBrBoletoLib.INSTANCE.Boleto_TotalTitulosLista(getHandle());
        checkResult(ret);
        
        return ret;
    }
    
    public void Imprimir() throws Exception {
        Imprimir("");
    }
    
    public void Imprimir(String eNomeImpressora) throws Exception {
        int ret = ACBrBoletoLib.INSTANCE.Boleto_Imprimir(getHandle(), eNomeImpressora);
        checkResult(ret);
    }
    
    public void ImprimirBoleto(int Indice) throws Exception {
        ImprimirBoleto(Indice, "");
    }
    
    public void ImprimirBoleto(int Indice, String eNomeImpressora) throws Exception {
        int ret = ACBrBoletoLib.INSTANCE.Boleto_ImprimirBoleto(getHandle(), Indice, eNomeImpressora);
        checkResult(ret);
    }
    
    public void GerarPDF() throws Exception {
        int ret = ACBrBoletoLib.INSTANCE.Boleto_GerarPDF(getHandle());
        checkResult(ret);
    }

    public void GerarHTML() throws Exception {
        int ret = ACBrBoletoLib.INSTANCE.Boleto_GerarHTML(getHandle());
        checkResult(ret);
    }

    public void GerarRemessa(String eDir, int eNumArquivo, String eNomeArquivo) throws Exception {
        int ret = ACBrBoletoLib.INSTANCE.Boleto_GerarRemessa(getHandle(), eDir, eNumArquivo, eNomeArquivo);
        checkResult(ret);
    }
    
    public void LerRetorno(String eDir, String eNomeArq) throws Exception {
        int ret = ACBrBoletoLib.INSTANCE.Boleto_LerRetorno(getHandle(), eDir, eNomeArq);
        checkResult(ret);
    }
    
    public String ObterRetorno(String eDir, String eNomeArq) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrBoletoLib.INSTANCE.Boleto_ObterRetorno(getHandle(), eDir, eNomeArq, buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }

    public void EnviarEmail(String ePara, String eAssunto, String eMensagem, String eCC) throws Exception {
        int ret = ACBrBoletoLib.INSTANCE.Boleto_EnviarEmail(getHandle(), ePara, eAssunto, eMensagem, eCC);
        checkResult(ret);
    }
    
    public void EnviarEmailBoleto(int eIndice, String ePara, String eAssunto, String eMensagem, String eCC ) throws Exception {
        int ret = ACBrBoletoLib.INSTANCE.Boleto_EnviarEmailBoleto(getHandle(),eIndice, ePara, eAssunto, eMensagem, eCC);
        checkResult(ret);
    }
    
    public void SetDiretorioArquivo(String eDir, String eArq) throws Exception {
        int ret = ACBrBoletoLib.INSTANCE.Boleto_SetDiretorioArquivo(getHandle(), eDir, eArq);
        checkResult(ret);
    }

    public String[] ListaBancos() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrBoletoLib.INSTANCE.Boleto_ListaBancos(getHandle(), buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen).split("|");
    }
    
    public String[] ListaCaractTitulo() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrBoletoLib.INSTANCE.Boleto_ListaCaractTitulo(getHandle(), buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen).split("|");
    }
    
    public String ListaOcorrencias() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrBoletoLib.INSTANCE.Boleto_ListaOcorrencias(getHandle(), buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }

    public String[] ListaOcorrenciasEX() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrBoletoLib.INSTANCE.Boleto_ListaOcorrenciasEX(getHandle(), buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen).split("|");
    }
    
    public int TamNossoNumero(String eCarteira, String enossoNumero, String eConvenio) throws Exception {
        int ret = ACBrBoletoLib.INSTANCE.Boleto_TamNossoNumero(getHandle(), eCarteira, enossoNumero, eConvenio);
        checkResult(ret);
        
        return ret;
    }
    
    public String CodigosMoraAceitos() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrBoletoLib.INSTANCE.Boleto_CodigosMoraAceitos(getHandle(), buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }
    
    public void SelecionaBanco(String eCodBanco) throws Exception {
        int ret = ACBrBoletoLib.INSTANCE.Boleto_SelecionaBanco(getHandle(), eCodBanco);
        checkResult(ret);
    }

    public String MontarNossoNumero(int eIndice) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrBoletoLib.INSTANCE.Boleto_MontarNossoNumero(getHandle(), eIndice, buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }

    public String RetornaLinhaDigitavel(int eIndice) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrBoletoLib.INSTANCE.Boleto_RetornaLinhaDigitavel(getHandle(), eIndice, buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }

    public String RetornaCodigoBarras(int eIndice) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrBoletoLib.INSTANCE.Boleto_RetornaCodigoBarras(getHandle(), eIndice, buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }    
    
    public String EnviarBoleto(int eCodigoOperacao) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrBoletoLib.INSTANCE.Boleto_EnviarBoleto(getHandle(), eCodigoOperacao, buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }
    
    public String ConsultarTitulosPorPeriodo(String eArquivoIni) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrBoletoLib.INSTANCE.Boleto_ConsultarTitulosPorPeriodo(getHandle(), eArquivoIni, buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }
    
    @Override
    protected void UltimoRetorno(ByteBuffer buffer, IntByReference bufferLen) {
        ACBrBoletoLib.INSTANCE.Boleto_UltimoRetorno(getHandle(), buffer, bufferLen);
    }
}
