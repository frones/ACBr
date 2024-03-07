package com.acbr.nfse;

import com.acbr.ACBrLibBase;
import com.acbr.ACBrSessao;
import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Platform;
import com.sun.jna.ptr.IntByReference;

import java.io.File;
import java.nio.ByteBuffer;
import java.nio.file.Paths;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;

public final class ACBrNFSe extends ACBrLibBase {

    private interface ACBrNFSeLib extends Library {

        static String JNA_LIBRARY_NAME = LibraryLoader.getLibraryName();
        public final static ACBrNFSeLib INSTANCE = LibraryLoader.getInstance();

        class LibraryLoader {

            private static String library = "";
            private static ACBrNFSeLib instance = null;

            private static String getLibraryName() {
                if (library.isEmpty()) {
                    if (Platform.isWindows()) {
                        library = Platform.is64Bit() ? "ACBrNFSe64" : "ACBrNFSe32";
                    } else {
                        library = Platform.is64Bit() ? "acbrnfse64" : "acbrnfse32";
                    }
                }
                return library;
            }

            public static ACBrNFSeLib getInstance() {
                if (instance == null) {
                    instance = (ACBrNFSeLib) Native.synchronizedLibrary(
                            (Library) Native.load(JNA_LIBRARY_NAME, ACBrNFSeLib.class));
                }

                return instance;
            }
        }

        int NFSE_Inicializar(String eArqConfig, String eChaveCrypt);

        int NFSE_Finalizar();

        int NFSE_Nome(ByteBuffer buffer, IntByReference bufferSize);

        int NFSE_Versao(ByteBuffer buffer, IntByReference bufferSize);

        int NFSE_UltimoRetorno(ByteBuffer buffer, IntByReference bufferSize);

        int NFSE_ConfigImportar(String eArqConfig);

        int NFSE_ConfigExportar(ByteBuffer buffer, IntByReference bufferSize);

        int NFSE_ConfigLer(String eArqConfig);

        int NFSE_ConfigGravar(String eArqConfig);

        int NFSE_ConfigLerValor(String eSessao, String eChave, ByteBuffer buffer, IntByReference bufferSize);

        int NFSE_ConfigGravarValor(String eSessao, String eChave, String valor);

        int NFSE_ConfigGravarValor(String eArquivoOuXML);
        
        int NFSE_CarregarXML(String eArquivoOuXml);

        int NFSE_CarregarINI(String eArquivoOuINI);

        int NFSE_ObterXml(Integer AIndex, ByteBuffer buffer, IntByReference bufferSize);

        int NFSE_GravarXml(Integer AIndex, String eNomeArquivo, String ePathArquivo);

        int NFSE_ObterIni(Integer AIndex, ByteBuffer buffer, IntByReference bufferSize);

        int NFSE_GravarIni(Integer AIndex, String eNomeArquivo, String ePathArquivo);

        int NFSE_LimparLista();
        
        int NFSE_ObterCertificados(ByteBuffer buffer, IntByReference bufferSize);
        
        int NFSE_Emitir(String aLote, Integer aModoEnvio, boolean aImprimir, ByteBuffer buffer, IntByReference bufferSize);
        
        int NFSE_Cancelar(String aInfCancelamento, ByteBuffer buffer, IntByReference bufferSize);
        
        int NFSE_SubstituirNFSe(String aNumeroNFSe, String aSerieNFSe, String aCodigoCancelamento, String aMotivoCancelamento, String aNumeroLote, String aCodigoVerificacao, ByteBuffer buffer, IntByReference bufferSize);
        
        int NFSE_LinkNFSE(String aNumeroNFSe, String aCodigoVerificacao, String aChaveAcesso, String aValorServico, ByteBuffer buffer, IntByReference bufferSize);
        
        int NFSE_GerarLote(String aLote, Integer aQtdMaximaRps, Integer aModoEnvio, ByteBuffer buffer, IntByReference bufferSize);
        
        int NFSE_GerarToken(ByteBuffer buffer, IntByReference bufferSize);
        
        int NFSE_ConsultarSituacao(String aProtocolo, String aNumeroLote, ByteBuffer buffer, IntByReference bufferSize);
        
        int NFSE_ConsultarLoteRps(String aProtocolo, String aNumLote, ByteBuffer buffer, IntByReference bufferSize);
        
        int NFSE_ConsultarNFSePorRps(String aNumeroRps, String aSerie, String aTipo, String aCodigoVerificacao, ByteBuffer buffer, IntByReference bufferSize);
        
        int NFSE_ConsultarNFSePorNumero(String aNumero, Integer aPagina, ByteBuffer buffer, IntByReference bufferSize);
        
        int NFSE_ConsultarNFSePorPeriodo(Date aDataInicial, Date aDataFinal, Integer aPagina, String aNumeroLote, Integer aTipoPeriodo, ByteBuffer buffer, IntByReference bufferSize);
        
        int NFSE_ConsultarNFSePorFaixa(String aNumeroInicial, String aNumeroFinal, Integer aPagina, ByteBuffer buffer, IntByReference bufferSize);
        
        int NFSE_ConsultarNFSeGenerico(String aInfConsultaNFSe, ByteBuffer buffer, IntByReference bufferSize);
        
        int NFSE_ConsultarLinkNFSe(String aInfConsultaLinkNFSe, ByteBuffer buffer, IntByReference bufferSize);
        
        int NFSE_EnviarEmail(String ePara, String eXmlNFSe, boolean aEnviaPDF, String eAssunto, String eCc, String eAnexos, String eMensagem);
        
        int NFSE_Imprimir(String cImpressora, Integer nNumCopias, String bGerarPDF, String bMostrarPreview, String cCancelada);
        
        int NFSE_ImprimirPDF();
        
        int NFSE_SalvarPDF(ByteBuffer buffer, IntByReference bufferSize);
        
        int NFSE_ConsultarNFSeServicoPrestadoPorNumero(String aNumero, Integer aPagina, Date aDataInicial, Date aDataFinal, Integer aTipoPeriodo, ByteBuffer buffer, IntByReference bufferSize);
        
        int NFSE_ConsultarNFSeServicoPrestadoPorPeriodo(Date aDataInicial, Date aDataFinal, Integer aPagina, Integer aTipoPeriodo, ByteBuffer buffer, IntByReference bufferSize);
        
        int NFSE_ConsultarNFSeServicoPrestadoPorTomador(String aCNPJ, String aInscMun, Integer aPagina, Date aDataInicial, Date aDataFinal, Integer aTipoPeriodo, ByteBuffer buffer, IntByReference bufferSize);
        
        int NFSE_ConsultarNFSeServicoPrestadoPorIntermediario(String aCNPJ, String aInscMun, Integer aPagina, Date aDataInicial, Date aDataFinal, Integer aTipoPeriodo, ByteBuffer buffer, IntByReference bufferSize);
        
        int NFSE_ConsultarNFSeServicoTomadoPorNumero(String aNumero, Integer aPagina, Date aDataInicial, Date aDataFinal, Integer aTipoPeriodo, ByteBuffer buffer, IntByReference bufferSize);
        
        int NFSE_ConsultarNFSeServicoTomadoPorPrestador(String aCNPJ, String aInscMun, Integer aPagina, Date aDataInicial, Date aDataFinal, Integer aTipoPeriodo, ByteBuffer buffer, IntByReference bufferSize);
        
        int NFSE_ConsultarNFSeServicoTomadoPorTomador(String aCNPJ, String aInscMun, Integer aPagina, Date aDataInicial, Date aDataFinal, Integer aTipoPeriodo, ByteBuffer buffer, IntByReference bufferSize);
        
        int NFSE_ConsultarNFSeServicoTomadoPorPeriodo(Date aDataInicial, Date aDataFinal, Integer aPagina, Integer aTipoPeriodo, ByteBuffer buffer, IntByReference bufferSize);
        
        int NFSE_ConsultarNFSeServicoTomadoPorIntermediario(String aCNPJ, String aInscMun, Integer aPagina, Date aDataInicial, Date aDataFinal, Integer aTipoPeriodo, ByteBuffer buffer, IntByReference bufferSize);
        
        int NFSE_EnviarEvento(String aInfEvento, ByteBuffer buffer, IntByReference bufferSize);
        
        int NFSE_ConsultarDPSPorChave(String aChaveDPS, ByteBuffer buffer, IntByReference bufferSize);
        
        int NFSE_ConsultarNFSePorChave(String aChaveNFSe, ByteBuffer buffer, IntByReference bufferSize);
        
        int NFSE_ConsultarEvento(String aChave, Integer aTipoEvento, Integer aNumSeq, ByteBuffer buffer, IntByReference bufferSize);
        
        int NFSE_ConsultarDFe(Integer aNSU, ByteBuffer buffer, IntByReference bufferSize);
        
        int NFSE_ObterDANFSE(String aChaveNFSe, ByteBuffer buffer, IntByReference bufferSize);
        
        int NFSE_ConsultarParametros(Integer aTipoParametroMunicipio, String aCodigoServico, Date aCompetencia, String aNumeroBeneficio, ByteBuffer buffer, IntByReference bufferSize);
        
    }

    public ACBrNFSe() throws Exception {
        File iniFile = Paths.get(System.getProperty("user.dir"), "ACBrLib.ini").toFile();
        if (!iniFile.exists()) {
            iniFile.createNewFile();
        }

        int ret = ACBrNFSeLib.INSTANCE.NFSE_Inicializar(toUTF8(iniFile.getAbsolutePath()), toUTF8(""));
        checkResult(ret);
    }

    public ACBrNFSe(String eArqConfig, String eChaveCrypt) throws Exception {
        int ret = ACBrNFSeLib.INSTANCE.NFSE_Inicializar(toUTF8(eArqConfig), toUTF8(eChaveCrypt));
        checkResult(ret);
    }

    @Override
    protected void dispose() throws Exception {
        int ret = ACBrNFSeLib.INSTANCE.NFSE_Finalizar();
        checkResult(ret);
    }

    public String nome() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrNFSeLib.INSTANCE.NFSE_Nome(buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());
    }

    public String versao() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrNFSeLib.INSTANCE.NFSE_Versao(buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());
    }

    public void configLer() throws Exception {
        configLer("");
    }

    public void configLer(String eArqConfig) throws Exception {
        int ret = ACBrNFSeLib.INSTANCE.NFSE_ConfigLer(toUTF8(eArqConfig));
        checkResult(ret);
    }

    public void configGravar() throws Exception {
        configGravar("");
    }

    public void configGravar(String eArqConfig) throws Exception {
        int ret = ACBrNFSeLib.INSTANCE.NFSE_ConfigGravar(toUTF8(eArqConfig));
        checkResult(ret);
    }

    @Override
    public String configLerValor(ACBrSessao eSessao, String eChave) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrNFSeLib.INSTANCE.NFSE_ConfigLerValor(toUTF8(eSessao.name()), toUTF8(eChave), buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);
    }

    @Override
    public void configGravarValor(ACBrSessao eSessao, String eChave, Object value) throws Exception {
        int ret = ACBrNFSeLib.INSTANCE.NFSE_ConfigGravarValor(toUTF8(eSessao.name()), toUTF8(eChave), toUTF8(value.toString()));
        checkResult(ret);
    }

    public void carregarXml(String eArquivoOuXML) throws Exception {
        int ret = ACBrNFSeLib.INSTANCE.NFSE_CarregarXML(toUTF8(eArquivoOuXML));
        checkResult(ret);
    }

    public String obterXml(int AIndex) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        int ret = ACBrNFSeLib.INSTANCE.NFSE_ObterXml(AIndex, buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);
    }

    public void gravarXml(int AIndex) throws Exception {
        gravarXml(AIndex, "", "");
    }

    public void gravarXml(int AIndex, String eNomeArquivo) throws Exception {
        gravarXml(AIndex, eNomeArquivo, "");
    }

    public void gravarXml(int AIndex, String eNomeArquivo, String ePathArquivo) throws Exception {
        int ret = ACBrNFSeLib.INSTANCE.NFSE_GravarXml(AIndex, toUTF8(eNomeArquivo), toUTF8(ePathArquivo));
        checkResult(ret);
    }

    public String obterIni(int AIndex) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        int ret = ACBrNFSeLib.INSTANCE.NFSE_ObterIni(AIndex, buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);
    }

    public void gravarIni(int AIndex) throws Exception {
        gravarIni(AIndex, "", "");
    }

    public void gravarIni(int AIndex, String eNomeArquivo) throws Exception {
        gravarIni(AIndex, eNomeArquivo, "");
    }

    public void gravarIni(int AIndex, String eNomeArquivo, String ePathArquivo) throws Exception {
        int ret = ACBrNFSeLib.INSTANCE.NFSE_GravarIni(AIndex, toUTF8(eNomeArquivo), toUTF8(ePathArquivo));
        checkResult(ret);
    }

    public void carregarIni(String eArquivoOuIni) throws Exception {
        int ret = ACBrNFSeLib.INSTANCE.NFSE_CarregarINI(toUTF8(eArquivoOuIni));
        checkResult(ret);
    }

    public void limparLista() throws Exception {
        int ret = ACBrNFSeLib.INSTANCE.NFSE_LimparLista();
        checkResult(ret);
    }
    
    public String obterCertificados() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrNFSeLib.INSTANCE.NFSE_ObterCertificados(buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
    
    public String emitir(String aLote, Integer aModoEnvio, boolean aImprimir) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrNFSeLib.INSTANCE.NFSE_Emitir(toUTF8(aLote), aModoEnvio, aImprimir, buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
    
    public String cancelar(String aInfCancelamento) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrNFSeLib.INSTANCE.NFSE_Cancelar(toUTF8(aInfCancelamento), buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
    
    public String substituirNFSe(String aNumeroNFSe, String aSerieNFSe, String aCodigoCancelamento, String aMotivoCancelamento, String aNumeroLote, String aCodigoVerificacao) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrNFSeLib.INSTANCE.NFSE_SubstituirNFSe(toUTF8(aNumeroLote), toUTF8(aNumeroLote), toUTF8(aNumeroLote), toUTF8(aNumeroLote), toUTF8(aNumeroLote), toUTF8(aCodigoVerificacao), buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }

    public String linkNFSE(String aNumeroNFSe, String aCodigoVerificacao, String aChaveAcesso, String aValorServico) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrNFSeLib.INSTANCE.NFSE_LinkNFSE(toUTF8(aNumeroNFSe), toUTF8(aCodigoVerificacao), toUTF8(aChaveAcesso), toUTF8(aValorServico), buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
    
    public String gerarLote(String aLote, Integer aQtdMaximaRps, Integer aModoEnvio) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrNFSeLib.INSTANCE.NFSE_GerarLote(toUTF8(aLote), aQtdMaximaRps, aModoEnvio, buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }

    public String gerarToken() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrNFSeLib.INSTANCE.NFSE_GerarToken(buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
    
    public String consultarSituacao(String aProtocolo, String aNumeroLote) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrNFSeLib.INSTANCE.NFSE_ConsultarSituacao(toUTF8(aProtocolo), toUTF8(aNumeroLote), buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
   
    public String consultarLoteRps(String aProtocolo, String aNumLote) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrNFSeLib.INSTANCE.NFSE_ConsultarLoteRps(toUTF8(aProtocolo), toUTF8(aNumLote), buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
    
    public String consultarNFSePorRps(String aNumeroRps, String aSerie, String aTipo, String aCodigoVerificacao) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrNFSeLib.INSTANCE.NFSE_ConsultarNFSePorRps(toUTF8(aNumeroRps), toUTF8(aSerie), toUTF8(aTipo), toUTF8(aCodigoVerificacao), buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
    
    public String consultarNFSePorNumero(String aNumero, Integer aPagina) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrNFSeLib.INSTANCE.NFSE_ConsultarNFSePorNumero(toUTF8(aNumero), aPagina, buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
    
    public String consultarNFSePorPeriodo(Date aDataInicial, Date aDataFinal, Integer aPagina, String aNumeroLote, Integer aTipoPeriodo) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
                
        int ret = ACBrNFSeLib.INSTANCE.NFSE_ConsultarNFSePorPeriodo(aDataInicial, aDataFinal, aPagina, toUTF8(aNumeroLote), aTipoPeriodo, buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
    
    public String consultarNFSePorFaixa(String aNumeroInicial, String aNumeroFinal, Integer aPagina) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrNFSeLib.INSTANCE.NFSE_ConsultarNFSePorFaixa(toUTF8(aNumeroInicial), toUTF8(aNumeroFinal), aPagina, buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);  
    }
        
    public String consultarNFSeGenerico(String aInfConsultaNFSe) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrNFSeLib.INSTANCE.NFSE_ConsultarNFSeGenerico(toUTF8(aInfConsultaNFSe), buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
    
    public String consultarLinkNFSe(String aInfConsultaLinkNFSe) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(8192);
        IntByReference bufferLen = new IntByReference(8192);
        
        int ret = ACBrNFSeLib.INSTANCE.NFSE_ConsultarLinkNFSe(toUTF8(aInfConsultaLinkNFSe), buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
        
    public void enviarEmail(String ePara, String eXmlNFSe, boolean aEnviaPDF, String eAssunto) throws Exception {
        enviarEmail(ePara, eXmlNFSe, aEnviaPDF, eAssunto, "", "", "");
    }
        
    public void enviarEmail(String ePara, String eXmlNFSe, boolean aEnviarPDF, String eAssunto, String eCc, String eAnexos, String eMensagem ) throws Exception {
        int ret = ACBrNFSeLib.INSTANCE.NFSE_EnviarEmail(ePara, eXmlNFSe, aEnviarPDF, eAssunto, eCc, eAnexos, eMensagem);
        checkResult(ret);
    }
    
    public void imprimir () throws Exception {
        imprimir("", 1, null, null, "");
    }
        
    public void imprimir(String cImpressora, Integer nNumCopias, Boolean bGerarPDF, Boolean bMostrarPreview, String cCancelada) throws Exception {
        
        String gerarPDF = bGerarPDF != null ? bGerarPDF ? "1" : "0" : "";
        String mostrarPreview = bMostrarPreview != null ? bMostrarPreview ? "1" : "0" : "";
        
        int ret = ACBrNFSeLib.INSTANCE.NFSE_Imprimir(cImpressora, nNumCopias, gerarPDF, mostrarPreview, cCancelada);
        checkResult(ret);
    }
    
    public void imprimirPDF() throws Exception {
        int ret = ACBrNFSeLib.INSTANCE.NFSE_ImprimirPDF();
        checkResult(ret);
    }
    
    public String salvarPDF() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrNFSeLib.INSTANCE.NFSE_SalvarPDF(buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
    
    public String consultarNFSeServicoPrestadoPorNumero(String aNumero, Integer aPagina, Date aDataInicial, Date aDataFinal, Integer aTipoPeriodo) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrNFSeLib.INSTANCE.NFSE_ConsultarNFSeServicoPrestadoPorNumero(toUTF8(aNumero), aPagina, aDataInicial, aDataFinal, aTipoPeriodo, buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
    
    public String consultarNFSeServicoPrestadoPorPeriodo(Date aDataInicial, Date aDataFinal, Integer aPagina, Integer aTipoPeriodo) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrNFSeLib.INSTANCE.NFSE_ConsultarNFSeServicoPrestadoPorPeriodo(aDataInicial, aDataFinal, aPagina, aTipoPeriodo, buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
    
    public String consultarNFSeServicoPrestadoPorTomador(String aCNPJ, String aInscMun, Integer aPagina, Date aDataInicial, Date aDataFinal, Integer aTipoPeriodo) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrNFSeLib.INSTANCE.NFSE_ConsultarNFSeServicoPrestadoPorTomador(toUTF8(aCNPJ), toUTF8(aInscMun), aPagina, aDataInicial, aDataFinal, aTipoPeriodo, buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
        
    public String consultarNFSeServicoPrestadoPorIntermediario(String aCNPJ, String aInscMun, Integer aPagina, Date aDataInicial, Date aDataFinal, Integer aTipoPeriodo) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrNFSeLib.INSTANCE.NFSE_ConsultarNFSeServicoPrestadoPorIntermediario(toUTF8(aCNPJ), toUTF8(aInscMun), aPagina, aDataInicial, aDataFinal, aTipoPeriodo, buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
        
    public String consultarNFSeServicoTomadoPorNumero(String aNumero, Integer aPagina, Date aDataInicial, Date aDataFinal, Integer aTipoPeriodo) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrNFSeLib.INSTANCE.NFSE_ConsultarNFSeServicoTomadoPorNumero(toUTF8(aNumero), aPagina, aDataInicial, aDataFinal, aTipoPeriodo, buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
        
    public String consultarNFSeServicoTomadoPorPrestador(String aCNPJ, String aInscMun, Integer aPagina, Date aDataInicial, Date aDataFinal, Integer aTipoPeriodo) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrNFSeLib.INSTANCE.NFSE_ConsultarNFSeServicoTomadoPorPrestador(toUTF8(aCNPJ), toUTF8(aInscMun), aPagina, aDataInicial, aDataFinal, aTipoPeriodo, buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
        
    public String consultarNFSeServicoTomadoPorTomador(String aCNPJ, String aInscMun, Integer aPagina, Date aDataInicial, Date aDataFinal, Integer aTipoPeriodo) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrNFSeLib.INSTANCE.NFSE_ConsultarNFSeServicoTomadoPorTomador(toUTF8(aCNPJ), toUTF8(aInscMun), aPagina, aDataInicial, aDataFinal, aTipoPeriodo, buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
        
    public String consultarNFSeServicoTomadoPorPeriodo(Date aDataInicial, Date aDataFinal, Integer aPagina, Integer aTipoPeriodo) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrNFSeLib.INSTANCE.NFSE_ConsultarNFSeServicoTomadoPorPeriodo(aDataInicial, aDataFinal, aPagina, aTipoPeriodo, buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
        
    public String consultarNFSeServicoTomadoPorIntermediario(String aCNPJ, String aInscMun, Integer aPagina, Date aDataInicial, Date aDataFinal, Integer aTipoPeriodo) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrNFSeLib.INSTANCE.NFSE_ConsultarNFSeServicoTomadoPorIntermediario(toUTF8(aCNPJ), toUTF8(aInscMun), aPagina, aDataInicial, aDataFinal, aTipoPeriodo, buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
    
    public String enviarEvento(String aInfEvento) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrNFSeLib.INSTANCE.NFSE_EnviarEvento(toUTF8(aInfEvento), buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
    
    public String consultarDPSPorChave(String aChaveDPS) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrNFSeLib.INSTANCE.NFSE_ConsultarDPSPorChave(toUTF8(aChaveDPS), buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
    
    public String consultarNFSePorChave(String aChaveNFSe) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrNFSeLib.INSTANCE.NFSE_ConsultarNFSePorChave(toUTF8(aChaveNFSe), buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
    
    public String consultarEvento(String aChave, Integer aTipoEvento, Integer aNumSeq) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrNFSeLib.INSTANCE.NFSE_ConsultarEvento(toUTF8(aChave), aTipoEvento, aNumSeq, buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
    
    public String consultarDFe(Integer aNSU) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrNFSeLib.INSTANCE.NFSE_ConsultarDFe(aNSU, buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
    
    public String obterDANFSE(String aChaveNFSe) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrNFSeLib.INSTANCE.NFSE_ObterDANFSE(toUTF8(aChaveNFSe), buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
    
    public String consultarParametros(Integer aTipoParametroMunicipio, String aCodigoServico, Date aCompetencia, String aNumeroBeneficio) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrNFSeLib.INSTANCE.NFSE_ConsultarParametros(aTipoParametroMunicipio, toUTF8(aCodigoServico), aCompetencia, toUTF8(aNumeroBeneficio), buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
    
    public void ConfigImportar(String eArqConfig) throws Exception {

        int ret = ACBrNFSeLib.INSTANCE.NFSE_ConfigImportar(eArqConfig);
        checkResult(ret);

    }

    public String ConfigExportar() throws Exception {

        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrNFSeLib.INSTANCE.NFSE_ConfigExportar(buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());

    }

    @Override
    protected void UltimoRetorno(ByteBuffer buffer, IntByReference bufferLen) {
        ACBrNFSeLib.INSTANCE.NFSE_UltimoRetorno(buffer, bufferLen);
    }
}
