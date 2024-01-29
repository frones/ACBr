package com.acbr.nfse;

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

        //MT
        
        int NFSE_Inicializar(PointerByReference libHandler, String eArqConfig, String eChaveCrypt);

        int NFSE_Finalizar(Pointer libHandler);

        int NFSE_Nome(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize);

        int NFSE_Versao(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize);

        int NFSE_UltimoRetorno(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize);

        int NFSE_ConfigImportar(Pointer libHandler, String eArqConfig);

        int NFSE_ConfigExportar(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize);

        int NFSE_ConfigLer(Pointer libHandler, String eArqConfig);

        int NFSE_ConfigGravar(Pointer libHandler, String eArqConfig);

        int NFSE_ConfigLerValor(Pointer libHandler, String eSessao, String eChave, ByteBuffer buffer, IntByReference bufferSize);

        int NFSE_ConfigGravarValor(Pointer libHandler, String eSessao, String eChave, String valor);

        int NFSE_ConfigGravarValor(Pointer libHandler, String eArquivoOuXML);
        
        int NFSE_CarregarXML(Pointer libHandler, String eArquivoOuXml);

        int NFSE_CarregarINI(Pointer libHandler, String eArquivoOuINI);

        int NFSE_ObterXml(Pointer libHandler, Integer AIndex, ByteBuffer buffer, IntByReference bufferSize);

        int NFSE_GravarXml(Pointer libHandler, Integer AIndex, String eNomeArquivo, String ePathArquivo);

        int NFSE_ObterIni(Pointer libHandler, Integer AIndex, ByteBuffer buffer, IntByReference bufferSize);

        int NFSE_GravarIni(Pointer libHandler, Integer AIndex, String eNomeArquivo, String ePathArquivo);

        int NFSE_LimparLista(Pointer libHandler);
        
        int NFSE_ObterCertificados(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize);
        
        int NFSE_Emitir(Pointer libHandler, String aLote, Integer aModoEnvio, boolean aImprimir, ByteBuffer buffer, IntByReference bufferSize);
        
        int NFSE_Cancelar(Pointer libHandler, String aInfCancelamento, ByteBuffer buffer, IntByReference bufferSize);
        
        int NFSE_SubstituirNFSe(Pointer libHandler, String aNumeroNFSe, String aSerieNFSe, String aCodigoCancelamento, String aMotivoCancelamento, String aNumeroLote, String aCodigoVerificacao, ByteBuffer buffer, IntByReference bufferSize);
        
        int NFSE_LinkNFSE(Pointer libHandler, String aNumeroNFSe, String aCodigoVerificacao, String aChaveAcesso, String aValorServico, ByteBuffer buffer, IntByReference bufferSize);
        
        int NFSE_GerarLote(Pointer libHandler, String aLote, Integer aQtdMaximaRps, Integer aModoEnvio, ByteBuffer buffer, IntByReference bufferSize);
        
        int NFSE_GerarToken(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize);
        
        int NFSE_ConsultarSituacao(Pointer libHandler, String aProtocolo, String aNumeroLote, ByteBuffer buffer, IntByReference bufferSize);
        
        int NFSE_ConsultarLoteRps(Pointer libHandler, String aProtocolo, String aNumLote, ByteBuffer buffer, IntByReference bufferSize);
        
        int NFSE_ConsultarNFSePorRps(Pointer libHandler, String aNumeroRps, String aSerie, String aTipo, String aCodigoVerificacao, ByteBuffer buffer, IntByReference bufferSize);
        
        int NFSE_ConsultarNFSePorNumero(Pointer libHandler, String aNumero, Integer aPagina, ByteBuffer buffer, IntByReference bufferSize);
        
        int NFSE_ConsultarNFSePorPeriodo(Pointer libHandler, Date aDataInicial, Date aDataFinal, Integer aPagina, String aNumeroLote, Integer aTipoPeriodo, ByteBuffer buffer, IntByReference bufferSize);
        
        int NFSE_ConsultarNFSePorFaixa(Pointer libHandler, String aNumeroInicial, String aNumeroFinal, Integer aPagina, ByteBuffer buffer, IntByReference bufferSize);
        
        int NFSE_ConsultarNFSeGenerico(Pointer libHandler, String aInfConsultaNFSe, ByteBuffer buffer, IntByReference bufferSize);
        
        int NFSE_EnviarEmail(Pointer libHandler, String ePara, String eXmlNFSe, boolean aEnviaPDF, String eAssunto, String eCc, String eAnexos, String eMensagem);
        
        int NFSE_Imprimir(Pointer libHandler, String cImpressora, Integer nNumCopias, String bGerarPDF, String bMostrarPreview, String cCancelada);
        
        int NFSE_ImprimirPDF(Pointer libHandler);
        
        int NFSE_SalvarPDF(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize);
        
        int NFSE_ConsultarNFSeServicoPrestadoPorNumero(Pointer libHandler, String aNumero, Integer aPagina, Date aDataInicial, Date aDataFinal, Integer aTipoPeriodo, ByteBuffer buffer, IntByReference bufferSize);
        
        int NFSE_ConsultarNFSeServicoPrestadoPorPeriodo(Pointer libHandler, Date aDataInicial, Date aDataFinal, Integer aPagina, Integer aTipoPeriodo, ByteBuffer buffer, IntByReference bufferSize);
        
        int NFSE_ConsultarNFSeServicoPrestadoPorTomador(Pointer libHandler, String aCNPJ, String aInscMun, Integer aPagina, Date aDataInicial, Date aDataFinal, Integer aTipoPeriodo, ByteBuffer buffer, IntByReference bufferSize);
        
        int NFSE_ConsultarNFSeServicoPrestadoPorIntermediario(Pointer libHandler, String aCNPJ, String aInscMun, Integer aPagina, Date aDataInicial, Date aDataFinal, Integer aTipoPeriodo, ByteBuffer buffer, IntByReference bufferSize);
        
        int NFSE_ConsultarNFSeServicoTomadoPorNumero(Pointer libHandler, String aNumero, Integer aPagina, Date aDataInicial, Date aDataFinal, Integer aTipoPeriodo, ByteBuffer buffer, IntByReference bufferSize);
        
        int NFSE_ConsultarNFSeServicoTomadoPorPrestador(Pointer libHandler, String aCNPJ, String aInscMun, Integer aPagina, Date aDataInicial, Date aDataFinal, Integer aTipoPeriodo, ByteBuffer buffer, IntByReference bufferSize);
        
        int NFSE_ConsultarNFSeServicoTomadoPorTomador(Pointer libHandler, String aCNPJ, String aInscMun, Integer aPagina, Date aDataInicial, Date aDataFinal, Integer aTipoPeriodo, ByteBuffer buffer, IntByReference bufferSize);
        
        int NFSE_ConsultarNFSeServicoTomadoPorPeriodo(Pointer libHandler, Date aDataInicial, Date aDataFinal, Integer aPagina, Integer aTipoPeriodo, ByteBuffer buffer, IntByReference bufferSize);
        
        int NFSE_ConsultarNFSeServicoTomadoPorIntermediario(Pointer libHandler, String aCNPJ, String aInscMun, Integer aPagina, Date aDataInicial, Date aDataFinal, Integer aTipoPeriodo, ByteBuffer buffer, IntByReference bufferSize);
    
        int NFSE_EnviarEvento(Pointer libHandler, String aInfEvento, ByteBuffer buffer, IntByReference bufferSize);
        
        int NFSE_ConsultarDPSPorChave(Pointer libHandler, String aChaveDPS, ByteBuffer buffer, IntByReference bufferSize);
        
        int NFSE_ConsultarNFSePorChave(Pointer libHandler, String aChaveNFSe, ByteBuffer buffer, IntByReference bufferSize);
        
        int NFSE_ConsultarEvento(Pointer libHandler, String aChave, Integer aTipoEvento, Integer aNumSeq, ByteBuffer buffer, IntByReference bufferSize);
        
        int NFSE_ConsultarDFe(Pointer libHandler, Integer aNSU, ByteBuffer buffer, IntByReference bufferSize);
        
        int NFSE_ObterDANFSE(Pointer libHandler, String aChaveNFSe, ByteBuffer buffer, IntByReference bufferSize);
        
        int NFSE_ConsultarParametros(Pointer libHandler, Integer aTipoParametroMunicipio, String aCodigoServico, Date aCompetencia, String aNumeroBeneficio, ByteBuffer buffer, IntByReference bufferSize);
        
    }

    public ACBrNFSe() throws Exception {
        File iniFile = Paths.get(System.getProperty("user.dir"), "ACBrLib.ini").toFile();
        if (!iniFile.exists()) {
            iniFile.createNewFile();
        }

        PointerByReference handle = new PointerByReference();
        int ret = ACBrNFSeLib.INSTANCE.NFSE_Inicializar(handle, toUTF8(iniFile.getAbsolutePath()), toUTF8(""));
        checkResult(ret);
        setHandle(handle.getValue());
    }

    public ACBrNFSe(String eArqConfig, String eChaveCrypt) throws Exception {
        PointerByReference handle = new PointerByReference();
        int ret = ACBrNFSeLib.INSTANCE.NFSE_Inicializar(handle, toUTF8(eArqConfig), toUTF8(eChaveCrypt));
        checkResult(ret);
        setHandle(handle.getValue());
    }

    @Override
    protected void dispose() throws Exception {
        int ret = ACBrNFSeLib.INSTANCE.NFSE_Finalizar(getHandle());
        checkResult(ret);
    }

    public String nome() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrNFSeLib.INSTANCE.NFSE_Nome(getHandle(), buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());
    }

    public String versao() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrNFSeLib.INSTANCE.NFSE_Versao(getHandle(), buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());
    }

    public void configLer() throws Exception {
        configLer("");
    }

    public void configLer(String eArqConfig) throws Exception {
        int ret = ACBrNFSeLib.INSTANCE.NFSE_ConfigLer(getHandle(), toUTF8(eArqConfig));
        checkResult(ret);
    }

    public void configGravar() throws Exception {
        configGravar("");
    }

    public void configGravar(String eArqConfig) throws Exception {
        int ret = ACBrNFSeLib.INSTANCE.NFSE_ConfigGravar(getHandle(), toUTF8(eArqConfig));
        checkResult(ret);
    }

    @Override
    public String configLerValor(ACBrSessao eSessao, String eChave) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrNFSeLib.INSTANCE.NFSE_ConfigLerValor(getHandle(), toUTF8(eSessao.name()), toUTF8(eChave), buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);
    }

    @Override
    public void configGravarValor(ACBrSessao eSessao, String eChave, Object value) throws Exception {
        int ret = ACBrNFSeLib.INSTANCE.NFSE_ConfigGravarValor(getHandle(), toUTF8(eSessao.name()), toUTF8(eChave), toUTF8(value.toString()));
        checkResult(ret);
    }

    public void carregarXml(String eArquivoOuXML) throws Exception {
        int ret = ACBrNFSeLib.INSTANCE.NFSE_CarregarXML(getHandle(), toUTF8(eArquivoOuXML));
        checkResult(ret);
    }

    public String obterXml(int AIndex) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        int ret = ACBrNFSeLib.INSTANCE.NFSE_ObterXml(getHandle(), AIndex, buffer, bufferLen);
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
        int ret = ACBrNFSeLib.INSTANCE.NFSE_GravarXml(getHandle(), AIndex, toUTF8(eNomeArquivo), toUTF8(ePathArquivo));
        checkResult(ret);
    }

    public String obterIni(int AIndex) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        int ret = ACBrNFSeLib.INSTANCE.NFSE_ObterIni(getHandle(), AIndex, buffer, bufferLen);
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
        int ret = ACBrNFSeLib.INSTANCE.NFSE_GravarIni(getHandle(), AIndex, toUTF8(eNomeArquivo), toUTF8(ePathArquivo));
        checkResult(ret);
    }

    public void carregarIni(String eArquivoOuIni) throws Exception {
        int ret = ACBrNFSeLib.INSTANCE.NFSE_CarregarINI(getHandle(), toUTF8(eArquivoOuIni));
        checkResult(ret);
    }

    public void limparLista() throws Exception {
        int ret = ACBrNFSeLib.INSTANCE.NFSE_LimparLista(getHandle());
        checkResult(ret);
    }
    
    public String obterCertificados() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrNFSeLib.INSTANCE.NFSE_ObterCertificados(getHandle(), buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
    
    public String emitir(String aLote, Integer aModoEnvio, boolean aImprimir) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrNFSeLib.INSTANCE.NFSE_Emitir(getHandle(), toUTF8(aLote), aModoEnvio, aImprimir, buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
    
    public String cancelar(String aInfCancelamento) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrNFSeLib.INSTANCE.NFSE_Cancelar(getHandle(), toUTF8(aInfCancelamento), buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
    
    public String substituirNFSe(String aNumeroNFSe, String aSerieNFSe, String aCodigoCancelamento, String aMotivoCancelamento, String aNumeroLote, String aCodigoVerificacao) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrNFSeLib.INSTANCE.NFSE_SubstituirNFSe(getHandle(), toUTF8(aNumeroLote), toUTF8(aNumeroLote), toUTF8(aNumeroLote), toUTF8(aNumeroLote), toUTF8(aNumeroLote), toUTF8(aCodigoVerificacao), buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }

    public String linkNFSE(String aNumeroNFSe, String aCodigoVerificacao, String aChaveAcesso, String aValorServico) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrNFSeLib.INSTANCE.NFSE_LinkNFSE(getHandle(), toUTF8(aNumeroNFSe), toUTF8(aCodigoVerificacao), toUTF8(aChaveAcesso), toUTF8(aValorServico), buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
    
    public String gerarLote(String aLote, Integer aQtdMaximaRps, Integer aModoEnvio) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrNFSeLib.INSTANCE.NFSE_GerarLote(getHandle(), toUTF8(aLote), aQtdMaximaRps, aModoEnvio, buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }

    public String gerarToken() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrNFSeLib.INSTANCE.NFSE_GerarToken(getHandle(), buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
    
    public String consultarSituacao(String aProtocolo, String aNumeroLote) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrNFSeLib.INSTANCE.NFSE_ConsultarSituacao(getHandle(), toUTF8(aProtocolo), toUTF8(aNumeroLote), buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
   
    public String consultarLoteRps(String aProtocolo, String aNumLote) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrNFSeLib.INSTANCE.NFSE_ConsultarLoteRps(getHandle(), toUTF8(aProtocolo), toUTF8(aNumLote), buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
    
    public String consultarNFSePorRps(String aNumeroRps, String aSerie, String aTipo, String aCodigoVerificacao) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrNFSeLib.INSTANCE.NFSE_ConsultarNFSePorRps(getHandle(), toUTF8(aNumeroRps), toUTF8(aSerie), toUTF8(aTipo), toUTF8(aCodigoVerificacao), buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
    
    public String consultarNFSePorNumero(String aNumero, Integer aPagina) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrNFSeLib.INSTANCE.NFSE_ConsultarNFSePorNumero(getHandle(), toUTF8(aNumero), aPagina, buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
    
    public String consultarNFSePorPeriodo(Date aDataInicial, Date aDataFinal, Integer aPagina, String aNumeroLote, Integer aTipoPeriodo) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
                
        int ret = ACBrNFSeLib.INSTANCE.NFSE_ConsultarNFSePorPeriodo(getHandle(), aDataInicial, aDataFinal, aPagina, toUTF8(aNumeroLote), aTipoPeriodo, buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
    
    public String consultarNFSePorFaixa(String aNumeroInicial, String aNumeroFinal, Integer aPagina) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrNFSeLib.INSTANCE.NFSE_ConsultarNFSePorFaixa(getHandle(), toUTF8(aNumeroInicial), toUTF8(aNumeroFinal), aPagina, buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);  
    }
        
    public String consultarNFSeGenerico(String aInfConsultaNFSe) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrNFSeLib.INSTANCE.NFSE_ConsultarNFSeGenerico(getHandle(), toUTF8(aInfConsultaNFSe), buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
        
    public void enviarEmail(String ePara, String eXmlNFSe, boolean aEnviaPDF, String eAssunto) throws Exception {
        enviarEmail(ePara, eXmlNFSe, aEnviaPDF, eAssunto, "", "", "");
    }
        
    public void enviarEmail(String ePara, String eXmlNFSe, boolean aEnviarPDF, String eAssunto, String eCc, String eAnexos, String eMensagem ) throws Exception {
        int ret = ACBrNFSeLib.INSTANCE.NFSE_EnviarEmail(getHandle(), ePara, eXmlNFSe, aEnviarPDF, eAssunto, eCc, eAnexos, eMensagem);
        checkResult(ret);
    }
    
    public void imprimir () throws Exception {
        imprimir("", 1, null, null, "");
    }
        
    public void imprimir(String cImpressora, Integer nNumCopias, Boolean bGerarPDF, Boolean bMostrarPreview, String cCancelada) throws Exception {
        
        String gerarPDF = bGerarPDF != null ? bGerarPDF ? "1" : "0" : "";
        String mostrarPreview = bMostrarPreview != null ? bMostrarPreview ? "1" : "0" : "";
        
        int ret = ACBrNFSeLib.INSTANCE.NFSE_Imprimir(getHandle(), cImpressora, nNumCopias, gerarPDF, mostrarPreview, cCancelada);
        checkResult(ret);
    }
    
    public void imprimirPDF() throws Exception {
        int ret = ACBrNFSeLib.INSTANCE.NFSE_ImprimirPDF(getHandle());
        checkResult(ret);
    }
    
    public String salvarPDF() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrNFSeLib.INSTANCE.NFSE_SalvarPDF(getHandle(), buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
    
    public String consultarNFSeServicoPrestadoPorNumero(String aNumero, Integer aPagina, Date aDataInicial, Date aDataFinal, Integer aTipoPeriodo) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrNFSeLib.INSTANCE.NFSE_ConsultarNFSeServicoPrestadoPorNumero(getHandle(), toUTF8(aNumero), aPagina, aDataInicial, aDataFinal, aTipoPeriodo, buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
    
    public String consultarNFSeServicoPrestadoPorPeriodo(Date aDataInicial, Date aDataFinal, Integer aPagina, Integer aTipoPeriodo) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrNFSeLib.INSTANCE.NFSE_ConsultarNFSeServicoPrestadoPorPeriodo(getHandle(), aDataInicial, aDataFinal, aPagina, aTipoPeriodo, buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
    
    public String consultarNFSeServicoPrestadoPorTomador(String aCNPJ, String aInscMun, Integer aPagina, Date aDataInicial, Date aDataFinal, Integer aTipoPeriodo) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrNFSeLib.INSTANCE.NFSE_ConsultarNFSeServicoPrestadoPorTomador(getHandle(), toUTF8(aCNPJ), toUTF8(aInscMun), aPagina, aDataInicial, aDataFinal, aTipoPeriodo, buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
        
    public String consultarNFSeServicoPrestadoPorIntermediario(String aCNPJ, String aInscMun, Integer aPagina, Date aDataInicial, Date aDataFinal, Integer aTipoPeriodo) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrNFSeLib.INSTANCE.NFSE_ConsultarNFSeServicoPrestadoPorIntermediario(getHandle(), toUTF8(aCNPJ), toUTF8(aInscMun), aPagina, aDataInicial, aDataFinal, aTipoPeriodo, buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
        
    public String consultarNFSeServicoTomadoPorNumero(String aNumero, Integer aPagina, Date aDataInicial, Date aDataFinal, Integer aTipoPeriodo) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrNFSeLib.INSTANCE.NFSE_ConsultarNFSeServicoTomadoPorNumero(getHandle(), toUTF8(aNumero), aPagina, aDataInicial, aDataFinal, aTipoPeriodo, buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
        
    public String consultarNFSeServicoTomadoPorPrestador(String aCNPJ, String aInscMun, Integer aPagina, Date aDataInicial, Date aDataFinal, Integer aTipoPeriodo) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrNFSeLib.INSTANCE.NFSE_ConsultarNFSeServicoTomadoPorPrestador(getHandle(), toUTF8(aCNPJ), toUTF8(aInscMun), aPagina, aDataInicial, aDataFinal, aTipoPeriodo, buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
        
    public String consultarNFSeServicoTomadoPorTomador(String aCNPJ, String aInscMun, Integer aPagina, Date aDataInicial, Date aDataFinal, Integer aTipoPeriodo) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrNFSeLib.INSTANCE.NFSE_ConsultarNFSeServicoTomadoPorTomador(getHandle(), toUTF8(aCNPJ), toUTF8(aInscMun), aPagina, aDataInicial, aDataFinal, aTipoPeriodo, buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
        
    public String consultarNFSeServicoTomadoPorPeriodo(Date aDataInicial, Date aDataFinal, Integer aPagina, Integer aTipoPeriodo) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrNFSeLib.INSTANCE.NFSE_ConsultarNFSeServicoTomadoPorPeriodo(getHandle(), aDataInicial, aDataFinal, aPagina, aTipoPeriodo, buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
        
    public String consultarNFSeServicoTomadoPorIntermediario(String aCNPJ, String aInscMun, Integer aPagina, Date aDataInicial, Date aDataFinal, Integer aTipoPeriodo) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrNFSeLib.INSTANCE.NFSE_ConsultarNFSeServicoTomadoPorIntermediario(getHandle(), toUTF8(aCNPJ), toUTF8(aInscMun), aPagina, aDataInicial, aDataFinal, aTipoPeriodo, buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
    
    public String enviarEvento(String aInfEvento) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrNFSeLib.INSTANCE.NFSE_EnviarEvento(getHandle(), toUTF8(aInfEvento), buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
    
    public String consultarDPSPorChave(String aChaveDPS) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrNFSeLib.INSTANCE.NFSE_ConsultarDPSPorChave(getHandle(), toUTF8(aChaveDPS), buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
    
    public String consultarNFSePorChave(String aChaveNFSe) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrNFSeLib.INSTANCE.NFSE_ConsultarNFSePorChave(getHandle(), toUTF8(aChaveNFSe), buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
    
    public String consultarEvento(String aChave, Integer aTipoEvento, Integer aNumSeq) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrNFSeLib.INSTANCE.NFSE_ConsultarEvento(getHandle(), toUTF8(aChave), aTipoEvento, aNumSeq, buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
    
    public String consultarDFe(Integer aNSU) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrNFSeLib.INSTANCE.NFSE_ConsultarDFe(getHandle(), aNSU, buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
    
    public String obterDANFSE(String aChaveNFSe) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrNFSeLib.INSTANCE.NFSE_ObterDANFSE(getHandle(), toUTF8(aChaveNFSe), buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
    
    public String consultarParametros(Integer aTipoParametroMunicipio, String aCodigoServico, Date aCompetencia, String aNumeroBeneficio) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrNFSeLib.INSTANCE.NFSE_ConsultarParametros(getHandle(), aTipoParametroMunicipio, toUTF8(aCodigoServico), aCompetencia, toUTF8(aNumeroBeneficio), buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
    
    public void ConfigImportar(String eArqConfig) throws Exception {

        int ret = ACBrNFSeLib.INSTANCE.NFSE_ConfigImportar(getHandle(), eArqConfig);
        checkResult(ret);

    }

    public String ConfigExportar() throws Exception {

        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrNFSeLib.INSTANCE.NFSE_ConfigExportar(getHandle(), buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());

    }

    @Override
    protected void UltimoRetorno(ByteBuffer buffer, IntByReference bufferLen) {
        ACBrNFSeLib.INSTANCE.NFSE_UltimoRetorno(getHandle(), buffer, bufferLen);
    }
}
