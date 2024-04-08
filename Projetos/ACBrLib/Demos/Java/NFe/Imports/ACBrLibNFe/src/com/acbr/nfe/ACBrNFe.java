package com.acbr.nfe;

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

public final class ACBrNFe extends ACBrLibBase {

    private interface ACBrNFeLib extends Library {

        static String JNA_LIBRARY_NAME = LibraryLoader.getLibraryName();
        public final static ACBrNFeLib INSTANCE = LibraryLoader.getInstance();

        class LibraryLoader {

            private static String library = "";
            private static ACBrNFeLib instance = null;

            private static String getLibraryName() {
                if (library.isEmpty()) {
                    if (Platform.isWindows()) {
                        library = Platform.is64Bit() ? "ACBrNFe64" : "ACBrNFe32";
                    } else {
                        library = Platform.is64Bit() ? "acbrnfe64" : "acbrnfe32";
                    }
                }
                return library;
            }

            public static ACBrNFeLib getInstance() {
                if (instance == null) {
                    instance = (ACBrNFeLib) Native.synchronizedLibrary(
                            (Library) Native.load(JNA_LIBRARY_NAME, ACBrNFeLib.class));
                }

                return instance;
            }
        }

        int NFE_Inicializar(String eArqConfig, String eChaveCrypt);

        int NFE_Finalizar();

        int NFE_Nome(ByteBuffer buffer, IntByReference bufferSize);

        int NFE_Versao(ByteBuffer buffer, IntByReference bufferSize);

        int NFE_UltimoRetorno(ByteBuffer buffer, IntByReference bufferSize);

        int NFE_ConfigImportar(String eArqConfig);

        int NFE_ConfigExportar(ByteBuffer buffer, IntByReference bufferSize);

        int NFE_ConfigLer(String eArqConfig);

        int NFE_ConfigGravar(String eArqConfig);

        int NFE_ConfigLerValor(String eSessao, String eChave, ByteBuffer buffer, IntByReference bufferSize);

        int NFE_ConfigGravarValor(String eSessao, String eChave, String valor);

        int NFE_CarregarXML(String eArquivoOuXML);

        int NFE_CarregarINI(String eArquivoOuINI);

        int NFE_ObterXml(Integer AIndex, ByteBuffer buffer, IntByReference bufferSize);

        int NFE_GravarXml(Integer AIndex, String eNomeArquivo, String ePathArquivo);

        int NFE_ObterIni(Integer AIndex, ByteBuffer buffer, IntByReference bufferSize);

        int NFE_GravarIni(Integer AIndex, String eNomeArquivo, String ePathArquivo);

        int NFE_LimparLista();

        int NFE_CarregarEventoXML(String eArquivoOuXml);

        int NFE_CarregarEventoINI(String eArquivoOuIni);

        int NFE_LimparListaEventos();

        int NFE_Assinar();

        int NFE_Validar();

        int NFE_ValidarRegrasdeNegocios(ByteBuffer buffer, IntByReference bufferSize);

        int NFE_VerificarAssinatura(ByteBuffer buffer, IntByReference bufferSize);

        int NFE_GerarChave(int ACodigoUF, int ACodigoNumerico, int AModelo, int ASerie, int ANumero,
                int ATpEmi, String AEmissao, String CPFCNPJ, ByteBuffer buffer, IntByReference bufferSize);

        int NFE_ObterCertificados(ByteBuffer buffer, IntByReference bufferSize);

        int NFE_OpenSSLInfo(ByteBuffer buffer, IntByReference bufferSize);

        int NFE_GetPath(int tipo, ByteBuffer buffer, IntByReference bufferSize);

        int NFE_GetPathEvento(String aCodEvento, ByteBuffer buffer, IntByReference bufferSize);

        int NFE_StatusServico(ByteBuffer buffer, IntByReference bufferSize);

        int NFE_Consultar(String eChaveOuNFe, boolean AExtrairEventos, ByteBuffer buffer, IntByReference bufferSize);

        int NFE_Inutilizar(String ACNPJ, String AJustificativa, int Ano, int Modelo, int Serie,
                int NumeroInicial, int NumeroFinal, ByteBuffer buffer, IntByReference bufferSize);

        int NFE_Enviar(int ALote, boolean Imprimir, boolean sincrono, boolean zipado, ByteBuffer buffer, IntByReference bufferSize);

        int NFE_ConsultarRecibo(String aRecibo, ByteBuffer buffer, IntByReference bufferSize);

        int NFE_ConsultaCadastro(String cUF, String nDocumento, Boolean nIE, ByteBuffer buffer, IntByReference bufferSize);

        int NFE_Cancelar(String eChave, String eJustificativa, String eCNPJ, int ALote,
                ByteBuffer buffer, IntByReference bufferSize);

        int NFE_EnviarEvento(int idLote, ByteBuffer buffer, IntByReference bufferSize);

        int NFE_DistribuicaoDFePorUltNSU(int AcUFAutor, String eCNPJCPF, String eultNsu, ByteBuffer buffer,
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

        int NFE_ImprimirEvento(String eArquivoXmlNFe, String eArquivoXmlEvento);

        int NFE_ImprimirEventoPDF(String eArquivoXmlNFe, String eArquivoXmlEvento);

        int NFE_ImprimirInutilizacao(String eArquivoXml);

        int NFE_ImprimirInutilizacaoPDF(String eArquivoXml);
    }

    public ACBrNFe() throws Exception {
        File iniFile = Paths.get(System.getProperty("user.dir"), "ACBrLib.ini").toFile();
        if (!iniFile.exists()) {
            iniFile.createNewFile();
        }

        int ret = ACBrNFeLib.INSTANCE.NFE_Inicializar(toUTF8(iniFile.getAbsolutePath()), toUTF8(""));
        checkResult(ret);
    }

    public ACBrNFe(String eArqConfig, String eChaveCrypt) throws Exception {
        int ret = ACBrNFeLib.INSTANCE.NFE_Inicializar(toUTF8(eArqConfig), toUTF8(eChaveCrypt));
        checkResult(ret);
    }

    @Override
    protected void dispose() throws Exception {
        int ret = ACBrNFeLib.INSTANCE.NFE_Finalizar();
        checkResult(ret);
    }

    public String nome() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrNFeLib.INSTANCE.NFE_Nome(buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());
    }

    public String versao() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrNFeLib.INSTANCE.NFE_Versao(buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());
    }

    public void configLer() throws Exception {
        configLer("");
    }

    public void configLer(String eArqConfig) throws Exception {
        int ret = ACBrNFeLib.INSTANCE.NFE_ConfigLer(toUTF8(eArqConfig));
        checkResult(ret);
    }

    public void configGravar() throws Exception {
        configGravar("");
    }

    public void configGravar(String eArqConfig) throws Exception {
        int ret = ACBrNFeLib.INSTANCE.NFE_ConfigGravar(toUTF8(eArqConfig));
        checkResult(ret);
    }

    @Override
    public String configLerValor(ACBrSessao eSessao, String eChave) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrNFeLib.INSTANCE.NFE_ConfigLerValor(toUTF8(eSessao.name()), toUTF8(eChave), buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);
    }

    @Override
    public void configGravarValor(ACBrSessao eSessao, String eChave, Object value) throws Exception {
        int ret = ACBrNFeLib.INSTANCE.NFE_ConfigGravarValor(toUTF8(eSessao.name()), toUTF8(eChave), toUTF8(value.toString()));
        checkResult(ret);
    }

    public void carregarXml(String eArquivoOuXML) throws Exception {
        int ret = ACBrNFeLib.INSTANCE.NFE_CarregarXML(toUTF8(eArquivoOuXML));
        checkResult(ret);
    }

    public String obterXml(int AIndex) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        int ret = ACBrNFeLib.INSTANCE.NFE_ObterXml(AIndex, buffer, bufferLen);
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
        int ret = ACBrNFeLib.INSTANCE.NFE_GravarXml(AIndex, toUTF8(eNomeArquivo), toUTF8(ePathArquivo));
        checkResult(ret);
    }

    public String obterIni(int AIndex) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        int ret = ACBrNFeLib.INSTANCE.NFE_ObterIni(AIndex, buffer, bufferLen);
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
        int ret = ACBrNFeLib.INSTANCE.NFE_GravarIni(AIndex, toUTF8(eNomeArquivo), toUTF8(ePathArquivo));
        checkResult(ret);
    }

    public void carregarIni(String eArquivoOuIni) throws Exception {
        int ret = ACBrNFeLib.INSTANCE.NFE_CarregarINI(toUTF8(eArquivoOuIni));
        checkResult(ret);
    }

    public void limparLista() throws Exception {
        int ret = ACBrNFeLib.INSTANCE.NFE_LimparLista();
        checkResult(ret);
    }

    public void carregarEventoXml(String eArquivoOuXML) throws Exception {
        int ret = ACBrNFeLib.INSTANCE.NFE_CarregarEventoXML(toUTF8(eArquivoOuXML));
        checkResult(ret);
    }

    public void carregarEventoINI(String eArquivoOuIni) throws Exception {
        int ret = ACBrNFeLib.INSTANCE.NFE_CarregarEventoINI(toUTF8(eArquivoOuIni));
        checkResult(ret);
    }

    public void limparListaEventos() throws Exception {
        int ret = ACBrNFeLib.INSTANCE.NFE_LimparListaEventos();
        checkResult(ret);
    }

    public void assinar() throws Exception {
        int ret = ACBrNFeLib.INSTANCE.NFE_Assinar();
        checkResult(ret);
    }

    public void validar() throws Exception {
        int ret = ACBrNFeLib.INSTANCE.NFE_Validar();
        checkResult(ret);
    }

    public String validarRegrasdeNegocios() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrNFeLib.INSTANCE.NFE_ValidarRegrasdeNegocios(buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);
    }

    public String verificarAssinatura() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrNFeLib.INSTANCE.NFE_VerificarAssinatura(buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);
    }

    public String gerarChave(int aCodigoUf, int aCodigoNumerico, int aModelo, int aSerie, int aNumero,
            int aTpEmi, Date aEmissao, String acpfcnpj) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        String pattern = "dd/MM/yyyy";
        DateFormat df = new SimpleDateFormat(pattern);

        int ret = ACBrNFeLib.INSTANCE.NFE_GerarChave(aCodigoUf, aCodigoNumerico, aModelo,
                aSerie, aNumero, aTpEmi, df.format(aEmissao),
                toUTF8(acpfcnpj), buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);
    }

    public String obterCertificados() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrNFeLib.INSTANCE.NFE_ObterCertificados(buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
    
    public String openSSLInfo() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrNFeLib.INSTANCE.NFE_OpenSSLInfo(buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }

    public String getPath(TipoPathNFe tipo) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrNFeLib.INSTANCE.NFE_GetPath(tipo.asInt(), buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);
    }

    public String getPathEvento(String aCodEvento) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrNFeLib.INSTANCE.NFE_GetPathEvento(toUTF8(aCodEvento), buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);
    }

    public String statusServico() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrNFeLib.INSTANCE.NFE_StatusServico(buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);
    }

    public String consultar(String eChaveOuNFe) throws Exception {
        return consultar(eChaveOuNFe, false);
    }

    public String consultar(String eChaveOuNFe, boolean AExtrairEventos) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrNFeLib.INSTANCE.NFE_Consultar(toUTF8(eChaveOuNFe), AExtrairEventos, buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);
    }

    public String consultarRecibo(String aRecibo) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrNFeLib.INSTANCE.NFE_ConsultarRecibo(toUTF8(aRecibo), buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);
    }

    public String consultaCadastro(String cUF, String nDocumento, Boolean nIE) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrNFeLib.INSTANCE.NFE_ConsultaCadastro(toUTF8(cUF), toUTF8(nDocumento), nIE, buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);
    }

    //TODO: Sobrescrever método com valores default
    public String inutilizar(String aCNPJ, String aJustificativa, int ano, int modelo, int serie,
            int numeroInicial, int numeroFinal) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrNFeLib.INSTANCE.NFE_Inutilizar(toUTF8(aCNPJ), toUTF8(aJustificativa), ano, modelo, serie,
                numeroInicial, numeroFinal, buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);
    }

    public String enviar(int aLote) throws Exception {
        return enviar(aLote, false, false, false);
    }

    public String enviar(int aLote, boolean imprimir, boolean sincrono, boolean zipado) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrNFeLib.INSTANCE.NFE_Enviar(aLote, imprimir, sincrono, zipado, buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);
    }

    public String cancelar(String aChave, String aJustificativa, String aCNPJ) throws Exception {
        return cancelar(aChave, aJustificativa, aCNPJ, 1);
    }

    public String cancelar(String aChave, String aJustificativa, String aCNPJ, int aLote) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrNFeLib.INSTANCE.NFE_Cancelar(toUTF8(aChave), toUTF8(aJustificativa), toUTF8(aCNPJ), aLote, buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);
    }

    public String enviarEvento(int idLote) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrNFeLib.INSTANCE.NFE_EnviarEvento(idLote, buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);
    }

    public String distribuicaoDFeporUltNSU(int acUFAutor, String aCNPJCPF, String eultNsu) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrNFeLib.INSTANCE.NFE_DistribuicaoDFePorUltNSU(acUFAutor, toUTF8(aCNPJCPF), toUTF8(eultNsu), buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);
    }

    public String distribuicaoDFeporNSU(int acUFAutor, String aCNPJCPF, String aNSU) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrNFeLib.INSTANCE.NFE_DistribuicaoDFePorNSU(acUFAutor, toUTF8(aCNPJCPF), toUTF8(aNSU), buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);
    }

    public String distribuicaoDFeporChave(int acUFAutor, String aCNPJCPF, String aChave) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrNFeLib.INSTANCE.NFE_DistribuicaoDFePorChave(acUFAutor, aCNPJCPF, aChave, buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);
    }

    public void enviarEmail(String aPara, String aChaveNFe, boolean aEnviarPDF, String aAssunto) throws Exception {
        enviarEmail(aPara, aChaveNFe, aEnviarPDF, aAssunto, "", "", "");
    }

    public void enviarEmail(String aPara, String aChaveNFe, boolean aEnviarPDF, String aAssunto,
            String aEmailCC, String aAnexos, String aMesagem) throws Exception {
        int ret = ACBrNFeLib.INSTANCE.NFE_EnviarEmail(aPara, aChaveNFe, aEnviarPDF, aAssunto,
                aEmailCC, aAnexos, aMesagem);
        checkResult(ret);
    }

    public void enviarEmailEvento(String aPara, String aChaveEvento, String aChaveNFe, boolean aEnviarPDF,
            String aAssunto) throws Exception {
        enviarEmailEvento(aPara, aChaveEvento, aChaveNFe, aEnviarPDF, aAssunto, "", "", "");
    }

    public void enviarEmailEvento(String aPara, String aChaveEvento, String aChaveNFe, boolean aEnviarPDF,
            String aAssunto, String aEmailCC, String aAnexos, String aMesagem) throws Exception {
        int ret = ACBrNFeLib.INSTANCE.NFE_EnviarEmailEvento(aPara, aChaveEvento, aChaveNFe, aEnviarPDF,
                aAssunto, aEmailCC, aAnexos, aMesagem);
        checkResult(ret);
    }

    public void imprimir() throws Exception {
        imprimir("", 1, "", null, null, null, null);
    }

    public void imprimir(String cImpressora, int nNumCopias, String cProtocolo, Boolean bMostrarPreview, Boolean cMarcaDagua,
            Boolean bViaConsumidor, Boolean bSimplificado) throws Exception {

        String mostrarPreview = bMostrarPreview != null ? bMostrarPreview ? "1" : "0" : "";
        String marcaDagua = cMarcaDagua != null ? cMarcaDagua ? "1" : "0" : "";
        String viaConsumidor = bViaConsumidor != null ? bViaConsumidor ? "1" : "0" : "";
        String simplificado = bSimplificado != null ? bSimplificado ? "1" : "0" : "";

        int ret = ACBrNFeLib.INSTANCE.NFE_Imprimir(toUTF8(cImpressora), nNumCopias, toUTF8(cProtocolo), toUTF8(mostrarPreview),
                toUTF8(marcaDagua), toUTF8(viaConsumidor), toUTF8(simplificado));
        checkResult(ret);
    }

    public void imprimirPDF() throws Exception {
        int ret = ACBrNFeLib.INSTANCE.NFE_ImprimirPDF();
        checkResult(ret);
    }

    public void imprimirEvento(String eArquivoXmlNFe, String eArquivoXmlEvento) throws Exception {
        int ret = ACBrNFeLib.INSTANCE.NFE_ImprimirEvento(toUTF8(eArquivoXmlNFe), toUTF8(eArquivoXmlEvento));
        checkResult(ret);
    }

    public void imprimirEventoPDF(String eArquivoXmlNFe, String eArquivoXmlEvento) throws Exception {
        int ret = ACBrNFeLib.INSTANCE.NFE_ImprimirEventoPDF(toUTF8(eArquivoXmlNFe), toUTF8(eArquivoXmlEvento));
        checkResult(ret);
    }

    public void imprimirInutilizacao(String eArquivoXml) throws Exception {
        int ret = ACBrNFeLib.INSTANCE.NFE_ImprimirInutilizacao(toUTF8(eArquivoXml));
        checkResult(ret);
    }

    public void imprimirInutilizacaoPDF(String eArquivoXml) throws Exception {
        int ret = ACBrNFeLib.INSTANCE.NFE_ImprimirInutilizacaoPDF(toUTF8(eArquivoXml));
        checkResult(ret);
    }

    public void ConfigImportar(String eArqConfig) throws Exception {

        int ret = ACBrNFeLib.INSTANCE.NFE_ConfigImportar(eArqConfig);
        checkResult(ret);

    }

    public String ConfigExportar() throws Exception {

        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrNFeLib.INSTANCE.NFE_ConfigExportar(buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());

    }

    @Override
    protected void UltimoRetorno(ByteBuffer buffer, IntByReference bufferLen) {
        ACBrNFeLib.INSTANCE.NFE_UltimoRetorno(buffer, bufferLen);
    }
}
