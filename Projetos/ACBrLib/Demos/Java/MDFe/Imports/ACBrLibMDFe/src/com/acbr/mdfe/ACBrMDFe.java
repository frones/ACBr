package com.acbr.mdfe;

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

public final class ACBrMDFe extends ACBrLibBase {

    private interface ACBrMDFeLib extends Library {

        static String JNA_LIBRARY_NAME = LibraryLoader.getLibraryName();
        public final static ACBrMDFeLib INSTANCE = LibraryLoader.getInstance();
        
        class LibraryLoader {

            private static String library = "";
            private static ACBrMDFeLib instance = null;

            private static String getLibraryName() {
                if (library.isEmpty()) {
                    if (Platform.isWindows()) {
                        library = Platform.is64Bit() ? "ACBrMDFe64" : "ACBrMDFe32";
                    } else {
                        library = Platform.is64Bit() ? "acbrmdfe64" : "acbrmdfe32";
                    }
                }

                return library;
            }

            public static ACBrMDFeLib getInstance() {
                if (instance == null) {
                    instance = (ACBrMDFeLib) Native.synchronizedLibrary(
                            (Library) Native.load(JNA_LIBRARY_NAME, ACBrMDFeLib.class));
                }

                return instance;
            }
        }

        int MDFE_Inicializar(String eArqConfig, String eChaveCrypt);

        int MDFE_Finalizar();

        int MDFE_Nome(ByteBuffer buffer, IntByReference bufferSize);

        int MDFE_Versao(ByteBuffer buffer, IntByReference bufferSize);

        int MDFE_UltimoRetorno(ByteBuffer buffer, IntByReference bufferSize);

        int MDFE_ConfigImportar(String eArqConfig);

        int MDFE_ConfigExportar(ByteBuffer buffer, IntByReference bufferSize);

        int MDFE_ConfigLer(String eArqConfig);

        int MDFE_ConfigGravar(String eArqConfig);

        int MDFE_ConfigLerValor(String eSessao, String eChave, ByteBuffer buffer, IntByReference bufferSize);

        int MDFE_ConfigGravarValor(String eSessao, String eChave, String valor);

        int MDFE_CarregarXML(String eArquivoOuXML);

        int MDFE_CarregarINI(String eArquivoOuINI);

        int MDFE_ObterXml(Integer AIndex, ByteBuffer buffer, IntByReference bufferSize);

        int MDFE_GravarXml(Integer AIndex, String eNomeArquivo, String ePathArquivo);

        int MDFE_ObterIni(Integer AIndex, ByteBuffer buffer, IntByReference bufferSize);

        int MDFE_GravarIni(Integer AIndex, String eNomeArquivo, String ePathArquivo);

        int MDFE_LimparLista();

        int MDFE_CarregarEventoXML(String eArquivoOuXml);

        int MDFE_CarregarEventoINI(String eArquivoOuIni);

        int MDFE_LimparListaEventos();

        int MDFE_Assinar();

        int MDFE_Validar();

        int MDFE_ValidarRegrasdeNegocios(ByteBuffer buffer, IntByReference bufferSize);

        int MDFE_VerificarAssinatura(ByteBuffer buffer, IntByReference bufferSize);

        int MDFE_GerarChave(int ACodigoUF, int ACodigoNumerico, int AModelo, int ASerie, int ANumero,
                int ATpEmi, String AEmissao, String CPFCNPJ, ByteBuffer buffer, IntByReference bufferSize);

        int MDFE_ObterCertificados(ByteBuffer buffer, IntByReference bufferSize);

        int MDFE_GetPath(int tipo, ByteBuffer buffer, IntByReference bufferSize);

        int MDFE_GetPathEvento(String aCodEvento, ByteBuffer buffer, IntByReference bufferSize);

        int MDFE_StatusServico(ByteBuffer buffer, IntByReference bufferSize);

        int MDFE_Consultar(String eChaveOuNFe, boolean AExtrairEventos, ByteBuffer buffer, IntByReference bufferSize);

        int MDFE_ConsultarMDFeNaoEnc(String aCNPJ, ByteBuffer buffer, IntByReference bufferSize);

        int MDFE_Enviar(int ALote, boolean Imprimir, boolean sincrono, ByteBuffer buffer, IntByReference bufferSize);

        int MDFE_ConsultarRecibo(String aRecibo, ByteBuffer buffer, IntByReference bufferSize);

        int MDFE_Cancelar(String eChave, String eJustificativa, String eCNPJ, int ALote,
                ByteBuffer buffer, IntByReference bufferSize);

        int MDFE_EncerrarMDFe(String eChaveOuMDFe, String eDtEnc, String cMunicipioDescarga, String nCNPJ, String nProtocolo, ByteBuffer buffer, IntByReference bufferSize);

        int MDFE_EnviarEvento(int idLote, ByteBuffer buffer, IntByReference bufferSize);

        int MDFE_DistribuicaoDFePorUltNSU(int AcUFAutor, String eCNPJCPF, String eultNsu, ByteBuffer buffer,
                IntByReference bufferSize);

        int MDFE_DistribuicaoDFePorNSU(int AcUFAutor, String eCNPJCPF, String eNSU,
                ByteBuffer buffer, IntByReference bufferSize);

        int MDFE_DistribuicaoDFePorChave(int AcUFAutor, String eCNPJCPF, String echNFe,
                ByteBuffer buffer, IntByReference bufferSize);

        int MDFE_EnviarEmail(String ePara, String eChaveNFe, boolean AEnviaPDF, String eAssunto,
                String eCC, String eAnexos, String eMensagem);

        int MDFE_EnviarEmailEvento(String ePara, String eChaveEvento, String eChaveNFe,
                boolean AEnviaPDF, String eAssunto, String eCC, String eAnexos, String eMensagem);

        int MDFE_Imprimir(String cImpressora, int nNumCopias, String cProtocolo, String bMostrarPreview);

        int MDFE_ImprimirPDF();

        int MDFE_ImprimirEvento(String eArquivoXmlCTe, String eArquivoXmlEvento);

        int MDFE_ImprimirEventoPDF(String eArquivoXmlCTe, String eArquivoXmlEvento);        
    }

    public ACBrMDFe() throws Exception {
        File iniFile = Paths.get(System.getProperty("user.dir"), "ACBrLib.ini").toFile();
        if (!iniFile.exists()) {
            iniFile.createNewFile();
        }

        int ret = ACBrMDFeLib.INSTANCE.MDFE_Inicializar(toUTF8(iniFile.getAbsolutePath()), toUTF8(""));
        checkResult(ret);
    }

    public ACBrMDFe(String eArqConfig, String eChaveCrypt) throws Exception {
        int ret = ACBrMDFeLib.INSTANCE.MDFE_Inicializar(toUTF8(eArqConfig), toUTF8(eChaveCrypt));
        checkResult(ret);
    }

    @Override
    protected void dispose() throws Exception {
        int ret = ACBrMDFeLib.INSTANCE.MDFE_Finalizar();
        checkResult(ret);
    }

    public String nome() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrMDFeLib.INSTANCE.MDFE_Nome(buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());
    }

    public String versao() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrMDFeLib.INSTANCE.MDFE_Versao(buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());
    }

    public void configLer() throws Exception {
        configLer("");
    }

    public void configLer(String eArqConfig) throws Exception {
        int ret = ACBrMDFeLib.INSTANCE.MDFE_ConfigLer(toUTF8(eArqConfig));
        checkResult(ret);
    }

    public void configGravar() throws Exception {
        configGravar("");
    }

    public void configGravar(String eArqConfig) throws Exception {
        int ret = ACBrMDFeLib.INSTANCE.MDFE_ConfigGravar(toUTF8(eArqConfig));
        checkResult(ret);
    }

    @Override
    public String configLerValor(ACBrSessao eSessao, String eChave) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrMDFeLib.INSTANCE.MDFE_ConfigLerValor(toUTF8(eSessao.name()), toUTF8(eChave), buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);
    }

    @Override
    public void configGravarValor(ACBrSessao eSessao, String eChave, Object value) throws Exception {
        int ret = ACBrMDFeLib.INSTANCE.MDFE_ConfigGravarValor(toUTF8(eSessao.name()), toUTF8(eChave), toUTF8(value.toString()));
        checkResult(ret);
    }

    public void carregarXml(String eArquivoOuXML) throws Exception {
        int ret = ACBrMDFeLib.INSTANCE.MDFE_CarregarXML(toUTF8(eArquivoOuXML));
        checkResult(ret);
    }

    public String obterXml(int AIndex) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        int ret = ACBrMDFeLib.INSTANCE.MDFE_ObterXml(AIndex, buffer, bufferLen);
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
        int ret = ACBrMDFeLib.INSTANCE.MDFE_GravarXml(AIndex, toUTF8(eNomeArquivo), toUTF8(ePathArquivo));
        checkResult(ret);
    }

    public String obterIni(int AIndex) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        int ret = ACBrMDFeLib.INSTANCE.MDFE_ObterIni(AIndex, buffer, bufferLen);
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
        int ret = ACBrMDFeLib.INSTANCE.MDFE_GravarIni(AIndex, toUTF8(eNomeArquivo), toUTF8(ePathArquivo));
        checkResult(ret);
    }

    public void carregarIni(String eArquivoOuIni) throws Exception {
        int ret = ACBrMDFeLib.INSTANCE.MDFE_CarregarINI(toUTF8(eArquivoOuIni));
        checkResult(ret);
    }

    public void limparLista() throws Exception {
        int ret = ACBrMDFeLib.INSTANCE.MDFE_LimparLista();
        checkResult(ret);
    }

    public void carregarEventoXml(String eArquivoOuXML) throws Exception {
        int ret = ACBrMDFeLib.INSTANCE.MDFE_CarregarEventoXML(toUTF8(eArquivoOuXML));
        checkResult(ret);
    }

    public void carregarEventoINI(String eArquivoOuIni) throws Exception {
        int ret = ACBrMDFeLib.INSTANCE.MDFE_CarregarEventoINI(toUTF8(eArquivoOuIni));
        checkResult(ret);
    }

    public void limparListaEventos() throws Exception {
        int ret = ACBrMDFeLib.INSTANCE.MDFE_LimparListaEventos();
        checkResult(ret);
    }

    public void assinar() throws Exception {
        int ret = ACBrMDFeLib.INSTANCE.MDFE_Assinar();
        checkResult(ret);
    }

    public void validar() throws Exception {
        int ret = ACBrMDFeLib.INSTANCE.MDFE_Validar();
        checkResult(ret);
    }

    public String validarRegrasdeNegocios() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrMDFeLib.INSTANCE.MDFE_ValidarRegrasdeNegocios(buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);
    }

    public String verificarAssinatura() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrMDFeLib.INSTANCE.MDFE_VerificarAssinatura(buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);
    }

    public String gerarChave(int aCodigoUf, int aCodigoNumerico, int aModelo, int aSerie, int aNumero,
            int aTpEmi, Date aEmissao, String acpfcnpj) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        String pattern = "dd/MM/yyyy";
        DateFormat df = new SimpleDateFormat(pattern);

        int ret = ACBrMDFeLib.INSTANCE.MDFE_GerarChave(aCodigoUf, aCodigoNumerico, aModelo,
                aSerie, aNumero, aTpEmi, df.format(aEmissao),
                toUTF8(acpfcnpj), buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);
    }

    public String obterCertificados() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrMDFeLib.INSTANCE.MDFE_ObterCertificados(buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }

    public String getPath(TipoPathMDFe tipo) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrMDFeLib.INSTANCE.MDFE_GetPath(tipo.asInt(), buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);
    }

    public String getPathEvento(String aCodEvento) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrMDFeLib.INSTANCE.MDFE_GetPathEvento(toUTF8(aCodEvento), buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);
    }

    public String statusServico() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrMDFeLib.INSTANCE.MDFE_StatusServico(buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);
    }

    public String consultar(String eChaveOuNFe) throws Exception {
        return consultar(eChaveOuNFe, false);
    }

    public String consultar(String eChaveOuNFe, boolean AExtrairEventos) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrMDFeLib.INSTANCE.MDFE_Consultar(toUTF8(eChaveOuNFe), AExtrairEventos, buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);
    }

    public String consultarRecibo(String aRecibo) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrMDFeLib.INSTANCE.MDFE_ConsultarRecibo(toUTF8(aRecibo), buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);
    }

    public String ConsultarMDFeNaoEnc(String aCNPJ) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrMDFeLib.INSTANCE.MDFE_ConsultarMDFeNaoEnc(toUTF8(aCNPJ), buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);
    }

    public String enviar(int aLote) throws Exception {
        return enviar(aLote, false, false, false);
    }

    public String enviar(int aLote, boolean imprimir) throws Exception {
        return enviar(aLote, imprimir, false, false);
    }

    public String enviar(int aLote, boolean imprimir, boolean sincrono, boolean zipado) throws Exception {
        
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrMDFeLib.INSTANCE.MDFE_Enviar(aLote, imprimir, sincrono, buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);
    }

    public String cancelar(String aChave, String aJustificativa, String aCNPJ) throws Exception {
        return cancelar(aChave, aJustificativa, aCNPJ, 1);
    }

    public String cancelar(String aChave, String aJustificativa, String aCNPJ, int aLote) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrMDFeLib.INSTANCE.MDFE_Cancelar(toUTF8(aChave), toUTF8(aJustificativa), toUTF8(aCNPJ), aLote, buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);
    }

    public String enviarEvento(int idLote) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrMDFeLib.INSTANCE.MDFE_EnviarEvento(idLote, buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);
    }

    public String distribuicaoDFeporUltNSU(int acUFAutor, String aCNPJCPF, String eultNsu) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrMDFeLib.INSTANCE.MDFE_DistribuicaoDFePorUltNSU(acUFAutor, toUTF8(aCNPJCPF), toUTF8(eultNsu), buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);
    }

    public String distribuicaoDFeporNSU(int acUFAutor, String aCNPJCPF, String aNSU) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrMDFeLib.INSTANCE.MDFE_DistribuicaoDFePorNSU(acUFAutor, toUTF8(aCNPJCPF), toUTF8(aNSU), buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);
    }

    public String distribuicaoDFeporChave(int acUFAutor, String aCNPJCPF, String aChave) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrMDFeLib.INSTANCE.MDFE_DistribuicaoDFePorChave(acUFAutor, aCNPJCPF, aChave, buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);
    }

    public void enviarEmail(String aPara, String aChaveNFe, boolean aEnviarPDF, String aAssunto) throws Exception {
        enviarEmail(aPara, aChaveNFe, aEnviarPDF, aAssunto, "", "", "");
    }

    public void enviarEmail(String aPara, String aChaveNFe, boolean aEnviarPDF, String aAssunto,
            String aEmailCC, String aAnexos, String aMesagem) throws Exception {
        int ret = ACBrMDFeLib.INSTANCE.MDFE_EnviarEmail(aPara, aChaveNFe, aEnviarPDF, aAssunto,
                aEmailCC, aAnexos, aMesagem);
        checkResult(ret);
    }

    public void enviarEmailEvento(String aPara, String aChaveEvento, String aChaveNFe, boolean aEnviarPDF,
            String aAssunto) throws Exception {
        enviarEmailEvento(aPara, aChaveEvento, aChaveNFe, aEnviarPDF, aAssunto, "", "", "");
    }

    public void enviarEmailEvento(String aPara, String aChaveEvento, String aChaveNFe, boolean aEnviarPDF,
            String aAssunto, String aEmailCC, String aAnexos, String aMesagem) throws Exception {
        int ret = ACBrMDFeLib.INSTANCE.MDFE_EnviarEmailEvento(aPara, aChaveEvento, aChaveNFe, aEnviarPDF,
                aAssunto, aEmailCC, aAnexos, aMesagem);
        checkResult(ret);
    }

    public void imprimir() throws Exception {
        imprimir("", 1, "", null);
    }

    public void imprimir(String cImpressora, int nNumCopias, String cProtocolo, Boolean bMostrarPreview) throws Exception {
        String mostrarPreview = bMostrarPreview != null ? bMostrarPreview ? "1" : "0" : "";
        int ret = ACBrMDFeLib.INSTANCE.MDFE_Imprimir(toUTF8(cImpressora), nNumCopias, toUTF8(cProtocolo), toUTF8(mostrarPreview));
        checkResult(ret);
    }

    public void imprimirPDF() throws Exception {
        int ret = ACBrMDFeLib.INSTANCE.MDFE_ImprimirPDF();
        checkResult(ret);
    }

    public void imprimirEvento(String eArquivoXmlCTe, String eArquivoXmlEvento) throws Exception {
        int ret = ACBrMDFeLib.INSTANCE.MDFE_ImprimirEvento(toUTF8(eArquivoXmlCTe), toUTF8(eArquivoXmlEvento));
        checkResult(ret);
    }

    public void imprimirEventoPDF(String eArquivoXmlCTe, String eArquivoXmlEvento) throws Exception {
        int ret = ACBrMDFeLib.INSTANCE.MDFE_ImprimirEventoPDF(toUTF8(eArquivoXmlCTe), toUTF8(eArquivoXmlEvento));
        checkResult(ret);
    }

    public void ConfigImportar(String eArqConfig) throws Exception {

        int ret = ACBrMDFeLib.INSTANCE.MDFE_ConfigImportar(eArqConfig);
        checkResult(ret);

    }

    public String ConfigExportar() throws Exception {

        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrMDFeLib.INSTANCE.MDFE_ConfigExportar(buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());

    }

    public String EncerrarMDFe(String eChaveOuMDFe, Date eDtEnc, String cMunicipioDescarga, String nCNPJ, String nProtocolo) throws Exception {

        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        String pattern = "dd/MM/yyyy";
        DateFormat df = new SimpleDateFormat(pattern);

        int ret = ACBrMDFeLib.INSTANCE.MDFE_EncerrarMDFe(eChaveOuMDFe, df.format(eDtEnc), cMunicipioDescarga, nCNPJ, nProtocolo, buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);
    }

    @Override
    protected void UltimoRetorno(ByteBuffer buffer, IntByReference bufferLen) {
        ACBrMDFeLib.INSTANCE.MDFE_UltimoRetorno(buffer, bufferLen);
    }

    public void EncerrarMDFe(String eChave, Date date, String cMunicipio) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }
}
