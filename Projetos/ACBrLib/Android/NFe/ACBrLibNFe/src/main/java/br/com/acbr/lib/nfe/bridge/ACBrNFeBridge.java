package br.com.acbr.lib.nfe.bridge;

import com.sun.jna.Library;
import com.sun.jna.Pointer;
import com.sun.jna.WString;
import com.sun.jna.ptr.IntByReference;
import com.sun.jna.ptr.PointerByReference;

import java.nio.ByteBuffer;

public interface ACBrNFeBridge  extends Library {
    /* Métodos de ACBrLibComum */
    public int NFE_Inicializar(PointerByReference libHandle, String eArqConfig, String eChaveCrypt);
    public int NFE_Finalizar(Pointer libHandle);

    public int NFE_ConfigGravarValor(Pointer libHandle,String eSessao,String  eChave, String eValor);
    public int NFE_ConfigLer(Pointer libHandle, String eArqConfig);
    public int NFE_ConfigLerValor(Pointer libHandle,String eSessao, String eChave, ByteBuffer sValor, IntByReference esTamanaho);
    public int NFE_ConfigImportar(Pointer libHandle,String eArqConfig);
    public int NFE_ConfigExportar(Pointer libHandle,ByteBuffer sMensagem, IntByReference esTamanaho);
    public int NFE_ConfigGravar(Pointer libHandle,String eArqConfig);

    public int NFE_UltimoRetorno(Pointer libHandle,ByteBuffer sMensagem, IntByReference sTamanho);
    public int NFE_Nome(Pointer libHandle,ByteBuffer sNome, IntByReference esTamanaho);
    public int NFE_Versao(Pointer libHandle,ByteBuffer sVersao, IntByReference esTamanaho);

    public int NFE_OpenSSLInfo(Pointer libHandle,ByteBuffer sOpenSslInfo, IntByReference esTamanho);

    /* Métodos de ACBrLibNFe */
    public int NFE_CarregarXML(Pointer libHandle, String eArquivoOuXML);

    public int NFE_CarregarINI(Pointer libHandle, String eArquivoOuINI);

    public int NFE_ObterXml(Pointer libHandle, int AIndex, ByteBuffer sResposta, IntByReference esTamanho);

    public int NFE_GravarXml(Pointer libHandle, int AIndex, String eNomeArquivo, String ePathArquivo);

    public int NFE_ObterIni(Pointer libHandle, int AIndex, ByteBuffer sResposta, IntByReference esTamanho);

    public int NFE_GravarIni(Pointer libHandle, int AIndex, String eNomeArquivo, String ePathArquivo);

    public int NFE_CarregarEventoXML(Pointer libHandle, String eArquivoOuXML);

    public int NFE_CarregarEventoINI(Pointer libHandle, String eArquivoOuINI);

    public int NFE_LimparLista(Pointer libHandle);

    public int NFE_LimparListaEventos(Pointer libHandle);

    public int NFE_Assinar(Pointer libHandle);

    public int NFE_Validar(Pointer libHandle);

    public int NFE_ValidarRegrasdeNegocios(Pointer libHandle, ByteBuffer sResposta, IntByReference esTamanho);

    public int NFE_VerificarAssinatura(Pointer libHandle, ByteBuffer sResposta, IntByReference esTamanho);

    public int NFE_GerarChave(Pointer libHandle, int ACodigoUF, int ACodigoNumerico, int AModelo, int ASerie,
                              int ANumero, int ATpEmi, String AEmissao, String ACNPJCPF, ByteBuffer sResposta, IntByReference esTamanho);

    public int NFE_ObterCertificados(Pointer libHandle, ByteBuffer sResposta, IntByReference esTamanho);

    public int NFE_GetPath(Pointer libHandle, int ATipo, ByteBuffer sResposta, IntByReference esTamanho);

    public int NFE_GetPathEvento(Pointer libHandle, String ACodEvento, ByteBuffer sResposta, IntByReference esTamanho);

    public int NFE_StatusServico(Pointer libHandle, ByteBuffer sResposta, IntByReference esTamanho);

    public int NFE_Consultar(Pointer libHandle, String eChaveOuNFe, boolean AExtrairEventos, ByteBuffer sResposta, IntByReference esTamanho);

    public int NFE_Inutilizar(Pointer libHandle, String ACNPJ, String AJustificativa, int Ano, int Modelo, int Serie,
                              int NumeroInicial, int NumeroFinal, ByteBuffer sResposta, IntByReference esTamanho);

    public int NFE_Enviar(Pointer libHandle, int ALote, boolean AImprimir, boolean ASincrono, boolean AZipado,
                          ByteBuffer sResposta, IntByReference esTamanho);

    public int NFE_ConsultarRecibo(Pointer libHandle, String ARecibo, ByteBuffer sResposta, IntByReference esTamanho);

    public int NFE_Cancelar(Pointer libHandle, String eChave, String eJustificativa, String eCNPJCPF, int ALote,
                            ByteBuffer sResposta, IntByReference esTamanho);

    public int NFE_EnviarEvento(Pointer libHandle, int idLote, ByteBuffer sResposta, IntByReference esTamanho);

    public int NFE_ConsultaCadastro(Pointer libHandle, String cUF, String nDocumento, boolean nIE,
                                    ByteBuffer sResposta, IntByReference esTamanho);

    public int NFE_DistribuicaoDFePorUltNSU(Pointer libHandler, int AcUFAutor, String eCNPJCPF, String eultNSU,
                                            ByteBuffer sResposta, IntByReference esTamanho);

    public int NFE_DistribuicaoDFe(Pointer libHandle, int AcUFAutor, String eCNPJCPF, String eultNSU, String eArquivoOuXML,
                                    ByteBuffer sResposta, IntByReference esTamanho);

    public int NFE_DistribuicaoDFePorNSU(Pointer libHandle, int AcUFAutor, String eCNPJCPF, String eNSU,
                                          ByteBuffer sResposta, IntByReference esTamanho);

    public int NFE_DistribuicaoDFePorChave(Pointer libHandle, int AcUFAutor, String eCNPJCPF, String echNFe,
                                            ByteBuffer sResposta, IntByReference esTamanho);

    public int NFE_EnviarEmail(Pointer libHandle, String ePara, String eChaveNFe, boolean AEnviaPDF,
                               String eAssunto, String eCC, String eAnexos, String eMensagem);

    public int NFE_EnviarEmailEvento(Pointer libHandle, String ePara, String eChaveEvento, String eChaveNFe, boolean AEnviaPDF,
                                     String eAssunto, String eCC, String eAnexos, String eMensagem);

    public int NFE_Imprimir(Pointer libHandle, String cImpressora, int nNumCopias, String cProtocolo, String bMostrarPreview,
                            String cMarcaDagua, String bViaConsumidor, String bSimplificado);

    public int NFE_ImprimirPDF(Pointer libHandle);

    public int NFE_SalvarPDF(Pointer libHandle, ByteBuffer sResposta, IntByReference esTamanho);

    public int NFE_ImprimirEvento(Pointer libHandle, String eArquivoXmlNFe, String eArquivoXmlEvento);

    public int NFE_ImprimirEventoPDF(Pointer libHandle, String eArquivoXmlNFe, String eArquivoXmlEvento);

    public int NFE_SalvarEventoPDF(Pointer libHandle, String eArquivoXmlNFe, String eArquivoXmlEvento,
                                   ByteBuffer sResposta, IntByReference esTamanho);

    public int NFE_ImprimirInutilizacao(Pointer libHandle, String eArquivoXml);

    public int NFE_ImprimirInutilizacaoPDF(Pointer libHandle, String eArquivoXml);

    public int NFE_SalvarInutilizacaoPDF(Pointer libHandle, String eArquivoXml, ByteBuffer sResposta, IntByReference esTamanho);

}