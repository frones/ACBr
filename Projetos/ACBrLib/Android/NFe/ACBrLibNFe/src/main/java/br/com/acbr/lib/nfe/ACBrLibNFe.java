package br.com.acbr.lib.nfe;

import android.util.Log;

import br.com.acbr.lib.comum.ACBrLibBase;
import br.com.acbr.lib.comum.ACBrLibBuffer;
import br.com.acbr.lib.comum.ACBrLibInstance;
import br.com.acbr.lib.nfe.bridge.ACBrNFeBridge;
import com.sun.jna.Library;
import com.sun.jna.Pointer;
import com.sun.jna.ptr.IntByReference;
import com.sun.jna.ptr.PointerByReference;

import java.nio.ByteBuffer;

/**
 * AcbrlibNFe é uma classe de alto nível para ACBrLibNFe para Android
 * Essa classe permite o uso do componente ACBrNFe no Android.
 *
 */

public class ACBrLibNFe extends ACBrLibBase {

    /**
     * @param configFile
     * @param configCryptKey
     */
    public ACBrLibNFe(String configFile, String configCryptKey) {
        super(configFile, configCryptKey);
    }

    //region
    /**
     *  Métodos que ** PRECISAM ** ser sobreescritos de ACBrLibBase, para correto funcionamento da Lib
     */

    @Override
    protected Library getInstance() {
        return ACBrLibInstance.getInstance(this.getLibName(), ACBrNFeBridge.class);
    }

    @Override
    protected String getLibName(){
        return "ACBrLibNFe";
    }

    //endregion

    //region
    /** Métodos específicos de ACBrLibNFe
     */

    /**
     * Método para uso da NFe (Nota Fiscal Eletrônica).
     *
     * @param eArquivoOuXML Path com o nome do arquivo XML a ser lido ou o conteúdo do XML.
     * @return
     */
    public void CarregarXML(String eArquivoOuXML) throws Exception {
        Log.i(this.getLibName(), "NFE_CarregarXML, eArquivoOuXML = " + eArquivoOuXML);
        int status = ((ACBrNFeBridge) this.getInstance()).NFE_CarregarXML(this.getHandle(), eArquivoOuXML);
        Log.i(this.getLibName(),"NFE_CarregarXML, status = " + status);
        checkResult(status);
    }

    /**
     * @param eArquivoOuINI Path com o nome do arquivo INI a ser lido ou o conteúdo do INI.
     * @return
     */
    public void CarregarINI(String eArquivoOuINI) throws Exception {
        Log.i(this.getLibName(), "NFE_CarregarINI, eArquivoOuINI = " + eArquivoOuINI);
        int status = ((ACBrNFeBridge) this.getInstance()).NFE_CarregarINI(this.getHandle(), eArquivoOuINI);
        Log.i(this.getLibName(),"NFE_CarregarINI, status = " + status);
        checkResult(status);
    }

    /**
     * @param AIndex Posição da NFe na lista, a lista inicia em 0.
     * @return
     */
    public String ObterXml(int AIndex) throws Exception {
        ACBrLibBuffer buffer =  new ACBrLibBuffer();
        Log.i(this.getLibName(), "NFE_ObterXml, AIndex = " + AIndex);
        int status = ((ACBrNFeBridge) this.getInstance()).NFE_ObterXml(this.getHandle(), AIndex, buffer.bufferData, buffer.bufferSizeNeeded);
        Log.i(this.getLibName(),"NFE_ObterXml, status = " + status);
        checkResult(status);
        return checkBuffer(buffer);
    }

    /**
     * @param AIndex Posição da NFe na lista, a lista inicia em 0.
     * @param eNomeArquivo Nome do arquivo xml a ser salvo.
     * @param ePathArquivo Local onde será salvo o xml.
     * @return
     */
    public void GravarXml(int AIndex, String eNomeArquivo, String ePathArquivo) throws Exception {
        Log.i(this.getLibName(), "NFE_GravarXml, AIndex = " + AIndex + ", eNomeArquivo = " + eNomeArquivo + ", ePathArquivo = " + ePathArquivo);
        int status = ((ACBrNFeBridge) this.getInstance()).NFE_GravarXml(this.getHandle(), AIndex, eNomeArquivo, ePathArquivo);
        Log.i(this.getLibName(),"NFE_GravarXml, status = " + status);
        checkResult(status);
    }

    /**
     * @param AIndex Posição da NFe na lista, a lista inicia em 0.
     * @return
     */
    public String ObterIni(int AIndex) throws Exception {
        ACBrLibBuffer buffer =  new ACBrLibBuffer();
        Log.i(this.getLibName(), "NFE_ObterIni, AIndex = " + AIndex);
        int status = ((ACBrNFeBridge) this.getInstance()).NFE_ObterIni(this.getHandle(), AIndex, buffer.bufferData, buffer.bufferSizeNeeded);
        Log.i(this.getLibName(),"NFE_ObterIni, status = " + status);
        checkResult(status);
        return checkBuffer(buffer);
    }

    /**
     * @param AIndex Posição da NFe na lista, a lista inicia em 0.
     * @param eNomeArquivo Nome do arquivo xml a ser salvo.
     * @param ePathArquivo Local onde será salvo o xml.
     * @return
     */
    public void GravarIni(int AIndex, String eNomeArquivo, String ePathArquivo) throws Exception {
        Log.i(this.getLibName(), "NFE_GravarIni, AIndex = " + AIndex + ", eNomeArquivo = " + eNomeArquivo + ", ePathArquivo = " + ePathArquivo);
        int status = ((ACBrNFeBridge) this.getInstance()).NFE_GravarIni(this.getHandle(), AIndex, eNomeArquivo, ePathArquivo);
        Log.i(this.getLibName(),"NFE_GravarIni, status = " + status);
        checkResult(status);
    }

    /**
    * @param eArquivoOuXML Path com o nome do arquivo XML a ser lido ou o conteúdo do XML.
    * @return
    */
    public void CarregarEventoXML(String eArquivoOuXML) throws Exception {
        Log.i(this.getLibName(), "NFE_CarregarEventoXML, eArquivoOuXML = " + eArquivoOuXML);
        int status = ((ACBrNFeBridge) this.getInstance()).NFE_CarregarEventoXML(this.getHandle(), eArquivoOuXML);
        Log.i(this.getLibName(),"NFE_CarregarEventoXML, status = " + status);
        checkResult(status);
    }

    /**
     * @param eArquivoOuINI Path com o nome do arquivo INI a ser lido ou o conteúdo do INI.
     * @return
     */
    public void CarregarEventoINI(String eArquivoOuINI) throws Exception {
        Log.i(this.getLibName(), "NFE_CarregarEventoINI, eArquivoOuINI = " + eArquivoOuINI);
        int status = ((ACBrNFeBridge) this.getInstance()).NFE_CarregarEventoINI(this.getHandle(), eArquivoOuINI);
        Log.i(this.getLibName(),"NFE_CarregarEventoINI, status = " + status);
        checkResult(status);
    }

    public void LimparLista() throws Exception {
        Log.i(this.getLibName(), "NFE_LimparLista");
        int status = ((ACBrNFeBridge) this.getInstance()).NFE_LimparLista(this.getHandle());
        Log.i(this.getLibName(),"NFE_LimparLista, status = " + status);
        checkResult(status);
    }

    public void LimparListaEventos() throws Exception {
        Log.i(this.getLibName(), "NFE_LimparListaEventos");
        int status = ((ACBrNFeBridge) this.getInstance()).NFE_LimparListaEventos(this.getHandle());
        Log.i(this.getLibName(),"NFE_LimparListaEventos, status = " + status);
        checkResult(status);
    }

    public void Assinar() throws Exception {
        Log.i(this.getLibName(), "NFE_Assinar");
        int status = ((ACBrNFeBridge) this.getInstance()).NFE_Assinar(this.getHandle());
        Log.i(this.getLibName(),"NFE_Assinar, status = " + status);
        checkResult(status);
    }

    public void Validar() throws Exception {
        Log.i(this.getLibName(), "NFE_Validar");
        int status = ((ACBrNFeBridge) this.getInstance()).NFE_Validar(this.getHandle());
        Log.i(this.getLibName(),"NFE_Validar, status = " + status);
        checkResult(status);
    }

    public String ValidarRegrasdeNegocios() throws Exception {
        ACBrLibBuffer buffer =  new ACBrLibBuffer();
        Log.i(this.getLibName(), "NFE_ValidarRegrasdeNegocios");
        int status = ((ACBrNFeBridge) this.getInstance()).NFE_ValidarRegrasdeNegocios(this.getHandle(), buffer.bufferData, buffer.bufferSizeNeeded);
        Log.i(this.getLibName(),"NFE_ValidarRegrasdeNegocios, status = " + status);
        checkResult(status);
        return checkBuffer(buffer);
    }

    public String VerificarAssinatura() throws Exception {
        ACBrLibBuffer buffer = new ACBrLibBuffer();
        Log.i(this.getLibName(), "NFE_VerificarAssinatura");
        int status = ((ACBrNFeBridge) this.getInstance()).NFE_VerificarAssinatura(this.getHandle(), buffer.bufferData, buffer.bufferSizeNeeded);
        Log.i(this.getLibName(),"NFE_VerificarAssinatura, status = " + status);
        checkResult(status);
        return checkBuffer(buffer);
    }

    /**
     * @param ACodigoUF Código da UF para gerar a chave.
     * @param ACodigoNumerico Código numérico da nota fiscal.
     * @param AModelo Modelo do documento 55 ou 65.
     * @param ASerie  Série da nota fiscal.
     * @param ANumero Número da nota fiscal.
     * @param ATpEmi Tipo de emissão -> 1 - teNormal, 2 - teContingencia, 3 - teSCAN,  4 - teDPEC, 5 - teFSDA, 6 - teSVCAN, 7 - teSVCRS, 8 - teSVCSP, 9 - teOffLine
     * @param AEmissao Data da emissão da NFe no formato [dd/MM/yyyy].
     * @param ACNPJCPF CPF/CNPJ do emissor da nota.
     * @return
     */
    public String GerarChave(int ACodigoUF, int ACodigoNumerico, int AModelo, int ASerie, int ANumero, int ATpEmi,
                             String AEmissao, String ACNPJCPF) throws Exception {
        ACBrLibBuffer buffer = new ACBrLibBuffer();
        Log.i(this.getLibName(), "NFE_GerarChave");
        int status = ((ACBrNFeBridge) this.getInstance()).NFE_GerarChave(this.getHandle(), ACodigoUF, ACodigoNumerico, AModelo, ASerie,
                ANumero, ATpEmi, AEmissao, ACNPJCPF, buffer.bufferData, buffer.bufferSizeNeeded);
        Log.i(this.getLibName(),"NFE_GerarChave, status = " + status);
        checkResult(status);
        return checkBuffer(buffer);
    }

    public String ObterCertificados() throws Exception {
        ACBrLibBuffer buffer = new ACBrLibBuffer();
        Log.i(this.getLibName(), "NFE_ObterCertificados");
        int status = ((ACBrNFeBridge) this.getInstance()).NFE_ObterCertificados(this.getHandle(), buffer.bufferData, buffer.bufferSizeNeeded);
        Log.i(this.getLibName(),"NFE_ObterCertificados, status = " + status);
        checkResult(status);
        return checkBuffer(buffer);
    }

    /**
     * @param Atipo Tipo de path que será retornado -> 0 NFe, 1 Inutilização, 2 CCe, 3 Cancelamento.
     * @return
     */
    public String GetPath(int Atipo) throws Exception {
        ACBrLibBuffer buffer = new ACBrLibBuffer();
        Log.i(this.getLibName(), "NFE_GetPath");
        int status = ((ACBrNFeBridge) this.getInstance()).NFE_GetPath(this.getHandle(), Atipo, buffer.bufferData, buffer.bufferSizeNeeded);
        Log.i(this.getLibName(),"NFE_GetPath, status = " + status);
        checkResult(status);
        return checkBuffer(buffer);
    }

    /**
     * @param ACodEvento O código do evento.
     * @return
     */
    public String GetPathEvento(String ACodEvento) throws Exception {
        ACBrLibBuffer buffer = new ACBrLibBuffer();
        Log.i(this.getLibName(), "NFE_GetPathEvento");
        int status = ((ACBrNFeBridge) this.getInstance()).NFE_GetPathEvento(this.getHandle(), ACodEvento, buffer.bufferData, buffer.bufferSizeNeeded);
        Log.i(this.getLibName(),"NFE_GetPathEvento, status = " + status);
        checkResult(status);
        return checkBuffer(buffer);
    }

    public String StatusServico() throws Exception {
        ACBrLibBuffer buffer = new ACBrLibBuffer();
        Log.i(this.getLibName(), "NFE_StatusServico");
        int status = ((ACBrNFeBridge) this.getInstance()).NFE_StatusServico(this.getHandle(), buffer.bufferData, buffer.bufferSizeNeeded);
        Log.i(this.getLibName(),"NFE_StatusServico, status = " + status);
        checkResult(status);
        return checkBuffer(buffer);
    }

    /**
     * @param eChaveOuNFe Path com o nome do arquivo XML a ser consultado ou o conteúdo do XML.
     * @param AExtrairEventos Informe se deve ou não extrair os eventos, se houver os mesmos na reposta.
     * @return
     */
    public String Consultar (String eChaveOuNFe, boolean AExtrairEventos) throws Exception {
        ACBrLibBuffer buffer = new ACBrLibBuffer();
        Log.i(this.getLibName(), "NFE_Consultar");
        int status = ((ACBrNFeBridge) this.getInstance()).NFE_Consultar(this.getHandle(), eChaveOuNFe, AExtrairEventos, buffer.bufferData, buffer.bufferSizeNeeded);
        Log.i(this.getLibName(),"NFE_Consultar, status = " + status);
        checkResult(status);
        return checkBuffer(buffer);
    }

    /**
     * @param ACNPJ CNPJ do emitente.
     * @param AJustificativa Motivo por estar solicitando a Inutilização.
     * @param Ano Ano.
     * @param Modelo Modelo deve ser informado 55 para NF-e ou 65 para NFC-e.
     * @param Serie Serie do Documento Fiscal.
     * @param NumeroInicial Numero Inicial a que se deseja Inutilizar.
     * @param NumeroFinal Numero Final a se se deseja Inutilizar.
     * @return
     */
    public String Inutilizar(String ACNPJ, String AJustificativa, int Ano, int Modelo, int Serie,
                             int NumeroInicial, int NumeroFinal) throws Exception {
        ACBrLibBuffer buffer = new ACBrLibBuffer();
        Log.i(this.getLibName(), "NFE_Inutilizar");
        int status = ((ACBrNFeBridge) this.getInstance()).NFE_Inutilizar(this.getHandle(), ACNPJ, AJustificativa, Ano, Modelo, Serie,
                NumeroInicial, NumeroFinal, buffer.bufferData, buffer.bufferSizeNeeded);
        Log.i(this.getLibName(),"NFE_Inutilizar, status = " + status);
        checkResult(status);
        return checkBuffer(buffer);
    }

    /**
     * @param ALote Numero do Lote a ser enviado.
     * @param AImprimir Se True imprime o DANFe caso o NF-e for autorizado.
     * @param ASincrono Se True imprime o envia em modo síncrono.
     * @param AZipado Se True imprime o envia o arquivo zipado.
     * @return
     */
    public String Enviar(int ALote, boolean AImprimir, boolean ASincrono, boolean AZipado) throws Exception {
        ACBrLibBuffer buffer = new ACBrLibBuffer();
        Log.i(this.getLibName(), "NFE_Enviar");
        int status = ((ACBrNFeBridge) this.getInstance()).NFE_Enviar(this.getHandle(), ALote, AImprimir, ASincrono, AZipado,
                buffer.bufferData, buffer.bufferSizeNeeded);
        Log.i(this.getLibName(),"NFE_Enviar, status = " + status);
        checkResult(status);
        return checkBuffer(buffer);
    }

    /**
     * @param ARecibo Número do recibo para consulta.
     * @return
     */
    public String ConsultarRecibo(String ARecibo) throws Exception {
        ACBrLibBuffer buffer = new ACBrLibBuffer();
        Log.i(this.getLibName(), "NFE_ConsultarRecibo");
        int status = ((ACBrNFeBridge) this.getInstance()).NFE_ConsultarRecibo(this.getHandle(), ARecibo, buffer.bufferData, buffer.bufferSizeNeeded);
        Log.i(this.getLibName(),"NFE_ConsultarRecibo, status = " + status);
        checkResult(status);
        return checkBuffer(buffer);
    }

    /**
     * @param eChave Chave de acesso do XML a ser cancelado.
     * @param eJustificativa Motivo do cancelamento.
     * @param eCNPJCPF CNPJ do emitente.
     * @param ALote Numero do Lote do evento de cancelamento.
     * @return
     */
    public String Cancelar (String eChave, String eJustificativa, String eCNPJCPF, int ALote) throws Exception {
        ACBrLibBuffer buffer = new ACBrLibBuffer();
        Log.i(this.getLibName(), "NFE_Cancelar");
        int status = ((ACBrNFeBridge) this.getInstance()).NFE_Cancelar(this.getHandle(), eChave, eJustificativa, eCNPJCPF, ALote,
                buffer.bufferData, buffer.bufferSizeNeeded);
        Log.i(this.getLibName(),"NFE_Cancelar, status = " + status);
        checkResult(status);
        return checkBuffer(buffer);
    }

    /**
     * @param idLote Numero do Lote do evento.
     * @return
     */
    public String EnviarEvento (int idLote) throws Exception {
        ACBrLibBuffer buffer = new ACBrLibBuffer();
        Log.i(this.getLibName(), "NFE_EnviarEvento");
        int status = ((ACBrNFeBridge) this.getInstance()).NFE_EnviarEvento(this.getHandle(), idLote, buffer.bufferData, buffer.bufferSizeNeeded);
        Log.i(this.getLibName(),"NFE_EnviarEvento, status = " + status);
        checkResult(status);
        return checkBuffer(buffer);
    }

    /**
     * @param cUF Sigla do estado do documento a ser consultado.
     * @param nDocumento Número do documento a ser consultado.
     * @param nIE Se passado true irá consultar pelo documento Inscrição Estadual, caso contrário irá consultar pelo CPF ou CNPJ.
     * @return
     */
    public String ConsultaCadastro(String cUF, String nDocumento, boolean nIE) throws Exception {
        ACBrLibBuffer buffer = new ACBrLibBuffer();
        Log.i(this.getLibName(), "NFE_ConsultaCadastro");
        int status = ((ACBrNFeBridge) this.getInstance()).NFE_ConsultaCadastro(this.getHandle(), cUF, nDocumento, nIE,
                buffer.bufferData, buffer.bufferSizeNeeded);
        Log.i(this.getLibName(),"NFE_ConsultaCadastro, status = " + status);
        checkResult(status);
        return checkBuffer(buffer);
    }

    /**
     * @param AcUFAutor Código da UF do autor da consulta.
     * @param eCNPJCPF CNPJ/CPF do autor da consulta.
     * @param eultNSU Numero do ultimo NSU.
     * @return
     */
    public String DistribuicaoDFePorUltNSU(int AcUFAutor, String eCNPJCPF, String eultNSU) throws Exception {
        ACBrLibBuffer buffer = new ACBrLibBuffer();
        Log.i(this.getLibName(), "NFE_DistribuicaoDFePorUltNSU");
        int status = ((ACBrNFeBridge) this.getInstance()).NFE_DistribuicaoDFePorUltNSU(this.getHandle(), AcUFAutor, eCNPJCPF, eultNSU,
                buffer.bufferData, buffer.bufferSizeNeeded);
        Log.i(this.getLibName(),"NFE_DistribuicaoDFePorUltNSU, status = " + status);
        checkResult(status);
        return checkBuffer(buffer);
    }

    /**
     * @param AcUFAutor Código da UF do autor da consulta.
     * @param eCNPJCPF CNPJ/CPF do autor da consulta.
     * @param eultNSU Numero do ultimo NSU.
     * @param eArquivoOuXML Path com o nome do arquivo XML a ser lido ou o conteúdo do XML.
     * @return
     */
    public String DistribuicaoDFe(int AcUFAutor, String eCNPJCPF, String eultNSU, String eArquivoOuXML) throws  Exception {
        ACBrLibBuffer buffer = new ACBrLibBuffer();
        Log.i(this.getLibName(), "NFE_DistribuicaoDFe");
        int status = ((ACBrNFeBridge) this.getInstance()).NFE_DistribuicaoDFe(this.getHandle(), AcUFAutor, eCNPJCPF, eultNSU, eArquivoOuXML,
                buffer.bufferData, buffer.bufferSizeNeeded);
        Log.i(this.getLibName(),"NFE_DistribuicaoDFe, status = " + status);
        checkResult(status);
        return checkBuffer(buffer);
    }

    /**
     * @param AcUFAutor Código da UF do autor da consulta.
     * @param eCNPJCPF CNPJ/CPF do autor da consulta.
     * @param eNSU Numero do NSU do documento.
     * @return
     */
    public String DistribuicaoDFePorNSU(int AcUFAutor, String eCNPJCPF, String eNSU) throws Exception {
        ACBrLibBuffer buffer = new ACBrLibBuffer();
        Log.i(this.getLibName(), "NFE_DistribuicaoDFePorNSU");
        int status = ((ACBrNFeBridge) this.getInstance()).NFE_DistribuicaoDFePorNSU(this.getHandle(), AcUFAutor, eCNPJCPF, eNSU,
                buffer.bufferData, buffer.bufferSizeNeeded);
        Log.i(this.getLibName(),"NFE_DistribuicaoDFePorNSU, status = " + status);
        checkResult(status);
        return checkBuffer(buffer);
    }

    /**
     * @param AcUFAutor Código da UF do autor da consulta.
     * @param eCNPJCPF CNPJ/CPF do autor da consulta.
     * @param echNFe Chave do NFe.
     * @return
     */
    public String DistribuicaoDFePorChave(int AcUFAutor, String eCNPJCPF, String echNFe) throws  Exception{
        ACBrLibBuffer buffer = new ACBrLibBuffer();
        Log.i(this.getLibName(), "NFE_DistribuicaoDFePorChave");
        int status = ((ACBrNFeBridge) this.getInstance()).NFE_DistribuicaoDFePorChave(this.getHandle(), AcUFAutor, eCNPJCPF, echNFe,
                buffer.bufferData, buffer.bufferSizeNeeded);
        Log.i(this.getLibName(),"NFE_DistribuicaoDFePorChave, status = " + status);
        checkResult(status);
        return checkBuffer(buffer);
    }

    /**
     * @param ePara Destinatário.
     * @param eChaveNFe Path ou conteúdo do xml.
     * @param AEnviaPDF Se True gera o PDF do DANFe e anexa ao e-mail.
     * @param eAssunto Texto contendo o assunto do e-mail.
     * @param eCC endereços separados por ponto e vírgula que receberão uma cópia do e-mail.
     * @param eAnexos Path com o nome de arquivos separados por ponto e vírgula a serem anexados ao e-mail.
     * @param eMensagem Texto referente a mensagem do e-mail.
     */
    public void EnviarEmail(String ePara, String eChaveNFe, boolean AEnviaPDF,
                            String eAssunto, String eCC, String eAnexos, String eMensagem) throws Exception {
        Log.i(this.getLibName(), "NFE_EnviarEmail");
        int status = ((ACBrNFeBridge) this.getInstance()).NFE_EnviarEmail(this.getHandle(), ePara, eChaveNFe, AEnviaPDF,
                eAssunto, eCC, eAnexos, eMensagem);
        Log.i(this.getLibName(),"NFE_EnviarEmail, status = " + status);
        checkResult(status);
    }

    /**
     * @param ePara Destinatário.
     * @param eChaveEvento Path com o nome do arquivo XML do Evento a ser anexado ao e-mail.
     * @param eChaveNFe Path com o nome do arquivo XML do NFe a ser anexado ao e-mail.
     * @param AEnviaPDF Se True gera o PDF do DANFe e anexa ao e-mail.
     * @param eAssunto Texto contendo o assunto do e-mail.
     * @param eCC endereços separados por ponto e vírgula que receberão uma cópia do e-mail.
     * @param eAnexos Path com o nome de arquivos separados por ponto e vírgula a serem anexados ao e-mail.
     * @param eMensagem Texto referente a mensagem do e-mail.
     */
    public void EnviarEmailEvento(String ePara, String eChaveEvento, String eChaveNFe, boolean AEnviaPDF,
                                  String eAssunto, String eCC, String eAnexos, String eMensagem) throws Exception {
        Log.i(this.getLibName(), "NFE_EnviarEmailEvento");
        int status = ((ACBrNFeBridge) this.getInstance()).NFE_EnviarEmailEvento(this.getHandle(), ePara, eChaveEvento, eChaveNFe, AEnviaPDF,
                eAssunto, eCC, eAnexos, eMensagem);
        Log.i(this.getLibName(),"NFE_EnviarEmailEvento, status = " + status);
        checkResult(status);
    }

    /**
     * @param cImpressora Nome da impressora onde será impresso o documento, senão informado será usado a impressora informada nas configurações.
     * @param nNumCopias Quantidade de copias a ser impresso, informe zero para usar o valor informado nas configurações.
     * @param cProtocolo Número do protocolo da NFe.
     * @param bMostrarPreview Se informado "True" exibira o preview, se "False" senão quiser mostra ou vazio para usar os valores das configurações.
     * @param cMarcaDagua Define o caminho da imagem que será usada como marca d'água na impressão da DANFe, senão informado será usado o valor da configuração.
     * @param bViaConsumidor Se informado "True" imprimira a via do consumidor, se "False" senão quiser mostra ou vazio para usar os valores das configurações, valido apenas para NFCe.
     * @param bSimplificado Se informado "True"  imprimira a DANFCe de forma simplificada, se "False" senão quiser mostra ou vazio para usar os valores das configurações, valido apenas para NFCe.
     */
    public void Imprimir(String cImpressora, int nNumCopias, String cProtocolo, String bMostrarPreview,
                         String cMarcaDagua, String bViaConsumidor, String bSimplificado) throws Exception {
        Log.i(this.getLibName(), "NFE_Imprimir");
        int status = ((ACBrNFeBridge) this.getInstance()).NFE_Imprimir(this.getHandle(), cImpressora, nNumCopias, cProtocolo,
                bMostrarPreview, cMarcaDagua, bViaConsumidor, bSimplificado);
        Log.i(this.getLibName(),"NFE_Imprimir, status = " + status);
        checkResult(status);
    }

    public void ImprimirPDF() throws Exception {
        Log.i(this.getLibName(), "NFE_ImprimirPDF");
        int status = ((ACBrNFeBridge) this.getInstance()).NFE_ImprimirPDF(this.getHandle());
        Log.i(this.getLibName(),"NFE_ImprimirPDF, status = " + status);
        checkResult(status);
    }

    public String SalvarPDF() throws Exception {
        ACBrLibBuffer buffer = new ACBrLibBuffer();
        Log.i(this.getLibName(), "NFE_SalvarPDF");
        int status = ((ACBrNFeBridge) this.getInstance()).NFE_SalvarPDF(this.getHandle(), buffer.bufferData, buffer.bufferSizeNeeded);
        Log.i(this.getLibName(),"NFE_SalvarPDF, status = " + status);
        checkResult(status);
        return checkBuffer(buffer);
    }

    /**
     * @param eArquivoXmlNFe Path do arquivo XML da NFe para impressão.
     * @param eArquivoXmlEvento Path do arquivo XML do evento para impressão.
     */
    public void ImprimirEvento(String eArquivoXmlNFe, String eArquivoXmlEvento) throws Exception {
        Log.i(this.getLibName(), "NFE_ImprimirEvento");
        int status = ((ACBrNFeBridge) this.getInstance()).NFE_ImprimirEvento(this.getHandle(), eArquivoXmlNFe, eArquivoXmlEvento);
        Log.i(this.getLibName(),"NFE_ImprimirEvento, status = " + status);
        checkResult(status);
    }

    /**
     * @param eArquivoXmlNFe Path do arquivo XML da NFe para impressão.
     * @param eArquivoXmlEvento Path do arquivo XML do evento para impressão.
     */
    public void ImprimirEventoPDF(String eArquivoXmlNFe, String eArquivoXmlEvento) throws Exception {
        Log.i(this.getLibName(), "NFE_ImprimirEventoPDF");
        int status = ((ACBrNFeBridge) this.getInstance()).NFE_ImprimirEventoPDF(this.getHandle(), eArquivoXmlNFe, eArquivoXmlEvento);
        Log.i(this.getLibName(),"NFE_ImprimirEventoPDF, status = " + status);
        checkResult(status);
    }

    /**
     * @param eArquivoXmlNFe Path do arquivo XML da NFe para  formato Base64.
     * @param eArquivoXmlEvento Path do arquivo XML do evento para  formato Base64.
     * @return
     */
    public String SalvarEventoPDF(String eArquivoXmlNFe, String eArquivoXmlEvento) throws Exception {
        ACBrLibBuffer buffer = new ACBrLibBuffer();
        Log.i(this.getLibName(), "NFE_SalvarEventoPDF");
        int status = ((ACBrNFeBridge) this.getInstance()).NFE_SalvarEventoPDF(this.getHandle(), eArquivoXmlNFe, eArquivoXmlEvento, buffer.bufferData, buffer.bufferSizeNeeded);
        Log.i(this.getLibName(),"NFE_SalvarEventoPDF, status = " + status);
        checkResult(status);
        return checkBuffer(buffer);
    }

    /**
     * @param eArquivoXml Path do arquivo XML da inutilização para impressão.
     */
    public void ImprimirInutilizacao(String eArquivoXml) throws Exception {
        Log.i(this.getLibName(), "NFE_ImprimirInutilizacao");
        int status = ((ACBrNFeBridge) this.getInstance()).NFE_ImprimirInutilizacao(this.getHandle(), eArquivoXml);
        Log.i(this.getLibName(),"NFE_ImprimirInutilizacao, status = " + status);
        checkResult(status);
    }

    /**
     * @param eArquivoXml Path do arquivo XML da inutilização para impressão.
     */
    public void ImprimirInutilizacaoPDF(String eArquivoXml) throws Exception {
        Log.i(this.getLibName(), "NFE_ImprimirInutilizacaoPDF");
        int status = ((ACBrNFeBridge) this.getInstance()).NFE_ImprimirInutilizacaoPDF(this.getHandle(), eArquivoXml);
        Log.i(this.getLibName(),"NFE_ImprimirInutilizacaoPDF, status = " + status);
        checkResult(status);
    }

    /**
     * @param eArquivoXml Path do arquivo XML da inutilização para  formato Base64.
     * @return
     */
    public String SalvarInutilizacaoPDF(String eArquivoXml) throws Exception {
        ACBrLibBuffer buffer = new ACBrLibBuffer();
        Log.i(this.getLibName(), "NFE_SalvarInutilizacaoPDF");
        int status = ((ACBrNFeBridge) this.getInstance()).NFE_SalvarInutilizacaoPDF(this.getHandle(), eArquivoXml, buffer.bufferData, buffer.bufferSizeNeeded);
        Log.i(this.getLibName(),"NFE_SalvarInutilizacaoPDF, status = " + status);
        checkResult(status);
        return checkBuffer(buffer);
    }

    //endregion

    //region
    /**
     * Métodos que devem ser sobreescritos de ACBrLibBase, para usarem a chamada específica com o prefixo
     * utilizado na LIB.. exemplo, considerando a ACBrLibNFe:  LIB_Inicializar -> NFE_Inicializar
     */

    @Override
    protected int LIB_Inicializar(PointerByReference libHandle, String eArqConfig, String eChaveCrypt){
        return ((ACBrNFeBridge) this.getInstance()).NFE_Inicializar(libHandle, eArqConfig, eChaveCrypt);
    }

    @Override
    protected int LIB_Finalizar(Pointer libHandle){
        return ((ACBrNFeBridge) this.getInstance()).NFE_Finalizar(libHandle);
    }

    @Override
    protected int LIB_ConfigGravarValor(Pointer libHandle,String eSessao,String  eChave, String eValor){
        return ((ACBrNFeBridge) this.getInstance()).NFE_ConfigGravarValor(libHandle, eSessao, eChave, eValor);
    }

    @Override
    protected int LIB_ConfigLer(Pointer libHandle,String eArqConfig){
        return ((ACBrNFeBridge) this.getInstance()).NFE_ConfigLer(libHandle, eArqConfig);
    }

    @Override
    protected int LIB_ConfigLerValor(Pointer libHandle, String eSessao, String eChave, ByteBuffer sValor, IntByReference esTamanaho){
        return ((ACBrNFeBridge) this.getInstance()).NFE_ConfigLerValor(libHandle, eSessao, eChave, sValor, esTamanaho);
    }

    @Override
    protected int LIB_ConfigImportar(Pointer libHandle,String eArqConfig){
        return ((ACBrNFeBridge) this.getInstance()).NFE_ConfigImportar(libHandle, eArqConfig);
    }

    @Override
    protected int LIB_ConfigExportar(Pointer libHandle,ByteBuffer sMensagem, IntByReference esTamanaho){
        return ((ACBrNFeBridge) this.getInstance()).NFE_ConfigExportar(libHandle, sMensagem, esTamanaho);
    }

    @Override
    protected int LIB_ConfigGravar(Pointer libHandle,String eArqConfig){
        return ((ACBrNFeBridge) this.getInstance()).NFE_ConfigGravar(libHandle, eArqConfig);
    }

    @Override
    protected int LIB_UltimoRetorno(Pointer libHandle,ByteBuffer sMensagem, IntByReference sTamanho){
        return ((ACBrNFeBridge) this.getInstance()).NFE_UltimoRetorno(libHandle, sMensagem, sTamanho);
    }

    @Override
    protected int LIB_Nome(Pointer libHandle,ByteBuffer sNome, IntByReference esTamanaho){
        return ((ACBrNFeBridge) this.getInstance()).NFE_Nome(libHandle, sNome, esTamanaho);
    }

    @Override
    protected int LIB_Versao(Pointer libHandle,ByteBuffer sVersao, IntByReference esTamanaho){
        return ((ACBrNFeBridge) this.getInstance()).NFE_Versao(libHandle, sVersao, esTamanaho);
    }

    @Override
    protected int  LIB_OpenSSLInfo(Pointer libHandle,ByteBuffer sOpenSslInfo, IntByReference esTamanho){
        return ((ACBrNFeBridge) this.getInstance()).NFE_OpenSSLInfo(libHandle, sOpenSslInfo, esTamanho);
    }

}


