package br.com.acbr.lib.pixcd;

import android.util.Log;

import com.sun.jna.Library;
import com.sun.jna.Pointer;
import com.sun.jna.ptr.IntByReference;
import com.sun.jna.ptr.PointerByReference;

import java.nio.ByteBuffer;
import java.util.Date;

import br.com.acbr.lib.comum.ACBrLibBase;
import br.com.acbr.lib.comum.ACBrLibBuffer;
import br.com.acbr.lib.comum.ACBrLibInstance;
import br.com.acbr.lib.pixcd.bridge.ACBrPIXCDBridge;

/**
 * ACBrLibNFe é uma classe de alto nível para ACBrLibNFe para Android
 * Essa classe permite o uso do componente ACBrPIXCD no Android.
 *
 */

public class ACBrLibPIXCD extends ACBrLibBase {

    /**
     * @param configFile
     * @param configCryptKey
     */

    public ACBrLibPIXCD(String configFile, String configCryptKey) {
        super(configFile, configCryptKey);
    }

    //region
    /**
     *  Métodos que ** PRECISAM ** ser sobreescritos de ACBrLibBase, para correto funcionamento da Lib
     */

    @Override
    protected Library getInstance(){
        return ACBrLibInstance.getInstance(this.getLibName(), ACBrPIXCDBridge.class);
    }

    @Override
    protected String getLibName(){
        return "ACBrLibPIXCD";
    }
    //endregion

    //region
    /**
     * Métodos que devem ser sobreescritos de ACBrLibBase, para usarem a chamada específica com o prefixo
     * utilizado na LIB.. exemplo, considerando a ACBrLibPIXCD:  LIB_Inicializar -> PIXCD_Inicializar
     */

    @Override
    protected int LIB_Inicializar(PointerByReference libHandle, String eArqConfig, String eChaveCrypt){
        return ((ACBrPIXCDBridge) this.getInstance()).PIXCD_Inicializar(libHandle, eArqConfig, eChaveCrypt);
    }

    @Override
    protected int LIB_Finalizar(Pointer libHandle){
        return ((ACBrPIXCDBridge) this.getInstance()).PIXCD_Finalizar(libHandle);
    }

    @Override
    protected int LIB_Nome(Pointer libHandle,ByteBuffer sNome, IntByReference esTamanaho){
        return ((ACBrPIXCDBridge) this.getInstance()).PIXCD_Nome(libHandle, sNome, esTamanaho);
    }

    @Override
    protected int  LIB_OpenSSLInfo(Pointer libHandle,ByteBuffer sOpenSslInfo, IntByReference esTamanho){
        return ((ACBrPIXCDBridge) this.getInstance()).PIXCD_OpenSSLInfo(libHandle, sOpenSslInfo, esTamanho);
    }

    @Override
    protected int LIB_Versao(Pointer libHandle,ByteBuffer sVersao, IntByReference esTamanaho){
        return ((ACBrPIXCDBridge) this.getInstance()).PIXCD_Versao(libHandle, sVersao, esTamanaho);
    }

    @Override
    protected int LIB_UltimoRetorno(Pointer libHandle,ByteBuffer sMensagem, IntByReference sTamanho){
        return ((ACBrPIXCDBridge) this.getInstance()).PIXCD_UltimoRetorno(libHandle, sMensagem, sTamanho);
    }

    @Override
    protected int LIB_ConfigImportar(Pointer libHandle,String eArqConfig){
        return ((ACBrPIXCDBridge) this.getInstance()).PIXCD_ConfigImportar(libHandle, eArqConfig);
    }

    @Override
    protected int LIB_ConfigExportar(Pointer libHandle,ByteBuffer sMensagem, IntByReference esTamanaho){
        return ((ACBrPIXCDBridge) this.getInstance()).PIXCD_ConfigExportar(libHandle, sMensagem, esTamanaho);
    }

    @Override
    protected int LIB_ConfigLer(Pointer libHandle,String eArqConfig){
        return ((ACBrPIXCDBridge) this.getInstance()).PIXCD_ConfigLer(libHandle, eArqConfig);
    }

    @Override
    protected int LIB_ConfigGravar(Pointer libHandle,String eArqConfig){
        return ((ACBrPIXCDBridge) this.getInstance()).PIXCD_ConfigGravar(libHandle, eArqConfig);
    }

    @Override
    protected int LIB_ConfigLerValor(Pointer libHandle, String eSessao, String eChave, ByteBuffer sValor, IntByReference esTamanaho){
        return ((ACBrPIXCDBridge) this.getInstance()).PIXCD_ConfigLerValor(libHandle, eSessao, eChave, sValor, esTamanaho);
    }

    @Override
    protected int LIB_ConfigGravarValor(Pointer libHandle,String eSessao,String  eChave, String eValor){
        return ((ACBrPIXCDBridge) this.getInstance()).PIXCD_ConfigGravarValor(libHandle, eSessao, eChave, eValor);
    }

    //endregion

    //region
    /** Métodos específicos de ACBrLibPIXCD **/

    /**
     *Métodos para uso do PIXCD (PIX e Carteiras Digitais).
     *
     *
     * @param AValor -> Valor da transação PIX.
     * @param AInfoAdicional -> Informações adicionais.
     * @param ATxID -> Identificador da transação PIX.
     * @return
     */
    public String GerarQRCodeEstatico(double AValor, String AInfoAdicional, String ATxID) throws Exception{
        ACBrLibBuffer buffer =  new ACBrLibBuffer();
        Log.i(this.getLibName(), "PIXCD_GerarQRCodeEstatico, AValor = " + AValor + " AInfoAdicional = " + AInfoAdicional + " ATxID = " + ATxID);
        int status = ((ACBrPIXCDBridge) this.getInstance()).PIXCD_GerarQRCodeEstatico(this.getHandle(), AValor, AInfoAdicional, ATxID, buffer.bufferData, buffer.bufferSizeNeeded);
        Log.i(this.getLibName(), "PIXCD_GerarQRCodeEstatico, status = " + status);
        checkResult(status);
        return checkBuffer(buffer);
    }

    //region endPoint/Pix

    /**
     * @param Ae2eid -> End-to-End Identification(Identificação de ponta a ponta).
     * @return
     */
    public String ConsultarPix(String Ae2eid) throws Exception{
        ACBrLibBuffer buffer =  new ACBrLibBuffer();
        Log.i(this.getLibName(), "PIXCD_ConsultarPix, Ae2eid = " + Ae2eid);
        int status = ((ACBrPIXCDBridge) this.getInstance()).PIXCD_ConsultarPix(this.getHandle(), Ae2eid, buffer.bufferData, buffer.bufferSizeNeeded);
        Log.i(this.getLibName(), "PIXCD_ConsultarPix, status = " + status);
        checkResult(status);
        return checkBuffer(buffer);
    }

    /**
     * @param ADataInicio -> Data de início.
     * @param ADataFim -> Data de fim.
     * @param ATxId -> Identificador da transação PIX.
     * @param ACpfCnpj -> CPF/CNPJ.
     * @param PagAtual -> Página atual.
     * @param ItensPorPagina -> Itens por página.
     * @return
     */
    public String ConsultarPixRecebidos(Date ADataInicio, Date ADataFim, String ATxId, String ACpfCnpj, int PagAtual, int ItensPorPagina) throws Exception{
        ACBrLibBuffer buffer =  new ACBrLibBuffer();
        Log.i(this.getLibName(), "PIXCD_ConsultarPixRecebidos, ADataInicio = " + ADataInicio + " ADataFim = " + ADataFim + " ATxId = " + ATxId + " ACpfCnpj = " + ACpfCnpj + " PagAtual = " + PagAtual + " ItensPorPagina = " + ItensPorPagina);
        int status = ((ACBrPIXCDBridge) this.getInstance()).PIXCD_ConsultarPixRecebidos(this.getHandle(), ADataInicio, ADataFim, ATxId, ACpfCnpj, PagAtual, ItensPorPagina, buffer.bufferData, buffer.bufferSizeNeeded);
        Log.i(this.getLibName(), "PIXCD_ConsultarPixRecebidos, status = " + status);
        checkResult(status);
        return checkBuffer(buffer);
    }

    /**
     *
     * @param AInfDevolucao -> Informações da devolução.
     * @param Ae2eid -> End-to-End Identification(Identificação de ponta a ponta).
     * @param AidDevolucao -> Identificador da devolução.
     * @return
     */
    public String SolicitarDevolucaoPix(String AInfDevolucao, String Ae2eid, String AidDevolucao) throws Exception{
        ACBrLibBuffer buffer =  new ACBrLibBuffer();
        Log.i(getLibName(), "PIXCD_SolicitarDevolucaoPix, AInfDevolucao = " + AInfDevolucao + " Ae2eid = " + Ae2eid + " AidDevolucao = " + AidDevolucao);
        int status = ((ACBrPIXCDBridge) this.getInstance()).PIXCD_SolicitarDevolucaoPix(this.getHandle(), AInfDevolucao, Ae2eid, AidDevolucao, buffer.bufferData, buffer.bufferSizeNeeded);
        Log.i(getLibName(), "PIXCD_SolicitarDevolucaoPix, status = " + status);
        checkResult(status);
        return checkBuffer(buffer);
    }

    /**
     * @param Ae2eid -> End-to-End Identification(Identificação de ponta a ponta).
     * @param AidDevolucao -> Identificador da devolução.
     * @return
     */
    public String ConsultarDevolucaoPix(String Ae2eid, String AidDevolucao) throws Exception{
        ACBrLibBuffer buffer =  new ACBrLibBuffer();
        Log.i(getLibName(), "PIXCD_ConsultarDevolucaoPix, Ae2eid = " + Ae2eid + " AidDevolucao = " + AidDevolucao);
        int status = ((ACBrPIXCDBridge) this.getInstance()).PIXCD_ConsultarDevolucaoPix(this.getHandle(), Ae2eid, AidDevolucao, buffer.bufferData, buffer.bufferSizeNeeded);
        Log.i(getLibName(), "PIXCD_ConsultarDevolucaoPix, status = " + status);
        checkResult(status);
        return checkBuffer(buffer);
    }

    //endregion

    //region endPoint/Cob

    /**
     * @param AInfCobSolicitada -> Informações da cobrança.
     * @param ATxId -> Identificador da cobrança.
     * @return
     */
    public String CriarCobrancaImediata(String AInfCobSolicitada, String ATxId) throws Exception{
        ACBrLibBuffer buffer =  new ACBrLibBuffer();
        Log.i(getLibName(), "PIXCD_CriarCobrancaImediata, AInfCobSolicitada = " + AInfCobSolicitada + " ATxId = " + ATxId);
        int status = ((ACBrPIXCDBridge) this.getInstance()).PIXCD_CriarCobrancaImediata(this.getHandle(), AInfCobSolicitada, ATxId, buffer.bufferData, buffer.bufferSizeNeeded);
        Log.i(getLibName(), "PIXCD_CriarCobrancaImediata, status = " + status);
        checkResult(status);
        return checkBuffer(buffer);
    }

    /**
     * @param ADataInicio -> Data de início.
     * @param ADataFim -> Data de fim.
     * @param ACpfCnpj -> CPF/CNPJ.
     * @param ALocationPresente -> Localização presente.
     * @param AStatus -> Status
     * @param PagAtual -> Página atual.
     * @param ItensPorPagina -> Itens por página.
     * @return
     */
    public String ConsultarCobrancasCob(Date ADataInicio, Date ADataFim, String ACpfCnpj, boolean ALocationPresente, int AStatus, int PagAtual, int ItensPorPagina) throws Exception{
        ACBrLibBuffer buffer =  new ACBrLibBuffer();
        Log.i(getLibName(), "PIXCD_ConsultarCobrancasCob, ADataInicio = " + ADataInicio + " ADataFim = " + ADataFim + " ACpfCnpj = " + ACpfCnpj + " ALocationPresente = " + ALocationPresente + " AStatus = " + AStatus + " PagAtual = " + PagAtual + " ItensPorPagina = " + ItensPorPagina);
        int status = ((ACBrPIXCDBridge) this.getInstance()).PIXCD_ConsultarCobrancasCob(this.getHandle(), ADataInicio, ADataFim, ACpfCnpj, ALocationPresente, AStatus, PagAtual, ItensPorPagina, buffer.bufferData, buffer.bufferSizeNeeded);
        Log.i(getLibName(), "PIXCD_ConsultarCobrancasCob, status = " + status);
        checkResult(status);
        return  checkBuffer(buffer);
    }

    /**
     * @param AInfCobRevisada -> Informações da cobrança revisada.
     * @param ATxId -> Identificador da cobrança.
     * @return
     */
    public String RevisarCobrancaImediata(String AInfCobRevisada, String ATxId) throws Exception{
        ACBrLibBuffer buffer =  new ACBrLibBuffer();
        Log.i(getLibName(), "PIXCD_RevisarCobrancaImediata, AInfCobRevisada = " + AInfCobRevisada + " ATxId = " + ATxId);
        int status = ((ACBrPIXCDBridge) this.getInstance()).PIXCD_RevisarCobrancaImediata(this.getHandle(), AInfCobRevisada, ATxId, buffer.bufferData, buffer.bufferSizeNeeded);
        Log.i(getLibName(), "PIXCD_RevisarCobrancaImediata, status = " + status);
        checkResult(status);
        return checkBuffer(buffer);
    }

    /**
     * @param ATxId -> Identificador da cobrança.
     * @return
     */
    public String CancelarCobrancaImediata(String ATxId) throws Exception{
        ACBrLibBuffer buffer =  new ACBrLibBuffer();
        Log.i(getLibName(), "PIXCD_CancelarCobrancaImediata, ATxId = " + ATxId);
        int status = ((ACBrPIXCDBridge) this.getInstance()).PIXCD_CancelarCobrancaImediata(this.getHandle(), ATxId, buffer.bufferData, buffer.bufferSizeNeeded);
        Log.i(getLibName(), "PIXCD_CancelarCobrancaImediata, status = " + status);
        checkResult(status);
        return checkBuffer(buffer);
    }

    //endregion

    //region endPoint/CobV

    /**
     *
     * @param AInfCobVSolicitada -> Informações da cobrança.
     * @param ATxId -> Identificador da cobrança.
     * @return
     */
    public String CriarCobranca(String AInfCobVSolicitada, String ATxId) throws Exception{
        ACBrLibBuffer buffer = new ACBrLibBuffer();
        Log.i(getLibName(), "PIXCD_CriarCobranca, AInfCobVSolicitada = " + AInfCobVSolicitada + " ATxId = " + ATxId);
        int status = ((ACBrPIXCDBridge) this.getInstance()).PIXCD_CriarCobranca(AInfCobVSolicitada, ATxId, buffer.bufferData, buffer.bufferSizeNeeded);
        Log.i(getLibName(), "PIXCD_CriarCobranca, status = " + status);
        checkResult(status);
        return checkBuffer(buffer);
    }

    /**
     * @param ATxId -> Identificador da cobrança.
     * @param ARevisao -> Revisão da cobrança.
     * @return
     */
    public String ConsultarCobranca(String ATxId, int ARevisao) throws Exception{
        ACBrLibBuffer buffer = new ACBrLibBuffer();
        Log.i(getLibName(), "PIXCD_ConsultarCobranca, ATxId = " + ATxId + " ARevisao = " + ARevisao);
        int status = ((ACBrPIXCDBridge) this.getInstance()).PIXCD_ConsultarCobranca(ATxId, ARevisao, buffer.bufferData, buffer.bufferSizeNeeded);
        Log.i(getLibName(), "PIXCD_ConsultarCobranca, status = " + status);
        checkResult(status);
        return checkBuffer(buffer);
    }

    /**
     *
     * @param ADataInicio -> Data de início.
     * @param ADataFim -> Data de fim.
     * @param ACpfCnpj -> CPF/CNPJ.
     * @param ALocationPresente -> Localização presente.
     * @param AStatus -> Status da transação.
     * @param PagAtual -> Página atual.
     * @param ItensPorPagina -> Itens por página.
     * @return
     */
    public String ConsultarCobrancasCobV(Date ADataInicio, Date ADataFim, String ACpfCnpj, boolean ALocationPresente, int AStatus, int PagAtual, int ItensPorPagina) throws Exception{
        ACBrLibBuffer buffer = new ACBrLibBuffer();
        Log.i(getLibName(), "PIXCD_ConsultarCobrancasCobV, ADataInicio = " + ADataInicio + " ADataFim = " + ADataFim + " ACpfCnpj = " + ACpfCnpj + " ALocationPresente = " + ALocationPresente + " AStatus = " + AStatus + " PagAtual = " + PagAtual + " ItensPorPagina = " + ItensPorPagina);
        int status = ((ACBrPIXCDBridge) this.getInstance()).PIXCD_ConsultarCobrancasCobV(ADataInicio, ADataFim, ACpfCnpj, ALocationPresente, AStatus, PagAtual, ItensPorPagina, buffer.bufferData, buffer.bufferSizeNeeded);
        Log.i(getLibName(), "PIXCD_ConsultarCobrancasCobV, status = " + status);
        checkResult(status);
        return checkBuffer(buffer);
    }

    /**
     * @param AInfCobVRevisada
     * @param ATxId
     * @return
     */
    public String RevisarCobranca(String AInfCobVRevisada, String ATxId) throws Exception{
        ACBrLibBuffer buffer = new ACBrLibBuffer();
        Log.i(getLibName(), "PIXCD_RevisarCobranca, AInfCobVRevisada = " + AInfCobVRevisada + " ATxId = " + ATxId);
        int status = ((ACBrPIXCDBridge) this.getInstance()).PIXCD_RevisarCobranca(AInfCobVRevisada, ATxId, buffer.bufferData, buffer.bufferSizeNeeded);
        Log.i(getLibName(), "PIXCD_RevisarCobranca, status = " + status);
        checkResult(status);
        return checkBuffer(buffer);
    }

    /**
     * @param ATxId
     * @return
     */
    public String CancelarCobranca(String ATxId) throws Exception{
        ACBrLibBuffer buffer = new ACBrLibBuffer();
        Log.i(getLibName(), "PIXCD_CancelarCobranca, ATxId = " + ATxId);
        int status = ((ACBrPIXCDBridge) this.getInstance()).PIXCD_CancelarCobranca(ATxId, buffer.bufferData, buffer.bufferSizeNeeded);
        Log.i(getLibName(), "PIXCD_CancelarCobranca, status = " + status);
        checkResult(status);
        return checkBuffer(buffer);
    }

    //endregion

    //endregion

}
