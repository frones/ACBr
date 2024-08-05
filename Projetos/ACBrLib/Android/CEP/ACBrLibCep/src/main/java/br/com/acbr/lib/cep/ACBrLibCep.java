package br.com.acbr.lib.cep;

import android.util.Log;

import br.com.acbr.lib.comum.ACBrLibBase;
import br.com.acbr.lib.comum.ACBrLibBuffer;
import br.com.acbr.lib.comum.ACBrLibInstance;
import br.com.acbr.lib.cep.bridge.ACBrCepBridge;
import com.sun.jna.Library;
import com.sun.jna.Pointer;
import com.sun.jna.ptr.IntByReference;
import com.sun.jna.ptr.PointerByReference;

import java.nio.ByteBuffer;

/**
 * AcbrlibCep é uma classe de alto nível para ACBrLibCep para Android
 * Essa classe permite o uso do componente ACBrCEP no Android.
 *
 */

public class ACBrLibCep extends ACBrLibBase {

    /**
     * @param configFile
     * @param configCryptKey
     */
    public ACBrLibCep(String configFile, String configCryptKey) {
        super(configFile, configCryptKey);
    }

    //region
    /**
     *  Métodos que ** PRECISAM ** ser sobreescritos de ACBrLibBase, para correto funcionamento da Lib
     */

    @Override
    protected Library getInstance() {
        return ACBrLibInstance.getInstance(this.getLibName(), ACBrCepBridge.class);
    }

    @Override
    protected String getLibName(){
        return "ACBrLibCep";
    }

    //endregion

    //region
    /** Métodos específicos de ACBrLibCEP
     */

    /**
     * Método para consultar CEP
     *
     * @param cep
     * @return
     */
    public String buscarPorCep(String cep) throws Exception {
        ACBrLibBuffer buffer =  new ACBrLibBuffer();
        Log.i(this.getLibName(), "CEP_BuscarPorCEP, cep = " + cep);
        int status = ((ACBrCepBridge) this.getInstance()).CEP_BuscarPorCEP(this.getHandle(), cep, buffer.bufferData, buffer.bufferSizeNeeded);
        Log.i(this.getLibName(),"CEP_BuscarPorCEP, status = " + status);
        checkResult(status);
        return checkBuffer(buffer);
    }

    /**
     * @param cidade   Contem a cidade.
     * @param tipo     Contem o tipo de logradouro.
     * @param endereco Contem o logradouro.
     * @param estado   Contem nome do UF (unidade da federação)
     * @param bairro   Contem nome do bairro
     * @return
     */
    public String buscarLogradouro(String cidade, String tipo, String endereco, String estado, String bairro) throws Exception {
        ACBrLibBuffer buffer =  new ACBrLibBuffer();
        Log.i(this.getLibName(), "CEP_BuscarPorLogradouro, cidade = " + cidade+", tipo = " +
                tipo + ", endereco = " + endereco + ", estado = " + estado + ", bairro = " + bairro);
        int status = ((ACBrCepBridge) this.getInstance()).CEP_BuscarPorLogradouro(this.getHandle(),
                cidade, tipo, endereco, estado, bairro, buffer.bufferData, buffer.bufferSizeNeeded);
        Log.i(this.getLibName(),"CEP_BuscarPorLogradouro, status = "  + status );
        checkResult(status);
        return checkBuffer(buffer);
    }
    //endregion


    //region
    /**
     * Métodos que devem ser sobreescritos de ACBrLibBase, para usarem a chamada específica com o prefixo
     * utilizado na LIB.. exemplo, considerando a ACBrLibCEP:  LIB_Inicializar -> CEP_Inicializar
     */

    @Override
    protected int LIB_Inicializar(PointerByReference libHandle, String eArqConfig, String eChaveCrypt){
        return ((ACBrCepBridge) this.getInstance()).CEP_Inicializar(libHandle, eArqConfig, eChaveCrypt);
    }

    @Override
    protected int LIB_Finalizar(Pointer libHandle){
        return ((ACBrCepBridge) this.getInstance()).CEP_Finalizar(libHandle);
    }

    @Override
    protected int LIB_ConfigGravarValor(Pointer libHandle,String eSessao,String  eChave, String eValor){
        return ((ACBrCepBridge) this.getInstance()).CEP_ConfigGravarValor(libHandle, eSessao, eChave, eValor);
    }

    @Override
    protected int LIB_ConfigLer(Pointer libHandle,String eArqConfig){
        return ((ACBrCepBridge) this.getInstance()).CEP_ConfigLer(libHandle, eArqConfig);
    }

    @Override
    protected int LIB_ConfigLerValor(Pointer libHandle, String eSessao, String eChave, ByteBuffer sValor, IntByReference esTamanaho){
        return ((ACBrCepBridge) this.getInstance()).CEP_ConfigLerValor(libHandle, eSessao, eChave, sValor, esTamanaho);
    }

    @Override
    protected int LIB_ConfigImportar(Pointer libHandle,String eArqConfig){
        return ((ACBrCepBridge) this.getInstance()).CEP_ConfigImportar(libHandle, eArqConfig);
    }

    @Override
    protected int LIB_ConfigExportar(Pointer libHandle,ByteBuffer sMensagem, IntByReference esTamanaho){
        return ((ACBrCepBridge) this.getInstance()).CEP_ConfigExportar(libHandle, sMensagem, esTamanaho);
    }

    @Override
    protected int LIB_ConfigGravar(Pointer libHandle,String eArqConfig){
        return ((ACBrCepBridge) this.getInstance()).CEP_ConfigGravar(libHandle, eArqConfig);
    }

    @Override
    protected int LIB_UltimoRetorno(Pointer libHandle,ByteBuffer sMensagem, IntByReference sTamanho){
        return ((ACBrCepBridge) this.getInstance()).CEP_UltimoRetorno(libHandle, sMensagem, sTamanho);
    }

    @Override
    protected int LIB_Nome(Pointer libHandle,ByteBuffer sNome, IntByReference esTamanaho){
        return ((ACBrCepBridge) this.getInstance()).CEP_Nome(libHandle, sNome, esTamanaho);
    }

    @Override
    protected int LIB_Versao(Pointer libHandle,ByteBuffer sVersao, IntByReference esTamanaho){
        return ((ACBrCepBridge) this.getInstance()).CEP_Versao(libHandle, sVersao, esTamanaho);
    }

    @Override
    protected int  LIB_OpenSSLInfo(Pointer libHandle,ByteBuffer sOpenSslInfo, IntByReference esTamanho){
        return ((ACBrCepBridge) this.getInstance()).CEP_OpenSSLInfo(libHandle, sOpenSslInfo, esTamanho);
    }

}


