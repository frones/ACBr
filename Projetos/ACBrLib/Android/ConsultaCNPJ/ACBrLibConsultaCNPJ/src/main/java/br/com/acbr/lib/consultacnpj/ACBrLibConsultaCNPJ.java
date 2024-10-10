package br.com.acbr.lib.consultacnpj;

import android.util.Log;

import br.com.acbr.lib.comum.ACBrLibBase;
import br.com.acbr.lib.comum.ACBrLibBuffer;
import br.com.acbr.lib.comum.ACBrLibInstance;
import br.com.acbr.lib.consultacnpj.bridge.ACBrConsultaCNPJBridge;
import com.sun.jna.Library;
import com.sun.jna.Pointer;
import com.sun.jna.ptr.IntByReference;
import com.sun.jna.ptr.PointerByReference;

import java.nio.ByteBuffer;

/**
 * AcbrlibConsultaCNPJ é uma classe de alto nível para ACBrLibConsultaCNPJ para Android
 * Essa classe permite o uso do componente ACBrConsultaCNPJ no Android.
 *
 */

public class ACBrLibConsultaCNPJ extends ACBrLibBase {

    /**
     * @param configFile
     * @param configCryptKey
     */
    public ACBrLibConsultaCNPJ(String configFile, String configCryptKey) {
        super(configFile, configCryptKey);
    }

    //region
    /**
     *  Métodos que ** PRECISAM ** ser sobreescritos de ACBrLibBase, para correto funcionamento da Lib
     */

    @Override
    protected Library getInstance() {
        return ACBrLibInstance.getInstance(this.getLibName(), ACBrConsultaCNPJBridge.class);
    }

    @Override
    protected String getLibName(){
        return "ACBrLibConsultaCNPJ";
    }

    //endregion

    //region
    /** Métodos específicos de ACBrLibConsultaCNPJ
     */

    /**
     * Método para Consultar CNPJ
     *
     * @param cnpj
     * @return
     */
    public String consultar(String cnpj) throws Exception {
        ACBrLibBuffer buffer =  new ACBrLibBuffer();
        Log.i(this.getLibName(), "CNPJ_Consultar, cnpj = " + cnpj);
        int status = ((ACBrConsultaCNPJBridge) this.getInstance()).CNPJ_Consultar(this.getHandle(), cnpj, buffer.bufferData, buffer.bufferSizeNeeded);
        Log.i(this.getLibName(),"CNPJ_Consultar, status = " + status);
        checkResult(status);
        return checkBuffer(buffer);
    }
    //endregion


    //region
    /**
     * Métodos que devem ser sobreescritos de ACBrLibBase, para usarem a chamada específica com o prefixo
     * utilizado na LIB.. exemplo, considerando a ACBrLibConsultaCNPJ:  LIB_Inicializar -> CNPJ_Inicializar
     */

    @Override
    protected int LIB_Inicializar(PointerByReference libHandle, String eArqConfig, String eChaveCrypt){
        return ((ACBrConsultaCNPJBridge) this.getInstance()).CNPJ_Inicializar(libHandle, eArqConfig, eChaveCrypt);
    }

    @Override
    protected int LIB_Finalizar(Pointer libHandle){
        return ((ACBrConsultaCNPJBridge) this.getInstance()).CNPJ_Finalizar(libHandle);
    }

    @Override
    protected int LIB_ConfigGravarValor(Pointer libHandle,String eSessao,String  eChave, String eValor){
        return ((ACBrConsultaCNPJBridge) this.getInstance()).CNPJ_ConfigGravarValor(libHandle, eSessao, eChave, eValor);
    }

    @Override
    protected int LIB_ConfigLer(Pointer libHandle,String eArqConfig){
        return ((ACBrConsultaCNPJBridge) this.getInstance()).CNPJ_ConfigLer(libHandle, eArqConfig);
    }

    @Override
    protected int LIB_ConfigLerValor(Pointer libHandle, String eSessao, String eChave, ByteBuffer sValor, IntByReference esTamanaho){
        return ((ACBrConsultaCNPJBridge) this.getInstance()).CNPJ_ConfigLerValor(libHandle, eSessao, eChave, sValor, esTamanaho);
    }

    @Override
    protected int LIB_ConfigImportar(Pointer libHandle,String eArqConfig){
        return ((ACBrConsultaCNPJBridge) this.getInstance()).CNPJ_ConfigImportar(libHandle, eArqConfig);
    }

    @Override
    protected int LIB_ConfigExportar(Pointer libHandle,ByteBuffer sMensagem, IntByReference esTamanaho){
        return ((ACBrConsultaCNPJBridge) this.getInstance()).CNPJ_ConfigExportar(libHandle, sMensagem, esTamanaho);
    }

    @Override
    protected int LIB_ConfigGravar(Pointer libHandle,String eArqConfig){
        return ((ACBrConsultaCNPJBridge) this.getInstance()).CNPJ_ConfigGravar(libHandle, eArqConfig);
    }

    @Override
    protected int LIB_UltimoRetorno(Pointer libHandle,ByteBuffer sMensagem, IntByReference sTamanho){
        return ((ACBrConsultaCNPJBridge) this.getInstance()).CNPJ_UltimoRetorno(libHandle, sMensagem, sTamanho);
    }

    @Override
    protected int LIB_Nome(Pointer libHandle,ByteBuffer sNome, IntByReference esTamanaho){
        return ((ACBrConsultaCNPJBridge) this.getInstance()).CNPJ_Nome(libHandle, sNome, esTamanaho);
    }

    @Override
    protected int LIB_Versao(Pointer libHandle,ByteBuffer sVersao, IntByReference esTamanaho){
        return ((ACBrConsultaCNPJBridge) this.getInstance()).CNPJ_Versao(libHandle, sVersao, esTamanaho);
    }

    @Override
    protected int  LIB_OpenSSLInfo(Pointer libHandle,ByteBuffer sOpenSslInfo, IntByReference esTamanho){
        return ((ACBrConsultaCNPJBridge) this.getInstance()).CNPJ_OpenSSLInfo(libHandle, sOpenSslInfo, esTamanho);
    }

}


