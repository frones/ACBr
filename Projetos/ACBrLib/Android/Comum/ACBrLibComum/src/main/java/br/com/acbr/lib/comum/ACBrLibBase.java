package br.com.acbr.lib.comum;

import android.util.Log;

import com.sun.jna.Library;
import com.sun.jna.Pointer;
import com.sun.jna.ptr.IntByReference;
import com.sun.jna.ptr.PointerByReference;

import java.nio.ByteBuffer;

/**
 * ACBrLibBase é uma classe de alto nível para ACBrLib para Android
 *
 */

public abstract class ACBrLibBase {
    private final Library acblib;
    private final String configFile;
    private final String configCryptKey;
    private final PointerByReference libHandle;

    /**
     *
     * @param configFile caminho absoluto do arquivo de configuração da biblioteca
     */

    public ACBrLibBase(String configFile, String configCryptKey) {
        this.configFile = configFile;
        this.configCryptKey = configCryptKey;
        this.libHandle = new PointerByReference();
        this.acblib = getInstance();
    }

    protected Library getInstance() {
        return ACBrLibInstance.getInstance(this.getLibName(), Library.class);
    }

    protected Pointer getHandle(){
        return  this.libHandle.getValue();
    }

    protected String getLibName(){
        return "ACBrLib";
    }

    protected Boolean isInitialized(){
        return (this.libHandle.getValue() != null);
    }

    protected String getConfigFile(){
        return this.configFile;
    }

    public int inicializar() throws Exception {
        Log.i(this.getLibName(), "LIB_Inicializar, arquivoConfig = " + this.configFile);
        int status = LIB_Inicializar(this.libHandle, this.configFile, this.configCryptKey);
        Log.i(this.getLibName(),"LIB_Inicializar, status = " + status);
        if (status != 0) {
            this.libHandle.setValue(null);
        }
        checkResult(status);
        return status;
    }

    public int finalizar() {
        if (this.isInitialized()) {
            Log.i(this.getLibName(),"LIB_Finalizar");
            int status = LIB_Finalizar(this.getHandle());
            this.libHandle.setValue(null);
            Log.i(this.getLibName(),"LIB_Finalizar, status = " + status);
            return status;
        } else {
            return 0;
        }
    }

    public int configLer(String eArqConfig) throws Exception {
        Log.i(this.getLibName(),"LIB_ConfigLer, arquivoConfig = " + this.configFile);
        int status = LIB_ConfigLer(getHandle(), this.configFile);
        Log.i(this.getLibName(),"LIB_ConfigLer, status = " + status);
        checkResult(status);
        return status;
    }

    public int configGravar() throws Exception {
        Log.i(this.getLibName(),"LIB_ConfigGravar, forceUpdateIni");
        int status = LIB_ConfigGravar(this.getHandle(), this.configFile);
        Log.i(this.getLibName(),"LIB_ConfigGravar, status = " + status);
        checkResult(status);
        return status;
    }

    /**
     * @param sessao
     * @param chave
     * @return
     */
    public String configLerValor(String sessao, String chave) throws Exception {
        ACBrLibBuffer buffer =  new ACBrLibBuffer();
        Log.i(this.getLibName(), "LIB_ConfigLerValor, sessao = " + sessao + ", chave = " + chave);
        int status = LIB_ConfigLerValor(this.getHandle(),sessao, chave, buffer.bufferData, buffer.bufferSizeNeeded);
        Log.i(this.getLibName(),"LIB_ConfigLerValor, status = " + status);
        checkResult(status);
        return checkBuffer(buffer);
    }

    /**
     * Método que grava informações no arquivo em arquivoConfig
     *
     * @param sessao
     * @param chave
     * @param valor
     * @return status
     */
    public int configGravarValor(String sessao, String chave, String valor) throws Exception {
        Log.i(this.getLibName(), "LIB_ConfigGravarValor, sessao = " + sessao + ", chave = " + chave);
        int status = LIB_ConfigGravarValor(this.getHandle(), sessao, chave, valor);
        Log.i(this.getLibName(),"LIB_ConfigGravarValor, status = " + status);
        checkResult(status);
        return status;
    }

    public int configImportar(String arquivoConfig) throws Exception {
        Log.i(this.getLibName(),"LIB_ConfigImportar, arquivoConfig = " + arquivoConfig);
        int status = LIB_ConfigImportar(this.getHandle(), arquivoConfig);
        Log.i(this.getLibName(),"LIB_ConfigImportar, status = " + status);
        checkResult(status);
        return status;
    }

    public String configExportar() throws Exception {
        ACBrLibBuffer buffer =  new ACBrLibBuffer();
        Log.i(this.getLibName(), "LIB_ConfigExportar");
        int status = LIB_ConfigExportar(this.getHandle(), buffer.bufferData, buffer.bufferSizeNeeded);
        Log.i(this.getLibName(),"LIB_ConfigExportar, status = " + status);
        checkResult(status);
        return checkBuffer(buffer);
    }

    /**
     * @return nome da biblioteca
     */
    public String nome() throws Exception {
        ACBrLibBuffer buffer =  new ACBrLibBuffer();
        Log.i(this.getLibName(), "LIB_Nome");
        int status = LIB_Nome(this.getHandle(), buffer.bufferData, buffer.bufferSizeNeeded);
        Log.i(this.getLibName(),"LIB_Nome, status = " + status);
        checkResult(status);
        return checkBuffer(buffer);
    }

    public String versao() throws Exception {
        ACBrLibBuffer buffer =  new ACBrLibBuffer();
        Log.i(this.getLibName(), "LIB_Versao");
        int status = LIB_Versao(this.getHandle(), buffer.bufferData, buffer.bufferSizeNeeded);
        Log.i(this.getLibName(),"LIB_Versao, status = " + status);
        checkResult(status);
        return checkBuffer(buffer);
    }

    /**
     * Método que retorna informações sobre a biblioteca OpenSSL
     *
     * @return
     */
    public String openSslInfo() throws Exception {
        ACBrLibBuffer buffer =  new ACBrLibBuffer();
        Log.i(this.getLibName(), "LIB_OpenSSLInfo");
        int status = LIB_OpenSSLInfo(this.getHandle(), buffer.bufferData, buffer.bufferSizeNeeded);
        Log.i(this.getLibName(),"LIB_OpenSSLInfo, status = " + status);
        checkResult(status);
        return checkBuffer(buffer);
    }

    public String ultimoRetorno() {
        return checkBuffer();
    }

    protected void checkResult(int result) throws Exception {
        if (result == 0) {
            return;
        }
        String s;
        Log.i(this.getLibName(),"checkResult = " + result);
        if (this.isInitialized()) {
            s = checkBuffer();
        } else {
            s = "Error: " + result;
        }

        Log.i(this.getLibName(),"Exception: " + s);
        throw new Exception(s);
    }

    protected String checkBuffer() {
        IntByReference bufferSizeNeeded = new IntByReference(0);
        Log.i(this.getLibName(), "LIB_UltimoRetorno, tamanhoBuffer = " + bufferSizeNeeded.getValue() );
        int status = LIB_UltimoRetorno(this.getHandle(), null , bufferSizeNeeded);
        Log.i(this.getLibName(),"LIB_UltimoRetorno, status = " + status + ", tamanhoBuffer = " + bufferSizeNeeded.getValue() );
        String s = "";
        if (bufferSizeNeeded.getValue() > 0) {
            s = checkBuffer(bufferSizeNeeded.getValue(), null);
        }
        return s;
    }

    protected String checkBuffer(ACBrLibBuffer buffer) {
        return checkBuffer(buffer.getBufferSizeNeeded(), buffer);
    }

    protected String checkBuffer(int bufferSizeNeeded, ACBrLibBuffer buffer) {
        String s = "";
        int bufferCapacity = 0;
        if (buffer != null) {
            bufferCapacity = buffer.getBufferCapacity();
            s = buffer.toString();
        }

        if (bufferCapacity < bufferSizeNeeded) {
            Log.i(this.getLibName(),"processaResult ,bufferCapacity: " + bufferCapacity + " < bufferSizeNeeded:"  + bufferSizeNeeded);
            ACBrLibBuffer newBuffer =  new ACBrLibBuffer((int) Math.round(bufferSizeNeeded * 1.3));
            Log.i(this.getLibName(), "LIB_UltimoRetorno, tamanhoBuffer = " + newBuffer.getBufferSizeNeeded() );
            int status = LIB_UltimoRetorno(this.getHandle(), newBuffer.bufferData, newBuffer.bufferSizeNeeded);
            Log.i(this.getLibName(),"LIB_UltimoRetorno, status = " + status + ", tamanhoBuffer = " + newBuffer.getBufferSizeNeeded() );
            s = newBuffer.toString();
        }

        return s;
    }

    abstract protected int LIB_Inicializar(PointerByReference libHandle, String eArqConfig, String eChaveCrypt);
    abstract protected int LIB_Finalizar(Pointer libHandle);
    abstract protected int LIB_ConfigGravarValor(Pointer libHandle,String eSessao,String  eChave, String eValor);
    abstract protected int LIB_ConfigLer(Pointer libHandle,String eArqConfig);
    abstract protected int LIB_ConfigLerValor(Pointer libHandle, String eSessao, String eChave, ByteBuffer sValor, IntByReference esTamanaho);
    abstract protected int LIB_ConfigImportar(Pointer libHandle,String eArqConfig);
    abstract protected int LIB_ConfigExportar(Pointer libHandle,ByteBuffer sMensagem, IntByReference esTamanaho);
    abstract protected int LIB_ConfigGravar(Pointer libHandle,String eArqConfig);
    abstract protected int LIB_UltimoRetorno(Pointer libHandle,ByteBuffer sMensagem, IntByReference sTamanho);
    abstract protected int LIB_Nome(Pointer libHandle,ByteBuffer sNome, IntByReference esTamanaho);
    abstract protected int LIB_Versao(Pointer libHandle,ByteBuffer sVersao, IntByReference esTamanaho);
    abstract protected int LIB_OpenSSLInfo(Pointer libHandle,ByteBuffer sOpenSslInfo, IntByReference esTamanho);
}


