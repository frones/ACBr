package br.com.acbr.lib.bal;

import android.util.Log;

import com.sun.jna.Library;
import com.sun.jna.Pointer;
import com.sun.jna.ptr.DoubleByReference;
import com.sun.jna.ptr.IntByReference;
import com.sun.jna.ptr.PointerByReference;

import java.nio.ByteBuffer;

import br.com.acbr.lib.bal.bridge.ACBrBALBridge;
import br.com.acbr.lib.comum.ACBrLibBase;
import br.com.acbr.lib.comum.ACBrLibBuffer;
import br.com.acbr.lib.comum.ACBrLibInstance;

public class ACBrLibBAL extends ACBrLibBase {
    public ACBrLibBAL(String configFile, String configCryptKey) {
        super(configFile, configCryptKey);
    }

    @Override
    protected String getLibName() {
        return "ACBrLibBAL";
    }

    protected Library getInstance() {
        return ACBrLibInstance.getInstance(this.getLibName(), ACBrBALBridge.class);
    }

    @Override
    protected int LIB_Inicializar(PointerByReference libHandle, String eArqConfig, String eChaveCrypt) {
        return ((ACBrBALBridge) this.getInstance()).BAL_Inicializar(libHandle, eArqConfig, eChaveCrypt);
    }

    @Override
    protected int LIB_Finalizar(Pointer libHandle) {
        return ((ACBrBALBridge) this.getInstance()).BAL_Finalizar(libHandle);
    }

    @Override
    protected int LIB_ConfigGravarValor(Pointer libHandle, String eSessao, String eChave, String sValor) {
        return ((ACBrBALBridge) this.getInstance()).BAL_ConfigGravarValor(libHandle, eSessao, eChave, sValor);
    }

    @Override
    protected int LIB_ConfigLer(Pointer libHandle, String eArqConfig) {
        return ((ACBrBALBridge) this.getInstance()).BAL_ConfigLer(libHandle, eArqConfig);
    }

    @Override
    protected int LIB_ConfigLerValor(Pointer libHandle, String eSessao, String eChave, ByteBuffer sValor, IntByReference esTamanho) {
        return ((ACBrBALBridge) this.getInstance()).BAL_ConfigLerValor(libHandle, eSessao, eChave, sValor, esTamanho);
    }

    @Override
    protected int LIB_ConfigImportar(Pointer libHandle, String eArqConfig) {
        return ((ACBrBALBridge) this.getInstance()).BAL_ConfigImportar(libHandle, eArqConfig);
    }

    @Override
    protected int LIB_ConfigExportar(Pointer libHandle, ByteBuffer sMensagem, IntByReference esTamanho) {
        return ((ACBrBALBridge) this.getInstance()).BAL_ConfigExportar(libHandle, sMensagem, esTamanho);
    }

    @Override
    protected int LIB_ConfigGravar(Pointer libHandle, String eArqConfig) {
        return ((ACBrBALBridge) this.getInstance()).BAL_ConfigGravar(libHandle, eArqConfig);
    }

    @Override
    protected int LIB_UltimoRetorno(Pointer libHandle, ByteBuffer sMensagem, IntByReference esTamanho) {
        return ((ACBrBALBridge) this.getInstance()).BAL_UltimoRetorno(libHandle, sMensagem, esTamanho);
    }

    @Override
    protected int LIB_Nome(Pointer libHandle, ByteBuffer sMensagem, IntByReference esTamanho) {
        return ((ACBrBALBridge) this.getInstance()).BAL_Nome(libHandle, sMensagem, esTamanho);
    }

    @Override
    protected int LIB_Versao(Pointer libHandle, ByteBuffer sNome, IntByReference esTamanho) {
        return ((ACBrBALBridge) this.getInstance()).BAL_Versao(libHandle, sNome, esTamanho);
    }

    @Override
    protected int LIB_OpenSSLInfo(Pointer libHandle, ByteBuffer sMensagem, IntByReference esTamanho) {
        return 0;
    }

    public void ativar() throws Exception {
        Log.i(this.getLibName(), "BAL_Ativar");
        int status = ((ACBrBALBridge) this.getInstance()).BAL_Ativar(this.getHandle());
        Log.i(this.getLibName(),"BAL_Ativar, status = " + status);
        checkResult(status);
    }

    public void desativar() throws Exception {
        Log.i(this.getLibName(), "BAL_Desativar");
        int status = ((ACBrBALBridge) this.getInstance()).BAL_Desativar(this.getHandle());
        Log.i(this.getLibName(),"BAL_Desativar, status = " + status);
        checkResult(status);
    }

    public void solicitarPeso() throws Exception {
        Log.i(this.getLibName(), "BAL_SolicitarPeso");
        int status = ((ACBrBALBridge) this.getInstance()).BAL_SolicitarPeso(this.getHandle());
        Log.i(this.getLibName(),"BAL_SolicitarPeso, status = " + status);
        checkResult(status);
    }

    public double lePeso(int millisecTimeOut) throws Exception {
        DoubleByReference peso = new DoubleByReference();
        Log.i(this.getLibName(), "BAL_LerPeso");
        int status = ((ACBrBALBridge) this.getInstance()).BAL_LePeso(this.getHandle(), millisecTimeOut, peso);
        Log.i(this.getLibName(),"BAL_LerPeso, status = " + status);
        checkResult(status);
        return peso.getValue();
    }

    public String lePesoStr(int millisecTimeOut) throws Exception {
        ACBrLibBuffer  sValor = new ACBrLibBuffer();
        Log.i(this.getLibName(), "BAL_LePesoStr");
        int status = ((ACBrBALBridge) this.getInstance()).BAL_LePesoStr(this.getHandle(), millisecTimeOut, sValor.bufferData);
        Log.i(this.getLibName(),"BAL_LePesoStr, status = " + status);
        checkResult(status);
        return checkBuffer(sValor);
    }

    public double ultimoPesoLido() throws Exception {
        DoubleByReference peso = new DoubleByReference();
        Log.i(this.getLibName(), "BAL_UltimoPesoLido");
        int status = ((ACBrBALBridge) this.getInstance()).BAL_UltimoPesoLido(this.getHandle(), peso);
        Log.i(this.getLibName(),"BAL_UltimoPesoLido, status = " + status);
        checkResult(status);
        return peso.getValue();
    }

    public String ultimoPesoLidoStr() throws Exception {
        ACBrLibBuffer buffer = new ACBrLibBuffer();
        Log.i(this.getLibName(), "BAL_UltimoPesoLisoStr");
        int status = ((ACBrBALBridge) this.getInstance()).BAL_UltimoPesoLidoStr(this.getHandle(),  buffer.bufferData);
        Log.i(this.getLibName(),"BAL_UltimoPesoLisoStr, status = " + status);
        checkResult(status);
        return checkBuffer(buffer);
    }

    public double interpretarRespostaPeso(String aResposta) throws Exception {
        DoubleByReference peso = new DoubleByReference();
        Log.i(this.getLibName(), "BAL_InterpretarRespostaPeso");
        int status = ((ACBrBALBridge) this.getInstance()).BAL_InterpretarRespostaPeso(this.getHandle(), aResposta, peso);
        Log.i(this.getLibName(),"BAL_InterpretarRespostaPeso, status = " + status);
        checkResult(status);
        return peso.getValue();
    }

    public String interpretarRespostaPesoStr(String aResposta) throws Exception {
        ACBrLibBuffer buffer = new ACBrLibBuffer();
        Log.i(this.getLibName(), "BAL_InterpretarRespostaPesoStr");
        int status = ((ACBrBALBridge) this.getInstance()).BAL_InterpretarRespostaPesoStr(this.getHandle(), aResposta, buffer.bufferData);
        Log.i(this.getLibName(),"BAL_InterpretarRespostaPesoStr, status = " + status);
        checkResult(status);
        return checkBuffer(buffer);
    }
}
