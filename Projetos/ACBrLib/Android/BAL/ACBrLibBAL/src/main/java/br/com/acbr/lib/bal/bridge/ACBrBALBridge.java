package br.com.acbr.lib.bal.bridge;

import com.sun.jna.Library;
import com.sun.jna.Pointer;
import com.sun.jna.ptr.DoubleByReference;
import com.sun.jna.ptr.IntByReference;
import com.sun.jna.ptr.PointerByReference;

import java.nio.ByteBuffer;

public interface ACBrBALBridge extends Library {
    public int BAL_Inicializar(PointerByReference libHandle, String eArqConfig, String eChaveCrypt);

    public int BAL_Finalizar(Pointer libHandle);

    public int BAL_UltimoRetorno(Pointer libHandle, ByteBuffer sMensagem, IntByReference sTamanho);

    public int BAL_Nome(Pointer libHandle, ByteBuffer sNome, IntByReference esTamanho);

    public int BAL_Versao(Pointer libHandle, ByteBuffer sVersao, IntByReference esTamanho);

    public int BAL_ConfigLer(Pointer libHandle, String eArqConfig);

    public int BAL_ConfigGravar(Pointer libHandle, String eArqConfig);

    public int BAL_ConfigLerValor(Pointer libHandle, String eSessao, String eChave, ByteBuffer sValor, IntByReference esTamanho);

    public int BAL_ConfigGravarValor(Pointer libHandle, String eSessao, String eChave, String sValor);

    public int BAL_ConfigImportar(Pointer libHandle, String eArqConfig);

    public int BAL_ConfigExportar(Pointer libHandle, ByteBuffer sMensagem, IntByReference esTamanho);

    public int BAL_Ativar(Pointer libHandle);

    public int BAL_Desativar(Pointer libHandle);

    public int BAL_SolicitarPeso(Pointer libHandle);

    public int BAL_LePeso(Pointer libHandle, int MillisecTimeOut, DoubleByReference Peso);

    public int BAL_LePesoStr(Pointer libHandle, int MillisecTimeOut, ByteBuffer sValor);

    public int BAL_UltimoPesoLido(Pointer libHandle, DoubleByReference Peso);

    public int BAL_UltimoPesoLidoStr(Pointer libHandle, ByteBuffer sValor);

    public int BAL_InterpretarRespostaPeso(Pointer libHandle, String aResposta, DoubleByReference Peso);

    public int BAL_InterpretarRespostaPesoStr(Pointer libHandle, String aResposta, ByteBuffer sValor);
}
