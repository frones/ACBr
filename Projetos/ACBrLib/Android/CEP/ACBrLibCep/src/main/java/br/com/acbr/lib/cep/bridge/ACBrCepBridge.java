package br.com.acbr.lib.cep.bridge;

import com.sun.jna.Library;
import com.sun.jna.Pointer;
import com.sun.jna.ptr.IntByReference;
import com.sun.jna.ptr.PointerByReference;

import java.nio.ByteBuffer;

public interface ACBrCepBridge  extends Library {
    /* Métodos de ACBrLibComum */
    public int CEP_Inicializar(PointerByReference libHandle, String eArqConfig, String eChaveCrypt);
    public int CEP_Finalizar(Pointer libHandle);

    public int CEP_ConfigGravarValor(Pointer libHandle,String eSessao,String  eChave, String eValor);
    public int CEP_ConfigLer(Pointer libHandle,String eArqConfig);
    public int CEP_ConfigLerValor(Pointer libHandle,String eSessao, String eChave, ByteBuffer sValor, IntByReference esTamanaho);
    public int CEP_ConfigImportar(Pointer libHandle,String eArqConfig);
    public int CEP_ConfigExportar(Pointer libHandle,ByteBuffer sMensagem, IntByReference esTamanaho);
    public int CEP_ConfigGravar(Pointer libHandle,String eArqConfig);

    public int CEP_UltimoRetorno(Pointer libHandle,ByteBuffer sMensagem, IntByReference sTamanho);
    public int CEP_Nome(Pointer libHandle,ByteBuffer sNome, IntByReference esTamanaho);
    public int CEP_Versao(Pointer libHandle,ByteBuffer sVersao, IntByReference esTamanaho);

    public int  CEP_OpenSSLInfo(Pointer libHandle,ByteBuffer sOpenSslInfo, IntByReference esTamanho);

    /* Métodos de ACBrLibCEP */
    public int CEP_BuscarPorCEP(Pointer libHandle, String eCep,
                                ByteBuffer eCepsResposta, IntByReference esTamanaho);
    public int CEP_BuscarPorLogradouro(Pointer libHandle, String eCidade, String eTipo, String eEndereco,
                                       String eEstado, String eBairro,
                                       ByteBuffer sResposta, IntByReference esTamanho);
}