package br.com.acbr.lib.consultacnpj.bridge;

import com.sun.jna.Library;
import com.sun.jna.Pointer;
import com.sun.jna.ptr.IntByReference;
import com.sun.jna.ptr.PointerByReference;

import java.nio.ByteBuffer;

public interface ACBrConsultaCNPJBridge extends Library {
    /* Métodos de ACBrLibComum */
    public int CNPJ_Inicializar(PointerByReference libHandle, String eArqConfig, String eChaveCrypt);
    public int CNPJ_Finalizar(Pointer libHandle);

    public int CNPJ_ConfigGravarValor(Pointer libHandle,String eSessao,String  eChave, String eValor);
    public int CNPJ_ConfigLer(Pointer libHandle,String eArqConfig);
    public int CNPJ_ConfigLerValor(Pointer libHandle,String eSessao, String eChave, ByteBuffer sValor, IntByReference esTamanaho);
    public int CNPJ_ConfigImportar(Pointer libHandle,String eArqConfig);
    public int CNPJ_ConfigExportar(Pointer libHandle,ByteBuffer sMensagem, IntByReference esTamanaho);
    public int CNPJ_ConfigGravar(Pointer libHandle,String eArqConfig);

    public int CNPJ_UltimoRetorno(Pointer libHandle,ByteBuffer sMensagem, IntByReference sTamanho);
    public int CNPJ_Nome(Pointer libHandle,ByteBuffer sNome, IntByReference esTamanaho);
    public int CNPJ_Versao(Pointer libHandle,ByteBuffer sVersao, IntByReference esTamanaho);

    public int  CNPJ_OpenSSLInfo(Pointer libHandle,ByteBuffer sOpenSslInfo, IntByReference esTamanho);

    /* Métodos de ACBrLibConsultaCNPJ */
    public int CNPJ_Consultar(Pointer libHandle, String eCNPJ, ByteBuffer sResposta, IntByReference esTamanho);
}