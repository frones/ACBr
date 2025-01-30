package br.com.acbr.lib.pixcd.bridge;

import com.sun.jna.Library;
import com.sun.jna.Pointer;
import com.sun.jna.ptr.IntByReference;
import com.sun.jna.ptr.PointerByReference;

import java.nio.ByteBuffer;
import java.util.Date;

public interface ACBrPIXCDBridge extends Library {

    /* Métodos de ACBrLibComum */
    public int PIXCD_Inicializar(PointerByReference libHandle, String eArqConfig, String eChaveCrypt);

    public int PIXCD_Finalizar(Pointer libHandle);

    public int PIXCD_Nome(Pointer libHandle,ByteBuffer sNome, IntByReference esTamanaho);

    public int PIXCD_Versao(Pointer libHandle,ByteBuffer sVersao, IntByReference esTamanaho);

    public int PIXCD_OpenSSLInfo(Pointer libHandle,ByteBuffer sOpenSslInfo, IntByReference esTamanho);

    public int PIXCD_UltimoRetorno(Pointer libHandle,ByteBuffer sMensagem, IntByReference sTamanho);

    public int PIXCD_ConfigImportar(Pointer libHandle,String eArqConfig);

    public int PIXCD_ConfigExportar(Pointer libHandle,ByteBuffer sMensagem, IntByReference esTamanaho);

    public int PIXCD_ConfigLer(Pointer libHandle, String eArqConfig);

    public int PIXCD_ConfigGravar(Pointer libHandle,String eArqConfig);

    public int PIXCD_ConfigLerValor(Pointer libHandle,String eSessao, String eChave, ByteBuffer sValor, IntByReference esTamanaho);

    public int PIXCD_ConfigGravarValor(Pointer libHandle,String eSessao,String  eChave, String eValor);

    /* Métodos de ACBrLibPIXCD */
    public int PIXCD_GerarQRCodeEstatico(Pointer libHandle, double AValor, String AInfoAdicional, String ATxID, ByteBuffer sResposta, IntByReference esTamanho);

    public int PIXCD_ConsultarPix(Pointer libHandle, String Ae2eid, ByteBuffer sResposta, IntByReference esTamanho);

    public int PIXCD_ConsultarPixRecebidos(Pointer libHandle, double ADataInicio, double ADataFim, String ATxId, String ACpfCnpj, int PagAtual,
                                           int ItensPorPagina, ByteBuffer sResposta, IntByReference esTamanho);

    public int PIXCD_SolicitarDevolucaoPix(Pointer libHandle, String AInfDevolucao, String Ae2eid, String AidDevolucao, ByteBuffer sResposta, IntByReference esTamanho);

    public int PIXCD_ConsultarDevolucaoPix(Pointer libHandle, String Ae2eid, String AidDevolucao, ByteBuffer sResposta, IntByReference esTamanho);

    public int PIXCD_CriarCobrancaImediata(Pointer libHandle, String AInfCobSolicitada, String ATxId, ByteBuffer sResposta, IntByReference esTamanho);

    public int PIXCD_ConsultarCobrancaImediata(Pointer libHandle, String ATxId, int ARevisao, ByteBuffer sResposata, IntByReference esTamanho);

    public int PIXCD_ConsultarCobrancasCob(Pointer libHandle, double ADataInicio, double ADataFim, String ACpfCnpj, boolean ALocationPresente, int AStatus,
                                           int PagAtual, int ItensPorPagina, ByteBuffer sResposta, IntByReference esTamanho);

    public int PIXCD_RevisarCobrancaImediata(Pointer libHandle, String AInfCobRevisada, String ATxId, ByteBuffer sResposta, IntByReference esTamanho);

    public int PIXCD_CancelarCobrancaImediata(Pointer libHandle, String ATxId, ByteBuffer sResposta, IntByReference esTamanho);

    public int PIXCD_CriarCobranca(Pointer libHandle, String AInfCobVSolicitada, String ATxId, ByteBuffer sResposta, IntByReference esTamanho);

    public int PIXCD_ConsultarCobranca(Pointer libHandle, String ATxId, int ARevisao, ByteBuffer sResposta, IntByReference esTamanho);

    public int PIXCD_ConsultarCobrancasCobV(Pointer libHandle, double ADataInicio, double ADataFim, String ACpfCnpj, boolean ALocationPresente, int AStatus, int PagAtual,
                                            int ItensPorPagina, ByteBuffer sResposta, IntByReference esTamanho);

    public int PIXCD_RevisarCobranca(Pointer libHandle, String AInfCobVRevisada, String ATxId, ByteBuffer sResposta, IntByReference esTamanho);

    public int PIXCD_CancelarCobranca(Pointer libHandle, String ATxId, ByteBuffer sResposta, IntByReference esTamanho);
}
