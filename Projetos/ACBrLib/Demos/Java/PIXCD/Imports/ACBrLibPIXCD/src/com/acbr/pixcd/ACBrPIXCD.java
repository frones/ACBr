package com.acbr.pixcd;

import com.acbr.ACBrLibBase;
import com.acbr.ACBrSessao;
import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Platform;
import com.sun.jna.ptr.IntByReference;

import java.io.File;
import java.nio.ByteBuffer;
import java.nio.file.Paths;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;

public final class ACBrPIXCD extends ACBrLibBase {

    private interface ACBrPIXCDLib extends Library {

        static String JNA_LIBRARY_NAME = LibraryLoader.getLibraryName();
        public final static ACBrPIXCDLib INSTANCE = LibraryLoader.getInstance();

        class LibraryLoader {

            private static String library = "";
            private static ACBrPIXCDLib instance = null;

            private static String getLibraryName() {
                if (library.isEmpty()) {
                    if (Platform.isWindows()) {
                        library = Platform.is64Bit() ? "ACBrPIXCD64" : "ACBrPIXCD32";
                    } else {
                        library = Platform.is64Bit() ? "acbrpixcd64" : "acbrpixcd32";
                    }
                }
                return library;
            }

            public static ACBrPIXCDLib getInstance() {
                if (instance == null) {
                    instance = (ACBrPIXCDLib) Native.synchronizedLibrary(
                            (Library) Native.load(JNA_LIBRARY_NAME, ACBrPIXCDLib.class));
                }

                return instance;
            }
        }

        int PIXCD_Inicializar(String eArqConfig, String eChaveCrypt);

        int PIXCD_Finalizar();

        int PIXCD_Nome(ByteBuffer buffer, IntByReference bufferSize);

        int PIXCD_Versao(ByteBuffer buffer, IntByReference bufferSize);

        int PIXCD_UltimoRetorno(ByteBuffer buffer, IntByReference bufferSize);

        int PIXCD_ConfigImportar(String eArqConfig);

        int PIXCD_ConfigExportar(ByteBuffer buffer, IntByReference bufferSize);

        int PIXCD_ConfigLer(String eArqConfig);

        int PIXCD_ConfigGravar(String eArqConfig);

        int PIXCD_ConfigLerValor(String eSessao, String eChave, ByteBuffer buffer, IntByReference bufferSize);

        int PIXCD_ConfigGravarValor(String eSessao, String eChave, String valor);

        int PIXCD_ConfigGravarValor(String eArquivoOuXML);
        
        int PIXCD_GerarQRCodeEstatico(Double AValor, String AinfoAdicional, String ATxID, ByteBuffer buffer, IntByReference bufferSize);
        
        int PIXCD_ConsultarPix(String Ae2eid, ByteBuffer buffer, IntByReference bufferSize);
    
        int PIXCD_ConsultarPixRecebidos(Date ADataInicio, Date ADataFim, String ATxId, String ACpfCnpj, Integer PagAtual, Integer ItensPorPagina, ByteBuffer buffer, IntByReference bufferSize);
    
        int PIXCD_SolicitarDevolucaoPix(String AInfDevolucao, String Ae2eid, String AidDevolucao, ByteBuffer buffer, IntByReference bufferSize);
        
        int PIXCD_ConsultarDevolucaoPix(String Ae2eid, String AidDevolucao, ByteBuffer buffer, IntByReference bufferSize);
        
        int PIXCD_CriarCobrancaImediata(String AInfCobSolicitada, String ATxId, ByteBuffer buffer, IntByReference bufferSize);
        
        int PIXCD_ConsultarCobrancaImediata(String ATxId, Integer ARevisao, ByteBuffer buffer, IntByReference bufferSize);
        
        int PIXCD_RevisarCobrancaImediata(String AInfCobRevisada, String ATxId, ByteBuffer buffer, IntByReference bufferSize);
        
        int PIXCD_CancelarCobrancaImediata(String ATxId, ByteBuffer buffer, IntByReference bufferSize);
    
        int PIXCD_CriarCobranca(String AInfCobVSolicitada, String ATxId, ByteBuffer buffer, IntByReference bufferSize);
        
        int PIXCD_ConsultarCobranca(String ATxId, Integer ARevisao, ByteBuffer buffer, IntByReference bufferSize);
        
        int PIXCD_RevisarCobranca(String AInfCobVRevisada, String ATxId, ByteBuffer buffer, IntByReference bufferSize);
        
        int PIXCD_CancelarCobranca(String ATxId, ByteBuffer buffer, IntByReference bufferSize);
    }

    public ACBrPIXCD() throws Exception {
        File iniFile = Paths.get(System.getProperty("user.dir"), "ACBrLib.ini").toFile();
        if (!iniFile.exists()) {
            iniFile.createNewFile();
        }

        int ret = ACBrPIXCDLib.INSTANCE.PIXCD_Inicializar(toUTF8(iniFile.getAbsolutePath()), toUTF8(""));
        checkResult(ret);
    }

    public ACBrPIXCD(String eArqConfig, String eChaveCrypt) throws Exception {
        int ret = ACBrPIXCDLib.INSTANCE.PIXCD_Inicializar(toUTF8(eArqConfig), toUTF8(eChaveCrypt));
        checkResult(ret);
    }

    @Override
    protected void dispose() throws Exception {
        int ret = ACBrPIXCDLib.INSTANCE.PIXCD_Finalizar();
        checkResult(ret);
    }

    public String nome() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrPIXCDLib.INSTANCE.PIXCD_Nome(buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());
    }

    public String versao() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrPIXCDLib.INSTANCE.PIXCD_Versao(buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());
    }

    public void configLer() throws Exception {
        configLer("");
    }

    public void configLer(String eArqConfig) throws Exception {
        int ret = ACBrPIXCDLib.INSTANCE.PIXCD_ConfigLer(toUTF8(eArqConfig));
        checkResult(ret);
    }

    public void configGravar() throws Exception {
        configGravar("");
    }

    public void configGravar(String eArqConfig) throws Exception {
        int ret = ACBrPIXCDLib.INSTANCE.PIXCD_ConfigGravar(toUTF8(eArqConfig));
        checkResult(ret);
    }

    @Override
    public String configLerValor(ACBrSessao eSessao, String eChave) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrPIXCDLib.INSTANCE.PIXCD_ConfigLerValor(toUTF8(eSessao.name()), toUTF8(eChave), buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);
    }

    @Override
    public void configGravarValor(ACBrSessao eSessao, String eChave, Object value) throws Exception {
        int ret = ACBrPIXCDLib.INSTANCE.PIXCD_ConfigGravarValor(toUTF8(eSessao.name()), toUTF8(eChave), toUTF8(value.toString()));
        checkResult(ret);
    }
    
    public void ConfigImportar(String eArqConfig) throws Exception {

        int ret = ACBrPIXCDLib.INSTANCE.PIXCD_ConfigImportar(eArqConfig);
        checkResult(ret);
    }

    public String ConfigExportar() throws Exception {

        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrPIXCDLib.INSTANCE.PIXCD_ConfigExportar(buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());
    }

    @Override
    protected void UltimoRetorno(ByteBuffer buffer, IntByReference bufferLen) {
        ACBrPIXCDLib.INSTANCE.PIXCD_UltimoRetorno(buffer, bufferLen);
    }
    
    public String GerarQRCodeEstatico(Double AValor, String AinfoAdicional, String ATxID) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrPIXCDLib.INSTANCE.PIXCD_GerarQRCodeEstatico(AValor, toUTF8(AinfoAdicional), toUTF8(ATxID), buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
    
    public String ConsultarPix(String Ae2eid) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrPIXCDLib.INSTANCE.PIXCD_ConsultarPix(toUTF8(Ae2eid), buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
    
    public String ConsultarPixRecebidos(Date ADataInicio, Date ADataFim, String ATxId, String ACpfCnpj, Integer PagAtual, Integer ItensPorPagina) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrPIXCDLib.INSTANCE.PIXCD_ConsultarPixRecebidos(ADataInicio, ADataFim, toUTF8(ATxId), toUTF8(ACpfCnpj), PagAtual, ItensPorPagina, buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
    
    public String SolicitarDevolucaoPix(String AInfDevolucao, String Ae2eid, String AidDevolucao) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrPIXCDLib.INSTANCE.PIXCD_SolicitarDevolucaoPix(toUTF8(AInfDevolucao), toUTF8(Ae2eid), AidDevolucao, buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
    
    public String ConsultarDevolucaoPix(String Ae2eid, String AidDevolucao) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrPIXCDLib.INSTANCE.PIXCD_ConsultarDevolucaoPix(toUTF8(Ae2eid), toUTF8(AidDevolucao), buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
    
    public String CriarCobrancaImediata(String AInfCobSolicitada, String ATxId) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrPIXCDLib.INSTANCE.PIXCD_CriarCobrancaImediata(toUTF8(AInfCobSolicitada), toUTF8(ATxId), buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
    
    public String ConsultarCobrancaImediata(String ATxId, Integer ARevisao) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrPIXCDLib.INSTANCE.PIXCD_ConsultarCobrancaImediata(toUTF8(ATxId), ARevisao, buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
    
    public String RevisarCobrancaImediata(String AInfCobRevisada, String ATxId) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrPIXCDLib.INSTANCE.PIXCD_RevisarCobrancaImediata(toUTF8(AInfCobRevisada), toUTF8(ATxId), buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
    
    public String CancelarCobrancaImediata(String ATxId) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrPIXCDLib.INSTANCE.PIXCD_CancelarCobrancaImediata(toUTF8(ATxId), buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
    
    public String CriarCobranca(String AInfCobVSolicitada, String ATxId) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrPIXCDLib.INSTANCE.PIXCD_CriarCobranca(toUTF8(AInfCobVSolicitada), toUTF8(ATxId), buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
    
    public String ConsultarCobranca(String ATxId, Integer ARevisao) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrPIXCDLib.INSTANCE.PIXCD_ConsultarCobranca(toUTF8(ATxId), ARevisao, buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
    
    public String RevisarCobranca(String AInfCobVRevisada, String ATxId) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrPIXCDLib.INSTANCE.PIXCD_RevisarCobranca(toUTF8(AInfCobVRevisada), toUTF8(ATxId), buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
    
    public String CancelarCobranca(String ATxId) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrPIXCDLib.INSTANCE.PIXCD_CancelarCobranca(toUTF8(ATxId), buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
}
