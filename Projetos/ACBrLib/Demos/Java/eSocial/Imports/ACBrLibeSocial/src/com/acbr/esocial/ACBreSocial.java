package com.acbr.esocial;

import com.acbr.ACBrLibBase;
import com.acbr.ACBrSessao;
import com.acbr.nfe.TipoPathNFe;
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

public final class ACBreSocial extends ACBrLibBase {

    private interface ACBreSocialLib extends Library {

        static String JNA_LIBRARY_NAME = LibraryLoader.getLibraryName();
        public final static ACBreSocialLib INSTANCE = LibraryLoader.getInstance();

        class LibraryLoader {

            private static String library = "";
            private static ACBreSocialLib instance = null;

            private static String getLibraryName() {
                if (library.isEmpty()) {
                    if (Platform.isWindows()) {
                        library = Platform.is64Bit() ? "ACBreSocial64" : "ACBreSocial32";
                    } else {
                        library = Platform.is64Bit() ? "acbresocial64" : "acbresocial32";
                    }
                }
                return library;
            }

            public static ACBreSocialLib getInstance() {
                if (instance == null) {
                    instance = (ACBreSocialLib) Native.synchronizedLibrary(
                            (Library) Native.load(JNA_LIBRARY_NAME, ACBreSocialLib.class));
                }

                return instance;
            }
        }

        int eSocial_Inicializar(String eArqConfig, String eChaveCrypt);

        int eSocial_Finalizar();

        int eSocial_Nome(ByteBuffer buffer, IntByReference bufferSize);

        int eSocial_Versao(ByteBuffer buffer, IntByReference bufferSize);

        int eSocial_UltimoRetorno(ByteBuffer buffer, IntByReference bufferSize);

        int eSocial_ConfigImportar(String eArqConfig);

        int eSocial_ConfigExportar(ByteBuffer buffer, IntByReference bufferSize);

        int eSocial_ConfigLer(String eArqConfig);

        int eSocial_ConfigGravar(String eArqConfig);

        int eSocial_ConfigLerValor(String eSessao, String eChave, ByteBuffer buffer, IntByReference bufferSize);

        int eSocial_ConfigGravarValor(String eSessao, String eChave, String valor);

        int eSocial_CriarEventoeSocial(String eArqIni);
        
        int eSocial_EnviareSocial(Integer aGrupo, ByteBuffer buffer, IntByReference bufferSize);
        
        int eSocial_ConsultareSocial(String eProtocolo, ByteBuffer buffer, IntByReference bufferSize);
    
        int eSocial_CriarEnviareSocial(String eArqIni, Integer aGrupo);
        
        int eSocial_LimpareSocial();
        
        int eSocial_CarregarXMLEventoeSocial(String eArquivoOuXML);
        
        int eSocial_SetIDEmpregador(String aIdEmpregador);
        
        int eSocial_SetIDTransmissor (String aIdTransmissor);
        
        int eSocial_SetTipoEmpregador(Integer aTipoEmpregador);
        
        int eSocial_SetVersaoDF(String sVersao);
        
        int eSocial_ConsultaIdentificadoresEventosEmpregador(String aIdEmpregador, Integer aTipoEvento, double aPeriodoApuracao, ByteBuffer buffer, IntByReference bufferSize);
    
        int eSocial_ConsultaIdentificadoresEventosTabela(String aIdEmpregador, Integer aTipoEvento, String aChave, double aDataInicial, double aDataFinal, ByteBuffer buffer, IntByReference bufferSize);
        
        int eSocial_ConsultaIdentificadoresEventosTrabalhador(String aIdEmpregador, String aCPFTrabalhador, double aDataInicial, double aDataFinal, ByteBuffer buffer, IntByReference bufferSize);
    
        int eSocial_DownloadEventos(String aIdEmpregador, String aCPFTrabalhador, double aDataInicial, double aDataFinal, ByteBuffer buffer, IntByReference bufferSize);
        
        int eSocial_ObterCertificados(ByteBuffer buffer, IntByReference bufferSize);
    }

    public ACBreSocial() throws Exception {
        File iniFile = Paths.get(System.getProperty("user.dir"), "ACBrLib.ini").toFile();
        if (!iniFile.exists()) {
            iniFile.createNewFile();
        }

        int ret = ACBreSocialLib.INSTANCE.eSocial_Inicializar(toUTF8(iniFile.getAbsolutePath()), toUTF8(""));
        checkResult(ret);
    }

    public ACBreSocial(String eArqConfig, String eChaveCrypt) throws Exception {
        int ret = ACBreSocialLib.INSTANCE.eSocial_Inicializar(toUTF8(eArqConfig), toUTF8(eChaveCrypt));
        checkResult(ret);
    }

    @Override
    protected void dispose() throws Exception {
        int ret = ACBreSocialLib.INSTANCE.eSocial_Finalizar();
        checkResult(ret);
    }

    public String nome() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBreSocialLib.INSTANCE.eSocial_Nome(buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());
    }

    public String versao() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBreSocialLib.INSTANCE.eSocial_Versao(buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());
    }

    public void configLer() throws Exception {
        configLer("");
    }

    public void configLer(String eArqConfig) throws Exception {
        int ret = ACBreSocialLib.INSTANCE.eSocial_ConfigLer(toUTF8(eArqConfig));
        checkResult(ret);
    }

    public void configGravar() throws Exception {
        configGravar("");
    }

    public void configGravar(String eArqConfig) throws Exception {
        int ret = ACBreSocialLib.INSTANCE.eSocial_ConfigGravar(toUTF8(eArqConfig));
        checkResult(ret);
    }

    @Override
    public String configLerValor(ACBrSessao eSessao, String eChave) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBreSocialLib.INSTANCE.eSocial_ConfigLerValor(toUTF8(eSessao.name()), toUTF8(eChave), buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);
    }

    @Override
    public void configGravarValor(ACBrSessao eSessao, String eChave, Object value) throws Exception {
        int ret = ACBreSocialLib.INSTANCE.eSocial_ConfigGravarValor(toUTF8(eSessao.name()), toUTF8(eChave), toUTF8(value.toString()));
        checkResult(ret);
    }


    public void ConfigImportar(String eArqConfig) throws Exception {

        int ret = ACBreSocialLib.INSTANCE.eSocial_ConfigImportar(eArqConfig);
        checkResult(ret);

    }

    public String ConfigExportar() throws Exception {

        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBreSocialLib.INSTANCE.eSocial_ConfigExportar(buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);

    }
    
    public void criarEventoeSocial(String eArqIni) throws Exception {
        int ret = ACBreSocialLib.INSTANCE.eSocial_CriarEventoeSocial(eArqIni);
        checkResult(ret);
    }
    
    public String enviareSocial(Integer aGrupo) throws Exception {
        
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBreSocialLib.INSTANCE.eSocial_EnviareSocial(aGrupo, buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }
    
    public String consultareSocial(String eProtocolo) throws Exception {
        
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBreSocialLib.INSTANCE.eSocial_ConsultareSocial(eProtocolo, buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }
    
    public void criarEnviareSocial(String eArqIni, Integer aGrupo) throws Exception {
        int ret = ACBreSocialLib.INSTANCE.eSocial_CriarEnviareSocial(eArqIni, aGrupo);
        checkResult(ret);
    }
    
    public void limpareSocial() throws Exception {
        int ret = ACBreSocialLib.INSTANCE.eSocial_LimpareSocial();
        checkResult(ret);
    }
    
    public void carregarXMLEventoeSocial(String eArquivoOuXML) throws Exception{
        int ret = ACBreSocialLib.INSTANCE.eSocial_CarregarXMLEventoeSocial(eArquivoOuXML);
        checkResult(ret);
    }
    
    public void setIDEmpregador(String aIdEmpregador) throws Exception {
        int ret = ACBreSocialLib.INSTANCE.eSocial_SetIDEmpregador(aIdEmpregador);
        checkResult(ret);
    }
    
    public void setIDTransmissor(String aIdTransmissor) throws Exception {
        int ret = ACBreSocialLib.INSTANCE.eSocial_SetIDTransmissor(aIdTransmissor);
        checkResult(ret);
    }
    
    public void setTipoEmpregador(Integer aTipoEmpregador) throws Exception {
        int ret = ACBreSocialLib.INSTANCE.eSocial_SetTipoEmpregador(aTipoEmpregador);
        checkResult(ret);
    }
    
    public void setVersaoDF(String sVersao) throws Exception {
        int ret = ACBreSocialLib.INSTANCE.eSocial_SetVersaoDF(sVersao);
        checkResult(ret);
    }
    
    public String consultaIdentificadoresEventosEmpregador(String aIdEmpregador, Integer aTipoEvento, double aPeriodoApuracao) throws Exception{
        
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBreSocialLib.INSTANCE.eSocial_ConsultaIdentificadoresEventosEmpregador(aIdEmpregador, aTipoEvento, aPeriodoApuracao, buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }
    
    public String consultaIdentificadoresEventosTabela(String aIdEmpregador, Integer aTipoEvento, String aChave, double aDataInicial, double aDataFinal) throws Exception {
        
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBreSocialLib.INSTANCE.eSocial_ConsultaIdentificadoresEventosTabela(aIdEmpregador, aTipoEvento, aChave, aDataInicial, aDataFinal, buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }
    
    public String consultaIdentificadoresEventosTrabalhador(String aIdEmpregador, String aCPFTrabalhador, double aDataInicial, double aDataFinal)throws Exception {
        
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBreSocialLib.INSTANCE.eSocial_ConsultaIdentificadoresEventosTrabalhador(aIdEmpregador, aCPFTrabalhador, aDataInicial, aDataFinal, buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }
    
    public String downloadEventos(String aIdEmpregador, String aCPFTrabalhador, double aDataInicial, double aDataFinal) throws Exception {
        
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBreSocialLib.INSTANCE.eSocial_DownloadEventos(aIdEmpregador, aCPFTrabalhador, aDataInicial, aDataFinal, buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }
    
    public String obterCertificados() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBreSocialLib.INSTANCE.eSocial_ObterCertificados(buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
    
    @Override
    protected void UltimoRetorno(ByteBuffer buffer, IntByReference bufferLen) {
        ACBreSocialLib.INSTANCE.eSocial_UltimoRetorno(buffer, bufferLen);
    }
}
