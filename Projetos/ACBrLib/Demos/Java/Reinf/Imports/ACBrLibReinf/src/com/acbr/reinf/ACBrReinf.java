package com.acbr.reinf;

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

public final class ACBrReinf extends ACBrLibBase {

    private interface ACBrReinfLib extends Library {

        static String JNA_LIBRARY_NAME = LibraryLoader.getLibraryName();
        public final static ACBrReinfLib INSTANCE = LibraryLoader.getInstance();

        class LibraryLoader {

            private static String library = "";
            private static ACBrReinfLib instance = null;

            private static String getLibraryName() {
                if (library.isEmpty()) {
                    if (Platform.isWindows()) {
                        library = Platform.is64Bit() ? "ACBrReinf64" : "ACBrReinf32";
                    } else {
                        library = Platform.is64Bit() ? "ACBrReinf64" : "ACBrReinf32";
                    }
                }
                return library;
            }

            public static ACBrReinfLib getInstance() {
                if (instance == null) {
                    instance = (ACBrReinfLib) Native.synchronizedLibrary(
                            (Library) Native.load(JNA_LIBRARY_NAME, ACBrReinfLib.class));
                }

                return instance;
            }
        }

        int Reinf_Inicializar(String eArqConfig, String eChaveCrypt);

        int Reinf_Finalizar();

        int Reinf_Nome(ByteBuffer buffer, IntByReference bufferSize);

        int Reinf_Versao(ByteBuffer buffer, IntByReference bufferSize);

        int Reinf_UltimoRetorno(ByteBuffer buffer, IntByReference bufferSize);

        int Reinf_ConfigImportar(String eArqConfig);

        int Reinf_ConfigExportar(ByteBuffer buffer, IntByReference bufferSize);

        int Reinf_ConfigLer(String eArqConfig);

        int Reinf_ConfigGravar(String eArqConfig);

        int Reinf_ConfigLerValor(String eSessao, String eChave, ByteBuffer buffer, IntByReference bufferSize);

        int Reinf_ConfigGravarValor(String eSessao, String eChave, String valor);

        int Reinf_CriarEventoReinf(String eArqIni);
        
        int Reinf_EnviarReinf(ByteBuffer buffer, IntByReference bufferSize);
        
        int Reinf_ConsultarReinf(String eProtocolo, ByteBuffer buffer, IntByReference bufferSize);
    
        int Reinf_ConsultarReciboReinf(String ePerApur, int aTipoEvento, String eNrInscEstab, 
                String eCnpjPrestador, String eNrInscTomador, String eDtApur, 
                String eCpfCnpjBenef, String eCnpjFonte, 
                ByteBuffer buffer, IntByReference bufferSize);
       
        int Reinf_CriarEnviarReinf(String eArqIni, ByteBuffer buffer, IntByReference bufferSize);
        
        int Reinf_LimparReinf();
        
        int Reinf_CarregarXMLEventoReinf(String eArquivoOuXML);
        
        int Reinf_SetIDContribuinte(String aIdContribuinte);
        
        int Reinf_SetIDTransmissor(String aIdTransmissor);
        
        int Reinf_SetTipoContribuinte(Integer aTipoContribuinte);
        
        int Reinf_SetVersaoDF(String sVersao);
        
        int Reinf_ObterCertificados(ByteBuffer buffer, IntByReference bufferSize);
    }

    public ACBrReinf() throws Exception {
        File iniFile = Paths.get(System.getProperty("user.dir"), "ACBrLib.ini").toFile();
        if (!iniFile.exists()) {
            iniFile.createNewFile();
        }

        int ret = ACBrReinfLib.INSTANCE.Reinf_Inicializar(toUTF8(iniFile.getAbsolutePath()), toUTF8(""));
        checkResult(ret);
    }

    public ACBrReinf(String eArqConfig, String eChaveCrypt) throws Exception {
        int ret = ACBrReinfLib.INSTANCE.Reinf_Inicializar(toUTF8(eArqConfig), toUTF8(eChaveCrypt));
        checkResult(ret);
    }

    @Override
    protected void dispose() throws Exception {
        int ret = ACBrReinfLib.INSTANCE.Reinf_Finalizar();
        checkResult(ret);
    }

    public String nome() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrReinfLib.INSTANCE.Reinf_Nome(buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());
    }

    public String versao() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrReinfLib.INSTANCE.Reinf_Versao(buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());
    }

    public void configLer() throws Exception {
        configLer("");
    }

    public void configLer(String eArqConfig) throws Exception {
        int ret = ACBrReinfLib.INSTANCE.Reinf_ConfigLer(toUTF8(eArqConfig));
        checkResult(ret);
    }

    public void configGravar() throws Exception {
        configGravar("");
    }

    public void configGravar(String eArqConfig) throws Exception {
        int ret = ACBrReinfLib.INSTANCE.Reinf_ConfigGravar(toUTF8(eArqConfig));
        checkResult(ret);
    }

    @Override
    public String configLerValor(ACBrSessao eSessao, String eChave) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrReinfLib.INSTANCE.Reinf_ConfigLerValor(toUTF8(eSessao.name()), toUTF8(eChave), buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);
    }

    @Override
    public void configGravarValor(ACBrSessao eSessao, String eChave, Object value) throws Exception {
        int ret = ACBrReinfLib.INSTANCE.Reinf_ConfigGravarValor(toUTF8(eSessao.name()), toUTF8(eChave), toUTF8(value.toString()));
        checkResult(ret);
    }

    public void ConfigImportar(String eArqConfig) throws Exception {
        int ret = ACBrReinfLib.INSTANCE.Reinf_ConfigImportar(eArqConfig);
        checkResult(ret);
    }

    public String ConfigExportar() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrReinfLib.INSTANCE.Reinf_ConfigExportar(buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);
    }
    
    public void criarEventoReinf(String eArqIni) throws Exception {
        int ret = ACBrReinfLib.INSTANCE.Reinf_CriarEventoReinf(eArqIni);
        checkResult(ret);
    }
    
    public String enviarReinf() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrReinfLib.INSTANCE.Reinf_EnviarReinf(buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }
    
    public String consultarReinf(String eProtocolo) throws Exception {
        // Buffer deve ser maior que 256 para suportar o retorno completo
        ByteBuffer buffer = ByteBuffer.allocate(16384);
        IntByReference bufferLen = new IntByReference(16384);
        
        int ret = ACBrReinfLib.INSTANCE.Reinf_ConsultarReinf(eProtocolo, buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }
    
    public String consultarReciboReinf(String ePerApur, int aTipoEvento, String eNrInscEstab, 
                String eCnpjPrestador, String eNrInscTomador, String eDtApur, 
                String eCpfCnpjBenef, String eCnpjFonte) throws Exception {
        
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrReinfLib.INSTANCE.Reinf_ConsultarReciboReinf(ePerApur, aTipoEvento, eNrInscEstab, 
                eCnpjPrestador, eNrInscTomador, eDtApur, 
                eCpfCnpjBenef, eCnpjFonte, buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }
    
    public String criarEnviarReinf(String eArqIni) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrReinfLib.INSTANCE.Reinf_CriarEnviarReinf(eArqIni, buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }
    
    public void limparReinf() throws Exception {
        int ret = ACBrReinfLib.INSTANCE.Reinf_LimparReinf();
        checkResult(ret);
    }
    
    public void carregarXMLEventoReinf(String eArquivoOuXML) throws Exception{
        int ret = ACBrReinfLib.INSTANCE.Reinf_CarregarXMLEventoReinf(eArquivoOuXML);
        checkResult(ret);
    }
    
    public void setIDContribuinte(String aIdContribuinte) throws Exception {
        int ret = ACBrReinfLib.INSTANCE.Reinf_SetIDContribuinte(aIdContribuinte);
        checkResult(ret);
    }
    
    public void setIDTransmissor(String aIdTransmissor) throws Exception {
        int ret = ACBrReinfLib.INSTANCE.Reinf_SetIDTransmissor(aIdTransmissor);
        checkResult(ret);
    }
    
    public void setTipoContribuinte(Integer aTipoContribuinte) throws Exception {
        int ret = ACBrReinfLib.INSTANCE.Reinf_SetTipoContribuinte(aTipoContribuinte);
        checkResult(ret);
    }
    
    public void setVersaoDF(String sVersao) throws Exception {
        int ret = ACBrReinfLib.INSTANCE.Reinf_SetVersaoDF(sVersao);
        checkResult(ret);
    }
    
    public String obterCertificados() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrReinfLib.INSTANCE.Reinf_ObterCertificados(buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
    
    @Override
    protected void UltimoRetorno(ByteBuffer buffer, IntByReference bufferLen) {
        ACBrReinfLib.INSTANCE.Reinf_UltimoRetorno(buffer, bufferLen);
    }
}
