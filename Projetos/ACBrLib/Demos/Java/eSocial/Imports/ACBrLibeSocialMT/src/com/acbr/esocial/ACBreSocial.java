package com.acbr.esocial;

import com.acbr.ACBrLibBase;
import com.acbr.ACBrSessao;
import com.acbr.nfe.TipoPathNFe;
import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Platform;
import com.sun.jna.Pointer;
import com.sun.jna.ptr.IntByReference;
import com.sun.jna.ptr.PointerByReference;

import java.io.File;
import java.nio.ByteBuffer;
import java.nio.file.Paths;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;

//lib MT

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

        int eSocial_Inicializar(PointerByReference libHandler, String eArqConfig, String eChaveCrypt);

        int eSocial_Finalizar(Pointer libHandler);

        int eSocial_Nome(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize);

        int eSocial_Versao(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize);

        int eSocial_UltimoRetorno(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize);

        int eSocial_ConfigImportar(Pointer libHandler, String eArqConfig);

        int eSocial_ConfigExportar(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize);

        int eSocial_ConfigLer(Pointer libHandler, String eArqConfig);

        int eSocial_ConfigGravar(Pointer libHandler, String eArqConfig);

        int eSocial_ConfigLerValor(Pointer libHandler, String eSessao, String eChave, ByteBuffer buffer, IntByReference bufferSize);

        int eSocial_ConfigGravarValor(Pointer libHandler, String eSessao, String eChave, String valor);

        int eSocial_CriarEventoeSocial(Pointer libHandler, String eArqIni);
        
        int eSocial_EnviareSocial(Pointer libHandler, Integer aGrupo, ByteBuffer buffer, IntByReference bufferSize);
        
        int eSocial_ConsultareSocial(Pointer libHandler, String eProtocolo, ByteBuffer buffer, IntByReference bufferSize);
    
        int eSocial_CriarEnviareSocial(Pointer libHandler, String eArqIni, Integer aGrupo);
        
        int eSocial_LimpareSocial(Pointer libHandler);
        
        int eSocial_CarregarXMLEventoeSocial(Pointer libHandler, String eArquivoOuXML);
        
        int eSocial_SetIDEmpregador(Pointer libHandler, String aIdEmpregador);
        
        int eSocial_SetIDTransmissor (Pointer libHandler, String aIdTransmissor);
        
        int eSocial_SetTipoEmpregador(Pointer libHandler, Integer aTipoEmpregador);
        
        int eSocial_SetVersaoDF(Pointer libHandler, String sVersao);
        
        int eSocial_ConsultaIdentificadoresEventosEmpregador(Pointer libHandler, String aIdEmpregador, Integer aTipoEvento, double aPeriodoApuracao, ByteBuffer buffer, IntByReference bufferSize);
    
        int eSocial_ConsultaIdentificadoresEventosTabela(Pointer libHandler, String aIdEmpregador, Integer aTipoEvento, String aChave, double aDataInicial, double aDataFinal, ByteBuffer buffer, IntByReference bufferSize);
        
        int eSocial_ConsultaIdentificadoresEventosTrabalhador(Pointer libHandler, String aIdEmpregador, String aCPFTrabalhador, double aDataInicial, double aDataFinal, ByteBuffer buffer, IntByReference bufferSize);
    
        int eSocial_DownloadEventos(Pointer libHandler, String aIdEmpregador, String aCPFTrabalhador, double aDataInicial, double aDataFinal, ByteBuffer buffer, IntByReference bufferSize);
    
        int eSocial_ObterCertificados( Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize );
    }

    public ACBreSocial() throws Exception {
        File iniFile = Paths.get(System.getProperty("user.dir"), "ACBrLib.ini").toFile();
        if (!iniFile.exists()) {
            iniFile.createNewFile();
        }

        PointerByReference handle = new PointerByReference();
        int ret = ACBreSocialLib.INSTANCE.eSocial_Inicializar(handle, toUTF8(iniFile.getAbsolutePath()), toUTF8(""));
        checkResult(ret);
        setHandle(handle.getValue());
    }

    public ACBreSocial(String eArqConfig, String eChaveCrypt) throws Exception {
        PointerByReference handle = new PointerByReference();
        int ret = ACBreSocialLib.INSTANCE.eSocial_Inicializar(handle, toUTF8(eArqConfig), toUTF8(eChaveCrypt));
        checkResult(ret);
        setHandle(handle.getValue());
    }

    @Override
    protected void dispose() throws Exception {
        int ret = ACBreSocialLib.INSTANCE.eSocial_Finalizar(getHandle());
        checkResult(ret);
    }

    public String nome() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBreSocialLib.INSTANCE.eSocial_Nome(getHandle(), buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());
    }

    public String versao() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBreSocialLib.INSTANCE.eSocial_Versao(getHandle(),buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());
    }

    public void configLer() throws Exception {
        configLer("");
    }

    public void configLer(String eArqConfig) throws Exception {
        int ret = ACBreSocialLib.INSTANCE.eSocial_ConfigLer(getHandle(), toUTF8(eArqConfig));
        checkResult(ret);
    }

    public void configGravar() throws Exception {
        configGravar("");
    }

    public void configGravar(String eArqConfig) throws Exception {
        int ret = ACBreSocialLib.INSTANCE.eSocial_ConfigGravar(getHandle(), toUTF8(eArqConfig));
        checkResult(ret);
    }

    @Override
    public String configLerValor(ACBrSessao eSessao, String eChave) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBreSocialLib.INSTANCE.eSocial_ConfigLerValor(getHandle(), toUTF8(eSessao.name()), toUTF8(eChave), buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);
    }

    @Override
    public void configGravarValor(ACBrSessao eSessao, String eChave, Object value) throws Exception {
        int ret = ACBreSocialLib.INSTANCE.eSocial_ConfigGravarValor(getHandle(), toUTF8(eSessao.name()), toUTF8(eChave), toUTF8(value.toString()));
        checkResult(ret);
    }


    public void ConfigImportar(String eArqConfig) throws Exception {

        int ret = ACBreSocialLib.INSTANCE.eSocial_ConfigImportar(getHandle(), eArqConfig);
        checkResult(ret);

    }

    public String ConfigExportar() throws Exception {

        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBreSocialLib.INSTANCE.eSocial_ConfigExportar(getHandle(), buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);

    }
    
    public void criarEventoeSocial(String eArqIni) throws Exception {
        int ret = ACBreSocialLib.INSTANCE.eSocial_CriarEventoeSocial(getHandle(), eArqIni);
        checkResult(ret);
    }
    
    public String enviareSocial(Integer aGrupo) throws Exception {
        
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBreSocialLib.INSTANCE.eSocial_EnviareSocial(getHandle(), aGrupo, buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }
    
    public String consultareSocial(String eProtocolo) throws Exception {
        
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBreSocialLib.INSTANCE.eSocial_ConsultareSocial(getHandle(), eProtocolo, buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }
    
    public void criarEnviareSocial(String eArqIni, Integer aGrupo) throws Exception {
        int ret = ACBreSocialLib.INSTANCE.eSocial_CriarEnviareSocial(getHandle(), eArqIni, aGrupo);
        checkResult(ret);
    }
    
    public void limpareSocial() throws Exception {
        int ret = ACBreSocialLib.INSTANCE.eSocial_LimpareSocial(getHandle());
        checkResult(ret);
    }
    
    public void carregarXMLEventoeSocial(String eArquivoOuXML) throws Exception{
        int ret = ACBreSocialLib.INSTANCE.eSocial_CarregarXMLEventoeSocial(getHandle(), eArquivoOuXML);
        checkResult(ret);
    }
    
    public void setIDEmpregador(String aIdEmpregador) throws Exception {
        int ret = ACBreSocialLib.INSTANCE.eSocial_SetIDEmpregador(getHandle(), aIdEmpregador);
        checkResult(ret);
    }
    
    public void setIDTransmissor(String aIdTransmissor) throws Exception {
        int ret = ACBreSocialLib.INSTANCE.eSocial_SetIDTransmissor(getHandle(), aIdTransmissor);
        checkResult(ret);
    }
    
    public void setTipoEmpregador(Integer aTipoEmpregador) throws Exception {
        int ret = ACBreSocialLib.INSTANCE.eSocial_SetTipoEmpregador(getHandle(), aTipoEmpregador);
        checkResult(ret);
    }
    
    public void setVersaoDF(String sVersao) throws Exception {
        int ret = ACBreSocialLib.INSTANCE.eSocial_SetVersaoDF(getHandle(), sVersao);
        checkResult(ret);
    }
    
    public String consultaIdentificadoresEventosEmpregador(String aIdEmpregador, Integer aTipoEvento, double aPeriodoApuracao) throws Exception{
        
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBreSocialLib.INSTANCE.eSocial_ConsultaIdentificadoresEventosEmpregador(getHandle(), aIdEmpregador, aTipoEvento, aPeriodoApuracao, buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }
    
    public String consultaIdentificadoresEventosTabela(String aIdEmpregador, Integer aTipoEvento, String aChave, double aDataInicial, double aDataFinal) throws Exception {
        
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBreSocialLib.INSTANCE.eSocial_ConsultaIdentificadoresEventosTabela(getHandle(), aIdEmpregador, aTipoEvento, aChave, aDataInicial, aDataFinal, buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }
    
    public String consultaIdentificadoresEventosTrabalhador(String aIdEmpregador, String aCPFTrabalhador, double aDataInicial, double aDataFinal)throws Exception {
        
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBreSocialLib.INSTANCE.eSocial_ConsultaIdentificadoresEventosTrabalhador(getHandle(), aIdEmpregador, aCPFTrabalhador, aDataInicial, aDataFinal, buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }
    
    public String downloadEventos(String aIdEmpregador, String aCPFTrabalhador, double aDataInicial, double aDataFinal) throws Exception {
        
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBreSocialLib.INSTANCE.eSocial_DownloadEventos(getHandle(), aIdEmpregador, aCPFTrabalhador, aDataInicial, aDataFinal, buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }
    
    public String obterCertificados() throws Exception {
      ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
      IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );
      
      int ret = ACBreSocialLib.INSTANCE.eSocial_ObterCertificados(getHandle(), buffer, bufferLen );
      checkResult( ret );
      return processResult( buffer, bufferLen );
  }
    
    @Override
    protected void UltimoRetorno(ByteBuffer buffer, IntByReference bufferLen) {
        ACBreSocialLib.INSTANCE.eSocial_UltimoRetorno(getHandle(), buffer, bufferLen);
    }
}
