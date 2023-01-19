package com.acbr.sat;

import com.acbr.ACBrLibBase;
import com.acbr.ACBrSessao;
import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Platform;
import com.sun.jna.Pointer;
import com.sun.jna.ptr.IntByReference;
import com.sun.jna.ptr.PointerByReference;
import java.io.File;
import java.nio.ByteBuffer;
import java.nio.file.Paths;

public final class ACBrSat extends ACBrLibBase {
         
    private interface ACBrSatLib extends Library {
        static String JNA_LIBRARY_NAME = LibraryLoader.getLibraryName();
        public final static ACBrSatLib INSTANCE = LibraryLoader.getInstance();
        
        class LibraryLoader {
            private static String library = "";
            private static ACBrSatLib instance = null;
            
            public static String getLibraryName() {
                if (library.isEmpty()) {
                    if(Platform.isWindows()){
                        library = Platform.is64Bit() ? "ACBrSAT64" : "ACBrSAT32";                        
                    }else{
                        library = Platform.is64Bit() ? "acbrsat64" : "acbrsat32";
                    }
                         
                }
                return library;
            }
            
            public static ACBrSatLib getInstance() {
                if (instance == null) {
                    instance = (ACBrSatLib) Native.synchronizedLibrary((Library) Native.load(JNA_LIBRARY_NAME, ACBrSatLib.class));
                }
                return instance;
            }
        }

    int SAT_Inicializar(PointerByReference libHandler, String eArqConfig, String eChaveCrypt);
    
    int SAT_Finalizar(Pointer libHandler);

    int SAT_Nome(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize);

    int SAT_Versao(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize);

    int SAT_UltimoRetorno(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize);

    int SAT_ConfigImportar(Pointer libHandler, String eArqConfig);
        
    int SAT_ConfigExportar(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize);
    
    int SAT_ConfigLer(Pointer libHandler, String eArqConfig);

    int SAT_ConfigGravar(Pointer libHandler, String eArqConfig);

    int SAT_ConfigLerValor(Pointer libHandler, String eSessao, String eChave, ByteBuffer buffer, IntByReference bufferSize);

    int SAT_ConfigGravarValor(Pointer libHandler, String eSessao, String eChave, String valor);

    int SAT_InicializarSAT(Pointer libHandler);

    int SAT_DesInicializar(Pointer libHandler);

    int SAT_AtivarSAT(Pointer libHandler, String CNPJValue, Integer cUF, ByteBuffer buffer, IntByReference bufferSize);
    
    int SAT_AssociarAssinatura(Pointer libHandler, String CNPJValue, String assinaturaCNPJs, ByteBuffer buffer, IntByReference bufferSize);

    int SAT_BloquearSAT(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize);

    int SAT_DesbloquearSAT(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize);

    int SAT_TrocarCodigoDeAtivacao(Pointer libHandler, String codigoDeAtivacaoOuEmergencia, int opcao, String novoCodigo, ByteBuffer buffer, IntByReference bufferSize);

    int SAT_ConsultarSAT(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize);

    int SAT_ConsultarStatusOperacional(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize);

    int SAT_ConsultarNumeroSessao(Pointer libHandler, int cNumeroDeSessao, ByteBuffer buffer, IntByReference bufferSize);

    int SAT_AtualizarSoftwareSAT(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize);

    int SAT_ComunicarCertificadoICPBRASIL(Pointer libHandler, String certificado, ByteBuffer buffer, IntByReference bufferSize);

    int SAT_ExtrairLogs(Pointer libHandler, String eArquivo);

    int SAT_TesteFimAFim(Pointer libHandler, String eArquivoXmlVenda);

    int SAT_CriarCFe(Pointer libHandler, String eArquivoIni, ByteBuffer buffer, IntByReference bufferSize);
    
    int SAT_ValidarCFe(Pointer libHandler, String eArquivoXml);

    int SAT_CriarEnviarCFe(Pointer libHandler, String eArquivoIni, ByteBuffer buffer, IntByReference bufferSize);

    int SAT_EnviarCFe(Pointer libHandler, String eArquivoXml, ByteBuffer buffer, IntByReference bufferSize);

    int SAT_CancelarCFe(Pointer libHandler, String eArquivoXml, ByteBuffer buffer, IntByReference bufferSize);

    int SAT_ImprimirExtratoVenda(Pointer libHandler, String eArquivoXml, String eNomeImpressora);

    int SAT_ImprimirExtratoResumido(Pointer libHandler, String eArquivoXml, String eNomeImpressora);

    int SAT_GerarPDFExtratoVenda(Pointer libHandler, String eArquivoXml, String eNomeArquivo, ByteBuffer buffer, IntByReference bufferSize);

    int SAT_GerarImpressaoFiscalMFe(Pointer libHandler, String eArquivoXml, ByteBuffer buffer, IntByReference bufferSize);

    int SAT_EnviarEmail(Pointer libHandler, String eArquivoXml, String ePara, String eAssunto, String eNomeArquivo,
            String sMensagem, String sCC, String eAnexos);
    }
        
    public ACBrSat() throws Exception {
        File iniFile = Paths.get(System.getProperty("user.dir"), "ACBrLib.ini").toFile();
        if (!iniFile.exists()) {
            iniFile.createNewFile();
        }

        PointerByReference handle = new PointerByReference();
        int ret = ACBrSatLib.INSTANCE.SAT_Inicializar(handle, toUTF8(iniFile.getAbsolutePath()), toUTF8(""));
        checkResult(ret);
        setHandle(handle.getValue());
    }

    public ACBrSat(String eArqConfig, String eChaveCrypt) throws Exception {
        PointerByReference handle = new PointerByReference();
        int ret = ACBrSatLib.INSTANCE.SAT_Inicializar(handle, toUTF8(eArqConfig), toUTF8(eChaveCrypt));
        checkResult(ret);
        setHandle(handle.getValue());
    }
           
    @Override
    protected void dispose() throws Exception {
        int ret = ACBrSatLib.INSTANCE.SAT_Finalizar(this.getHandle());
        checkResult(ret);        
    }
    
    public String nome() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrSatLib.INSTANCE.SAT_Nome(getHandle(), buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());        
    }

    public String versao() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrSatLib.INSTANCE.SAT_Versao(getHandle(), buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());
    }

    public void configLer() throws Exception {
        configLer("");
    }

    public void configLer(String eArqConfig) throws Exception {
        int ret = ACBrSatLib.INSTANCE.SAT_ConfigLer(getHandle(), toUTF8(eArqConfig));
        checkResult(ret);
    }

    public void configGravar() throws Exception {
        configGravar("");
    }

    public void configGravar(String eArqConfig) throws Exception {
        int ret = ACBrSatLib.INSTANCE.SAT_ConfigGravar(getHandle(), toUTF8(eArqConfig));
        checkResult(ret);
    }

    @Override
    public String configLerValor(ACBrSessao eSessao, String eChave) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrSatLib.INSTANCE.SAT_ConfigLerValor(getHandle(), toUTF8(eSessao.name()), toUTF8(eChave), buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }

    @Override
    public void configGravarValor(ACBrSessao eSessao, String eChave, Object value) throws Exception {
        int ret = ACBrSatLib.INSTANCE.SAT_ConfigGravarValor(getHandle(), toUTF8(eSessao.name()), toUTF8(eChave), toUTF8(value.toString()));
        checkResult(ret);
    }
    
    public void inicializar() throws Exception  {  
        int ret = ACBrSatLib.INSTANCE.SAT_InicializarSAT(getHandle());
        checkResult(ret);
    }
    
    public void desInicializar() throws Exception  { 
        int ret = ACBrSatLib.INSTANCE.SAT_DesInicializar(getHandle());
        checkResult(ret);
    }
    
    public String ativarSAT(String CNPJValue, Integer cUF) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);          
        
        int ret = ACBrSatLib.INSTANCE.SAT_AtivarSAT(getHandle(), toUTF8(CNPJValue), cUF, buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
   
    public String associarAssinatura(String CNPJValue, String assinaturaCNPJs) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);          
        
        int ret = ACBrSatLib.INSTANCE.SAT_AssociarAssinatura(getHandle(), toUTF8(CNPJValue), toUTF8(assinaturaCNPJs), buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
    
    public String bloquearSAT() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrSatLib.INSTANCE.SAT_BloquearSAT(getHandle(), buffer, bufferLen);
        checkResult(ret);        
        return processResult(buffer, bufferLen);
    }
    
    public String desbloquearSAT() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrSatLib.INSTANCE.SAT_DesbloquearSAT(getHandle(), buffer, bufferLen);
        checkResult(ret);        
        return processResult(buffer, bufferLen);
    }
        
    public String trocarCodigoDeAtivacao(String codigoDeAtivacaoOuEmergencia, int opcao, String novoCodigo) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrSatLib.INSTANCE.SAT_TrocarCodigoDeAtivacao(getHandle(), toUTF8(codigoDeAtivacaoOuEmergencia), opcao,
                toUTF8(novoCodigo), buffer, bufferLen);        
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }
    
    public String consultarSAT() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrSatLib.INSTANCE.SAT_ConsultarSAT(getHandle(), buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }
    
    public String consultarStatusOperacional() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrSatLib.INSTANCE.SAT_ConsultarStatusOperacional(getHandle(), buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }
    
    public String consultarNumeroSessao(int cNumeroDeSessao) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrSatLib.INSTANCE.SAT_ConsultarNumeroSessao(getHandle(), cNumeroDeSessao, buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }
    
    public String atualizarSoftwareSAT() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrSatLib.INSTANCE.SAT_AtualizarSoftwareSAT(getHandle(), buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }
    
    public String comunicarCertificadoICPBRASIL(String certificado) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrSatLib.INSTANCE.SAT_ComunicarCertificadoICPBRASIL(getHandle(), toUTF8(certificado), buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }
           
    public void extrairLogs(String eArquivo) throws Exception {
        int ret = ACBrSatLib.INSTANCE.SAT_ExtrairLogs(getHandle(), toUTF8(eArquivo));
        checkResult(ret);
    }
    
    public void testeFimAFim(String eArquivoXmlVenda) throws Exception {
        int ret = ACBrSatLib.INSTANCE.SAT_TesteFimAFim(getHandle(), toUTF8(eArquivoXmlVenda));
        checkResult(ret);
    }
    
    public String criarCFe(String eArquivoIni) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrSatLib.INSTANCE.SAT_CriarCFe(getHandle(), toUTF8(eArquivoIni), buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }
    
     public void validarCFe(String eArquivoXml) throws Exception {
        int ret = ACBrSatLib.INSTANCE.SAT_ValidarCFe(getHandle(), toUTF8(eArquivoXml));
        checkResult(ret);
    }
    
    public String criarEnviarCFe(String eArquivoIni) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrSatLib.INSTANCE.SAT_CriarEnviarCFe(getHandle(), toUTF8(eArquivoIni), buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }
    
    public String enviarCFe(String eArquivoXml) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrSatLib.INSTANCE.SAT_EnviarCFe(getHandle(), toUTF8(eArquivoXml), buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }
    
    public String cancelarCFe(String eArquivoXml) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrSatLib.INSTANCE.SAT_CancelarCFe(getHandle(), toUTF8(eArquivoXml), buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }
    
    public void imprimirExtratoVenda(String eArquivoXml) throws Exception {
        imprimirExtratoVenda(eArquivoXml, "");
    }
    
    public void imprimirExtratoVenda(String eArquivoXml, String eNomeImpressora) throws Exception {
        int ret = ACBrSatLib.INSTANCE.SAT_ImprimirExtratoVenda(getHandle(), toUTF8(eArquivoXml), toUTF8(eNomeImpressora));
        checkResult(ret);
    }
    
    public void imprimirExtratoResumido(String eArquivoXml) throws Exception {
        imprimirExtratoResumido(eArquivoXml, "");
    }
    
    public void imprimirExtratoResumido(String eArquivoXml, String eNomeImpressora) throws Exception {
        int ret = ACBrSatLib.INSTANCE.SAT_ImprimirExtratoResumido(getHandle(), toUTF8(eArquivoXml), toUTF8(eNomeImpressora));
        checkResult(ret);
    }
    
    public String gerarPDFExtratoVenda(String eArquivoXml) throws Exception {
        return gerarPDFExtratoVenda(eArquivoXml, "");
    }
    
    public String gerarPDFExtratoVenda(String eArquivoXml, String eNomeArquivo) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrSatLib.INSTANCE.SAT_GerarPDFExtratoVenda(getHandle(), toUTF8(eArquivoXml), toUTF8(eNomeArquivo), buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }
    
    public String GerarImpressaoFiscalMFe(String eArquivoXml) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrSatLib.INSTANCE.SAT_GerarImpressaoFiscalMFe(getHandle(), toUTF8(eArquivoXml), buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }
    
    public void EnviarEmail(String eArquivoXml, String ePara, String eAssunto, String eNomeArquivo,
            String sMensagem, String sCC, String eAnexos) throws Exception {
        int ret = ACBrSatLib.INSTANCE.SAT_EnviarEmail(getHandle(), toUTF8(eArquivoXml), toUTF8(ePara), toUTF8(eAssunto),
                toUTF8(eNomeArquivo), toUTF8(sMensagem), toUTF8(sCC), toUTF8(eAnexos));
        checkResult(ret);
    }
    
    public void ConfigImportar(String eArqConfig) throws Exception {
        
        int ret = ACBrSatLib.INSTANCE.SAT_ConfigImportar(getHandle(), eArqConfig);
        checkResult(ret);
        
    }
    
    public String ConfigExportar() throws Exception {
		
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrSatLib.INSTANCE.SAT_ConfigExportar(getHandle(),buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());
		
    }
    
    @Override
    protected void UltimoRetorno(ByteBuffer buffer, IntByReference bufferLen) {
        ACBrSatLib.INSTANCE.SAT_UltimoRetorno(getHandle(), buffer, bufferLen);
    }
}
