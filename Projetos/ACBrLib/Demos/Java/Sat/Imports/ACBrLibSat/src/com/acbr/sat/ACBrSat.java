package com.acbr.sat;

import com.acbr.ACBrLibBase;
import com.acbr.ACBrSessao;
import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Platform;
import com.sun.jna.ptr.IntByReference;
import java.io.File;
import java.nio.ByteBuffer;
import java.nio.file.Paths;

public final class ACBrSat extends ACBrLibBase implements AutoCloseable {
      
    private interface ACBrSatLib extends Library {
        static String JNA_LIBRARY_NAME = LibraryLoader.getLibraryName();
        public final static ACBrSatLib INSTANCE = LibraryLoader.getInstance();
        
        class LibraryLoader {
            private static String library = "";
            private static ACBrSatLib instance = null;
            
            public static String getLibraryName() {
                if (library.isEmpty()) {
                    library = Platform.is64Bit() ? "ACBrSAT64" : "ACBrSAT32";
                }
                return library;
            }
            
            public static ACBrSatLib getInstance() {
                if (instance == null) {
                    instance = (ACBrSatLib) Native.synchronizedLibrary((Library) Native.loadLibrary(JNA_LIBRARY_NAME, ACBrSatLib.class));
                }
                return instance;
            }
        }

    int SAT_Inicializar(String eArqConfig, String eChaveCrypt);

    int SAT_Finalizar();

    int SAT_Nome(ByteBuffer buffer, IntByReference bufferSize);

    int SAT_Versao(ByteBuffer buffer, IntByReference bufferSize);

    int SAT_UltimoRetorno(ByteBuffer buffer, IntByReference bufferSize);

    int SAT_ConfigLer(String eArqConfig);

    int SAT_ConfigGravar(String eArqConfig);

    int SAT_ConfigLerValor(String eSessao, String eChave, ByteBuffer buffer, IntByReference bufferSize);

    int SAT_ConfigGravarValor(String eSessao, String eChave, String valor);

    int SAT_InicializarSAT();

    int SAT_DesInicializar();

    int SAT_AssociarAssinatura(String CNPJValue, String assinaturaCNPJs, ByteBuffer buffer, IntByReference bufferSize);

    int SAT_BloquearSAT(ByteBuffer buffer, IntByReference bufferSize);

    int SAT_DesbloquearSAT(ByteBuffer buffer, IntByReference bufferSize);

    int SAT_TrocarCodigoDeAtivacao(String codigoDeAtivacaoOuEmergencia, int opcao, String novoCodigo, ByteBuffer buffer, IntByReference bufferSize);

    int SAT_ConsultarSAT(ByteBuffer buffer, IntByReference bufferSize);

    int SAT_ConsultarStatusOperacional(ByteBuffer buffer, IntByReference bufferSize);

    int SAT_ConsultarNumeroSessao(int cNumeroDeSessao, ByteBuffer buffer, IntByReference bufferSize);

    int SAT_AtualizarSoftwareSAT(ByteBuffer buffer, IntByReference bufferSize);

    int SAT_ComunicarCertificadoICPBRASIL(String certificado, ByteBuffer buffer, IntByReference bufferSize);

    int SAT_ExtrairLogs(String eArquivo);

    int SAT_TesteFimAFim(String eArquivoXmlVenda);

    int SAT_CriarCFe(String eArquivoIni, ByteBuffer buffer, IntByReference bufferSize);

    int SAT_CriarEnviarCFe(String eArquivoIni, ByteBuffer buffer, IntByReference bufferSize);

    int SAT_EnviarCFe(String eArquivoXml, ByteBuffer buffer, IntByReference bufferSize);

    int SAT_CancelarCFe(String eArquivoXml, ByteBuffer buffer, IntByReference bufferSize);

    int SAT_ImprimirExtratoVenda(String eArquivoXml, String eNomeImpressora);

    int SAT_ImprimirExtratoResumido(String eArquivoXml, String eNomeImpressora);

    int SAT_GerarPDFExtratoVenda(String eArquivoXml, String eNomeArquivo, ByteBuffer buffer, IntByReference bufferSize);

    int SAT_GerarImpressaoFiscalMFe(String eArquivoXml, String eNomeArquivo, ByteBuffer buffer, IntByReference bufferSize);

    int SAT_EnviarEmail(String eArquivoXml, String ePara, String eAssunto, String eNomeArquivo,
            String sMensagem, String sCC, String eAnexos);
    }
        
    public ACBrSat() throws Exception {
        File iniFile = Paths.get(System.getProperty("user.dir"), "ACBrLib.ini").toFile();
        if (!iniFile.exists()) {
            iniFile.createNewFile();
        }

        int ret = ACBrSatLib.INSTANCE.SAT_Inicializar(toUTF8(iniFile.getAbsolutePath()), toUTF8(""));
        checkResult(ret);
    }

    public ACBrSat(String eArqConfig, String eChaveCrypt) throws Exception {
        int ret = ACBrSatLib.INSTANCE.SAT_Inicializar(toUTF8(eArqConfig), toUTF8(eChaveCrypt));
        checkResult(ret);
    }
    
    @Override
    public void close() throws Exception {
        int ret = ACBrSatLib.INSTANCE.SAT_Finalizar();
        checkResult(ret);
    }
    
    @Override
    protected void finalize() throws Throwable {
        try {
            int ret = ACBrSatLib.INSTANCE.SAT_Finalizar();
            checkResult(ret);
        } finally {
            super.finalize();
        }
    }
    
    public String nome() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrSatLib.INSTANCE.SAT_Nome(buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());
    }

    public String versao() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrSatLib.INSTANCE.SAT_Versao(buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());
    }

    public void configLer() throws Exception {
        configLer("");
    }

    public void configLer(String eArqConfig) throws Exception {
        int ret = ACBrSatLib.INSTANCE.SAT_ConfigLer(toUTF8(eArqConfig));
        checkResult(ret);
    }

    public void configGravar() throws Exception {
        configGravar("");
    }

    public void configGravar(String eArqConfig) throws Exception {
        int ret = ACBrSatLib.INSTANCE.SAT_ConfigGravar(toUTF8(eArqConfig));
        checkResult(ret);
    }

    public String configLerValor(ACBrSessao eSessao, String eChave) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrSatLib.INSTANCE.SAT_ConfigLerValor(toUTF8(eSessao.name()), toUTF8(eChave), buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }

    public void configGravarValor(ACBrSessao eSessao, String eChave, Object value) throws Exception {
        int ret = ACBrSatLib.INSTANCE.SAT_ConfigGravarValor(toUTF8(eSessao.name()), toUTF8(eChave), toUTF8(value.toString()));
        checkResult(ret);
    }
    
    public void inicializar() throws Exception  {  
        int ret = ACBrSatLib.INSTANCE.SAT_InicializarSAT();
        checkResult(ret);
    }
    
    public void desInicializar() throws Exception  { 
        int ret = ACBrSatLib.INSTANCE.SAT_DesInicializar();
        checkResult(ret);
    }
    
    public String associarAssinatura(String CNPJValue, String assinaturaCNPJs) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);          
        
        int ret = ACBrSatLib.INSTANCE.SAT_AssociarAssinatura(toUTF8(CNPJValue), toUTF8(assinaturaCNPJs), buffer, bufferLen);
        checkResult(ret);
        return processResult(buffer, bufferLen);
    }
    
    public String bloquearSAT() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrSatLib.INSTANCE.SAT_BloquearSAT(buffer, bufferLen);
        checkResult(ret);        
        return processResult(buffer, bufferLen);
    }
    
    public String desbloquearSAT() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrSatLib.INSTANCE.SAT_DesbloquearSAT(buffer, bufferLen);
        checkResult(ret);        
        return processResult(buffer, bufferLen);
    }
        
    public String trocarCodigoDeAtivacao(String codigoDeAtivacaoOuEmergencia, int opcao, String novoCodigo) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrSatLib.INSTANCE.SAT_TrocarCodigoDeAtivacao(toUTF8(codigoDeAtivacaoOuEmergencia), opcao,
                toUTF8(novoCodigo), buffer, bufferLen);        
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }
    
    public String consultarSAT() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);
        
        int ret = ACBrSatLib.INSTANCE.SAT_ConsultarSAT(buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }
    
    public String consultarStatusOperacional() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrSatLib.INSTANCE.SAT_ConsultarStatusOperacional(buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }
    
    public String consultarNumeroSessao(int cNumeroDeSessao) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrSatLib.INSTANCE.SAT_ConsultarNumeroSessao(cNumeroDeSessao, buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }
    
    public String atualizarSoftwareSAT() throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrSatLib.INSTANCE.SAT_AtualizarSoftwareSAT(buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }
    
    public String comunicarCertificadoICPBRASIL(String certificado) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrSatLib.INSTANCE.SAT_ComunicarCertificadoICPBRASIL(toUTF8(certificado), buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }
           
    public void extrairLogs(String eArquivo) throws Exception {
        int ret = ACBrSatLib.INSTANCE.SAT_ExtrairLogs(toUTF8(eArquivo));
        checkResult(ret);
    }
    
    public void testeFimAFim(String eArquivoXmlVenda) throws Exception {
        int ret = ACBrSatLib.INSTANCE.SAT_TesteFimAFim(toUTF8(eArquivoXmlVenda));
        checkResult(ret);
    }
    
    public String criarCFe(String eArquivoIni) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrSatLib.INSTANCE.SAT_CriarCFe(toUTF8(eArquivoIni), buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }
    
    public String criarEnviarCFe(String eArquivoIni) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrSatLib.INSTANCE.SAT_CriarEnviarCFe(toUTF8(eArquivoIni), buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }
    
    public String enviarCFe(String eArquivoXml) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrSatLib.INSTANCE.SAT_EnviarCFe(toUTF8(eArquivoXml), buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }
    
    public String cancelarCFe(String eArquivoXml) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrSatLib.INSTANCE.SAT_CancelarCFe(toUTF8(eArquivoXml), buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }
    
    public void imprimirExtratoVenda(String eArquivoXml) throws Exception {
        imprimirExtratoVenda(eArquivoXml, "");
    }
    
    public void imprimirExtratoVenda(String eArquivoXml, String eNomeImpressora) throws Exception {
        int ret = ACBrSatLib.INSTANCE.SAT_ImprimirExtratoVenda(toUTF8(eArquivoXml), toUTF8(eNomeImpressora));
        checkResult(ret);
    }
    
    public void imprimirExtratoResumido(String eArquivoXml) throws Exception {
        imprimirExtratoResumido(eArquivoXml, "");
    }
    
    public void imprimirExtratoResumido(String eArquivoXml, String eNomeImpressora) throws Exception {
        int ret = ACBrSatLib.INSTANCE.SAT_ImprimirExtratoResumido(toUTF8(eArquivoXml), toUTF8(eNomeImpressora));
        checkResult(ret);
    }
    
    public String gerarPDFExtratoVenda(String eArquivoXml) throws Exception {
        return gerarPDFExtratoVenda(eArquivoXml, "");
    }
    
    public String gerarPDFExtratoVenda(String eArquivoXml, String eNomeArquivo) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrSatLib.INSTANCE.SAT_GerarPDFExtratoVenda(toUTF8(eArquivoXml), toUTF8(eNomeArquivo), buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }
    
    public String GerarImpressaoFiscalMFe(String eArquivoXml, String eNomeArquivo) throws Exception {
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrSatLib.INSTANCE.SAT_GerarImpressaoFiscalMFe(toUTF8(eArquivoXml), toUTF8(eNomeArquivo), buffer, bufferLen);
        checkResult(ret);
        
        return processResult(buffer, bufferLen);
    }
    
    public void EnviarEmail(String eArquivoXml, String ePara, String eAssunto, String eNomeArquivo,
            String sMensagem, String sCC, String eAnexos) throws Exception {
        int ret = ACBrSatLib.INSTANCE.SAT_EnviarEmail(toUTF8(eArquivoXml), toUTF8(ePara), toUTF8(eAssunto),
                toUTF8(eNomeArquivo), toUTF8(sMensagem), toUTF8(sCC), toUTF8(eAnexos));
        checkResult(ret);
    }
    
    @Override
    protected void UltimoRetorno(ByteBuffer buffer, IntByReference bufferLen) {
        ACBrSatLib.INSTANCE.SAT_UltimoRetorno(buffer, bufferLen);
    }
}
