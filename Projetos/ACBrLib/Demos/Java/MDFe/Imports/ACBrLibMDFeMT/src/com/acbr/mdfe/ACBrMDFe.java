package com.acbr.mdfe;

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
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;

/**
 * Implementa AutoCloseable para ser possível usar em try-with-resources
 *  Uso:
 *  try( ACBrCTe wrapper =  new ACBrCTe(...) ){
 *  nfe.statusServico();
 *  }
 *  Dessa forma ele retirará o ACBrCTe da memória ao terminar de utilizar, então não precisa do bloco
 *  finally, para setar tirar o nfe da memória, pois ele chama o finalize() no método close();
 *
 *  É melhor utilizar em um try-with-resources, que depender do método finalize(), pois o finalize() será
 *  chamado somente quando for efetuado o garbage collection do objeto, então fica impossível determinar
 *  QUANDO e SE o finalize() será chamado. Já o try-with-resources chama o close() ao finalizar seu escopo.
 */

public final class ACBrMDFe extends ACBrLibBase {

    private interface ACBrMDFeLib extends Library {
        
        static String JNA_LIBRARY_NAME = LibraryLoader.getLibraryName();
        public final static ACBrMDFeLib INSTANCE = LibraryLoader.getInstance();

        class LibraryLoader {
            
            private static String library = "";
            private static ACBrMDFeLib instance = null;

            private static String getLibraryName() {
                if ( library.isEmpty() ) {
                    if(Platform.isWindows()){
                        library = Platform.is64Bit() ? "ACBrMDFe64" : "ACBrMDFe32";                        
                    }else{
                        library = Platform.is64Bit() ? "acbrmdfe64" : "acbrmdfe32";
                    }                    
                }
                return library;
            }

            public static ACBrMDFeLib getInstance() {
                if ( instance == null ) {
                instance = ( ACBrMDFeLib ) Native.synchronizedLibrary(
                ( Library ) Native.load( JNA_LIBRARY_NAME, ACBrMDFeLib.class ) );
                }
                return instance;
            }
    }
    
    int MDFE_Inicializar(PointerByReference libHandler, String eArqConfig, String eChaveCrypt );

    int MDFE_Finalizar(Pointer libHandler);

    int MDFE_Nome(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize );

    int MDFE_Versao(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize );

    int MDFE_OpenSSLInfo(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize);
    
    int MDFE_UltimoRetorno(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize );

    int MDFE_ConfigImportar(Pointer libHandler, String eArqConfig);
        
    int MDFE_ConfigExportar(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize);
    
    int MDFE_ConfigLer(Pointer libHandler, String eArqConfig );

    int MDFE_ConfigGravar(Pointer libHandler, String eArqConfig );

    int MDFE_ConfigLerValor(Pointer libHandler, String eSessao, String eChave, ByteBuffer buffer, IntByReference bufferSize );

    int MDFE_ConfigGravarValor(Pointer libHandler, String eSessao, String eChave, String valor );

    int MDFE_CarregarXML(Pointer libHandler, String eArquivoOuXML );

    int MDFE_CarregarINI(Pointer libHandler, String eArquivoOuINI );
    
    int MDFE_ObterXml(Pointer libHandler, Integer AIndex, ByteBuffer buffer, IntByReference bufferSize );
    
    int MDFE_GravarXml(Pointer libHandler, Integer AIndex, String eNomeArquivo, String ePathArquivo );
    
    int MDFE_ObterIni(Pointer libHandler, Integer AIndex, ByteBuffer buffer, IntByReference bufferSize );
    
    int MDFE_GravarIni(Pointer libHandler, Integer AIndex, String eNomeArquivo, String ePathArquivo );

    int MDFE_LimparLista(Pointer libHandler);

    int MDFE_CarregarEventoXML(Pointer libHandler, String eArquivoOuXml );

    int MDFE_CarregarEventoINI(Pointer libHandler, String eArquivoOuIni );

    int MDFE_LimparListaEventos(Pointer libHandler);

    int MDFE_Assinar(Pointer libHandler);

    int MDFE_Validar(Pointer libHandler);

    int MDFE_ValidarRegrasdeNegocios(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize );

    int MDFE_VerificarAssinatura(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize );
    
    int MDFE_GerarChave(Pointer libHandler, int ACodigoUF, int ACodigoNumerico, int AModelo, int ASerie, int ANumero,
                int ATpEmi, String AEmissao, String CPFCNPJ, ByteBuffer buffer, IntByReference bufferSize);
    
    int MDFE_ObterCertificados(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize );
    
    int MDFE_GetPath(Pointer libHandler, int tipo, ByteBuffer buffer, IntByReference bufferSize );
    
    int MDFE_GetPathEvento(Pointer libHandler, String aCodEvento, ByteBuffer buffer, IntByReference bufferSize );

    int MDFE_StatusServico(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize );

    int MDFE_Consultar(Pointer libHandler, String eChaveOuNFe, boolean AExtrairEventos, ByteBuffer buffer, IntByReference bufferSize );

    int MDFE_ConsultaMDFeNaoEnc(Pointer libHandler, String aCNPJ, ByteBuffer buffer, IntByReference bufferSize);
    
    int MDFE_Enviar(Pointer libHandler, int ALote, boolean Imprimir, boolean Sincrono, ByteBuffer buffer, IntByReference bufferSize );

    int MDFE_ConsultarRecibo(Pointer libHandler, String aRecibo, ByteBuffer buffer, IntByReference bufferSize );

    int MDFE_Cancelar(Pointer libHandler, String eChave, String eJustificativa, String eCNPJ, int ALote,
                      ByteBuffer buffer, IntByReference bufferSize );

    int MDFE_EncerrarMDFe(Pointer libHandler, String eChaveOuMDFe, String eDtEnc, String cMunicipioDescarga, String nCNPJ, String nProtocolo, ByteBuffer buffer, IntByReference bufferSize);
    
    int MDFE_EnviarEvento( Pointer libHandler, int idLote, ByteBuffer buffer, IntByReference bufferSize );

    int MDFE_DistribuicaoDFePorUltNSU( Pointer libHandler, int AcUFAutor, String eCNPJCPF, String eultNsu, ByteBuffer buffer,
                                      IntByReference bufferSize );

    int MDFE_DistribuicaoDFePorNSU( Pointer libHandler, int AcUFAutor, String eCNPJCPF, String eNSU,
                                   ByteBuffer buffer, IntByReference bufferSize );

    int MDFE_DistribuicaoDFePorChave( Pointer libHandler, int AcUFAutor, String eCNPJCPF, String echNFe,
                                     ByteBuffer buffer, IntByReference bufferSize );

    int MDFE_EnviarEmail( Pointer libHandler, String ePara, String eChaveNFe, boolean AEnviaPDF, String eAssunto,
                         String eCC, String eAnexos, String eMensagem );

    int MDFE_EnviarEmailEvento( Pointer libHandler, String ePara, String eChaveEvento, String eChaveNFe,
                               boolean AEnviaPDF, String eAssunto, String eCC, String eAnexos, String eMensagem );

    int MDFE_Imprimir(Pointer libHandler, String cImpressora, int nNumCopias, String cProtocolo, String bMostrarPreview);

    int MDFE_ImprimirPDF(Pointer libHandler);

    int MDFE_ImprimirEvento( Pointer libHandler, String eArquivoXmlCTe, String eArquivoXmlEvento );

    int MDFE_ImprimirEventoPDF( Pointer libHandler, String eArquivoXmlCTe, String eArquivoXmlEvento );
    
  }
    
  public ACBrMDFe() throws Exception {
    File iniFile = Paths.get( System.getProperty( "user.dir" ), "ACBrLib.ini" ).toFile();
    if ( !iniFile.exists() ) {
      iniFile.createNewFile();
    }

    PointerByReference handle = new PointerByReference();
    int ret = ACBrMDFeLib.INSTANCE.MDFE_Inicializar(handle, toUTF8( iniFile.getAbsolutePath() ), toUTF8( "" ) );
    checkResult( ret );
	setHandle(handle.getValue());
  }

  public ACBrMDFe( String eArqConfig, String eChaveCrypt ) throws Exception {
    PointerByReference handle = new PointerByReference();
    int ret = ACBrMDFeLib.INSTANCE.MDFE_Inicializar(handle, toUTF8( eArqConfig ), toUTF8( eChaveCrypt ) );
    checkResult( ret );
	setHandle(handle.getValue());
  }

  @Override
  protected void dispose() throws Exception {
    int ret = ACBrMDFeLib.INSTANCE.MDFE_Finalizar(getHandle());
    checkResult( ret );
  }

  public String nome() throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrMDFeLib.INSTANCE.MDFE_Nome( getHandle(), buffer, bufferLen );
    checkResult( ret );

    return fromUTF8( buffer, bufferLen.getValue() );
  }

  public String versao() throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrMDFeLib.INSTANCE.MDFE_Versao( getHandle(), buffer, bufferLen );
    checkResult( ret );

    return fromUTF8( buffer, bufferLen.getValue() );
  }
  
  public String openSSLInfo() throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
    IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

    int ret = ACBrMDFeLib.INSTANCE.MDFE_OpenSSLInfo(getHandle(), buffer, bufferLen);
    checkResult(ret);
    return processResult(buffer, bufferLen);
  }

  public void configLer() throws Exception {
    configLer( "" );
  }

  public void configLer( String eArqConfig ) throws Exception {
    int ret = ACBrMDFeLib.INSTANCE.MDFE_ConfigLer( getHandle(), toUTF8( eArqConfig ) );
    checkResult( ret );
  }

  public void configGravar() throws Exception {
    configGravar( "" );
  }

  public void configGravar( String eArqConfig ) throws Exception {
    int ret = ACBrMDFeLib.INSTANCE.MDFE_ConfigGravar( getHandle(), toUTF8( eArqConfig ) );
    checkResult( ret );
  }

    @Override
  public String configLerValor( ACBrSessao eSessao, String eChave ) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrMDFeLib.INSTANCE.MDFE_ConfigLerValor( getHandle(), toUTF8( eSessao.name() ), toUTF8( eChave ), buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }

    @Override
  public void configGravarValor( ACBrSessao eSessao, String eChave, Object value ) throws Exception {
    int ret = ACBrMDFeLib.INSTANCE.MDFE_ConfigGravarValor( getHandle(), toUTF8( eSessao.name() ), toUTF8( eChave ), toUTF8( value.toString() ) );
    checkResult( ret );
  }

  public void carregarXml( String eArquivoOuXML ) throws Exception {
    int ret = ACBrMDFeLib.INSTANCE.MDFE_CarregarXML( getHandle(), toUTF8( eArquivoOuXML ) );
    checkResult( ret );
  }

  public String obterXml( int AIndex ) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );
    int ret = ACBrMDFeLib.INSTANCE.MDFE_ObterXml(getHandle(), AIndex, buffer, bufferLen );
    checkResult( ret );
    
    return processResult( buffer, bufferLen );
  }
  
  public void gravarXml ( int AIndex ) throws Exception {
    gravarXml(AIndex, "", "");
  }
  
  public void gravarXml ( int AIndex, String eNomeArquivo ) throws Exception {
    gravarXml(AIndex, eNomeArquivo, "");
  }
  
  public void gravarXml ( int AIndex, String eNomeArquivo, String ePathArquivo ) throws Exception {
    int ret = ACBrMDFeLib.INSTANCE.MDFE_GravarXml( getHandle(), AIndex, toUTF8( eNomeArquivo ), toUTF8( ePathArquivo ) );
    checkResult( ret );
  }
  
  public String obterIni( int AIndex ) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );
    int ret = ACBrMDFeLib.INSTANCE.MDFE_ObterIni(getHandle(), AIndex, buffer, bufferLen );
    checkResult( ret );
      
    return processResult( buffer, bufferLen );
  }
  
  public void gravarIni ( int AIndex ) throws Exception {
    gravarIni(AIndex, "", "");
  }
  
  public void gravarIni ( int AIndex, String eNomeArquivo ) throws Exception {
    gravarIni(AIndex, eNomeArquivo, "");
  }
  
  public void gravarIni ( int AIndex, String eNomeArquivo, String ePathArquivo ) throws Exception {
    int ret = ACBrMDFeLib.INSTANCE.MDFE_GravarIni( getHandle(), AIndex, toUTF8( eNomeArquivo ), toUTF8( ePathArquivo ) );
    checkResult( ret );
  }
  
  public void carregarIni( String eArquivoOuIni ) throws Exception {
    int ret = ACBrMDFeLib.INSTANCE.MDFE_CarregarINI( getHandle(), toUTF8( eArquivoOuIni ) );
    checkResult( ret );
  }

  public void limparLista() throws Exception {
    int ret = ACBrMDFeLib.INSTANCE.MDFE_LimparLista(getHandle());
    checkResult( ret );
  }
  
  public void carregarEventoXml( String eArquivoOuXML ) throws Exception {
    int ret = ACBrMDFeLib.INSTANCE.MDFE_CarregarEventoXML( getHandle(), toUTF8( eArquivoOuXML ) );
    checkResult( ret );
  }

  public void carregarEventoINI( String eArquivoOuIni ) throws Exception {
    int ret = ACBrMDFeLib.INSTANCE.MDFE_CarregarEventoINI(getHandle(), toUTF8( eArquivoOuIni ) );
    checkResult( ret );
  }

  public void limparListaEventos() throws Exception {
    int ret = ACBrMDFeLib.INSTANCE.MDFE_LimparListaEventos(getHandle());
    checkResult( ret );
  }

  public void assinar() throws Exception {
    int ret = ACBrMDFeLib.INSTANCE.MDFE_Assinar(getHandle());
    checkResult( ret );
  }

  public void validar() throws Exception {
    int ret = ACBrMDFeLib.INSTANCE.MDFE_Validar(getHandle());
    checkResult( ret );
  }

  public String validarRegrasdeNegocios() throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrMDFeLib.INSTANCE.MDFE_ValidarRegrasdeNegocios( getHandle(), buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }

  public String verificarAssinatura() throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrMDFeLib.INSTANCE.MDFE_VerificarAssinatura( getHandle(), buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }
  
  public String gerarChave(int aCodigoUf, int aCodigoNumerico, int aModelo, int aSerie, int aNumero,
            int aTpEmi, Date aEmissao, String acpfcnpj) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );
    
    String pattern = "dd/MM/yyyy";
    DateFormat df = new SimpleDateFormat(pattern);

    int ret = ACBrMDFeLib.INSTANCE.MDFE_GerarChave( getHandle(), aCodigoUf, aCodigoNumerico, aModelo,
                                                  aSerie, aNumero, aTpEmi, df.format(aEmissao), 
                                                  toUTF8(acpfcnpj),  buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }
  
  public String obterCertificados() throws Exception {
      ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
      IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );
      
      int ret = ACBrMDFeLib.INSTANCE.MDFE_ObterCertificados(getHandle(), buffer, bufferLen );
      checkResult( ret );
      return processResult( buffer, bufferLen );
  }
  
  public String getPath(TipoPathMDFe tipo) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrMDFeLib.INSTANCE.MDFE_GetPath( getHandle(), tipo.asInt(), buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }
  
  public String getPathEvento(String aCodEvento) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrMDFeLib.INSTANCE.MDFE_GetPathEvento( getHandle(), toUTF8(aCodEvento), buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }

  public String statusServico() throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrMDFeLib.INSTANCE.MDFE_StatusServico( getHandle(), buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }
  
  public String consultar( String eChaveOuNFe ) throws Exception {
      return consultar(eChaveOuNFe, false);
  }

  public String consultar( String eChaveOuNFe, boolean AExtrairEventos ) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrMDFeLib.INSTANCE.MDFE_Consultar( getHandle(), toUTF8( eChaveOuNFe ), AExtrairEventos, buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }

  public String consultarRecibo( String aRecibo ) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrMDFeLib.INSTANCE.MDFE_ConsultarRecibo( getHandle(), toUTF8( aRecibo ), buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }
  
  public String ConsultarMDFeNaoEnc(String aCNPJ) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
    IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

    int ret = ACBrMDFeLib.INSTANCE.MDFE_ConsultaMDFeNaoEnc(getHandle(), toUTF8(aCNPJ), buffer, bufferLen);
    checkResult(ret);

    return processResult(buffer, bufferLen);
  }
  
  public String enviar(int aLote) throws Exception {
    return enviar(aLote, false, false, false);
  }

  public String enviar(int aLote, boolean imprimir) throws Exception {
    return enviar(aLote, imprimir, false, false);
  }
  
  public String enviar(int aLote, boolean imprimir, boolean sincrono, boolean zipado) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
    IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

    int ret = ACBrMDFeLib.INSTANCE.MDFE_Enviar(getHandle(), aLote, imprimir, sincrono, buffer, bufferLen);
    checkResult(ret);

    return processResult(buffer, bufferLen);
  }

  public String cancelar( String aChave, String aJustificativa, String aCNPJ ) throws Exception {
    return cancelar( aChave, aJustificativa, aCNPJ, 1 );
  }

  public String cancelar( String aChave, String aJustificativa, String aCNPJ, int aLote ) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrMDFeLib.INSTANCE.MDFE_Cancelar( getHandle(), toUTF8( aChave ), toUTF8( aJustificativa ), toUTF8( aCNPJ ), aLote, buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }

  public String enviarEvento( int idLote ) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrMDFeLib.INSTANCE.MDFE_EnviarEvento( getHandle(), idLote, buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }

  public String distribuicaoDFeporUltNSU( int acUFAutor, String aCNPJCPF, String eultNsu ) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrMDFeLib.INSTANCE.MDFE_DistribuicaoDFePorUltNSU( getHandle(), acUFAutor, toUTF8( aCNPJCPF ), toUTF8( eultNsu ), buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }

  public String distribuicaoDFeporNSU( int acUFAutor, String aCNPJCPF, String aNSU ) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrMDFeLib.INSTANCE.MDFE_DistribuicaoDFePorNSU( getHandle(), acUFAutor, toUTF8( aCNPJCPF ), toUTF8( aNSU ), buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }

  public String distribuicaoDFeporChave( int acUFAutor, String aCNPJCPF, String aChave ) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrMDFeLib.INSTANCE.MDFE_DistribuicaoDFePorChave( getHandle(), acUFAutor, aCNPJCPF, aChave, buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }

  public void enviarEmail( String aPara, String aChaveNFe, boolean aEnviarPDF, String aAssunto ) throws Exception {
    enviarEmail( aPara, aChaveNFe, aEnviarPDF, aAssunto, "", "", "" );
  }

  public void enviarEmail( String aPara, String aChaveNFe, boolean aEnviarPDF, String aAssunto,
                           String aEmailCC, String aAnexos, String aMesagem ) throws Exception {
    int ret = ACBrMDFeLib.INSTANCE.MDFE_EnviarEmail( getHandle(), aPara, aChaveNFe, aEnviarPDF, aAssunto,
        aEmailCC, aAnexos, aMesagem );
    checkResult( ret );
  }

  public void enviarEmailEvento( String aPara, String aChaveEvento, String aChaveNFe, boolean aEnviarPDF,
                                 String aAssunto ) throws Exception {
    enviarEmailEvento( aPara, aChaveEvento, aChaveNFe, aEnviarPDF, aAssunto, "", "", "" );
  }

  public void enviarEmailEvento( String aPara, String aChaveEvento, String aChaveNFe, boolean aEnviarPDF,
                                 String aAssunto, String aEmailCC, String aAnexos, String aMesagem ) throws Exception {
    int ret = ACBrMDFeLib.INSTANCE.MDFE_EnviarEmailEvento( getHandle(), aPara, aChaveEvento, aChaveNFe, aEnviarPDF,
        aAssunto, aEmailCC, aAnexos, aMesagem );
    checkResult( ret );
  }
  
  public void imprimir() throws Exception {
      imprimir( "", 1, "", null);
  }
  
  public void imprimir( String cImpressora, int nNumCopias, String cProtocolo, Boolean bMostrarPreview ) throws Exception {
      String mostrarPreview = bMostrarPreview != null ? bMostrarPreview ? "1" : "0" : "";
      int ret = ACBrMDFeLib.INSTANCE.MDFE_Imprimir( getHandle(), toUTF8( cImpressora ), nNumCopias, toUTF8( cProtocolo ), toUTF8( mostrarPreview ) );
      checkResult( ret );
  }

  public void imprimirPDF() throws Exception {
    int ret = ACBrMDFeLib.INSTANCE.MDFE_ImprimirPDF(getHandle());
    checkResult( ret );
  }

  public void imprimirEvento( String eArquivoXmlCTe, String eArquivoXmlEvento ) throws Exception {
    int ret = ACBrMDFeLib.INSTANCE.MDFE_ImprimirEvento( getHandle(), toUTF8(eArquivoXmlCTe), toUTF8(eArquivoXmlEvento) );
    checkResult( ret );
  }

  public void imprimirEventoPDF( String eArquivoXmlCTe, String eArquivoXmlEvento ) throws Exception {
    int ret = ACBrMDFeLib.INSTANCE.MDFE_ImprimirEventoPDF( getHandle(), toUTF8(eArquivoXmlCTe), toUTF8(eArquivoXmlEvento) );
    checkResult( ret );
  }

  public void ConfigImportar(String eArqConfig) throws Exception {
        
        int ret = ACBrMDFeLib.INSTANCE.MDFE_ConfigImportar(getHandle(), eArqConfig);
        checkResult(ret);
        
    }
    
    public String ConfigExportar() throws Exception {
		
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrMDFeLib.INSTANCE.MDFE_ConfigExportar(getHandle(), buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());
		
    }
    
    public String EncerrarMDFe(String eChaveOuMDFe, Date eDtEnc, String cMunicipioDescarga, String nCNPJ, String nProtocolo) throws Exception {

        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        String pattern = "dd/MM/yyyy";
        DateFormat df = new SimpleDateFormat(pattern);

        int ret = ACBrMDFeLib.INSTANCE.MDFE_EncerrarMDFe(getHandle(), eChaveOuMDFe, df.format(eDtEnc), cMunicipioDescarga, nCNPJ, nProtocolo, buffer, bufferLen);
        checkResult(ret);

        return processResult(buffer, bufferLen);
    }
  
  @Override
  protected void UltimoRetorno( ByteBuffer buffer, IntByReference bufferLen ) {
    ACBrMDFeLib.INSTANCE.MDFE_UltimoRetorno( getHandle(), buffer, bufferLen );
  }
  
      public void EncerrarMDFe(String eChave, Date date, String cMunicipio) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }
}