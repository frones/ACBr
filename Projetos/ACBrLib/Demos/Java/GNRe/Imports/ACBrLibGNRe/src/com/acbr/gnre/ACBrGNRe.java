package com.acbr.gnre;

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

public final class ACBrGNRe extends ACBrLibBase implements AutoCloseable {

    public int enviar() {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }
    
    private interface ACBrGNReLib extends Library {
        static String JNA_LIBRARY_NAME = LibraryLoader.getLibraryName();
        public final static ACBrGNReLib INSTANCE = LibraryLoader.getInstance();

        
    
    
        class LibraryLoader {
            private static String library = "";
            private static ACBrGNReLib instance = null;

            private static String getLibraryName() {
                if ( library.isEmpty() ) {
                library = Platform.is64Bit() ? "ACBrGNRe64" : "ACBrGNRe32";
            }
            return library;
            }

            public static ACBrGNReLib getInstance() {
                if ( instance == null ) {
                instance = ( ACBrGNReLib ) Native.synchronizedLibrary(
                ( Library ) Native.loadLibrary( JNA_LIBRARY_NAME, ACBrGNReLib.class ) );
            }
            return instance;
        }
    }

    int GNRE_Inicializar( String eArqConfig, String eChaveCrypt );

    int GNRE_Finalizar();

    int GNRE_Nome( ByteBuffer buffer, IntByReference bufferSize );

    int GNRE_Versao( ByteBuffer buffer, IntByReference bufferSize );

    int GNRE_UltimoRetorno( ByteBuffer buffer, IntByReference bufferSize );

    int GNRE_ConfigLer( String eArqConfig );

    int GNRE_ConfigGravar( String eArqConfig );

    int GNRE_ConfigLerValor( String eSessao, String eChave, ByteBuffer buffer, IntByReference bufferSize );

    int GNRE_ConfigGravarValor( String eSessao, String eChave, String valor );
    
    int GNRE_LimparLista ();

    int GNRE_CarregarXML( String eArquivoOuXML );

    int GNRE_CarregarINI( String eArquivoOuINI );
    
    int GNRE_ObterXml( Integer AIndex, ByteBuffer buffer, IntByReference bufferSize );
    
    int GNRE_GravarXml( Integer AIndex, String eNomeArquivo, String ePathArquivo );
    
    int GNRE_LimparListaGuiaRetorno ();
    
    int GNRE_CarregarGuiaRetorno (String eArquivoOuXml);
    
    int GNRE_Assinar ();
    
    int GNRE_Validar ();
    
    int GNRE_VerificarAssinatura(ByteBuffer buffer, IntByReference BufferSize);
    
    int GNRE_ObterCertificados(ByteBuffer buffer, IntByReference BufferSize);
    
    int GNRE_Enviar (ByteBuffer buffer, IntByReference bufferLen);
                                      
    int GNRE_Consultar(String eUF, long AReceita, ByteBuffer buffer, IntByReference BufferLen);
    
    int GNRE_EnviarEmail (String ePara, String eArquivoOuXml, boolean aEnviaPDF, String eAssunto, String eCc, String eAnexos, String eMensagem);
                                      
    int GNRE_Imprimir (String eNomeImpressora, String eMostrarPreview);
                                      
    int GNRE_ImprimirPDF ();
    
  }

  public ACBrGNRe() throws Exception {
    File iniFile = Paths.get( System.getProperty( "user.dir" ), "ACBrLib.ini" ).toFile();
    if ( !iniFile.exists() ) {
      iniFile.createNewFile();
    }

    int ret = ACBrGNReLib.INSTANCE.GNRE_Inicializar( toUTF8( iniFile.getAbsolutePath() ), toUTF8( "" ) );
    checkResult( ret );
  }

  public ACBrGNRe( String eArqConfig, String eChaveCrypt ) throws Exception {
    int ret = ACBrGNReLib.INSTANCE.GNRE_Inicializar( toUTF8( eArqConfig ), toUTF8( eChaveCrypt ) );
    checkResult( ret );
  }

  @Override
  public void close() throws Exception {
    int ret = ACBrGNReLib.INSTANCE.GNRE_Finalizar();
    checkResult( ret );
  }

  @Override
  protected void finalize() throws Throwable {
    try {
      int ret = ACBrGNReLib.INSTANCE.GNRE_Finalizar();
      checkResult( ret );
    }
    finally {
      super.finalize();
    }
  }

  public String nome() throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrGNReLib.INSTANCE.GNRE_Nome( buffer, bufferLen );
    checkResult( ret );

    return fromUTF8( buffer, bufferLen.getValue() );
  }

  public String versao() throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrGNReLib.INSTANCE.GNRE_Versao( buffer, bufferLen );
    checkResult( ret );

    return fromUTF8( buffer, bufferLen.getValue() );
  }

  public void configLer() throws Exception {
    configLer( "" );
  }
  
  public String Consultar(String eUF, long AReceita) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
    IntByReference BufferLen = new IntByReference(STR_BUFFER_LEN);
    
    int ret = ACBrGNReLib.INSTANCE.GNRE_Consultar(toUTF8( eUF ), AReceita, buffer, BufferLen);
    checkResult(ret);
      
    return processResult(buffer, BufferLen);
  }

  public void configLer( String eArqConfig ) throws Exception {
    int ret = ACBrGNReLib.INSTANCE.GNRE_ConfigLer( toUTF8( eArqConfig ) );
    checkResult( ret );
  }

  public void configGravar() throws Exception {
    configGravar( "" );
  }
  
  public void LimparListaGuiaRetorno () throws Exception{
      int ret = ACBrGNReLib.INSTANCE.GNRE_LimparListaGuiaRetorno();
      checkResult(ret);
  }

  public void configGravar( String eArqConfig ) throws Exception {
    int ret = ACBrGNReLib.INSTANCE.GNRE_ConfigGravar( toUTF8( eArqConfig ) );
    checkResult( ret );
  }

  public String configLerValor( ACBrSessao eSessao, String eChave ) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrGNReLib.INSTANCE.GNRE_ConfigLerValor( toUTF8( eSessao.name() ), toUTF8( eChave ), buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }

  public void configGravarValor( ACBrSessao eSessao, String eChave, Object value ) throws Exception {
    int ret = ACBrGNReLib.INSTANCE.GNRE_ConfigGravarValor( toUTF8( eSessao.name() ), toUTF8( eChave ), toUTF8( value.toString() ) );
    checkResult( ret );
  }

  public void carregarXml( String eArquivoOuXML ) throws Exception {
    int ret = ACBrGNReLib.INSTANCE.GNRE_CarregarXML( toUTF8( eArquivoOuXML ) );
    checkResult( ret );
  }
  
  public void CarregarGuiaRetorno (String eArquivoOuXML)  throws Exception {
      int ret = ACBrGNReLib.INSTANCE.GNRE_CarregarGuiaRetorno(toUTF8(eArquivoOuXML));
      checkResult(ret);
  }

  public String obterXml( int AIndex ) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );
    int ret = ACBrGNReLib.INSTANCE.GNRE_ObterXml(AIndex, buffer, bufferLen );
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
    int ret = ACBrGNReLib.INSTANCE.GNRE_GravarXml( AIndex, toUTF8( eNomeArquivo ), toUTF8( ePathArquivo ) );
    checkResult( ret );
  }
  
  public void carregarIni( String eArquivoOuIni ) throws Exception {
    int ret = ACBrGNReLib.INSTANCE.GNRE_CarregarINI( toUTF8( eArquivoOuIni ) );
    checkResult( ret );
  }

  public void limparLista() throws Exception {
    int ret = ACBrGNReLib.INSTANCE.GNRE_LimparLista();
    checkResult( ret );
  }

  public void assinar() throws Exception {
    int ret = ACBrGNReLib.INSTANCE.GNRE_Assinar();
    checkResult( ret );
  }

  public void validar() throws Exception {
    int ret = ACBrGNReLib.INSTANCE.GNRE_Validar();
    checkResult( ret );
  }

  public String verificarAssinatura() throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrGNReLib.INSTANCE.GNRE_VerificarAssinatura( buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }
   
  public String obterCertificados() throws Exception {
      ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
      IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );
      
      int ret = ACBrGNReLib.INSTANCE.GNRE_ObterCertificados(buffer, bufferLen );
      checkResult( ret );
      return processResult( buffer, bufferLen );
  }

  public String enviar( int aLote ) throws Exception {
    return enviar( aLote, false, false );
  }
  
  public String enviar( int aLote, boolean imprimir ) throws Exception {
    return enviar( aLote, imprimir, false );
  }

  public String enviar( int aLote, boolean imprimir, boolean sincrono ) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrGNReLib.INSTANCE.GNRE_Enviar( buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }

  public void enviarEmail( String aPara, String aChaveNFe, boolean aEnviarPDF, String aAssunto ) throws Exception {
    enviarEmail( aPara, aChaveNFe, aEnviarPDF, aAssunto, "", "", "" );
  }

  public void enviarEmail( String aPara, String aChaveNFe, boolean aEnviarPDF, String aAssunto,
                           String aEmailCC, String aAnexos, String aMesagem ) throws Exception {
    int ret = ACBrGNReLib.INSTANCE.GNRE_EnviarEmail( aPara, aChaveNFe, aEnviarPDF, aAssunto,
        aEmailCC, aAnexos, aMesagem );
    checkResult( ret );
  }
  
  public void imprimir (String eNomeImpressora, String eMostrarPreview) throws Exception{
      int ret = ACBrGNReLib.INSTANCE.GNRE_Imprimir(eNomeImpressora, eMostrarPreview);
      checkResult( ret );
  }

  public void imprimirPDF() throws Exception {
    int ret = ACBrGNReLib.INSTANCE.GNRE_ImprimirPDF();
    checkResult( ret );
  }
  
  @Override
  protected void UltimoRetorno( ByteBuffer buffer, IntByReference bufferLen ) {
    ACBrGNReLib.INSTANCE.GNRE_UltimoRetorno( buffer, bufferLen );
    
  }
}