package com.acbr.cte;

import com.acbr.ACBrLibBase;
import com.acbr.ACBrSessao;
import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Platform;
import com.sun.jna.ptr.IntByReference;

import java.io.File;
import java.nio.ByteBuffer;
import java.nio.charset.Charset;
import java.nio.file.Paths;

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

public final class ACBrCTe extends ACBrLibBase implements AutoCloseable {

  private static final Charset UTF8 = Charset.forName( "UTF-8" );
  private static final int STR_BUFFER_LEN = 256;

  public ACBrCTe() throws Exception {
    File iniFile = Paths.get( System.getProperty( "user.dir" ), "ACBrLib.ini" ).toFile();
    if ( !iniFile.exists() ) {
      iniFile.createNewFile();
    }

    int ret = ACBrCTeLib.INSTANCE.CTE_Inicializar( toUTF8( iniFile.getAbsolutePath() ), toUTF8( "" ) );
    checkResult( ret );
  }

  public ACBrCTe( String eArqConfig, String eChaveCrypt ) throws Exception {
    int ret = ACBrCTeLib.INSTANCE.CTE_Inicializar( toUTF8( eArqConfig ), toUTF8( eChaveCrypt ) );
    checkResult( ret );
  }

  @Override
  public void close() throws Exception {
    int ret = ACBrCTeLib.INSTANCE.CTE_Finalizar();
    checkResult( ret );
  }

  @Override
  protected void finalize() throws Throwable {
    try {
      int ret = ACBrCTeLib.INSTANCE.CTE_Finalizar();
      checkResult( ret );
    }
    finally {
      super.finalize();
    }
  }

  public String nome() throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrCTeLib.INSTANCE.CTE_Nome( buffer, bufferLen );
    checkResult( ret );

    return fromUTF8( buffer, bufferLen.getValue() );
  }

  public String versao() throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrCTeLib.INSTANCE.CTE_Versao( buffer, bufferLen );
    checkResult( ret );

    return fromUTF8( buffer, bufferLen.getValue() );
  }

  public void configLer() throws Exception {
    configLer( "" );
  }

  public void configLer( String eArqConfig ) throws Exception {
    int ret = ACBrCTeLib.INSTANCE.CTE_ConfigLer( toUTF8( eArqConfig ) );
    checkResult( ret );
  }

  public void configGravar() throws Exception {
    configGravar( "" );
  }

  public void configGravar( String eArqConfig ) throws Exception {
    int ret = ACBrCTeLib.INSTANCE.CTE_ConfigGravar( toUTF8( eArqConfig ) );
    checkResult( ret );
  }

  public String configLerValor( ACBrSessao eSessao, String eChave ) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrCTeLib.INSTANCE.CTE_ConfigLerValor( toUTF8( eSessao.name() ), toUTF8( eChave ), buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }

  public void configGravarValor( ACBrSessao eSessao, String eChave, Object value ) throws Exception {
    int ret = ACBrCTeLib.INSTANCE.CTE_ConfigGravarValor( toUTF8( eSessao.name() ), toUTF8( eChave ), toUTF8( value.toString() ) );
    checkResult( ret );
  }

  public void carregarXml( String eArquivoOuXML ) throws Exception {
    int ret = ACBrCTeLib.INSTANCE.CTE_CarregarXML( toUTF8( eArquivoOuXML ) );
    checkResult( ret );
  }

  public String obterXml( int AIndex ) throws Exception {
      ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
      IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );
      int ret = ACBrCTeLib.INSTANCE.CTE_ObterXml(AIndex, buffer, bufferLen );
      checkResult( ret );
      
      return processResult( buffer, bufferLen );
  }
  
  public void gravarXml ( int AIndex ) throws Exception {
      gravarXml(AIndex, "", "");
  }
  
  public void gravarXml ( int AIndex, String eNomeArquivo, String ePathArquivo ) throws Exception {
      int ret = ACBrCTeLib.INSTANCE.CTE_GravarXml( AIndex, toUTF8( eNomeArquivo ), toUTF8( ePathArquivo ) );
    checkResult( ret );
  }
  
  public void carregarIni( String eArquivoOuIni ) throws Exception {
    int ret = ACBrCTeLib.INSTANCE.CTE_CarregarINI( toUTF8( eArquivoOuIni ) );
    checkResult( ret );
  }

  public void limparLista() throws Exception {
    int ret = ACBrCTeLib.INSTANCE.CTE_LimparLista();
    checkResult( ret );
  }
  
  public void carregarEventoXml( String eArquivoOuXML ) throws Exception {
    int ret = ACBrCTeLib.INSTANCE.CTE_CarregarEventoXML( toUTF8( eArquivoOuXML ) );
    checkResult( ret );
  }

  public void carregarEventoINI( String eArquivoOuIni ) throws Exception {
    int ret = ACBrCTeLib.INSTANCE.CTE_CarregarEventoINI(toUTF8( eArquivoOuIni ) );
    checkResult( ret );
  }

  public void limparListaEventos() throws Exception {
    int ret = ACBrCTeLib.INSTANCE.CTE_LimparListaEventos();
    checkResult( ret );
  }

  public void assinar() throws Exception {
    int ret = ACBrCTeLib.INSTANCE.CTE_Assinar();
    checkResult( ret );
  }

  public void validar() throws Exception {
    int ret = ACBrCTeLib.INSTANCE.CTE_Validar();
    checkResult( ret );
  }

  public String validarRegrasdeNegocios() throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrCTeLib.INSTANCE.CTE_ValidarRegrasdeNegocios( buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }

  public String verificarAssinatura() throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrCTeLib.INSTANCE.CTE_VerificarAssinatura( buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }

  public String statusServico() throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrCTeLib.INSTANCE.CTE_StatusServico( buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }

  public String consultar( String eChaveOuNFe ) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrCTeLib.INSTANCE.CTE_Consultar( toUTF8( eChaveOuNFe ), buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }

  public String consultarRecibo( String aRecibo ) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrCTeLib.INSTANCE.CTE_ConsultarRecibo( toUTF8( aRecibo ), buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }

  //TODO: Sobrescrever método com valores default
  public String inutilizar( String aCNPJ, String aJustificativa, int ano, int modelo, int serie,
                            int numeroInicial, int numeroFinal ) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrCTeLib.INSTANCE.CTE_Inutilizar( toUTF8( aCNPJ ), toUTF8( aJustificativa ), ano, modelo, serie,
        numeroInicial, numeroFinal, buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }

  public String enviar( int aLote ) throws Exception {
    return enviar( aLote, false );
  }

  public String enviar( int aLote, boolean imprimir ) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrCTeLib.INSTANCE.CTE_Enviar( aLote, imprimir, buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }

  public String cancelar( String aChave, String aJustificativa, String aCNPJ ) throws Exception {
    return cancelar( aChave, aJustificativa, aCNPJ, 1 );
  }

  public String cancelar( String aChave, String aJustificativa, String aCNPJ, int aLote ) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrCTeLib.INSTANCE.CTE_Cancelar( toUTF8( aChave ), toUTF8( aJustificativa ), toUTF8( aCNPJ ), aLote, buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }

  public String enviarEvento( int idLote ) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrCTeLib.INSTANCE.CTE_EnviarEvento( idLote, buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }

  public String distribuicaoDFeporUltNSU( int acUFAutor, String aCNPJCPF, String eultNsu ) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrCTeLib.INSTANCE.CTE_DistribuicaoDFePorUltNSU( acUFAutor, toUTF8( aCNPJCPF ), toUTF8( eultNsu ), buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }

  public String distribuicaoDFeporNSU( int acUFAutor, String aCNPJCPF, String aNSU ) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrCTeLib.INSTANCE.CTE_DistribuicaoDFePorNSU( acUFAutor, toUTF8( aCNPJCPF ), toUTF8( aNSU ), buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }

  public String distribuicaoDFeporChave( int acUFAutor, String aCNPJCPF, String aChave ) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrCTeLib.INSTANCE.CTE_DistribuicaoDFePorChave( acUFAutor, aCNPJCPF, aChave, buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }

  public void enviarEmail( String aPara, String aChaveNFe, boolean aEnviarPDF, String aAssunto ) throws Exception {
    enviarEmail( aPara, aChaveNFe, aEnviarPDF, aAssunto, "", "", "" );
  }

  public void enviarEmail( String aPara, String aChaveNFe, boolean aEnviarPDF, String aAssunto,
                           String aEmailCC, String aAnexos, String aMesagem ) throws Exception {
    int ret = ACBrCTeLib.INSTANCE.CTE_EnviarEmail( aPara, aChaveNFe, aEnviarPDF, aAssunto,
        aEmailCC, aAnexos, aMesagem );
    checkResult( ret );
  }

  public void enviarEmailEvento( String aPara, String aChaveEvento, String aChaveNFe, boolean aEnviarPDF,
                                 String aAssunto ) throws Exception {
    enviarEmailEvento( aPara, aChaveEvento, aChaveNFe, aEnviarPDF, aAssunto, "", "", "" );
  }

  public void enviarEmailEvento( String aPara, String aChaveEvento, String aChaveNFe, boolean aEnviarPDF,
                                 String aAssunto, String aEmailCC, String aAnexos, String aMesagem ) throws Exception {
    int ret = ACBrCTeLib.INSTANCE.CTE_EnviarEmailEvento( aPara, aChaveEvento, aChaveNFe, aEnviarPDF,
        aAssunto, aEmailCC, aAnexos, aMesagem );
    checkResult( ret );
  }
  
  public void imprimir() throws Exception {
      
    int ret = ACBrCTeLib.INSTANCE.CTE_Imprimir();
    checkResult( ret );
  }

  public void imprimirPDF() throws Exception {
    int ret = ACBrCTeLib.INSTANCE.CTE_ImprimirPDF();
    checkResult( ret );
  }

  public void imprimirEvento( String aChaveNFe, String aChaveEvento ) throws Exception {
    int ret = ACBrCTeLib.INSTANCE.CTE_ImprimirEvento( aChaveNFe, aChaveEvento );
    checkResult( ret );
  }

  public void imprimirEventoPDF( String aChaveNFe, String aChaveEvento ) throws Exception {
    int ret = ACBrCTeLib.INSTANCE.CTE_ImprimirEventoPDF( aChaveNFe, aChaveEvento );
    checkResult( ret );
  }

  public void imprimirInutilizacao( String aChave ) throws Exception {
    int ret = ACBrCTeLib.INSTANCE.CTE_ImprimirInutilizacao( aChave );
    checkResult( ret );
  }

  public void imprimirInutilizacaoPDF( String aChave ) throws Exception {
    int ret = ACBrCTeLib.INSTANCE.CTE_ImprimirInutilizacaoPDF( aChave );
    checkResult( ret );
  }

  @Override
  protected void UltimoRetorno( ByteBuffer buffer, IntByReference bufferLen ) {
    ACBrCTeLib.INSTANCE.CTE_UltimoRetorno( buffer, bufferLen );
  }

  private interface ACBrCTeLib extends Library {
    static String JNA_LIBRARY_NAME = LibraryLoader.getLibraryName();
    public final static ACBrCTeLib INSTANCE = LibraryLoader.getInstance();

    int CTE_Inicializar( String eArqConfig, String eChaveCrypt );

    int CTE_Finalizar();

    int CTE_Nome( ByteBuffer buffer, IntByReference bufferSize );

    int CTE_Versao( ByteBuffer buffer, IntByReference bufferSize );

    int CTE_UltimoRetorno( ByteBuffer buffer, IntByReference bufferSize );

    int CTE_ConfigLer( String eArqConfig );

    int CTE_ConfigGravar( String eArqConfig );

    int CTE_ConfigLerValor( String eSessao, String eChave, ByteBuffer buffer, IntByReference bufferSize );

    int CTE_ConfigGravarValor( String eSessao, String eChave, String valor );

    int CTE_CarregarXML( String eArquivoOuXML );

    int CTE_CarregarINI( String eArquivoOuINI );
    
    int CTE_ObterXml( Integer AIndex, ByteBuffer buffer, IntByReference bufferSize );
    
    int CTE_GravarXml( Integer AIndex, String eNomeArquivo, String ePathArquivo );

    int CTE_LimparLista();

    int CTE_CarregarEventoXML( String eArquivoOuXml );

    int CTE_CarregarEventoINI( String eArquivoOuIni );

    int CTE_LimparListaEventos();

    int CTE_Assinar();

    int CTE_Validar();

    int CTE_ValidarRegrasdeNegocios( ByteBuffer buffer, IntByReference bufferSize );

    int CTE_VerificarAssinatura( ByteBuffer buffer, IntByReference bufferSize );

    int CTE_StatusServico( ByteBuffer buffer, IntByReference bufferSize );

    int CTE_Consultar( String eChaveOuNFe, ByteBuffer buffer, IntByReference bufferSize );

    int CTE_Inutilizar( String ACNPJ, String AJustificativa, int Ano, int Modelo, int Serie,
                        int NumeroInicial, int NumeroFinal, ByteBuffer buffer, IntByReference bufferSize );

    int CTE_Enviar( int ALote, boolean Imprimir, ByteBuffer buffer, IntByReference bufferSize );

    int CTE_ConsultarRecibo( String aRecibo, ByteBuffer buffer, IntByReference bufferSize );

    int CTE_Cancelar( String eChave, String eJustificativa, String eCNPJ, int ALote,
                      ByteBuffer buffer, IntByReference bufferSize );

    int CTE_EnviarEvento( int idLote, ByteBuffer buffer, IntByReference bufferSize );

    int CTE_DistribuicaoDFePorUltNSU( int AcUFAutor, String eCNPJCPF, String eultNsu, ByteBuffer buffer,
                                      IntByReference bufferSize );

    int CTE_DistribuicaoDFePorNSU( int AcUFAutor, String eCNPJCPF, String eNSU,
                                   ByteBuffer buffer, IntByReference bufferSize );

    int CTE_DistribuicaoDFePorChave( int AcUFAutor, String eCNPJCPF, String echNFe,
                                     ByteBuffer buffer, IntByReference bufferSize );

    int CTE_EnviarEmail( String ePara, String eChaveNFe, boolean AEnviaPDF, String eAssunto,
                         String eCC, String eAnexos, String eMensagem );

    int CTE_EnviarEmailEvento( String ePara, String eChaveEvento, String eChaveNFe,
                               boolean AEnviaPDF, String eAssunto, String eCC, String eAnexos, String eMensagem );

    int CTE_Imprimir();

    int CTE_ImprimirPDF();

    int CTE_ImprimirEvento( String eChaveNFe, String eChaveEvento );

    int CTE_ImprimirEventoPDF( String eChaveNFe, String eChaveEvento );

    int CTE_ImprimirInutilizacao( String eChave );

    int CTE_ImprimirInutilizacaoPDF( String eChave );

    class LibraryLoader {
      private static String library = "";
      private static ACBrCTeLib instance = null;

      private static String getLibraryName() {
        if ( library.isEmpty() ) {
          library = Platform.is64Bit() ? "ACBrCTe64" : "ACBrCTe32";
        }
        return library;
      }

      public static ACBrCTeLib getInstance() {
        if ( instance == null ) {
          instance = ( ACBrCTeLib ) Native.synchronizedLibrary(
              ( Library ) Native.loadLibrary( JNA_LIBRARY_NAME, ACBrCTeLib.class ) );
        }

        return instance;
      }
    }
  }
}