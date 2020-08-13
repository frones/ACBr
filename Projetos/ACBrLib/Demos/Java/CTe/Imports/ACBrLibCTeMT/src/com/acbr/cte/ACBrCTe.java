package com.acbr.cte;

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

public final class ACBrCTe extends ACBrLibBase implements AutoCloseable {
    
    private interface ACBrCTeLib extends Library {
        
        static String JNA_LIBRARY_NAME = LibraryLoader.getLibraryName();
        public final static ACBrCTeLib INSTANCE = LibraryLoader.getInstance();
        
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
        
    int CTE_Inicializar(PointerByReference libHandler, String eArqConfig, String eChaveCrypt );

    int CTE_Finalizar(Pointer libHandler);

    int CTE_Nome(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize );

    int CTE_Versao(Pointer libHandler,  ByteBuffer buffer, IntByReference bufferSize );

    int CTE_UltimoRetorno(Pointer libHandler,  ByteBuffer buffer, IntByReference bufferSize );

    int CTE_ConfigLer(Pointer libHandler,  String eArqConfig );

    int CTE_ConfigGravar(Pointer libHandler,  String eArqConfig );

    int CTE_ConfigLerValor(Pointer libHandler,  String eSessao, String eChave, ByteBuffer buffer, IntByReference bufferSize );

    int CTE_ConfigGravarValor(Pointer libHandler,  String eSessao, String eChave, String valor );

    int CTE_CarregarXML(Pointer libHandler,  String eArquivoOuXML );

    int CTE_CarregarINI(Pointer libHandler,  String eArquivoOuINI );
    
    int CTE_ObterXml(Pointer libHandler,  Integer AIndex, ByteBuffer buffer, IntByReference bufferSize );
    
    int CTE_GravarXml(Pointer libHandler,  Integer AIndex, String eNomeArquivo, String ePathArquivo );
    
    int CTE_ObterIni(Pointer libHandler,  Integer AIndex, ByteBuffer buffer, IntByReference bufferSize );
    
    int CTE_GravarIni(Pointer libHandler,  Integer AIndex, String eNomeArquivo, String ePathArquivo );

    int CTE_LimparLista(Pointer libHandler);

    int CTE_CarregarEventoXML(Pointer libHandler,  String eArquivoOuXml );

    int CTE_CarregarEventoINI(Pointer libHandler,  String eArquivoOuIni );

    int CTE_LimparListaEventos(Pointer libHandler);

    int CTE_Assinar(Pointer libHandler);

    int CTE_Validar(Pointer libHandler);

    int CTE_ValidarRegrasdeNegocios(Pointer libHandler,  ByteBuffer buffer, IntByReference bufferSize );

    int CTE_VerificarAssinatura(Pointer libHandler,  ByteBuffer buffer, IntByReference bufferSize );
    
    int CTE_GerarChave(Pointer libHandler, int ACodigoUF, int ACodigoNumerico, int AModelo, int ASerie, int ANumero,
                int ATpEmi, String AEmissao, String CPFCNPJ, ByteBuffer buffer, IntByReference bufferSize);
    
    int CTE_ObterCertificados(Pointer libHandler,  ByteBuffer buffer, IntByReference bufferSize );
    
    int CTE_GetPath(Pointer libHandler,  int tipo, ByteBuffer buffer, IntByReference bufferSize );
    
    int CTE_GetPathEvento( Pointer libHandler, String aCodEvento, ByteBuffer buffer, IntByReference bufferSize );

    int CTE_StatusServico( Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize );

    int CTE_Consultar( Pointer libHandler, String eChaveOuNFe, boolean AExtrairEventos, ByteBuffer buffer, IntByReference bufferSize );

    int CTE_Inutilizar( Pointer libHandler, String ACNPJ, String AJustificativa, int Ano, int Modelo, int Serie,
                        int NumeroInicial, int NumeroFinal, ByteBuffer buffer, IntByReference bufferSize );

    int CTE_Enviar( Pointer libHandler, int ALote, boolean Imprimir, boolean Sincrono, ByteBuffer buffer, IntByReference bufferSize );

    int CTE_ConsultarRecibo( Pointer libHandler, String aRecibo, ByteBuffer buffer, IntByReference bufferSize );
    
    int CTE_ConsultaCadastro( Pointer libHandler, String cUF, String nDocumento, Boolean nIE, ByteBuffer buffer, IntByReference bufferSize );

    int CTE_Cancelar( Pointer libHandler, String eChave, String eJustificativa, String eCNPJ, int ALote,
                      ByteBuffer buffer, IntByReference bufferSize );

    int CTE_EnviarEvento( Pointer libHandler, int idLote, ByteBuffer buffer, IntByReference bufferSize );

    int CTE_DistribuicaoDFePorUltNSU( Pointer libHandler, int AcUFAutor, String eCNPJCPF, String eultNsu, ByteBuffer buffer,
                                      IntByReference bufferSize );

    int CTE_DistribuicaoDFePorNSU( Pointer libHandler, int AcUFAutor, String eCNPJCPF, String eNSU,
                                   ByteBuffer buffer, IntByReference bufferSize );

    int CTE_DistribuicaoDFePorChave( Pointer libHandler, int AcUFAutor, String eCNPJCPF, String echNFe,
                                     ByteBuffer buffer, IntByReference bufferSize );

    int CTE_EnviarEmail( Pointer libHandler, String ePara, String eChaveNFe, boolean AEnviaPDF, String eAssunto,
                         String eCC, String eAnexos, String eMensagem );

    int CTE_EnviarEmailEvento( Pointer libHandler, String ePara, String eChaveEvento, String eChaveNFe,
                               boolean AEnviaPDF, String eAssunto, String eCC, String eAnexos, String eMensagem );

    int CTE_Imprimir(Pointer libHandler, String cImpressora, int nNumCopias, String cProtocolo, String bMostrarPreview);

    int CTE_ImprimirPDF(Pointer libHandler);

    int CTE_ImprimirEvento(Pointer libHandler,  String eArquivoXmlCTe, String eArquivoXmlEvento );

    int CTE_ImprimirEventoPDF( Pointer libHandler, String eArquivoXmlCTe, String eArquivoXmlEvento );

    int CTE_ImprimirInutilizacao( Pointer libHandler, String eArquivoXml );

    int CTE_ImprimirInutilizacaoPDF(Pointer libHandler, String eArquivoXml );

    
  }
    
  public ACBrCTe() throws Exception {
    File iniFile = Paths.get( System.getProperty( "user.dir" ), "ACBrLib.ini" ).toFile();
    if ( !iniFile.exists() ) {
      iniFile.createNewFile();
    }

    PointerByReference handle = new PointerByReference();
    int ret = ACBrCTeLib.INSTANCE.CTE_Inicializar(handle, toUTF8( iniFile.getAbsolutePath() ), toUTF8( "" ) );
    checkResult( ret );
  }

  public ACBrCTe( String eArqConfig, String eChaveCrypt ) throws Exception {
      PointerByReference handle = new PointerByReference();
    int ret = ACBrCTeLib.INSTANCE.CTE_Inicializar(handle ,toUTF8( eArqConfig ), toUTF8( eChaveCrypt ) );
    checkResult( ret );
  }

  @Override
  public void close() throws Exception {
    int ret = ACBrCTeLib.INSTANCE.CTE_Finalizar(getHandle());
    checkResult( ret );
  }

  @Override
  protected void finalize() throws Throwable {
    try {
      int ret = ACBrCTeLib.INSTANCE.CTE_Finalizar(getHandle());
      checkResult( ret );
    }
    finally {
      super.finalize();
    }
  }

  public String nome() throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrCTeLib.INSTANCE.CTE_Nome(getHandle(), buffer, bufferLen );
    checkResult( ret );

    return fromUTF8( buffer, bufferLen.getValue() );
  }

  public String versao() throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrCTeLib.INSTANCE.CTE_Versao(getHandle(),  buffer, bufferLen );
    checkResult( ret );

    return fromUTF8( buffer, bufferLen.getValue() );
  }

  public void configLer() throws Exception {
    configLer( "" );
  }

  public void configLer( String eArqConfig ) throws Exception {
    int ret = ACBrCTeLib.INSTANCE.CTE_ConfigLer(getHandle(), toUTF8( eArqConfig ) );
    checkResult( ret );
  }

  public void configGravar() throws Exception {
    configGravar( "" );
  }

  public void configGravar( String eArqConfig ) throws Exception {
    int ret = ACBrCTeLib.INSTANCE.CTE_ConfigGravar(getHandle(), toUTF8( eArqConfig ) );
    checkResult( ret );
  }

  public String configLerValor( ACBrSessao eSessao, String eChave ) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrCTeLib.INSTANCE.CTE_ConfigLerValor(getHandle(), toUTF8( eSessao.name() ), toUTF8( eChave ), buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }

  public void configGravarValor( ACBrSessao eSessao, String eChave, Object value ) throws Exception {
    int ret = ACBrCTeLib.INSTANCE.CTE_ConfigGravarValor(getHandle(), toUTF8( eSessao.name() ), toUTF8( eChave ), toUTF8( value.toString() ) );
    checkResult( ret );
  }

  public void carregarXml( String eArquivoOuXML ) throws Exception {
    int ret = ACBrCTeLib.INSTANCE.CTE_CarregarXML(getHandle(), toUTF8( eArquivoOuXML ) );
    checkResult( ret );
  }

  public String obterXml( int AIndex ) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );
    int ret = ACBrCTeLib.INSTANCE.CTE_ObterXml(getHandle(),AIndex, buffer, bufferLen );
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
    int ret = ACBrCTeLib.INSTANCE.CTE_GravarXml(getHandle(), AIndex, toUTF8( eNomeArquivo ), toUTF8( ePathArquivo ) );
    checkResult( ret );
  }
  
  public String obterIni( int AIndex ) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );
    int ret = ACBrCTeLib.INSTANCE.CTE_ObterIni(getHandle(),AIndex, buffer, bufferLen );
    checkResult( ret );
      
    return processResult( buffer, bufferLen );
  }
  
  public void gravarIni ( int AIndex, String eNomeArquivo ) throws Exception {
    gravarIni(AIndex, eNomeArquivo, "");
  }
  
  public void gravarIni ( int AIndex, String eNomeArquivo, String ePathArquivo ) throws Exception {
    int ret = ACBrCTeLib.INSTANCE.CTE_GravarIni(getHandle(), AIndex, toUTF8( eNomeArquivo ), toUTF8( ePathArquivo ) );
    checkResult( ret );
  }
  
  public void carregarIni( String eArquivoOuIni ) throws Exception {
    int ret = ACBrCTeLib.INSTANCE.CTE_CarregarINI(getHandle(), toUTF8( eArquivoOuIni ) );
    checkResult( ret );
  }

  public void limparLista() throws Exception {
    int ret = ACBrCTeLib.INSTANCE.CTE_LimparLista(getHandle());
    checkResult( ret );
  }
  
  public void carregarEventoXml( String eArquivoOuXML ) throws Exception {
    int ret = ACBrCTeLib.INSTANCE.CTE_CarregarEventoXML(getHandle(), toUTF8( eArquivoOuXML ) );
    checkResult( ret );
  }

  public void carregarEventoINI( String eArquivoOuIni ) throws Exception {
    int ret = ACBrCTeLib.INSTANCE.CTE_CarregarEventoINI(getHandle(), toUTF8( eArquivoOuIni ) );
    checkResult( ret );
  }

  public void limparListaEventos() throws Exception {
    int ret = ACBrCTeLib.INSTANCE.CTE_LimparListaEventos(getHandle());
    checkResult( ret );
  }

  public void assinar() throws Exception {
    int ret = ACBrCTeLib.INSTANCE.CTE_Assinar(getHandle());
    checkResult( ret );
  }

  public void validar() throws Exception {
    int ret = ACBrCTeLib.INSTANCE.CTE_Validar(getHandle());
    checkResult( ret );
  }

  public String validarRegrasdeNegocios() throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrCTeLib.INSTANCE.CTE_ValidarRegrasdeNegocios(getHandle(), buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }

  public String verificarAssinatura() throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrCTeLib.INSTANCE.CTE_VerificarAssinatura(getHandle(), buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }
  
  public String gerarChave(int aCodigoUf, int aCodigoNumerico, int aModelo, int aSerie, int aNumero,
            int aTpEmi, Date aEmissao, String acpfcnpj) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );
    
    String pattern = "dd/MM/yyyy";
    DateFormat df = new SimpleDateFormat(pattern);

    int ret = ACBrCTeLib.INSTANCE.CTE_GerarChave(getHandle(), aCodigoUf, aCodigoNumerico, aModelo,
                                                  aSerie, aNumero, aTpEmi, df.format(aEmissao), 
                                                  toUTF8(acpfcnpj),  buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }
  
  public String obterCertificados() throws Exception {
      ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
      IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );
      
      int ret = ACBrCTeLib.INSTANCE.CTE_ObterCertificados(getHandle(), buffer, bufferLen );
      checkResult( ret );
      return processResult( buffer, bufferLen );
  }
  
  public String getPath(TipoPathCTe tipo) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrCTeLib.INSTANCE.CTE_GetPath(getHandle(), tipo.asInt(), buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }
  
  public String getPathEvento(String aCodEvento) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrCTeLib.INSTANCE.CTE_GetPathEvento(getHandle(), toUTF8(aCodEvento), buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }

  public String statusServico() throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrCTeLib.INSTANCE.CTE_StatusServico(getHandle(), buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }
  
  public String consultar( String eChaveOuNFe ) throws Exception {
      return consultar(eChaveOuNFe, false);
  }

  public String consultar( String eChaveOuNFe, boolean AExtrairEventos ) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrCTeLib.INSTANCE.CTE_Consultar(getHandle(), toUTF8( eChaveOuNFe ), AExtrairEventos, buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }

  public String consultarRecibo( String aRecibo ) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrCTeLib.INSTANCE.CTE_ConsultarRecibo(getHandle(), toUTF8( aRecibo ), buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }
  
  public String consultaCadastro( String cUF, String nDocumento, Boolean nIE ) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrCTeLib.INSTANCE.CTE_ConsultaCadastro(getHandle(), toUTF8( cUF ), toUTF8( nDocumento), nIE, buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }

  //TODO: Sobrescrever método com valores default
  public String inutilizar( String aCNPJ, String aJustificativa, int ano, int modelo, int serie,
                            int numeroInicial, int numeroFinal ) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrCTeLib.INSTANCE.CTE_Inutilizar(getHandle(), toUTF8( aCNPJ ), toUTF8( aJustificativa ), ano, modelo, serie,
        numeroInicial, numeroFinal, buffer, bufferLen );
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

    int ret = ACBrCTeLib.INSTANCE.CTE_Enviar(getHandle(), aLote, imprimir, sincrono, buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }

  public String cancelar( String aChave, String aJustificativa, String aCNPJ ) throws Exception {
    return cancelar( aChave, aJustificativa, aCNPJ, 1 );
  }

  public String cancelar( String aChave, String aJustificativa, String aCNPJ, int aLote ) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrCTeLib.INSTANCE.CTE_Cancelar(getHandle(), toUTF8( aChave ), toUTF8( aJustificativa ), toUTF8( aCNPJ ), aLote, buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }

  public String enviarEvento( int idLote ) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrCTeLib.INSTANCE.CTE_EnviarEvento(getHandle(), idLote, buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }

  public String distribuicaoDFeporUltNSU( int acUFAutor, String aCNPJCPF, String eultNsu ) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrCTeLib.INSTANCE.CTE_DistribuicaoDFePorUltNSU(getHandle(), acUFAutor, toUTF8( aCNPJCPF ), toUTF8( eultNsu ), buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }

  public String distribuicaoDFeporNSU( int acUFAutor, String aCNPJCPF, String aNSU ) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrCTeLib.INSTANCE.CTE_DistribuicaoDFePorNSU(getHandle(), acUFAutor, toUTF8( aCNPJCPF ), toUTF8( aNSU ), buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }

  public String distribuicaoDFeporChave( int acUFAutor, String aCNPJCPF, String aChave ) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrCTeLib.INSTANCE.CTE_DistribuicaoDFePorChave(getHandle(), acUFAutor, aCNPJCPF, aChave, buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }

  public void enviarEmail( String aPara, String aChaveNFe, boolean aEnviarPDF, String aAssunto ) throws Exception {
    enviarEmail( aPara, aChaveNFe, aEnviarPDF, aAssunto, "", "", "" );
  }

  public void enviarEmail( String aPara, String aChaveNFe, boolean aEnviarPDF, String aAssunto,
                           String aEmailCC, String aAnexos, String aMesagem ) throws Exception {
    int ret = ACBrCTeLib.INSTANCE.CTE_EnviarEmail(getHandle(), aPara, aChaveNFe, aEnviarPDF, aAssunto,
        aEmailCC, aAnexos, aMesagem );
    checkResult( ret );
  }

  public void enviarEmailEvento( String aPara, String aChaveEvento, String aChaveNFe, boolean aEnviarPDF,
                                 String aAssunto ) throws Exception {
    enviarEmailEvento( aPara, aChaveEvento, aChaveNFe, aEnviarPDF, aAssunto, "", "", "" );
  }

  public void enviarEmailEvento( String aPara, String aChaveEvento, String aChaveNFe, boolean aEnviarPDF,
                                 String aAssunto, String aEmailCC, String aAnexos, String aMesagem ) throws Exception {
    int ret = ACBrCTeLib.INSTANCE.CTE_EnviarEmailEvento(getHandle(), aPara, aChaveEvento, aChaveNFe, aEnviarPDF,
        aAssunto, aEmailCC, aAnexos, aMesagem );
    checkResult( ret );
  }
  
  public void imprimir() throws Exception {
      imprimir( "", 1, "", null);
  }
  
  public void imprimir( String cImpressora, int nNumCopias, String cProtocolo, Boolean bMostrarPreview ) throws Exception {
      String mostrarPreview = bMostrarPreview != null ? bMostrarPreview ? "1" : "0" : "";
      int ret = ACBrCTeLib.INSTANCE.CTE_Imprimir(getHandle(), toUTF8( cImpressora ), nNumCopias, toUTF8( cProtocolo ), toUTF8( mostrarPreview ) );
      checkResult( ret );
  }

  public void imprimirPDF() throws Exception {
    int ret = ACBrCTeLib.INSTANCE.CTE_ImprimirPDF(getHandle());
    checkResult( ret );
  }

  public void imprimirEvento( String eArquivoXmlCTe, String eArquivoXmlEvento ) throws Exception {
    int ret = ACBrCTeLib.INSTANCE.CTE_ImprimirEvento(getHandle(), toUTF8(eArquivoXmlCTe), toUTF8(eArquivoXmlEvento) );
    checkResult( ret );
  }

  public void imprimirEventoPDF( String eArquivoXmlCTe, String eArquivoXmlEvento ) throws Exception {
    int ret = ACBrCTeLib.INSTANCE.CTE_ImprimirEventoPDF(getHandle(), toUTF8(eArquivoXmlCTe), toUTF8(eArquivoXmlEvento) );
    checkResult( ret );
  }

  public void imprimirInutilizacao( String eArquivoXml ) throws Exception {
    int ret = ACBrCTeLib.INSTANCE.CTE_ImprimirInutilizacao(getHandle(), toUTF8(eArquivoXml) );
    checkResult( ret );
  }

  public void imprimirInutilizacaoPDF( String eArquivoXml ) throws Exception {
    int ret = ACBrCTeLib.INSTANCE.CTE_ImprimirInutilizacaoPDF(getHandle(), toUTF8(eArquivoXml) );
    checkResult( ret );
  }

  @Override
  protected void UltimoRetorno( ByteBuffer buffer, IntByReference bufferLen ) {
    ACBrCTeLib.INSTANCE.CTE_UltimoRetorno(getHandle(), buffer, bufferLen );
  }

}