package com.acbr.nfe;

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

public final class ACBrNFe extends ACBrLibBase {

    private interface ACBrNFeLib extends Library {
    
        static String JNA_LIBRARY_NAME = LibraryLoader.getLibraryName();
        public final static ACBrNFeLib INSTANCE = LibraryLoader.getInstance();

        class LibraryLoader {
      
            private static String library = "";
            private static ACBrNFeLib instance = null;

            private static String getLibraryName() {
                if ( library.isEmpty() ) {
                    if(Platform.isWindows()){
                        library = Platform.is64Bit() ? "ACBrNFe64" : "ACBrNFe32";                        
                    }else{
                        library = Platform.is64Bit() ? "acbrnfe64" : "acbrnfe32";
                    }
                  }
                return library;
                
            }

            public static ACBrNFeLib getInstance() {
                if ( instance == null ) {
                    instance = ( ACBrNFeLib ) Native.synchronizedLibrary(
                ( Library ) Native.load( JNA_LIBRARY_NAME, ACBrNFeLib.class ) );
                }
                return instance;
            }
    }
        
    int NFE_Inicializar( PointerByReference libHandler, String eArqConfig, String eChaveCrypt );

    int NFE_Finalizar(Pointer libHandler);

    int NFE_Nome( Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize );

    int NFE_Versao( Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize );

    int NFE_UltimoRetorno( Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize );

    int NFE_ConfigImportar(Pointer libHandler, String eArqConfig);
        
    int NFE_ConfigExportar(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize);
    
    int NFE_ConfigLer( Pointer libHandler, String eArqConfig );

    int NFE_ConfigGravar( Pointer libHandler, String eArqConfig );

    int NFE_ConfigLerValor( Pointer libHandler, String eSessao, String eChave, ByteBuffer buffer, IntByReference bufferSize );

    int NFE_ConfigGravarValor( Pointer libHandler, String eSessao, String eChave, String valor );

    int NFE_CarregarXML( Pointer libHandler, String eArquivoOuXML );

    int NFE_CarregarINI( Pointer libHandler, String eArquivoOuINI );
    
    int NFE_ObterXml( Pointer libHandler, Integer AIndex, ByteBuffer buffer, IntByReference bufferSize );
    
    int NFE_GravarXml( Pointer libHandler, Integer AIndex, String eNomeArquivo, String ePathArquivo );
    
    int NFE_ObterIni( Pointer libHandler, Integer AIndex, ByteBuffer buffer, IntByReference bufferSize );
    
    int NFE_GravarIni( Pointer libHandler, Integer AIndex, String eNomeArquivo, String ePathArquivo );

    int NFE_LimparLista(Pointer libHandler);

    int NFE_CarregarEventoXML( Pointer libHandler, String eArquivoOuXml );

    int NFE_CarregarEventoINI( Pointer libHandler, String eArquivoOuIni );

    int NFE_LimparListaEventos(Pointer libHandler);

    int NFE_Assinar(Pointer libHandler);

    int NFE_Validar(Pointer libHandler);
    
    int NFE_SalvarPDF(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize);

    int NFE_ValidarRegrasdeNegocios( Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize );

    int NFE_VerificarAssinatura( Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize );
    
    int NFE_GerarChave(Pointer libHandler, int ACodigoUF, int ACodigoNumerico, int AModelo, int ASerie, int ANumero,
                int ATpEmi, String AEmissao, String CPFCNPJ, ByteBuffer buffer, IntByReference bufferSize);
    
    int NFE_ObterCertificados( Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize );
    
    int NFE_GetPath( Pointer libHandler, int tipo, ByteBuffer buffer, IntByReference bufferSize );
    
    int NFE_GetPathEvento( Pointer libHandler, String aCodEvento, ByteBuffer buffer, IntByReference bufferSize );

    int NFE_StatusServico( Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize );

    int NFE_Consultar( Pointer libHandler, String eChaveOuNFe, boolean AExtrairEventos, ByteBuffer buffer, IntByReference bufferSize );

    int NFE_Inutilizar( Pointer libHandler, String ACNPJ, String AJustificativa, int Ano, int Modelo, int Serie,
                        int NumeroInicial, int NumeroFinal, ByteBuffer buffer, IntByReference bufferSize );

    int NFE_Enviar( Pointer libHandler, int ALote, boolean Imprimir, boolean sincrono, boolean zipado, ByteBuffer buffer, IntByReference bufferSize );

    int NFE_ConsultarRecibo( Pointer libHandler, String aRecibo, ByteBuffer buffer, IntByReference bufferSize );
    
    int NFE_ConsultaCadastro( Pointer libHandler, String cUF, String nDocumento, Boolean nIE, ByteBuffer buffer, IntByReference bufferSize );

    int NFE_Cancelar( Pointer libHandler, String eChave, String eJustificativa, String eCNPJ, int ALote,
                      ByteBuffer buffer, IntByReference bufferSize );

    int NFE_EnviarEvento( Pointer libHandler, int idLote, ByteBuffer buffer, IntByReference bufferSize );

    int NFE_DistribuicaoDFePorUltNSU( Pointer libHandler, int AcUFAutor, String eCNPJCPF, String eultNsu, ByteBuffer buffer,
                                      IntByReference bufferSize );

    int NFE_DistribuicaoDFePorNSU( Pointer libHandler, int AcUFAutor, String eCNPJCPF, String eNSU,
                                   ByteBuffer buffer, IntByReference bufferSize );

    int NFE_DistribuicaoDFePorChave( Pointer libHandler, int AcUFAutor, String eCNPJCPF, String echNFe,
                                     ByteBuffer buffer, IntByReference bufferSize );

    int NFE_EnviarEmail( Pointer libHandler, String ePara, String eChaveNFe, boolean AEnviaPDF, String eAssunto,
                         String eCC, String eAnexos, String eMensagem );

    int NFE_EnviarEmailEvento( Pointer libHandler, String ePara, String eChaveEvento, String eChaveNFe,
                               boolean AEnviaPDF, String eAssunto, String eCC, String eAnexos, String eMensagem );

    int NFE_Imprimir( Pointer libHandler, String cImpressora, int nNumCopias, String cProtocolo,
                      String bMostrarPreview, String cMarcaDagua, String bViaConsumidor, String bSimplificado );

    int NFE_ImprimirPDF(Pointer libHandler);

    int NFE_ImprimirEvento( Pointer libHandler, String eArquivoXmlNFe, String eArquivoXmlEvento );

    int NFE_ImprimirEventoPDF( Pointer libHandler, String eArquivoXmlNFe, String eArquivoXmlEvento );

    int NFE_ImprimirInutilizacao( Pointer libHandler, String eArquivoXml );

    int NFE_ImprimirInutilizacaoPDF( Pointer libHandler, String eArquivoXml );

  }
    
  public ACBrNFe() throws Exception {
    File iniFile = Paths.get( System.getProperty( "user.dir" ), "ACBrLib.ini" ).toFile();
    if ( !iniFile.exists() ) {
      iniFile.createNewFile();
    }

    PointerByReference handle = new PointerByReference();
    int ret = ACBrNFeLib.INSTANCE.NFE_Inicializar( handle,toUTF8( iniFile.getAbsolutePath() ), toUTF8( "" ) );
    checkResult( ret );
    setHandle(handle.getValue());
  }

  public ACBrNFe( String eArqConfig, String eChaveCrypt ) throws Exception {
    PointerByReference handle = new PointerByReference();
    int ret = ACBrNFeLib.INSTANCE.NFE_Inicializar( handle,toUTF8( eArqConfig ), toUTF8( eChaveCrypt ) );
    checkResult( ret );
    setHandle(handle.getValue());
  }

  @Override
  protected void dispose() throws Exception {
    int ret = ACBrNFeLib.INSTANCE.NFE_Finalizar(getHandle());
    checkResult( ret );
  }

  public String nome() throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrNFeLib.INSTANCE.NFE_Nome( getHandle(), buffer, bufferLen );
    checkResult( ret );

    return fromUTF8( buffer, bufferLen.getValue() );
  }

  public String versao() throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrNFeLib.INSTANCE.NFE_Versao( getHandle(), buffer, bufferLen );
    checkResult( ret );

    return fromUTF8( buffer, bufferLen.getValue() );
  }

  public void configLer() throws Exception {
    configLer( "" );
  }

  public void configLer( String eArqConfig ) throws Exception {
    int ret = ACBrNFeLib.INSTANCE.NFE_ConfigLer( getHandle(), toUTF8( eArqConfig ) );
    checkResult( ret );
  }

  public void configGravar() throws Exception {
    configGravar( "" );
  }

  public void configGravar( String eArqConfig ) throws Exception {
    int ret = ACBrNFeLib.INSTANCE.NFE_ConfigGravar( getHandle(), toUTF8( eArqConfig ) );
    checkResult( ret );
  }

    @Override
  public String configLerValor( ACBrSessao eSessao, String eChave ) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrNFeLib.INSTANCE.NFE_ConfigLerValor( getHandle(), toUTF8( eSessao.name() ), toUTF8( eChave ), buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }

    @Override
  public void configGravarValor( ACBrSessao eSessao, String eChave, Object value ) throws Exception {
    int ret = ACBrNFeLib.INSTANCE.NFE_ConfigGravarValor(getHandle(), toUTF8(eSessao.name()), toUTF8(eChave), toUTF8(value.toString()) );
    checkResult( ret );
  }

  public void carregarXml( String eArquivoOuXML ) throws Exception {
    int ret = ACBrNFeLib.INSTANCE.NFE_CarregarXML(getHandle(),  toUTF8( eArquivoOuXML ) );
    checkResult( ret );
  }

  public String obterXml( int AIndex ) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );
    int ret = ACBrNFeLib.INSTANCE.NFE_ObterXml(getHandle(), AIndex, buffer, bufferLen );
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
    int ret = ACBrNFeLib.INSTANCE.NFE_GravarXml( getHandle(), AIndex, toUTF8( eNomeArquivo ), toUTF8( ePathArquivo ) );
    checkResult( ret );
  }
  
  public String obterIni( int AIndex ) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );
    int ret = ACBrNFeLib.INSTANCE.NFE_ObterIni(getHandle(), AIndex, buffer, bufferLen );
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
    int ret = ACBrNFeLib.INSTANCE.NFE_GravarIni( getHandle(), AIndex, toUTF8( eNomeArquivo ), toUTF8( ePathArquivo ) );
    checkResult( ret );
  }
  
  public void carregarIni( String eArquivoOuIni ) throws Exception {
    int ret = ACBrNFeLib.INSTANCE.NFE_CarregarINI( getHandle(), toUTF8( eArquivoOuIni ) );
    checkResult( ret );
  }

  public void limparLista() throws Exception {
    int ret = ACBrNFeLib.INSTANCE.NFE_LimparLista(getHandle());
    checkResult( ret );
  }
  
  public void carregarEventoXml( String eArquivoOuXML ) throws Exception {
    int ret = ACBrNFeLib.INSTANCE.NFE_CarregarEventoXML( getHandle(), toUTF8( eArquivoOuXML ) );
    checkResult( ret );
  }

  public void carregarEventoINI( String eArquivoOuIni ) throws Exception {
    int ret = ACBrNFeLib.INSTANCE.NFE_CarregarEventoINI(getHandle(), toUTF8( eArquivoOuIni ) );
    checkResult( ret );
  }

  public void limparListaEventos() throws Exception {
    int ret = ACBrNFeLib.INSTANCE.NFE_LimparListaEventos(getHandle());
    checkResult( ret );
  }

  public void assinar() throws Exception {
    int ret = ACBrNFeLib.INSTANCE.NFE_Assinar(getHandle());
    checkResult( ret );
  }

  public void validar() throws Exception {
    int ret = ACBrNFeLib.INSTANCE.NFE_Validar(getHandle());
    checkResult( ret );
  }
  
  public String salvarPDF() throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
    IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

    int ret = ACBrNFeLib.INSTANCE.NFE_SalvarPDF(getHandle(), buffer, bufferLen);
    checkResult(ret);

    return processResult(buffer, bufferLen);
  }   
  

  public String validarRegrasdeNegocios() throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrNFeLib.INSTANCE.NFE_ValidarRegrasdeNegocios( getHandle(), buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }

  public String verificarAssinatura() throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrNFeLib.INSTANCE.NFE_VerificarAssinatura( getHandle(), buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }
  
  public String gerarChave(int aCodigoUf, int aCodigoNumerico, int aModelo, int aSerie, int aNumero,
            int aTpEmi, Date aEmissao, String acpfcnpj) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );
    
    String pattern = "dd/MM/yyyy";
    DateFormat df = new SimpleDateFormat(pattern);

    int ret = ACBrNFeLib.INSTANCE.NFE_GerarChave( getHandle(), aCodigoUf, aCodigoNumerico, aModelo,
                                                  aSerie, aNumero, aTpEmi, df.format(aEmissao), 
                                                  toUTF8(acpfcnpj),  buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }
  
  public String obterCertificados() throws Exception {
      ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
      IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );
      
      int ret = ACBrNFeLib.INSTANCE.NFE_ObterCertificados(getHandle(), buffer, bufferLen );
      checkResult( ret );
      return processResult( buffer, bufferLen );
  }
  
  public String getPath(TipoPathNFe tipo) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrNFeLib.INSTANCE.NFE_GetPath( getHandle(), tipo.asInt(), buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }
  
  public String getPathEvento(String aCodEvento) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrNFeLib.INSTANCE.NFE_GetPathEvento( getHandle(), toUTF8(aCodEvento), buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }

  public String statusServico() throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrNFeLib.INSTANCE.NFE_StatusServico( getHandle(), buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }
  
  public String consultar( String eChaveOuNFe ) throws Exception {
      return consultar(eChaveOuNFe, false);
  }

  public String consultar( String eChaveOuNFe, boolean AExtrairEventos ) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrNFeLib.INSTANCE.NFE_Consultar( getHandle(), toUTF8( eChaveOuNFe ), AExtrairEventos, buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }

  public String consultarRecibo( String aRecibo ) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrNFeLib.INSTANCE.NFE_ConsultarRecibo( getHandle(), toUTF8( aRecibo ), buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }
  
  public String consultaCadastro( String cUF, String nDocumento, Boolean nIE ) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrNFeLib.INSTANCE.NFE_ConsultaCadastro( getHandle(), toUTF8( cUF ), toUTF8( nDocumento), nIE, buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }

  //TODO: Sobrescrever método com valores default
  public String inutilizar( String aCNPJ, String aJustificativa, int ano, int modelo, int serie,
                            int numeroInicial, int numeroFinal ) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrNFeLib.INSTANCE.NFE_Inutilizar( getHandle(), toUTF8( aCNPJ ), toUTF8( aJustificativa ), ano, modelo, serie,
        numeroInicial, numeroFinal, buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }

  public String enviar( int aLote ) throws Exception {
    return enviar( aLote, false, false, false );
  }

  public String enviar( int aLote, boolean imprimir, boolean sincrono, boolean zipado ) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrNFeLib.INSTANCE.NFE_Enviar( getHandle(), aLote, imprimir, sincrono, zipado, buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }

  public String cancelar( String aChave, String aJustificativa, String aCNPJ ) throws Exception {
    return cancelar( aChave, aJustificativa, aCNPJ, 1 );
  }

  public String cancelar( String aChave, String aJustificativa, String aCNPJ, int aLote ) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrNFeLib.INSTANCE.NFE_Cancelar( getHandle(), toUTF8( aChave ), toUTF8( aJustificativa ), toUTF8( aCNPJ ), aLote, buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }

  public String enviarEvento( int idLote ) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrNFeLib.INSTANCE.NFE_EnviarEvento( getHandle(), idLote, buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }

  public String distribuicaoDFeporUltNSU( int acUFAutor, String aCNPJCPF, String eultNsu ) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrNFeLib.INSTANCE.NFE_DistribuicaoDFePorUltNSU( getHandle(), acUFAutor, toUTF8( aCNPJCPF ), toUTF8( eultNsu ), buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }

  public String distribuicaoDFeporNSU( int acUFAutor, String aCNPJCPF, String aNSU ) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrNFeLib.INSTANCE.NFE_DistribuicaoDFePorNSU( getHandle(), acUFAutor, toUTF8( aCNPJCPF ), toUTF8( aNSU ), buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }

  public String distribuicaoDFeporChave( int acUFAutor, String aCNPJCPF, String aChave ) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrNFeLib.INSTANCE.NFE_DistribuicaoDFePorChave( getHandle(), acUFAutor, aCNPJCPF, aChave, buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }

  public void enviarEmail( String aPara, String aChaveNFe, boolean aEnviarPDF, String aAssunto ) throws Exception {
    enviarEmail( aPara, aChaveNFe, aEnviarPDF, aAssunto, "", "", "" );
  }

  public void enviarEmail( String aPara, String aChaveNFe, boolean aEnviarPDF, String aAssunto,
                           String aEmailCC, String aAnexos, String aMesagem ) throws Exception {
    int ret = ACBrNFeLib.INSTANCE.NFE_EnviarEmail( getHandle(), aPara, aChaveNFe, aEnviarPDF, aAssunto,
        aEmailCC, aAnexos, aMesagem );
    checkResult( ret );
  }

  public void enviarEmailEvento( String aPara, String aChaveEvento, String aChaveNFe, boolean aEnviarPDF,
                                 String aAssunto ) throws Exception {
    enviarEmailEvento( aPara, aChaveEvento, aChaveNFe, aEnviarPDF, aAssunto, "", "", "" );
  }

  public void enviarEmailEvento( String aPara, String aChaveEvento, String aChaveNFe, boolean aEnviarPDF,
                                 String aAssunto, String aEmailCC, String aAnexos, String aMesagem ) throws Exception {
    int ret = ACBrNFeLib.INSTANCE.NFE_EnviarEmailEvento( getHandle(), aPara, aChaveEvento, aChaveNFe, aEnviarPDF,
        aAssunto, aEmailCC, aAnexos, aMesagem );
    checkResult( ret );
  }

  public void imprimir() throws Exception {
    imprimir( "", 1, "", null, null, null, null );
  }

  public void imprimir( String cImpressora, int nNumCopias, String cProtocolo, Boolean bMostrarPreview, Boolean cMarcaDagua,
                        Boolean bViaConsumidor, Boolean bSimplificado ) throws Exception {

    String mostrarPreview = bMostrarPreview != null ? bMostrarPreview ? "1" : "0" : "";
    String marcaDagua = cMarcaDagua != null ? cMarcaDagua ? "1" : "0" : "";
    String viaConsumidor = bViaConsumidor != null ? bViaConsumidor ? "1" : "0" : "";
    String simplificado = bSimplificado != null ? bSimplificado ? "1" : "0" : "";

    int ret = ACBrNFeLib.INSTANCE.NFE_Imprimir( getHandle(), toUTF8(cImpressora ), nNumCopias, toUTF8( cProtocolo ), toUTF8( mostrarPreview ),
        toUTF8( marcaDagua ), toUTF8( viaConsumidor ), toUTF8( simplificado ) );
    checkResult( ret );
  }

  public void imprimirPDF() throws Exception {
    int ret = ACBrNFeLib.INSTANCE.NFE_ImprimirPDF(getHandle());
    checkResult( ret );
  }

  public void imprimirEvento( String eArquivoXmlNFe, String eArquivoXmlEvento ) throws Exception {
    int ret = ACBrNFeLib.INSTANCE.NFE_ImprimirEvento( getHandle(), toUTF8(eArquivoXmlNFe), toUTF8(eArquivoXmlEvento) );
    checkResult( ret );
  }

  public void imprimirEventoPDF( String eArquivoXmlNFe, String eArquivoXmlEvento ) throws Exception {
    int ret = ACBrNFeLib.INSTANCE.NFE_ImprimirEventoPDF( getHandle(), toUTF8(eArquivoXmlNFe), toUTF8(eArquivoXmlEvento)  );
    checkResult( ret );
  }

  public void imprimirInutilizacao( String eArquivoXml ) throws Exception {
    int ret = ACBrNFeLib.INSTANCE.NFE_ImprimirInutilizacao( getHandle(), toUTF8(eArquivoXml) );
    checkResult( ret );
  }

  public void imprimirInutilizacaoPDF( String eArquivoXml ) throws Exception {
    int ret = ACBrNFeLib.INSTANCE.NFE_ImprimirInutilizacaoPDF( getHandle(), toUTF8(eArquivoXml) );
    checkResult( ret );
  }
  
  public void ConfigImportar(String eArqConfig) throws Exception {
        
        int ret = ACBrNFeLib.INSTANCE.NFE_ConfigImportar(getHandle(), eArqConfig);
        checkResult(ret);
        
    }
    
    public String ConfigExportar() throws Exception {
		
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrNFeLib.INSTANCE.NFE_ConfigExportar(getHandle(), buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());
		
    }
  
    @Override
  protected void UltimoRetorno( ByteBuffer buffer, IntByReference bufferLen ) {
    ACBrNFeLib.INSTANCE.NFE_UltimoRetorno(getHandle(), buffer, bufferLen );
  }
  
}