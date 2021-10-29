package com.acbr.sedex;

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

public final class ACBrSedex extends ACBrLibBase implements AutoCloseable {
    
  private interface ACBrSedexLib extends Library {
    static String JNA_LIBRARY_NAME = LibraryLoader.getLibraryName();
    public final static ACBrSedexLib INSTANCE = LibraryLoader.getInstance();

    int Sedex_Inicializar( String eArqConfig, String eChaveCrypt );

    int Sedex_Finalizar();

    int Sedex_Nome( ByteBuffer buffer, IntByReference bufferSize );

    int Sedex_Versao( ByteBuffer buffer, IntByReference bufferSize );

    int Sedex_UltimoRetorno( ByteBuffer buffer, IntByReference bufferSize );

    int Sedex_ConfigImportar(String eArqConfig);
        
    int Sedex_ConfigExportar(ByteBuffer buffer, IntByReference bufferSize);
    
    int Sedex_ConfigLer( String eArqConfig );

    int Sedex_ConfigGravar( String eArqConfig );

    int Sedex_ConfigLerValor( String eSessao, String eChave, ByteBuffer buffer, IntByReference bufferSize );

    int Sedex_ConfigGravarValor( String eSessao, String eChave, String valor );
    
    int Sedex_Consultar( ByteBuffer buffer, IntByReference bufferSize );
    
    int Sedex_Rastrear( String eCodRastreio, ByteBuffer buffer, IntByReference bufferSize );
    
    
    class LibraryLoader {
      private static String library = "";
      private static ACBrSedexLib instance = null;

      private static String getLibraryName() {
        if ( library.isEmpty() ) {
            if(Platform.isWindows()){
          library = Platform.is64Bit() ? "ACBrSedex64" : "ACBrSedex32";
        } else {
          library = Platform.is64Bit() ? "acbrsedex64" : "acbrsedex32";  
        }
      }
        return library;
      }

      public static ACBrSedexLib getInstance() {
        if ( instance == null ) {
          instance = ( ACBrSedexLib ) Native.synchronizedLibrary(
              ( Library ) Native.loadLibrary( JNA_LIBRARY_NAME, ACBrSedexLib.class ) );
        }
        
        return instance;
      }
    }
  }
    
  public ACBrSedex() throws Exception {
    File iniFile = Paths.get( System.getProperty( "user.dir" ), "ACBrLib.ini" ).toFile();
    if ( !iniFile.exists() ) {
      iniFile.createNewFile();
    }

    int ret = ACBrSedexLib.INSTANCE.Sedex_Inicializar( toUTF8( iniFile.getAbsolutePath() ), toUTF8( "" ) );
    checkResult( ret );
  }

  public ACBrSedex( String eArqConfig, String eChaveCrypt ) throws Exception {
    int ret = ACBrSedexLib.INSTANCE.Sedex_Inicializar( toUTF8( eArqConfig ), toUTF8( eChaveCrypt ) );
    checkResult( ret );
  }

  @Override
  public void close() throws Exception {
    int ret = ACBrSedexLib.INSTANCE.Sedex_Finalizar();
    checkResult( ret );
  }

  @Override
  protected void finalize() throws Throwable {
    try {
      int ret = ACBrSedexLib.INSTANCE.Sedex_Finalizar();
      checkResult( ret );
    }
    finally {
      super.finalize();
    }
  }

  public String nome() throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrSedexLib.INSTANCE.Sedex_Nome( buffer, bufferLen );
    checkResult( ret );

    return fromUTF8( buffer, bufferLen.getValue() );
  }

  public String versao() throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrSedexLib.INSTANCE.Sedex_Versao( buffer, bufferLen );
    checkResult( ret );

    return fromUTF8( buffer, bufferLen.getValue() );
  }

  public void configLer() throws Exception {
    configLer( "" );
  }

  public void configLer( String eArqConfig ) throws Exception {
    int ret = ACBrSedexLib.INSTANCE.Sedex_ConfigLer( toUTF8( eArqConfig ) );
    checkResult( ret );
  }

  public void configGravar() throws Exception {
    configGravar( "" );
  }

  public void configGravar( String eArqConfig ) throws Exception {
    int ret = ACBrSedexLib.INSTANCE.Sedex_ConfigGravar( toUTF8( eArqConfig ) );
    checkResult( ret );
  }

  @Override
  public String configLerValor( ACBrSessao eSessao, String eChave ) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrSedexLib.INSTANCE.Sedex_ConfigLerValor( toUTF8( eSessao.name() ), toUTF8( eChave ), buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }

  @Override
  public void configGravarValor( ACBrSessao eSessao, String eChave, Object value ) throws Exception {
    int ret = ACBrSedexLib.INSTANCE.Sedex_ConfigGravarValor( toUTF8( eSessao.name() ), toUTF8( eChave ), toUTF8( value.toString() ) );
    checkResult( ret );
  }
  
  public String consultar() throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrSedexLib.INSTANCE.Sedex_Consultar(buffer, bufferLen);
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }
  
  public String Rastrear( String eCodRastreio) throws Exception {
    return rastrear( eCodRastreio );
  }

  public String rastrear( String eCodRastreio ) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrSedexLib.INSTANCE.Sedex_Rastrear(toUTF8(eCodRastreio), buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }

  public void ConfigImportar(String eArqConfig) throws Exception {
        
        int ret = ACBrSedexLib.INSTANCE.Sedex_ConfigImportar(eArqConfig);
        checkResult(ret);
        
    }
    
    public String ConfigExportar() throws Exception {
		
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrSedexLib.INSTANCE.Sedex_ConfigExportar(buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());
		
    }
  
    @Override
  protected void UltimoRetorno( ByteBuffer buffer, IntByReference bufferLen ) {
    ACBrSedexLib.INSTANCE.Sedex_UltimoRetorno( buffer, bufferLen );
  }
}