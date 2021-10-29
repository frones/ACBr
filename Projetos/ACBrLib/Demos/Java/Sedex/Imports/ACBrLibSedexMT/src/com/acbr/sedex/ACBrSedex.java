package com.acbr.sedex;

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

public final class ACBrSedex extends ACBrLibBase implements AutoCloseable {
    
  private interface ACBrSedexLib extends Library {
    static String JNA_LIBRARY_NAME = LibraryLoader.getLibraryName();
    public final static ACBrSedexLib INSTANCE = LibraryLoader.getInstance();

    int Sedex_Inicializar( PointerByReference libHandler, String eArqConfig, String eChaveCrypt );

    int Sedex_Finalizar(Pointer libHandler);

    int Sedex_Nome( Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize );

    int Sedex_Versao( Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize );

    int Sedex_UltimoRetorno( Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize );

    int Sedex_ConfigImportar(Pointer libHandler, String eArqConfig);
        
    int Sedex_ConfigExportar(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize);
    
    int Sedex_ConfigLer( Pointer libHandler, String eArqConfig );

    int Sedex_ConfigGravar( Pointer libHandler, String eArqConfig );

    int Sedex_ConfigLerValor( Pointer libHandler, String eSessao, String eChave, ByteBuffer buffer, IntByReference bufferSize );

    int Sedex_ConfigGravarValor( Pointer libHandler, String eSessao, String eChave, String valor );
    
    int Sedex_Consultar( Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize );
    
    int Sedex_Rastrear( Pointer libHandler, String eCodRastreio, ByteBuffer buffer, IntByReference bufferSize );
    
    
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
    
    PointerByReference handle = new PointerByReference();
    int ret = ACBrSedexLib.INSTANCE.Sedex_Inicializar( handle, toUTF8( iniFile.getAbsolutePath() ), toUTF8( "" ) );
    checkResult( ret );
    setHandle(handle.getValue());
  }

  public ACBrSedex( String eArqConfig, String eChaveCrypt ) throws Exception {
    PointerByReference handle = new PointerByReference();  
    int ret = ACBrSedexLib.INSTANCE.Sedex_Inicializar( handle, toUTF8( eArqConfig ), toUTF8( eChaveCrypt ) );
    checkResult( ret );
      setHandle(handle.getValue());
  }

  @Override
  public void close() throws Exception {
    int ret = ACBrSedexLib.INSTANCE.Sedex_Finalizar(getHandle());
    checkResult( ret );
  }

  @Override
  protected void finalize() throws Throwable {
    try {
      int ret = ACBrSedexLib.INSTANCE.Sedex_Finalizar(getHandle());
      checkResult( ret );
    }
    finally {
      super.finalize();
    }
  }

  public String nome() throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrSedexLib.INSTANCE.Sedex_Nome(getHandle(), buffer, bufferLen );
    checkResult( ret );

    return fromUTF8( buffer, bufferLen.getValue() );
  }

  public String versao() throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrSedexLib.INSTANCE.Sedex_Versao( getHandle(), buffer, bufferLen );
    checkResult( ret );

    return fromUTF8( buffer, bufferLen.getValue() );
  }

  public void configLer() throws Exception {
    configLer( "" );
  }

  public void configLer( String eArqConfig ) throws Exception {
    int ret = ACBrSedexLib.INSTANCE.Sedex_ConfigLer( getHandle(), toUTF8( eArqConfig ) );
    checkResult( ret );
  }

  public void configGravar() throws Exception {
    configGravar( "" );
  }

  public void configGravar( String eArqConfig ) throws Exception {
    int ret = ACBrSedexLib.INSTANCE.Sedex_ConfigGravar( getHandle(), toUTF8( eArqConfig ) );
    checkResult( ret );
  }

  @Override
  public String configLerValor( ACBrSessao eSessao, String eChave ) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrSedexLib.INSTANCE.Sedex_ConfigLerValor( getHandle(), toUTF8( eSessao.name() ), toUTF8( eChave ), buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }

  @Override
  public void configGravarValor( ACBrSessao eSessao, String eChave, Object value ) throws Exception {
    int ret = ACBrSedexLib.INSTANCE.Sedex_ConfigGravarValor( getHandle(), toUTF8( eSessao.name() ), toUTF8( eChave ), toUTF8( value.toString() ) );
    checkResult( ret );
  }
  
  public String consultar() throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrSedexLib.INSTANCE.Sedex_Consultar(getHandle(), buffer, bufferLen);
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }
  
  public String Rastrear( String eCodRastreio) throws Exception {
    return rastrear( eCodRastreio );
  }

  public String rastrear( String eCodRastreio ) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrSedexLib.INSTANCE.Sedex_Rastrear(getHandle(), toUTF8(eCodRastreio), buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }

  public void ConfigImportar(String eArqConfig) throws Exception {
        
        int ret = ACBrSedexLib.INSTANCE.Sedex_ConfigImportar(getHandle(), eArqConfig);
        checkResult(ret);
        
    }
    
    public String ConfigExportar() throws Exception {
		
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrSedexLib.INSTANCE.Sedex_ConfigExportar(getHandle(), buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());
		
    }
  
    @Override
  protected void UltimoRetorno( ByteBuffer buffer, IntByReference bufferLen ) {
    ACBrSedexLib.INSTANCE.Sedex_UltimoRetorno( getHandle(), buffer, bufferLen );
  }
}