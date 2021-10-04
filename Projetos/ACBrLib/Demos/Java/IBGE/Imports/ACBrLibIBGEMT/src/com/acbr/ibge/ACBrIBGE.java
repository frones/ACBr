package com.acbr.ibge;

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

public final class ACBrIBGE extends ACBrLibBase implements AutoCloseable {
    
  private interface ACBrIBGELib extends Library {
    static String JNA_LIBRARY_NAME = LibraryLoader.getLibraryName();
    public final static ACBrIBGELib INSTANCE = LibraryLoader.getInstance();

    class LibraryLoader {
      private static String library = "";
      private static ACBrIBGELib instance = null;

      private static String getLibraryName() {
        if ( library.isEmpty() ) {
            if(Platform.isWindows()){
          library = Platform.is64Bit() ? "ACBrIBGE64" : "ACBrIBGE32";
        } else {
          library = Platform.is64Bit() ? "acbribge64" : "acbribge32";  
        }
      }
        return library;
      }

      public static ACBrIBGELib getInstance() {
        if ( instance == null ) {
          instance = ( ACBrIBGELib ) Native.synchronizedLibrary(
              ( Library ) Native.loadLibrary( JNA_LIBRARY_NAME, ACBrIBGELib.class ) );
        }
        
        return instance;
      }
    }
    
    int IBGE_Inicializar( PointerByReference libHandler, String eArqConfig, String eChaveCrypt );

    int IBGE_Finalizar(Pointer libHandler);

    int IBGE_Nome( Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize );

    int IBGE_Versao( Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize );

    int IBGE_UltimoRetorno( Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize );

    int IBGE_ConfigImportar(Pointer libHandler, String eArqConfig);
        
    int IBGE_ConfigExportar(Pointer libHandler, ByteBuffer buffer, IntByReference bufferSize);
    
    int IBGE_ConfigLer( Pointer libHandler, String eArqConfig );

    int IBGE_ConfigGravar( Pointer libHandler, String eArqConfig );

    int IBGE_ConfigLerValor( Pointer libHandler, String eSessao, String eChave, ByteBuffer buffer, IntByReference bufferSize );

    int IBGE_ConfigGravarValor( Pointer libHandler, String eSessao, String eChave, String valor );
    
    int IBGE_BuscarPorCodigo( Pointer libHandler, int ACodMun, ByteBuffer buffer, IntByReference bufferSize );
    
    int IBGE_BuscarPorNome( Pointer libHandler, String eCidade, String eUF, boolean exata, ByteBuffer buffer, IntByReference bufferSize );
  }
    
  public ACBrIBGE() throws Exception {
    File iniFile = Paths.get( System.getProperty( "user.dir" ), "ACBrLib.ini" ).toFile();
    if ( !iniFile.exists() ) {
      iniFile.createNewFile();
    }
    
    PointerByReference handle = new PointerByReference();
    int ret = ACBrIBGELib.INSTANCE.IBGE_Inicializar( handle, toUTF8( iniFile.getAbsolutePath() ), toUTF8( "" ) );
    checkResult( ret );
      setHandle(handle.getValue());
    
  }

  public ACBrIBGE( String eArqConfig, String eChaveCrypt ) throws Exception {
    PointerByReference handle = new PointerByReference();
    int ret = ACBrIBGELib.INSTANCE.IBGE_Inicializar( handle, toUTF8( eArqConfig ), toUTF8( eChaveCrypt ) );
    checkResult( ret );
    setHandle(handle.getValue());
  }

  @Override
  public void close() throws Exception {
    int ret = ACBrIBGELib.INSTANCE.IBGE_Finalizar(getHandle());
    checkResult( ret );
  }

  @Override
  protected void finalize() throws Throwable {
    try {
      int ret = ACBrIBGELib.INSTANCE.IBGE_Finalizar(getHandle());
      checkResult( ret );
    }
    finally {
      super.finalize();
    }
  }

  public String nome() throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrIBGELib.INSTANCE.IBGE_Nome( getHandle(), buffer, bufferLen );
    checkResult( ret );

    return fromUTF8( buffer, bufferLen.getValue() );
  }

  public String versao() throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrIBGELib.INSTANCE.IBGE_Versao( getHandle(), buffer, bufferLen );
    checkResult( ret );

    return fromUTF8( buffer, bufferLen.getValue() );
  }

  public void configLer() throws Exception {
    configLer( "" );
  }

  public void configLer( String eArqConfig ) throws Exception {
    int ret = ACBrIBGELib.INSTANCE.IBGE_ConfigLer( getHandle(), toUTF8( eArqConfig ) );
    checkResult( ret );
  }

  public void configGravar() throws Exception {
    configGravar( "" );
  }

  public void configGravar( String eArqConfig ) throws Exception {
    int ret = ACBrIBGELib.INSTANCE.IBGE_ConfigGravar( getHandle(), toUTF8( eArqConfig ) );
    checkResult( ret );
  }

  public String configLerValor( ACBrSessao eSessao, String eChave ) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrIBGELib.INSTANCE.IBGE_ConfigLerValor( getHandle(), toUTF8( eSessao.name() ), toUTF8( eChave ), buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }

  public void configGravarValor( ACBrSessao eSessao, String eChave, Object value ) throws Exception {
    int ret = ACBrIBGELib.INSTANCE.IBGE_ConfigGravarValor( getHandle(), toUTF8( eSessao.name() ), toUTF8( eChave ), toUTF8( value.toString() ) );
    checkResult( ret );
  }
  
  public String buscarPorCodigo(int ACodMun) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrIBGELib.INSTANCE.IBGE_BuscarPorCodigo(getHandle(), ACodMun, buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }
  
  public String buscarPorNome( String eCidade, String eUF) throws Exception {
    return buscarPorNome( eCidade, eUF, false );
  }

  public String buscarPorNome( String eCidade, String eUF, boolean exata ) throws Exception {
    ByteBuffer buffer = ByteBuffer.allocate( STR_BUFFER_LEN );
    IntByReference bufferLen = new IntByReference( STR_BUFFER_LEN );

    int ret = ACBrIBGELib.INSTANCE.IBGE_BuscarPorNome(getHandle(), toUTF8(eCidade) , toUTF8(eUF), exata, buffer, bufferLen );
    checkResult( ret );

    return processResult( buffer, bufferLen );
  }

  public void ConfigImportar(String eArqConfig) throws Exception {
        
        int ret = ACBrIBGELib.INSTANCE.IBGE_ConfigImportar(getHandle(), eArqConfig);
        checkResult(ret);
        
    }
    
    public String ConfigExportar() throws Exception {
		
        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        int ret = ACBrIBGELib.INSTANCE.IBGE_ConfigExportar(getHandle(), buffer, bufferLen);
        checkResult(ret);

        return fromUTF8(buffer, bufferLen.getValue());
		
    }
  
    @Override
  protected void UltimoRetorno( ByteBuffer buffer, IntByReference bufferLen ) {
    ACBrIBGELib.INSTANCE.IBGE_UltimoRetorno( getHandle(), buffer, bufferLen );
  }
}