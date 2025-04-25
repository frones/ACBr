import 'dart:ffi';
import 'dart:io';

import 'package:demo_acbrlib_posprinter/acbrposprinter/acbrlib_base.dart';
import 'package:demo_acbrlib_posprinter/acbrposprinter/brigde/acbrlib_posprinter_bridge.dart';
import 'package:ffi/ffi.dart';

class ACBrLibPosPrinter implements ACBrLibBase {
  late String name;
  late String arquivoConfig = "";
  late String chaveCrypt = "";
  late ACBrLibPosPrinterBridge _posPrinterBridge;
  late Pointer<Pointer<Void>> _handle;


  ACBrLibPosPrinter(String configFile, String cryptKey) {
    this._handle = calloc<Pointer<Void>>();
    this.arquivoConfig = configFile;
    this.chaveCrypt = cryptKey;
    this._handle.value = nullptr;
    this._posPrinterBridge = ACBrLibPosPrinterBridge(_getNameByPlatform());

  }

  String _getNameByPlatform(){
    String platform = "";
    //se Linux
    if (Platform.isLinux) {
      platform =  "libacbrposprinter64.so";
    }else if (Platform.isWindows) {
      platform = "ACBrLibPosPrinter64.dll";
    }else if ( Platform.isAndroid) {
      platform = "libACBrLibPosPrinter.so";
    }else {
      throw UnsupportedError("Plataforma não suportada");
    }
    return platform;
  }

  int inicializar() {
    return  _posPrinterBridge.POS_Inicializar(
      _handle,
      arquivoConfig.toNativeUtf8(),
      chaveCrypt.toNativeUtf8(),
    );
  }

  int ativar() {
    int status = _posPrinterBridge.POS_Ativar(_handle.value);
    return status;
  }

  @override
  int finalizar() {
   return  _posPrinterBridge.POS_Finalizar(_handle.value);
  }

  int desativar() {
    return _posPrinterBridge.POS_Desativar(_handle.value);
  }

  @override
  int configGravar([String? arquivoConfig]) {
    return _posPrinterBridge.POS_ConfigGravar(
      _handle.value,
      arquivoConfig  == null ? this.arquivoConfig.toNativeUtf8() : arquivoConfig.toNativeUtf8(),
    );
  }

  @override
  int configGravarValor(String secao, String chave, String valor) {
    return _posPrinterBridge.POS_ConfigGravarValor(
      _handle.value,
      secao.toNativeUtf8(),
      chave.toNativeUtf8(),
      valor.toNativeUtf8(),
    );
  }

  @override
  int configLer(String arquivoConfig) {
    return _posPrinterBridge.POS_ConfigLer(
      _handle.value,
      arquivoConfig.toNativeUtf8(),
    );
  }

  int imprimir(String texto,  int pularLinha, int decodificarTag, int codificarPagina, int copias){
    return _posPrinterBridge.POS_Imprimir(
      _handle.value,
      texto.toNativeUtf8(),
      pularLinha,
      decodificarTag,
      codificarPagina,
      copias
    );
  }

  @override
  String configLerValor(String secao, String chave) {
    Pointer<Int32> tamanho = calloc<Int32>();
    tamanho.value = 1024;
    Pointer<Utf8> retorno = malloc.allocate<Utf8>(1024);
    int status = _posPrinterBridge.POS_ConfigLerValor(
      _handle.value,
      secao.toNativeUtf8(),
      chave.toNativeUtf8(),
      retorno,
      tamanho,
    );
    String resultado = retorno.toDartString();
    malloc.free(retorno);
    calloc.free(tamanho);
    return resultado;
  }

  @override
  String nome() {
    Pointer<Int32> tamanho = calloc<Int32>();
    tamanho.value = 1024;
    Pointer<Utf8> retorno = malloc.allocate<Utf8>(1024);
    int status = _posPrinterBridge.POS_Nome(
      _handle.value,
      retorno,
      tamanho,
    );
    String resultado = retorno.toDartString();
    malloc.free(retorno);
    calloc.free(tamanho);
    return resultado;
  }

  @override
  String ultimoRetorno() {
    Pointer<Int32> tamanho = calloc<Int32>();
    tamanho.value = 1024;
    Pointer<Utf8> retorno = malloc.allocate<Utf8>(1024);
    int status = _posPrinterBridge.POS_UltimoRetorno(
      _handle.value,
      retorno,
      tamanho,
    );
    String resultado = retorno.toDartString();
    malloc.free(retorno);
    calloc.free(tamanho);
    return resultado;
  }

  @override
  String versao() {
    Pointer<Int32> tamanho = calloc<Int32>();
    tamanho.value = 1024;
    Pointer<Utf8> retorno = malloc.allocate<Utf8>(1024);
    int status = _posPrinterBridge.POS_Versao(
      _handle.value,
      retorno,
      tamanho,
    );
    String resultado = retorno.toDartString();
    malloc.free(retorno);
    calloc.free(tamanho);
    return resultado;
  }

}
