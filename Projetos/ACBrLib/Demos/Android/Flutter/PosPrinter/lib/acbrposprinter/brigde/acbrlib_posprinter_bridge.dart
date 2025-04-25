import 'dart:ffi';
import 'package:ffi/ffi.dart';

class ACBrLibPosPrinterBridge {
  final DynamicLibrary _nativeLib;

  ACBrLibPosPrinterBridge(String libraryPath)
      : _nativeLib = DynamicLibrary.open(libraryPath);

// Método para inicializar a biblioteca
  late final POS_Inicializar = _nativeLib.lookupFunction<
      Int32 Function(Pointer<Pointer<Void>>, Pointer<Utf8>, Pointer<Utf8>),
      int Function(Pointer<Pointer<Void>>, Pointer<Utf8>, Pointer<Utf8>)>(
    'POS_Inicializar',
  );

// Método para obter o último retorno
  late final POS_UltimoRetorno = _nativeLib.lookupFunction<
      Int32 Function(Pointer<Void>, Pointer<Utf8>, Pointer<Int32>),
      int Function(Pointer<Void>, Pointer<Utf8>, Pointer<Int32>)>(
    'POS_UltimoRetorno',
  );

  late final POS_Finalizar = _nativeLib.lookupFunction<
      Int32 Function(Pointer<Void>), int Function(Pointer<Void>)>(
    'POS_Finalizar',
  );

  late final POS_Ativar = _nativeLib.lookupFunction<
      Int32 Function(Pointer<Void>), int Function(Pointer<Void>)>(
    'POS_Ativar',
  );

  late final POS_Desativar = _nativeLib.lookupFunction<
      Int32 Function(Pointer<Void>), int Function(Pointer<Void>)>(
    'POS_Desativar',
  );

  late final POS_ConfigGravarValor = _nativeLib.lookupFunction<
      Int32 Function(
          Pointer<Void>, Pointer<Utf8>, Pointer<Utf8>, Pointer<Utf8>),
      int Function(Pointer<Void>, Pointer<Utf8>, Pointer<Utf8>, Pointer<Utf8>)>(
    'POS_ConfigGravarValor',
  );

  late final POS_ConfigLer = _nativeLib.lookupFunction<
      Int32 Function(Pointer<Void>, Pointer<Utf8>),
      int Function(Pointer<Void>, Pointer<Utf8>)>(
    'POS_ConfigLer',
  );

  late final POS_ConfigGravar = _nativeLib.lookupFunction<
      Int32 Function(Pointer<Void>, Pointer<Utf8>),
      int Function(Pointer<Void>, Pointer<Utf8>)>(
    'POS_ConfigGravar',
  );

  late final POS_Imprimir = _nativeLib.lookupFunction<
      Int32 Function(Pointer<Void>, Pointer<Utf8>, Int32, Int32, Int32, Int32),
      int Function(Pointer<Void>, Pointer<Utf8>, int, int, int, int)>(
    'POS_Imprimir',
  );

  late final POS_ConfigLerValor = _nativeLib.lookupFunction<
      Int32 Function(Pointer<Void>, Pointer<Utf8>, Pointer<Utf8>, Pointer<Utf8>,
          Pointer<Int32>),
      int Function(Pointer<Void>, Pointer<Utf8>, Pointer<Utf8>, Pointer<Utf8>,
          Pointer<Int32>)>(
    'POS_ConfigLerValor',
  );

  late final POS_Versao = _nativeLib.lookupFunction<
      Int32 Function(Pointer<Void>, Pointer<Utf8>, Pointer<Int32>),
      int Function(Pointer<Void>, Pointer<Utf8>, Pointer<Int32>)>(
    'POS_Versao',
  );

  late final POS_Nome = _nativeLib.lookupFunction<
      Int32 Function(Pointer<Void>, Pointer<Utf8>, Pointer<Int32>),
      int Function(Pointer<Void>, Pointer<Utf8>, Pointer<Int32>)>('POS_Nome');
}
