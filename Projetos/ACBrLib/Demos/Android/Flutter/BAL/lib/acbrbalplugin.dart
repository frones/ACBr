import 'package:flutter/services.dart';

class Acbrbalplugin {
  static const  double errorPeso = -9.00;
  static const MethodChannel _channel =
      MethodChannel('acbrlib_bal_flutter');

  Future<int> inicializar() async {
    try {
      final int result = await _channel.invokeMethod('inicializar');
      return result;
    } on PlatformException catch (e) {
      print("Erro: '${e.message}'.");
      return -1;
    }
  }

  Future<int> finalizar() async {
    try {
      final int result = await _channel.invokeMethod('finalizar');
      return result;
    } on PlatformException catch (e) {
      print("Erro: '${e.message}'.");
      return -1;
    }
  }

  Future<int> configGravar() async {
    try {
      final int result = await _channel.invokeMethod('configGravar');
      return result;
    } on PlatformException catch (e) {
      print("Erro: '${e.message}'.");
      return -1;
    }
  }

  Future<int> configLer(String eArqConfig) async {
    try {
      final int result =
          await _channel.invokeMethod('configLer', {"eArqConfig": eArqConfig});
      return result;
    } on PlatformException catch (e) {
      print("Erro: '${e.message}'.");
      return -1;
    }
  }

  Future<int> configGravarValor(
      String sessao, String chave, String valor) async {
    try {
      final int result = await _channel.invokeMethod('configGravarValor',
          {'sessao': sessao, 'chave': chave, 'valor': valor});
      return result;
    } on PlatformException catch (e) {
      print("Erro: '${e.message}'.");
      return -1;
    }
  }

  Future<String> configLerValor(String sessao, String chave) async {
    try {
      final String result = await _channel
          .invokeMethod('configLerValor', {'sessao': sessao, 'chave': chave});
      return result;
    } on PlatformException catch (e) {
      print("Erro: '${e.message}'.");
      return "";
    }
  }

  Future<bool> ativar() async {
    try {
      final bool result = await _channel.invokeMethod('ativar');
      return result;
    } on PlatformException catch (e) {
      print("Erro: '${e.message}'.");
      return false;
    }
  }

  Future<bool> desativar() async {
    try {
      final bool result = await _channel.invokeMethod('desativar');
      return result;
    } on PlatformException catch (e) {
      print("Erro: '${e.message}'.");
      return false;
    }
  }

  Future<void> solicitarPeso() async {
    try {
      await _channel.invokeMethod('solicitarPeso');
    } on PlatformException catch (e) {
      print("Erro: '${e.message}'.");
    }
  }

  Future<double> lePeso(int millisecTimeOut) async {
    try {
      final double result = await _channel
          .invokeMethod('lePeso', {"millisecTimeOut": millisecTimeOut});
      return result;
    } on PlatformException catch (e) {
      print("Erro: '${e.message}'.");
      return errorPeso;
    }
  }

  Future<String> lePesoStr(int millisecTimeOut) async {
    try {
      final String result = await _channel
          .invokeMethod('lePesoStr', {"millisecTimeOut": millisecTimeOut});
      return result;
    } on PlatformException catch (e) {
      print("Erro: '${e.message}'.");
      return errorPeso.toString() ;
    }
  }

  Future<double> ultimoPesoLido() async {
    try {
      final double result = await _channel.invokeMethod('ultimoPesoLido');
      return result;
    } on PlatformException catch (e) {
      print("Erro: '${e.message}'.");
      return errorPeso;
    }
  }

  Future<String> ultimoPesoLidoStr() async {
    try {
      final String result = await _channel.invokeMethod('ultimoPesoLidoStr');
      return result;
    } on PlatformException catch (e) {
      print("Erro: '${e.message}'.");
      return  errorPeso.toString();
    }
  }

  Future<double> interpretarRespostaPeso(String aResposta) async {
    try {
      final double result = await _channel
          .invokeMethod('interpretarRespostaPeso', {"aResposta": aResposta});
      return result;
    } on PlatformException catch (e) {
      print("Erro: '${e.message}'.");
      return errorPeso;
    }
  }

  Future<String> interpretarRespostaPesoStr(String aResposta) async {
    try {
      final String result = await _channel
          .invokeMethod('interpretarRespostaPesoStr', {"aResposta": aResposta});
      return result;
    } on PlatformException catch (e) {
      print("Erro: '${e.message}'.");
      return errorPeso.toString();
    }
  }
}
