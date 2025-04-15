import 'package:flutter/cupertino.dart';
import 'package:flutter/services.dart';

class ACBrCepAarPlugin {
  static const MethodChannel _channel =
      MethodChannel('com.example.demo_acbrcep_flutter');

  Future<String> buscarPorCep(cep) async {
    final String result =
        await _channel.invokeMethod('buscarPorCep', {'cep': cep});
    return result;
  }

  Future<String> inicializar() async {
    final String result = await _channel.invokeMethod('inicializar');
    return result;
  }

  Future<String> finalizar() async {
    final String result = await _channel.invokeMethod('finalizar');
    return result;
  }

  Future<int> configGravar() async {
    final int result = await _channel.invokeMethod('configGravar');
    return result;
  }

  Future<int> configLer(String eArqConfig) async {
    final int result =
        await _channel.invokeMethod('configLer', {"eArqConfig": eArqConfig});
    return result;
  }

  Future<int> configGravarValor(
      String sessao, String chave, String valor) async {
    final int result = await _channel.invokeMethod('configGravarValor',
        {'sessao': sessao, 'chave': chave, 'valor': valor});
    return result;
  }

  Future<String> configLerValor(String sessao, String chave) async {
    final String result = await _channel
        .invokeMethod('configLerValor', {'sessao': sessao, 'chave': chave});
    return result;
  }
}
