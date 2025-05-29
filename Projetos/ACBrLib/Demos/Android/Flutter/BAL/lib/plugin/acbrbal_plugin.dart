import 'package:flutter/services.dart';

/// [AcbrbalPlugin]
///
/// Este plugin Flutter atua como uma ponte para a biblioteca nativa [ACBrLibBAL]
/// Ele facilita a comunicação entre o código Dart/Flutter e as funcionalidades
/// expostas pelo SDK nativo Android (AAR).
///
/// **Dependências Nativas**
/// - Android: `app/libs/ACBrLibBAL-release.aar`
///
/// **Como Usar:**
/// 1. Instancie a classe: `final acbrBalPlugin = AcbrbalPlugin();`
/// 2. Chame os métodos: `final resultado = await acbrBalPlugin.inicializar();`
///
class AcbrbalPlugin {
  /// Variável usada para indicar erro
  static const  double errorPeso = -9.00;

  /// O MethodChannel é a ponte de comunicação com o código nativo.
  // O nome do canal deve ser único e o mesmo usado no código nativo.
  static const MethodChannel _channel =
      MethodChannel('acbrlib_bal_flutter');

  /// Método usado para inicializar o componente para uso da biblioteca.
  Future<int> inicializar() async {
    try {
      final int result = await _channel.invokeMethod('inicializar');
      return result;
    } on PlatformException catch (e) {
      print("Erro: '${e.message}'.");
      return -1;
    }
  }

  /// Método usado para remover o componente ACBrBAL e suas classes da memoria.
  Future<int> finalizar() async {
    try {
      final int result = await _channel.invokeMethod('finalizar');
      return result;
    } on PlatformException catch (e) {
      print("Erro: '${e.message}'.");
      return -1;
    }
  }

  /// Método usado para gravar a configuração da biblioteca no arquivo INI informado.
  Future<int> configGravar() async {
    try {
      final int result = await _channel.invokeMethod('configGravar');
      return result;
    } on PlatformException catch (e) {
      print("Erro: '${e.message}'.");
      return -1;
    }
  }

  /// Método usado para ler a configuração da biblioteca do arquivo INI informado.
  ///
  /// - [eArqConfig] Arquivo INI para ler, se informado vazio será usado o valor padrão.
  ///
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

  /// Método usado para gravar um determinado item da configuração.
  ///
  /// - [sessao] Nome da sessão de configuração.
  /// - [chave] Nome da chave da sessão.
  /// - [valor] Valor para ser gravado na configuração lembrando que o mesmo deve ser um valor string compatível com a configuração.
  ///
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

  /// Método usado para ler um determinado item da configuração.
  ///
  /// - [sessao] Nome da sessão de configuração.
  /// - [chave] Nome da chave da sessão.
  ///
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

  /// Método usado para ativar o componente ACBrBAL.
  Future<bool> ativar() async {
    try {
      final bool result = await _channel.invokeMethod('ativar');
      return result;
    } on PlatformException catch (e) {
      print("Erro: '${e.message}'.");
      return false;
    }
  }

  /// Método usado para desativar o componente ACBrBAL.
  Future<bool> desativar() async {
    try {
      final bool result = await _channel.invokeMethod('desativar');
      return result;
    } on PlatformException catch (e) {
      print("Erro: '${e.message}'.");
      return false;
    }
  }

  /// Método usado para solicitar o peso da balança no componente ACBrBAL.
  Future<void> solicitarPeso() async {
    try {
      await _channel.invokeMethod('solicitarPeso');
    } on PlatformException catch (e) {
      print("Erro: '${e.message}'.");
    }
  }

  /// Método usado para ler o peso na balança no componente ACBrBAL.
  ///
  /// - [millisecTimeOut] Define o timeout em milissegundos.
  ///
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

  /// Método usado para ler o peso na balança no componente ACBrBAL.
  ///
  /// - [millisecTimeOut] Define o timeout em milissegundos.
  ///
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

  /// Método usado para ler o peso na balança no componente ACBrBAL.
  Future<double> ultimoPesoLido() async {
    try {
      final double result = await _channel.invokeMethod('ultimoPesoLido');
      return result;
    } on PlatformException catch (e) {
      print("Erro: '${e.message}'.");
      return errorPeso;
    }
  }

  /// Método usado para ler o peso na balança no componente ACBrBAL.
  Future<String> ultimoPesoLidoStr() async {
    try {
      final String result = await _channel.invokeMethod('ultimoPesoLidoStr');
      return result;
    } on PlatformException catch (e) {
      print("Erro: '${e.message}'.");
      return  errorPeso.toString();
    }
  }

  /// Método usado para interpretar uma resposta da balança do componente ACBrBAL.
  ///
  /// - [aResposta] Contem a resposta da balança a ser interpretada.
  ///
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

  /// Método usado para interpretar uma resposta da balança do componente ACBrBAL.
  ///
  /// - [aResposta] Contem a resposta da balança a ser interpretada.
  ///
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


  /// Método usado para exportar a configuração da biblioteca do arquivo INI informado.
  Future<String> configExportar() async {
    return await _channel.invokeMethod('configExportar');
  }
}
