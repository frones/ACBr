import 'package:flutter/services.dart';

class ACBrNFePlugin {
  static const MethodChannel _channel =
      MethodChannel('br.com.projetoacbr.example.acbrlib.nfe.acbrlibnfe');

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

  Future<int> configGravar() async {
    try {
      final int result = await _channel.invokeMethod('configGravar');
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

  Future<int> configGravarValor(String sessao, String chave, String valor) async {
    try {
      final int result = await _channel.invokeMethod('configGravarValor',
          {'sessao': sessao, 'chave': chave, 'valor': valor});
      return result;
    } on PlatformException catch (e) {
      print("Erro: '${e.message}'.");
      return -1;
    }
  }

  Future<int> configImportar(String arquivoConfig) async {
    try {
      final int result = await _channel
          .invokeMethod('configImportar', {"arquivoConfig": arquivoConfig});
      return result;
    } on PlatformException catch (e) {
      print("Erro: '${e.message}'.");
      return -1;
    }
  }

  Future<String> configExportar() async {
    try {
      final String result = await _channel.invokeMethod('configExportar');
      return result;
    } on PlatformException catch (e) {
      print("Erro: '${e.message}'.");
      return "";
    }
  }

  Future<String> nome() async {
    try {
      final String result = await _channel.invokeMethod('nome');
      return result;
    } on PlatformException catch (e) {
      print("Erro: '${e.message}'.");
      return "";
    }
  }

  Future<String> versao() async {
    try {
      final String result = await _channel.invokeMethod('versao');
      return result;
    } on PlatformException catch (e) {
      print("Erro: '${e.message}'.");
      return "";
    }
  }

  Future<String> openSslInfo() async {
    try {
      final String result = await _channel.invokeMethod('openSSLInfo');
      return result;
    } on PlatformException catch (e) {
      print("Erro: '${e.message}'.");
      return "";
    }
  }

  Future<String> carregarXML(eArquivoOuXML) async {
    final String result =
    await _channel.invokeMethod('carregarXML', {'eArquivoOuXML': eArquivoOuXML});
    return result;
  }

  Future<String> carregarINI(eArquivoOuINI) async {
    final String result =
    await _channel.invokeMethod('carregarINI', {'eArquivoOuINI': eArquivoOuINI});
    return result;
  }

  Future<int> obterXML(AIndex) async {
    final int result =
    await _channel.invokeMethod('obterXML', {'AIndex': AIndex});
    return result;
  }

  Future<String> gravarXML(AIndex, eNomeArquivo, ePathArquivo) async {
    final String result =
    await _channel.invokeMethod('gravarXML', {'AIndex': AIndex, 'eNomeArquivo': eNomeArquivo, 'ePathArquivo': ePathArquivo});
    return result;
  }

  Future<String> obterINI(AIndex) async {
    final String result =
    await _channel.invokeMethod('obterINI', {'AIndex': AIndex});
    return result;
  }

  Future<String> gravarINI(AIndex, eNomeArquivo, ePathArquivo) async {
    final String result =
    await _channel.invokeMethod('gravarINI', {'AIndex': AIndex, 'eNomeArquivo': eNomeArquivo, 'ePathArquivo': ePathArquivo});
    return result;
  }

  Future<String> carregarEventoXML(eArquivoOuXML) async {
    final String result =
    await _channel.invokeMethod('carregarEventoXML', {'eArquivoOuXML': eArquivoOuXML});
    return result;
  }

  Future<String> carregarEventoINI(eArquivoOuINI) async {
    final String result =
    await _channel.invokeMethod('carregarEventoINI', {'eArquivoOuINI': eArquivoOuINI});
    return result;
  }

  Future<String> limparLista() async {
    final String result = await _channel.invokeMethod('limparLista');
    return result;
  }

  Future<String> limparListaEventos() async {
    final String result = await _channel.invokeMethod('limparListaEventos');
    return result;
  }

  Future<String> validar() async {
    final String result = await _channel.invokeMethod('validar');
    return result;
  }

  Future<String> validarRegrasdeNegocios() async {
    final String result = await _channel.invokeMethod('validarRegrasdeNegocios');
    return result;
  }

  Future<String> verificarAssinatura() async {
    final String result = await _channel.invokeMethod('verificarAssinatura');
    return result;
  }

  Future<String> gerarChave(ACodigoUF, ACodigoNumerico, AModelo, ASerie, ANumero, ATpEmi, AEmissao, ACNPJCPF) async {
    final String result =
    await _channel.invokeMethod('gerarChave', {'ACodigoUF': ACodigoUF, 'ACodigoNumerico': ACodigoNumerico, 'AModelo': AModelo, 'ASerie': ASerie, 'ANumero': ANumero, 'ATpEmi': ATpEmi, 'AEmissao': AEmissao});
    return result;
  }

  Future<String> obterCertificados() async {
    final String result = await _channel.invokeMethod('obterCertificados');
    return result;
  }

  Future<String> getPath(ATipo) async {
    final String result = await _channel.invokeMethod('getPath', {'ATipo': ATipo});
    return result;
  }

  Future<String> getPathEvento(ACodEvento) async {
    final String result = await _channel.invokeMethod('getPathEvento', {'ACodEvento': ACodEvento});
    return result;
  }

  Future<String> statusServico() async {
    final String result = await _channel.invokeMethod('statusServico');
    return result;
  }

  Future<String> consultar(eChaveOuNFe, AExtrairEventos) async {
    final String result = await _channel.invokeMethod('consultar', {'eChaveOuNFe': eChaveOuNFe, 'AExtrairEventos': AExtrairEventos});
    return result;
  }

  Future<String> consultarRecibo(ARecibo) async {
    final String result = await _channel.invokeMethod('consultarRecibo', {'ARecibo': ARecibo});
    return result;
  }

  Future<String> consultaCadastro(cUF, nDocumento, nIE) async {
    final String result = await _channel.invokeMethod('consultaCadastro', {'cUF': cUF, 'nDocumento': nDocumento, 'nIE': nIE});
    return result;
  }

  Future<String> inutilizar(ACNPJ, AJustificativa, Ano, Modelo, Serie, numeroInicial, numeroFinal) async {
    final String result = await _channel.invokeMethod('inutilizar', {'ACNPJ': ACNPJ, 'AJustificativa': AJustificativa, 'Ano': Ano, 'Modelo': Modelo, 'Serie': Serie, 'numeroInicial': numeroInicial, 'numeroFinal': numeroFinal});
    return result;
  }

  Future<String> enviar(ALote, AImprimir, ASincrono, AZipado) async {
    final String result = await _channel.invokeMethod('enviar', {'ALote': ALote, 'AImprimir': AImprimir, 'ASincrono': ASincrono, 'AZipado': AZipado});
    return result;
  }

  Future<String> cancelar(eChave, AJustificativa, eCNPJ, ALote) async {
    final String result = await _channel.invokeMethod('cancelar', {'eChave': eChave, 'AJustificativa': AJustificativa, 'eCNPJ': eCNPJ, 'ALote': ALote});
    return result;
  }

  Future<String> enviarEvento(idLote) async {
    final String result = await _channel.invokeMethod('enviarEvento', {'idLote': idLote});
    return result;
  }

  Future<String> distribuicaoDFe(AcUFAutor, eCNPJCPF, eUltNSU, eArquivoOuXML) async {
    final String result = await _channel.invokeMethod('distribuicaoDFe', {'AcUFAutor': AcUFAutor, 'eCNPJCPF': eCNPJCPF, 'eUltNSU': eUltNSU, 'eArquivoOuXML': eArquivoOuXML});
    return result;
  }

  Future<String> distribuicaoDFePorUltNSU(AcUFAutor, eCNPJCPF, eUltNSU) async {
    final String result = await _channel.invokeMethod('distribuicaoDFePorUltNSU', {'AcUFAutor': AcUFAutor, 'eCNPJCPF': eCNPJCPF, 'eUltNSU': eUltNSU});
    return result;
  }

  Future<String> distribuicaoDFePorNSU(AcUFAutor, eCNPJCPF, eNSU) async {
    final String result = await _channel.invokeMethod('distribuicaoDFePorNSU', {'AcUFAutor': AcUFAutor, 'eCNPJCPF': eCNPJCPF, 'eNSU': eNSU});
    return result;
  }

  Future<String> distribuicaoDFePorChave(AcUFAutor, eCNPJCPF, eChave) async {
    final String result = await _channel.invokeMethod('distribuicaoDFePorChave', {'AcUFAutor': AcUFAutor, 'eCNPJCPF': eCNPJCPF, 'eChave': eChave});
    return result;
  }

  Future<String> enviarEmail(ePara, eXMLNFe, AEnviaPDF, eAssunto, eCC, eAnexos, eMensagem) async {
    final String result = await _channel.invokeMethod('enviarEmail', {'ePara': ePara, 'eXMLNFe': eXMLNFe, 'AEnviaPDF': AEnviaPDF, 'eAssunto': eAssunto, 'eCC': eCC, 'eAnexos': eAnexos, 'eMensagem': eMensagem});
    return result;
  }

  Future<String> enviarEmailEvento(ePara, eChaveEvento, eChaveNFe, AEnviaPDF, eAssunto, eCC, eAnexos, eMensagem) async {
    final String result = await _channel.invokeMethod('enviarEmailEvento', {'ePara': ePara, 'eChaveEvento': eChaveEvento, 'eChaveNFe': eChaveNFe, 'AEnviaPDF': AEnviaPDF, 'eAssunto': eAssunto, 'eCC': eCC, 'eAnexos': eAnexos, 'eMensagem': eMensagem});
    return result;
  }

  Future<String> imprimir(cImpressora, nNumCopias, cProtocolo, bMostrarPreview, cMarcaDagua, bViaConsumidor, bSimplificado) async {
    final String result = await _channel.invokeMethod('imprimir', {'cImpressora': cImpressora, 'nNumCopias': nNumCopias, 'cProtocolo': cProtocolo, 'bMostrarPreview': bMostrarPreview, 'cMarcaDagua': cMarcaDagua, 'bViaConsumidor': bViaConsumidor, 'bSimplificado': bSimplificado});
    return result;
  }

  Future<String> imprimirPDF() async {
    final String result = await _channel.invokeMethod('imprimirPDF');
    return result;
  }

  Future<String> salvarPDF() async {
    final String result = await _channel.invokeMethod('salvarPDF');
    return result;
  }

  Future<String> imprimirEvento(eArquivoXmlNFe, eArquivoXmlEvento) async {
    final String result = await _channel.invokeMethod('imprimirEvento', {'eArquivoXmlNFe': eArquivoXmlNFe, 'eArquivoXmlEvento': eArquivoXmlEvento});
    return result;
  }

  Future<String> imprimirEventoPDF(eArquivoXmlNFe, eArquivoXmlEvento) async {
    final String result = await _channel.invokeMethod('imprimirEventoPDF', {'eArquivoXmlNFe': eArquivoXmlNFe, 'eArquivoXmlEvento': eArquivoXmlEvento});
    return result;
  }

  Future<String> salvarEventoPDF(eArquivoXmlNFe, eArquivoXmlEvento) async {
    final String result = await _channel.invokeMethod('salvarEventoPDF', {'eArquivoXmlNFe': eArquivoXmlNFe, 'eArquivoXmlEvento': eArquivoXmlEvento});
    return result;
  }

  Future<String> imprimirInutilizacao(eArquivoXml) async {
    final String result = await _channel.invokeMethod('imprimirInutilizacao', {'eArquivoXml': eArquivoXml});
    return result;
  }

  Future<String> imprimirInutilizacaoPDF(eArquivoXml) async {
    final String result = await _channel.invokeMethod('imprimirInutilizacaoPDF', {'eArquivoXml': eArquivoXml});
    return result;
  }

  Future<String> salvarInutilizacaoPDF(eArquivoXml) async {
    final String result = await _channel.invokeMethod('salvarInutilizacaoPDF', {'eArquivoXml': eArquivoXml});
    return result;
  }
}
