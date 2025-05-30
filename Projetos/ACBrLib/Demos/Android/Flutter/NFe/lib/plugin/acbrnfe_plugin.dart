import 'package:flutter/material.dart';
import 'package:flutter/services.dart';

/// [ACBrNFePlugin]
///
/// Este plugin Flutter atua como uma ponte para a biblioteca nativa [ACBrLibNFe].
/// Ele facilita a comunicação entre o código Dart/Flutter e as funcionalidades
/// expostas pelo SDK nativo Android (AAR).
///
/// **Dependências Nativas:**
/// - Android: `app/libs/ACBrLibNFe-release.aar`
///
/// **Como Usar:**
/// 1. Instancie a classe: `final acbrNFePlugin = ACBrNFePlugin();`
/// 2. Chame os métodos: `final resultado = await acbrNFePlugin.inicializar();`
///
class ACBrNFePlugin {
  /// O MethodChannel é a ponte de comunicação com o código nativo.
  // O nome do canal deve ser único e o mesmo usado no código nativo.
  static const MethodChannel _channel =
      MethodChannel('br.com.projetoacbr.example.acbrlib.nfe.acbrlibnfe');

  /// Método usado para inicializar o componente para uso da biblioteca.
  Future<int> inicializar() async {
    try {
      final int result = await _channel.invokeMethod('inicializar');
      return result;
    } on PlatformException catch (e) {
      debugPrint("Erro: '${e.message}'.");
      return -1;
    }
  }

  /// Método usado para remover o componente ACBrNFe e suas classes da memória.
  Future<int> finalizar() async {
    try {
      final int result = await _channel.invokeMethod('finalizar');
      return result;
    } on PlatformException catch (e) {
      debugPrint("Erro: '${e.message}'.");
      return -1;
    }
  }

  /// Método que retorna o nome da biblioteca.
  Future<String> nome() async {
    try {
      final String result = await _channel.invokeMethod('nome');
      return result;
    } on PlatformException catch (e) {
      debugPrint("Erro: '${e.message}'.");
      return "";
    }
  }

  /// Método que retorna a versão da biblioteca.
  Future<String> versao() async {
    try {
      final String result = await _channel.invokeMethod('versao');
      return result;
    } on PlatformException catch (e) {
      debugPrint("Erro: '${e.message}'.");
      return "";
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
      debugPrint("Erro: '${e.message}'.");
      return -1;
    }
  }

  /// Método usado para gravar a configuração da biblioteca no arquivo INI informado.
  Future<int> configGravar() async {
    try {
      final int result = await _channel.invokeMethod('configGravar');
      return result;
    } on PlatformException catch (e) {
      debugPrint("Erro: '${e.message}'.");
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
      debugPrint("Erro: '${e.message}'.");
      return "";
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
      debugPrint("Erro: '${e.message}'.");
      return -1;
    }
  }

  /// Método usado para importar a configuração da biblioteca do arquivo INI informado.
  ///
  /// - [arquivoConfig] Arquivo INI para ler, se informado vazio será usado o valor padrão.
  ///
  Future<int> configImportar(String arquivoConfig) async {
    try {
      final int result = await _channel
          .invokeMethod('configImportar', {"arquivoConfig": arquivoConfig});
      return result;
    } on PlatformException catch (e) {
      debugPrint("Erro: '${e.message}'.");
      return -1;
    }
  }

  /// Método usado para exportar a configuração da biblioteca do arquivo INI informado.
  Future<String> configExportar() async {
    try {
      final String result = await _channel.invokeMethod('configExportar');
      return result;
    } on PlatformException catch (e) {
      debugPrint("Erro: '${e.message}'.");
      return "";
    }
  }

  Future<String> openSslInfo() async {
    try {
      final String result = await _channel.invokeMethod('openSSLInfo');
      return result;
    } on PlatformException catch (e) {
      debugPrint("Erro: '${e.message}'.");
      return "";
    }
  }

  /// Método usado para ler o arquivo XML para o componente ACBrNFe.
  ///
  /// - [eArquivoOuXML] Path com o nome do arquivo XML a ser lido ou o conteúdo do XML.
  ///
  Future<String> carregarXML(eArquivoOuXML) async {
    final String result = await _channel
        .invokeMethod('carregarXML', {'eArquivoOuXML': eArquivoOuXML});
    return result;
  }

  /// Método usado para ler o arquivo INI para o componente ACBrNFe.
  ///
  /// - [eArquivoOuINI] Path com o nome do arquivo INI a ser lido ou o conteúdo do INI.
  ///
  Future<String> carregarINI(eArquivoOuINI) async {
    final String result = await _channel
        .invokeMethod('carregarINI', {'eArquivoOuINI': eArquivoOuINI});
    return result;
  }

  /// Método para retornar o XML da NFe.
  ///
  /// - [aIndex] Posição da NFe na lista, a lista inicia em 0.
  ///
  Future<int> obterXML(aIndex) async {
    final int result =
        await _channel.invokeMethod('obterXML', {'AIndex': aIndex});
    return result;
  }

  /// Método para gravar o XML da NFe.
  ///
  /// - [aIndex] Posição da NFe na lista, a lista inicia em 0.
  /// - [eNomeArquivo] Nome do arquivo xml a ser salvo.
  /// - [ePathArquivo] Local onde será salvo o xml.
  ///
  Future<String> gravarXML(aIndex, eNomeArquivo, ePathArquivo) async {
    final String result = await _channel.invokeMethod('gravarXML', {
      'AIndex': aIndex,
      'eNomeArquivo': eNomeArquivo,
      'ePathArquivo': ePathArquivo
    });
    return result;
  }

  /// Método para retornar o XML da NFe em formato INI.
  ///
  /// - [aIndex] Posição da NFe na lista, a lista inicia em 0.
  ///
  Future<String> obterINI(aIndex) async {
    final String result =
        await _channel.invokeMethod('obterINI', {'AIndex': aIndex});
    return result;
  }

  /// Método para gravar o XML da NFe em formato INI.
  ///
  /// - [aIndex] Posição da NFe na lista, a lista inicia em 0.
  /// - [eNomeArquivo] Nome do arquivo ini a ser salvo.
  /// - [ePathArquivo] Local onde será salvo o INI.
  ///
  Future<String> gravarINI(aIndex, eNomeArquivo, ePathArquivo) async {
    final String result = await _channel.invokeMethod('gravarINI', {
      'AIndex': aIndex,
      'eNomeArquivo': eNomeArquivo,
      'ePathArquivo': ePathArquivo
    });
    return result;
  }

  /// Método usado para ler o arquivo XML para o componente ACBrNFe.
  ///
  /// - [eArquivoOuXML] Path com o nome do arquivo XML a ser lido ou o conteúdo do XML.
  ///
  Future<String> carregarEventoXML(eArquivoOuXML) async {
    final String result = await _channel
        .invokeMethod('carregarEventoXML', {'eArquivoOuXML': eArquivoOuXML});
    return result;
  }

  /// Método usado para ler o arquivo INI para o componente ACBrNFe.
  ///
  /// - [eArquivoOuINI] Path com o nome do arquivo INI a ser lido ou o conteúdo do INI.
  ///
  Future<String> carregarEventoINI(eArquivoOuINI) async {
    final String result = await _channel
        .invokeMethod('carregarEventoINI', {'eArquivoOuINI': eArquivoOuINI});
    return result;
  }

  /// Método usado para limpar a lista de notas no componente ACBrNFe.
  Future<String> limparLista() async {
    final String result = await _channel.invokeMethod('limparLista');
    return result;
  }

  /// Método usado para limpar a lista de eventos no componente ACBrNFe.
  Future<String> limparListaEventos() async {
    final String result = await _channel.invokeMethod('limparListaEventos');
    return result;
  }

  /// Método usado para validar as notas assinadas através do componente ACBrNFe.
  Future<String> validar() async {
    final String result = await _channel.invokeMethod('validar');
    return result;
  }

  /// Método usado para Validar Regras de Negócios dos dados que se encontram no XML de uma NF-e.
  Future<String> validarRegrasdeNegocios() async {
    final String result =
        await _channel.invokeMethod('validarRegrasdeNegocios');
    return result;
  }

  /// Método usado para Verificar a assinatura de um XML.
  Future<String> verificarAssinatura() async {
    final String result = await _channel.invokeMethod('verificarAssinatura');
    return result;
  }

  /// Método usado para gerar uma chave para o documento fiscal.
  ///
  /// - [aCodigoUF] Código da UF para gerar a chave.
  /// - [aCodigoNumerico] Código numérico da nota fiscal.
  /// - [aModelo] Modelo do documento 55 ou 65.
  /// - [aSerie] Série da nota fiscal.
  /// - [aNumero] Número da nota fiscal.
  /// - [aTpEmi] Tipo de Emissão:
  ///   - 1 - teNormal
  ///   - 2 - teContingencia
  ///   - 3 - teSCAN
  ///   - 4 - teDPEC
  ///   - 5 - teFSDA
  ///   - 6 - teSVCAN
  ///   - 7 - teSVCRS
  ///   - 8 - teSVCSP
  ///   - 9 - teOffLine
  /// - [aEmissao] Data da emissão da NFe no formato **dd/MM/yyyy**.
  /// - [aCNPJCPF] CPF/CNPJ do emissor da nota.
  ///
  Future<String> gerarChave(aCodigoUF, aCodigoNumerico, aModelo, aSerie,
      aNumero, aTpEmi, aEmissao, aCNPJCPF) async {
    final String result = await _channel.invokeMethod('gerarChave', {
      'ACodigoUF': aCodigoUF,
      'ACodigoNumerico': aCodigoNumerico,
      'AModelo': aModelo,
      'ASerie': aSerie,
      'ANumero': aNumero,
      'ATpEmi': aTpEmi,
      'AEmissao': aEmissao
    });
    return result;
  }

  /// Método usado retornar uma lista de dados dos certificados instalados na maquina.
  Future<String> obterCertificados() async {
    final String result = await _channel.invokeMethod('obterCertificados');
    return result;
  }

  /// Método usado retornar o path onde será salvos os documentos gerados pela biblioteca.
  ///
  /// - [aTipo] Tipo de path que será retornado:
  ///   - 0 = NFe
  ///   - 1 = Inutilização
  ///   - 2 = CCe
  ///   - 3 = Cancelamento
  ///
  Future<String> getPath(aTipo) async {
    final String result =
        await _channel.invokeMethod('getPath', {'ATipo': aTipo});
    return result;
  }

  /// Método usado retornar o path onde será salvos os eventos gerados pela biblioteca.
  ///
  /// - [aCodEvento] O código do evento.
  ///
  Future<String> getPathEvento(aCodEvento) async {
    final String result = await _channel
        .invokeMethod('getPathEvento', {'ACodEvento': aCodEvento});
    return result;
  }

  /// Método usado para Consultar o Status de Serviço na SEFAZ.
  Future<String> statusServico() async {
    final String result = await _channel.invokeMethod('statusServico');
    return result;
  }

  /// Método usado para consultar um NFe na SEFAZ.
  ///
  /// - [eChaveOuNFe] Path com o nome do arquivo XML a ser consultado ou o conteúdo do XML.
  /// - [aExtrairEventos] Informe se deve ou não extrair os eventos, se houver os mesmos na reposta.
  ///
  Future<String> consultar(eChaveOuNFe, aExtrairEventos) async {
    final String result = await _channel.invokeMethod('consultar',
        {'eChaveOuNFe': eChaveOuNFe, 'AExtrairEventos': aExtrairEventos});
    return result;
  }

  /// Método usado para consultar o recibo de envio na SEFAZ.
  ///
  /// - [aRecibo] Número do recibo para consulta.
  ///
  Future<String> consultarRecibo(aRecibo) async {
    final String result =
        await _channel.invokeMethod('consultarRecibo', {'ARecibo': aRecibo});
    return result;
  }

  /// Método usado para consultar o cadastro da SEFAZ.
  ///
  /// - [cUF] Sigla do estado do documento a ser consultado.
  /// - [nDocumento] Número do documento a ser consultado.
  /// - [nIE] Se passado true irá consultar pelo documento Inscrição Estadual, caso contrário irá consultar pelo CPF ou CNPJ.
  ///
  Future<String> consultaCadastro(cUF, nDocumento, nIE) async {
    final String result = await _channel.invokeMethod(
        'consultaCadastro', {'cUF': cUF, 'nDocumento': nDocumento, 'nIE': nIE});
    return result;
  }

  /// Método usado para Inutilizar um numero ou faixa de números na SEFAZ.
  ///
  /// - [aCNPJ] CNPJ do emitente.
  /// - [aJustificativa] Motivo por estar solicitando a Inutilização.
  /// - [ano] Ano.
  /// - [modelo] Modelo deve ser informado 55 para NF-e ou 65 para NFC-e.
  /// - [serie] Serie do Documento Fiscal.
  /// - [numeroInicial] Número Inicial a que se deseja Inutilizar.
  /// - [numeroFinal] Número Final a que se deseja Inutilizar.
  ///
  Future<String> inutilizar(aCNPJ, aJustificativa, ano, modelo, serie,
      numeroInicial, numeroFinal) async {
    final String result = await _channel.invokeMethod('inutilizar', {
      'ACNPJ': aCNPJ,
      'AJustificativa': aJustificativa,
      'Ano': ano,
      'Modelo': modelo,
      'Serie': serie,
      'numeroInicial': numeroInicial,
      'numeroFinal': numeroFinal
    });
    return result;
  }

  /// Método usado para enviar um lote de NFe para SEFAZ.
  ///
  /// - [aLote] Numero do Lote a ser enviado.
  /// - [aImprimir] Se True imprime o DANFe caso o NF-e for autorizado.
  /// - [aSincrono] Se True envia em modo síncrono.
  /// - [aZipado] Se True envia o arquivo zipado.
  ///
  Future<String> enviar(aLote, aImprimir, aSincrono, aZipado) async {
    final String result = await _channel.invokeMethod('enviar', {
      'ALote': aLote,
      'AImprimir': aImprimir,
      'ASincrono': aSincrono,
      'AZipado': aZipado
    });
    return result;
  }

  /// Método usado para Cancelar um NFe na SEFAZ.
  ///
  /// - [eChave] Chave de acesso do XML a ser cancelado.
  /// - [aJustificativa] Motivo do cancelamento.
  /// - [eCNPJ] CNPJ do emitente.
  /// - [aLote] Numero do Lote do evento de cancelamento.
  ///
  Future<String> cancelar(eChave, aJustificativa, eCNPJ, aLote) async {
    final String result = await _channel.invokeMethod('cancelar', {
      'eChave': eChave,
      'AJustificativa': aJustificativa,
      'eCNPJ': eCNPJ,
      'ALote': aLote
    });
    return result;
  }

  /// Método usado para Enviar um Evento para SEFAZ.
  ///
  /// - [idLote] Numero do Lote do evento.
  ///
  Future<String> enviarEvento(idLote) async {
    final String result =
        await _channel.invokeMethod('enviarEvento', {'idLote': idLote});
    return result;
  }

  /// Método usado para Baixar documentos do Ambiente Nacional através do método DistribuicaoDFe informando o último NSU retornado pela execução anterior.
  ///
  /// - [acUFAutor] Código da UF do autor da consulta.
  /// - [eCNPJCPF] CNPJ/CPF do autor da consulta.
  /// - [eUltNSU] Numero do ultimo NSU.
  /// - [eArquivoOuXML] Path com o nome do arquivo XML a ser lido ou o conteúdo do XML.
  ///
  Future<String> distribuicaoDFe(
      acUFAutor, eCNPJCPF, eUltNSU, eArquivoOuXML) async {
    final String result = await _channel.invokeMethod('distribuicaoDFe', {
      'AcUFAutor': acUFAutor,
      'eCNPJCPF': eCNPJCPF,
      'eUltNSU': eUltNSU,
      'eArquivoOuXML': eArquivoOuXML
    });
    return result;
  }

  /// Método usado para Baixar documentos do Ambiente Nacional através do método DistribuicaoDFe informando o último NSU retornado pela execução anterior.
  ///
  /// - [acUFAutor] Código da UF do autor da consulta.
  /// - [eCNPJCPF] CNPJ/CPF do autor da consulta.
  /// - [eUltNSU] Numero do ultimo NSU.
  ///
  Future<String> distribuicaoDFePorUltNSU(acUFAutor, eCNPJCPF, eUltNSU) async {
    final String result = await _channel.invokeMethod(
        'distribuicaoDFePorUltNSU',
        {'AcUFAutor': acUFAutor, 'eCNPJCPF': eCNPJCPF, 'eUltNSU': eUltNSU});
    return result;
  }

  /// Método usado para Baixar o documento do Ambiente Nacional através do método DistribuicaoDFe informando o seu NSU.
  ///
  /// - [acUFAutor] Código da UF do autor da consulta.
  /// - [eCNPJCPF] CNPJ/CPF do autor da consulta.
  /// - [eNSU] Numero do NSU do documento.
  ///
  Future<String> distribuicaoDFePorNSU(acUFAutor, eCNPJCPF, eNSU) async {
    final String result = await _channel.invokeMethod('distribuicaoDFePorNSU',
        {'AcUFAutor': acUFAutor, 'eCNPJCPF': eCNPJCPF, 'eNSU': eNSU});
    return result;
  }

  /// Método usado para Baixar o NFe do Ambiente Nacional através do método DistribuicaoDFe informando o a sua chave.
  ///
  /// - [acUFAutor] Código da UF do autor da consulta.
  /// - [eCNPJCPF] CNPJ/CPF do autor da consulta.
  /// - [eChave] Chave do NFe.
  ///
  Future<String> distribuicaoDFePorChave(acUFAutor, eCNPJCPF, eChave) async {
    final String result = await _channel.invokeMethod('distribuicaoDFePorChave',
        {'AcUFAutor': acUFAutor, 'eCNPJCPF': eCNPJCPF, 'eChave': eChave});
    return result;
  }

  /// Método usado para enviar e-mail através do componente ACBrNFe.
  ///
  /// - [ePara] Destinatário.
  /// - [eXMLNFe] Path ou conteúdo do xml.
  /// - [aEnviaPDF] Se True gera o PDF do DANFe e anexa ao e-mail.
  /// - [eAssunto] Texto contendo o assunto do e-mail.
  /// - [eCC] Endereços separados por ponto e vírgula que receberão uma cópia do e-mail.
  /// - [eAnexos] Path com o nome de arquivos separados por ponto e vírgula a serem anexados ao e-mail.
  /// - [eMensagem] Texto referente a mensagem do e-mail.
  ///
  Future<String> enviarEmail(
      ePara, eXMLNFe, aEnviaPDF, eAssunto, eCC, eAnexos, eMensagem) async {
    final String result = await _channel.invokeMethod('enviarEmail', {
      'ePara': ePara,
      'eXMLNFe': eXMLNFe,
      'AEnviaPDF': aEnviaPDF,
      'eAssunto': eAssunto,
      'eCC': eCC,
      'eAnexos': eAnexos,
      'eMensagem': eMensagem
    });
    return result;
  }

  /// Método usado para enviar evento por e-mail através do componente ACBrNFe.
  ///
  /// - [ePara] Destinatário.
  /// - [eChaveEvento] Path com o nome do arquivo XML do Evento a ser anexado ao e-mail.
  /// - [eChaveNFe] Path com o nome do arquivo XML do NFe a ser anexado ao e-mail.
  /// - [aEnviaPDF] Se True gera o PDF do DANFe e anexa ao e-mail.
  /// - [eAssunto] Texto contendo o assunto do e-mail.
  /// - [eCC] Endereços separados por ponto e vírgula que receberão uma cópia do e-mail.
  /// - [eAnexos] Path com o nome de arquivos separados por ponto e vírgula a serem anexados ao e-mail.
  /// - [eMensagem] Texto referente a mensagem do e-mail.
  ///
  Future<String> enviarEmailEvento(ePara, eChaveEvento, eChaveNFe, aEnviaPDF,
      eAssunto, eCC, eAnexos, eMensagem) async {
    final String result = await _channel.invokeMethod('enviarEmailEvento', {
      'ePara': ePara,
      'eChaveEvento': eChaveEvento,
      'eChaveNFe': eChaveNFe,
      'AEnviaPDF': aEnviaPDF,
      'eAssunto': eAssunto,
      'eCC': eCC,
      'eAnexos': eAnexos,
      'eMensagem': eMensagem
    });
    return result;
  }

  /// Método usado para imprimir o DANFe/DANFCe dos NFes/NFCes carregados.
  ///
  /// - [cImpressora] Nome da impressora onde será impresso o documento, senão informado será usado a impressora informada nas configurações.
  /// - [nNumCopias] Quantidade de copias a ser impresso, informe zero para usar o valor informado nas configurações.
  /// - [cProtocolo] Número do protocolo da NFe.
  /// - [bMostrarPreview] Se informado "True" exibira o preview, se "False" senão quiser mostra ou vazio para usar os valores das configurações.
  /// - [cMarcaDagua] Define o caminho da imagem que será usada como marca d'água na impressão da DANFe, senão informado será usado o valor da configuração.
  /// - [bViaConsumidor] Se informado "True" imprimira a via do consumidor, se "False" senão quiser mostra ou vazio para usar os valores das configurações, valido apenas para NFCe.
  /// - [bSimplificado] Se informado "True"  imprimira a DANFCe de forma simplificada, se "False" senão quiser mostra ou vazio para usar os valores das configurações, valido apenas para NFCe.
  ///
  Future<String> imprimir(cImpressora, nNumCopias, cProtocolo, bMostrarPreview,
      cMarcaDagua, bViaConsumidor, bSimplificado) async {
    final String result = await _channel.invokeMethod('imprimir', {
      'cImpressora': cImpressora,
      'nNumCopias': nNumCopias,
      'cProtocolo': cProtocolo,
      'bMostrarPreview': bMostrarPreview,
      'cMarcaDagua': cMarcaDagua,
      'bViaConsumidor': bViaConsumidor,
      'bSimplificado': bSimplificado
    });
    return result;
  }

  /// Método usado para gerar o PDF do DANFe de um NFe carregado.
  Future<String> imprimirPDF() async {
    final String result = await _channel.invokeMethod('imprimirPDF');
    return result;
  }

  /// Método para retornar o pdf da DANFe em formato Base64.
  Future<String> salvarPDF() async {
    final String result = await _channel.invokeMethod('salvarPDF');
    return result;
  }

  /// Método usado para imprimir um evento.
  ///
  /// - [eArquivoXmlNFe] Path do arquivo XML da NFe para impressão.
  /// - [eArquivoXmlEvento] Path do arquivo XML do evento para impressão.
  ///
  Future<String> imprimirEvento(eArquivoXmlNFe, eArquivoXmlEvento) async {
    final String result = await _channel.invokeMethod('imprimirEvento', {
      'eArquivoXmlNFe': eArquivoXmlNFe,
      'eArquivoXmlEvento': eArquivoXmlEvento
    });
    return result;
  }

  /// Método usado para gerar o PDF de um evento.
  ///
  /// - [eArquivoXmlNFe] Path do arquivo XML da NFe para impressão.
  /// - [eArquivoXmlEvento] Path do arquivo XML do evento para impressão.
  ///
  Future<String> imprimirEventoPDF(eArquivoXmlNFe, eArquivoXmlEvento) async {
    final String result = await _channel.invokeMethod('imprimirEventoPDF', {
      'eArquivoXmlNFe': eArquivoXmlNFe,
      'eArquivoXmlEvento': eArquivoXmlEvento
    });
    return result;
  }

  /// Método usado para salvar o PDF de um evento em formato Base64.
  ///
  /// - [eArquivoXmlNFe] Path do arquivo XML da NFe para  formato Base64.
  /// - [eArquivoXmlEvento] Path do arquivo XML do evento para  formato Base64.
  ///
  Future<String> salvarEventoPDF(eArquivoXmlNFe, eArquivoXmlEvento) async {
    final String result = await _channel.invokeMethod('salvarEventoPDF', {
      'eArquivoXmlNFe': eArquivoXmlNFe,
      'eArquivoXmlEvento': eArquivoXmlEvento
    });
    return result;
  }

  /// Método usado para imprimir a Inutilização.
  ///
  /// - [eArquivoXml] Path do arquivo XML da inutilização para impressão.
  ///
  Future<String> imprimirInutilizacao(eArquivoXml) async {
    final String result = await _channel
        .invokeMethod('imprimirInutilizacao', {'eArquivoXml': eArquivoXml});
    return result;
  }

  /// Método usado para gerar o PDF da Inutilização.
  ///
  /// - [eArquivoXml] Path do arquivo XML da inutilização para impressão.
  ///
  Future<String> imprimirInutilizacaoPDF(eArquivoXml) async {
    final String result = await _channel
        .invokeMethod('imprimirInutilizacaoPDF', {'eArquivoXml': eArquivoXml});
    return result;
  }

  /// Método usado para salvar o PDF da Inutilização em formato Base64.
  ///
  /// - [eArquivoXml] Path do arquivo XML da inutilização para  formato Base64.
  ///
  Future<String> salvarInutilizacaoPDF(eArquivoXml) async {
    final String result = await _channel
        .invokeMethod('salvarInutilizacaoPDF', {'eArquivoXml': eArquivoXml});
    return result;
  }
}
