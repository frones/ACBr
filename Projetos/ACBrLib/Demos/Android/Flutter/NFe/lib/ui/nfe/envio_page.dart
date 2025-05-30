import 'dart:io';
import 'dart:typed_data';

import 'package:acbrlibnfe/ui/nfe/nfe_utils.dart';
import 'package:acbrlibnfe/ui/nfe/widgets/painel_resposta.dart';
import 'package:acbrlibnfe/utils/utils.dart';
import 'package:flutter/material.dart';
import 'package:image/image.dart' as img;
import 'package:pdfx/pdfx.dart';

import '../../plugin/acbrnfe_plugin.dart';
import '../../utils/acbrlib_nfe_helper.dart';
import '../_core/app_colors.dart';

/// Página com comandos de Envio
class EnvioPage extends StatefulWidget {
  const EnvioPage({super.key});

  @override
  State<EnvioPage> createState() => _EnvioPageState();
}

class _EnvioPageState extends State<EnvioPage> {
  /// Instância do plugin
  late ACBrNFePlugin _acbrNFePlugin;

  /// Gerencia respostas dos comandos
  String? _response;

  // Controllers dos inputs
  final TextEditingController _nfeIniController = TextEditingController();
  final TextEditingController _nfeXmlController = TextEditingController();
  final TextEditingController _destinatarioController = TextEditingController();

  /// Controlador de rolagem
  final ScrollController _scrollController = ScrollController();

  /// Função para limpar a resposta
  void _onResponseClear() {
    setState(() {
      _response = null;
    });
  }

  @override
  void initState() {
    super.initState();
    _acbrNFePlugin = ACBrLibNFeHelper().acbrNFePlugin;
  }

  @override
  void dispose() {
    // Descartando controllers para evitar vazamento de memória
    _nfeIniController.dispose();
    _nfeXmlController.dispose();
    _destinatarioController.dispose();
    _scrollController.dispose();
    super.dispose();
  }

  /// Função para executar o comando de Limpar Lista NFe
  void _onClickLimparListaNFe() async {
    String result = "";
    try {
      result = await _acbrNFePlugin.limparLista();
      debugPrint('Resultado de limparLista: "$result"');
    } catch (e) {
      result = "ERRO: Erro ao Limpar Lista NFe.\nChecar logs.";
      debugPrint("Erro ao chamar limparLista: '$e'.");
    } finally {
      setState(() {
        _response = result;
      });
      NfeUtils.goToPainelResposta(_scrollController);
    }
  }

  /// Função para executar o comando de Enviar NFe - INI
  void _onClickEnviarNFe() async {
    String result = "";
    try {
      await _acbrNFePlugin.carregarINI(
        _nfeIniController.text,
      );
      result = await _acbrNFePlugin.enviar(1, false, false, false);
      debugPrint('Resultado de enviarNFeINI: "$result"');
    } catch (e) {
      result = "ERRO: Erro ao Enviar NFe INI\nChecar logs.";
      debugPrint("Erro ao chamar enviarNFeINI: '$e'.");
    } finally {
      setState(() {
        _response = result;
      });
      NfeUtils.goToPainelResposta(_scrollController);
    }
  }

  /// Função para executar o comando de Enviar DANFCe por email
  void _onClickEnviarDANFCeEmail() async {
    String result = "";
    try {
      result = await _acbrNFePlugin.enviarEmail(
          _destinatarioController.text,
          _nfeXmlController.text,
          true,
          "Envio NFCe Email",
          "",
          "",
          "Envio NFCe");
      debugPrint('Resultado de enviarEmail: "$result"');
    } catch (e) {
      result = "ERRO: Erro ao Enviar DANFCe por E-mail\nChecar logs.";
      debugPrint("Erro ao chamar enviarEmail: '$e'.");
    } finally {
      setState(() {
        _response = result;
      });
      NfeUtils.goToPainelResposta(_scrollController);
    }
  }

  /// Função para executar o comando de Imprimir PDF DANFCe
  void _onClickImprimirPDFNFCe() async {
    String result = "";
    try {
      debugPrint("Iniciando impressão PDF DANFCe...");
      debugPrint("Step 1: Limpando lista antes da impressão.");
      await _acbrNFePlugin.limparLista();
      debugPrint("Step 2: Carregando XML para impressão.");
      if (_nfeXmlController.text.isEmpty) {
        result = "ERRO: XML não pode ser vazio.";
        debugPrint(result);
        return;
      }
      await _acbrNFePlugin.carregarXML(_nfeXmlController.text);
      debugPrint("Step 3: Imprimindo PDF DANFCe.");
      result = await _acbrNFePlugin.imprimir(
        "",
        1,
        'False',
        'False',
        'False',
        'False',
        'False',
      );
      debugPrint('Resultado de imprimir PDF DANFCe: "$result"');
    } catch (e) {
      result = "ERRO: Erro ao Imprimir PDF DANFCe\nChecar logs.";
      debugPrint("Erro ao chamar imprimir PDF DANFCe: '$e'.");
    } finally {
      setState(() {
        _response = result;
      });
      NfeUtils.goToPainelResposta(_scrollController);
    }
  }

  /// Função para executar o comando de Converter DANFCe PDF em BMP
  void _onClickConverterNFCePDFemBMP() async {
    String result = "";
    try {
      debugPrint('Limpando lista antes da impressão');
      _acbrNFePlugin.limparLista();

      String xmlContent = _nfeXmlController.text;
      if (xmlContent.isEmpty) {
        result = 'ERRO: XML não pode ser vazio.';
        return;
      }

      debugPrint('Carregando XML.');
      _acbrNFePlugin.carregarXML(xmlContent);

      debugPrint('Preparando para imprimir.');
      _acbrNFePlugin.imprimirPDF();

      final String appDir = await getAppDirByPlatform();
      // Caminho do PDF gerado
      final File pdfFile = File('$appDir/nfc_e.pdf');

      // Verifica se o arquivo PDF foi criado com sucesso
      if (!await pdfFile.exists()) {
        result = "ERRO: PDF não encontrado no caminho: ${pdfFile.path}";
        debugPrint(result);
        return;
      }

      int targetWidth = 850;
      int targetHeight = 1200;

      Uint8List? imageBytes = await savePdfAndConvertToPngBytes(
          pdfFile.path, targetWidth, targetHeight);

      if (imageBytes == null || imageBytes.isEmpty) {
        result =
            "ERRO: Imagem está nula ou vazia. Conversão de PDF para Imagem falhou.\nChecar logs.";
        debugPrint(result);
        return;
      }

      bool salvou = await savePngToBmpFile(imageBytes);

      result = salvou
          ? "NFC-e processada e imagem salva!"
          : "ERRO: Erro ao converter DANFCe PDF em BMP\nChecar logs.";
    } catch (e) {
      result = "ERRO: Erro ao converter DANFCe PDF em BMP\nChecar logs.";
      debugPrint('Erro ao converter DANFCe PDF em BMP: $e');
    } finally {
      setState(() {
        _response = result;
      });
      NfeUtils.goToPainelResposta(_scrollController);
    }
  }

  /// Converte um PDF em bytes de imagem (PNG) usando o plugin pdfx
  Future<Uint8List?> savePdfAndConvertToPngBytes(
      String pdfPath, int targetWidth, int targetHeight) async {
    try {
      final PdfDocument pdfDocument = await PdfDocument.openFile(pdfPath);
      if (pdfDocument.pagesCount == 0) {
        debugPrint("Erro: O PDF não contém páginas.");
        return null;
      }

      // Renderiza a primeira página do PDF
      final PdfPage page = await pdfDocument.getPage(1);
      final PdfPageImage? pageImage = await page.render(
        width: targetWidth.toDouble(),
        height: targetHeight.toDouble(),
        format: PdfPageImageFormat.png,
        backgroundColor: '#FFFFFF',
      );

      await page.close();
      await pdfDocument.close();

      if (pageImage == null) {
        debugPrint("Erro: Não foi possível renderizar a imagem da página.");
        return null;
      }

      debugPrint("Imagem convertida de PDF.");
      return pageImage.bytes;
    } catch (e) {
      debugPrint("Erro ao renderizar PDF: $e");
      return null;
    }
  }

  /// Converte bytes de imagem (ex: PNG) para formato BMP e salva
  Future<bool> savePngToBmpFile(Uint8List pngBytes) async {
    try {
      final img.Image? image = img.decodePng(pngBytes);
      if (image == null) {
        debugPrint("Erro: Não foi possível decodificar PNG para Image.");
        return false;
      }

      final Uint8List bmpBytes = img.encodeBmp(image);

      final String appDir = await getAppDirByPlatform();
      final String bmpPath = '$appDir/images';

      final Directory directory = Directory(bmpPath);

      if (!await directory.exists()) {
        try {
          await directory.create(recursive: true);
          debugPrint("Diretório criado: ${directory.path}");
        } catch (e) {
          final String errorMessage =
              "Falha ao criar diretório: ${directory.path}. Erro: $e";
          debugPrint("Erro: $errorMessage");
          return false;
        }
      }

      final File file = File('${directory.path}/imagem_convertida.bmp');

      await file.writeAsBytes(bmpBytes);

      debugPrint("Imagem salva em: ${file.path}");

      return true;
    } catch (e) {
      debugPrint("Erro ao salvar BMP: $e");
      return false;
    }
  }

  /// Função para executar o comando de Imprimir DANFCe
  void _onClickImprimirDANFCe() async {
    String result = "";
    try {
      debugPrint("Iniciando impressão DANFCe...");
      debugPrint("Step 1: Limpando lista antes da impressão.");
      await _acbrNFePlugin.limparLista();

      debugPrint("Step 2: Carregando XML para impressão.");
      if (_nfeXmlController.text.isEmpty) {
        result = "ERRO: XML não pode ser vazio.";
        debugPrint(result);
        return;
      }
      await _acbrNFePlugin.carregarXML(_nfeXmlController.text);
      debugPrint("Step 3: Imprimindo DANFCe.");
      result = await _acbrNFePlugin.imprimirPDF();
      debugPrint('Resultado de imprimirPDF: "$result"');
    } catch (e) {
      result = "ERRO: Erro ao Imprimir DANFCe\nChecar logs.";
      debugPrint("Erro ao chamar imprimirPDF: '$e'.");
    } finally {
      setState(() {
        _response = result;
      });
      NfeUtils.goToPainelResposta(_scrollController);
    }
  }

  @override
  Widget build(BuildContext context) {
    return SingleChildScrollView(
      controller: _scrollController,
      child: Padding(
        padding: const EdgeInsets.all(12.00),
        child: Column(
          spacing: 20.0,
          children: [
            PainelResposta(
              response: _response,
              onClear: _onResponseClear,
            ),
            Divider(height: 2.0, color: Colors.black),

            Text(
              'Comandos',
              style: TextStyle(fontWeight: FontWeight.bold, fontSize: 18.0),
            ),

            /// Seção Limpar Lista NFe
            ExpansionTile(
              shape: const Border(),
              title: Text('Limpar Lista NFe',
                  style:
                      TextStyle(fontWeight: FontWeight.bold, fontSize: 18.0)),
              children: [
                SizedBox(height: 10.0),
                SizedBox(
                  width: double.infinity,
                  child: ElevatedButton(
                    style: ElevatedButton.styleFrom(
                      backgroundColor: AppColors.secondary,
                    ),
                    onPressed: _onClickLimparListaNFe,
                    child: const Text(
                      'Limpar Lista NFe',
                      style: TextStyle(fontWeight: FontWeight.bold),
                    ),
                  ),
                ),
              ],
            ),

            Divider(height: 2.0),

            /// Seção NFe - INI
            ExpansionTile(
              shape: const Border(),
              title: Text('NFe - INI',
                  style:
                      TextStyle(fontWeight: FontWeight.bold, fontSize: 18.0)),
              children: [
                Column(
                  spacing: 14.0,
                  children: [
                    SizedBox(height: 10.0),
                    TextFormField(
                      controller: _nfeIniController,
                      minLines: 12,
                      maxLines: 12,
                      decoration: InputDecoration(
                        labelText: 'NFe INI',
                        border: OutlineInputBorder(),
                      ),
                    ),
                    SizedBox(
                      width: double.infinity,
                      child: ElevatedButton(
                        style: ElevatedButton.styleFrom(
                          backgroundColor: AppColors.secondary,
                        ),
                        onPressed: _onClickEnviarNFe,
                        child: const Text(
                          'Enviar NFe',
                          style: TextStyle(fontWeight: FontWeight.bold),
                        ),
                      ),
                    ),
                  ],
                ),
              ],
            ),

            Divider(height: 2.0),

            /// Seção NFe - XML
            ExpansionTile(
              shape: const Border(),
              title: Text('NFe - XML',
                  style:
                      TextStyle(fontWeight: FontWeight.bold, fontSize: 18.0)),
              children: [
                Column(
                  spacing: 14.0,
                  children: [
                    SizedBox(height: 10.0),
                    TextFormField(
                      controller: _nfeXmlController,
                      minLines: 12,
                      maxLines: 12,
                      decoration: InputDecoration(
                        labelText: 'NFe XML',
                        border: OutlineInputBorder(),
                      ),
                    ),
                    TextFormField(
                      controller: _destinatarioController,
                      decoration: InputDecoration(
                        labelText: 'Destinatário',
                        border: OutlineInputBorder(),
                      ),
                    ),
                    SizedBox(height: 10.0),
                    SizedBox(
                      width: double.infinity,
                      child: ElevatedButton(
                        style: ElevatedButton.styleFrom(
                          backgroundColor: AppColors.secondary,
                        ),
                        onPressed: _onClickEnviarDANFCeEmail,
                        child: const Text(
                          'Enviar DANFCe por E-mail',
                          style: TextStyle(fontWeight: FontWeight.bold),
                        ),
                      ),
                    ),
                    SizedBox(
                      width: double.infinity,
                      child: ElevatedButton(
                        style: ElevatedButton.styleFrom(
                          backgroundColor: AppColors.secondary,
                        ),
                        onPressed: _onClickImprimirPDFNFCe,
                        child: const Text(
                          'Imprimir PDF DANFCe',
                          style: TextStyle(fontWeight: FontWeight.bold),
                        ),
                      ),
                    ),
                    SizedBox(
                      width: double.infinity,
                      child: ElevatedButton(
                        style: ElevatedButton.styleFrom(
                          backgroundColor: AppColors.secondary,
                        ),
                        onPressed: _onClickConverterNFCePDFemBMP,
                        child: const Text(
                          'Converter DANFCe PDF em BMP',
                          style: TextStyle(fontWeight: FontWeight.bold),
                        ),
                      ),
                    ),
                    SizedBox(
                      width: double.infinity,
                      child: ElevatedButton(
                        style: ElevatedButton.styleFrom(
                          backgroundColor: AppColors.secondary,
                        ),
                        onPressed: _onClickImprimirDANFCe,
                        child: const Text(
                          'Imprimir DANFCe',
                          style: TextStyle(fontWeight: FontWeight.bold),
                        ),
                      ),
                    ),
                  ],
                ),
              ],
            ),
          ],
        ),
      ),
    );
  }
}
