import 'dart:io';

import 'package:acbrlibnfe/ui/configuracoes/widgets/config_buttons.dart';
import 'package:acbrlibnfe/utils/utils.dart';
import 'package:file_picker/file_picker.dart';
import 'package:flutter/material.dart';

import '../../plugin/acbrnfe_plugin.dart';
import '../../utils/acbrlib_nfe_helper.dart';
import '../_core/app_colors.dart';

/// Página de Configurações de Certificados
class CertificadosPage extends StatefulWidget {
  const CertificadosPage({super.key});

  @override
  State<CertificadosPage> createState() => _CertificadosPageState();
}

class _CertificadosPageState extends State<CertificadosPage> {
  /// Instância do plugin
  late ACBrNFePlugin _acbrNFePlugin;

  // Controllers para os campos de texto
  final TextEditingController _pathController = TextEditingController();
  final TextEditingController _dadosPfxController = TextEditingController();
  final TextEditingController _senhaController = TextEditingController();
  final TextEditingController _numeroSerieController = TextEditingController();
  final TextEditingController _retornoController = TextEditingController();

  /// Variável para controlar o estado de carregamento
  bool _isLoading = true;

  @override
  void initState() {
    super.initState();
    _acbrNFePlugin = ACBrLibNFeHelper().acbrNFePlugin;
    // Carrega as configurações ao iniciar a página
    _loadConfig();
  }

  @override
  void dispose() {
    // Descartando controllers para evitar vazamento de memória
    _pathController.dispose();
    _dadosPfxController.dispose();
    _senhaController.dispose();
    _numeroSerieController.dispose();
    _retornoController.dispose();
    super.dispose();
  }

  /// Função para carregar as configurações dos campos
  void _loadConfig() async {
    try {
      // Atualiza o estado para indicar que as configurações estão sendo carregadas
      setState(() {
        _isLoading = true;
      });

      bool temCertificado = await doesCertificateExist();

      _pathController.text = temCertificado
          ? pathCertificado
          : "certificado.pfx não foi encontrado na pasta 'cert'";
      _dadosPfxController.text =
          await _acbrNFePlugin.configLerValor("DFe", "DadosPFX");
      _senhaController.text =
          await _acbrNFePlugin.configLerValor("DFe", "Senha");
      _numeroSerieController.text =
          await _acbrNFePlugin.configLerValor("DFe", "NumeroSerie");
    } catch (e) {
      debugPrint('Erro ao carregar configurações: $e');

      // Exibe uma mensagem de erro caso não consiga carregar as configurações
      if (mounted) {
        ScaffoldMessenger.of(context).showSnackBar(
          SnackBar(
              content: Text('Erro ao carregar configurações: Checar logs.')),
        );
      }
    } finally {
      // Atualiza o estado para indicar que o carregamento foi concluído
      setState(() {
        _isLoading = false;
      });
    }
  }

  /// Função para salvar as configurações
  void _onSave() async {
    String messageSnackBar = '';
    try {
      // Atualiza o estado para indicar que as configurações estão sendo salvas
      setState(() {
        _isLoading = true;
      });

      await _acbrNFePlugin.configGravarValor(
          "DFe", "DadosPFX", _dadosPfxController.text);
      await _acbrNFePlugin.configGravarValor(
          "DFe", "Senha", _senhaController.text);
      await _acbrNFePlugin.configGravarValor(
          "DFe", "NumeroSerie", _numeroSerieController.text);

      _acbrNFePlugin.configGravar();

      messageSnackBar = 'Configurações salvas com sucesso!';
    } catch (e) {
      messageSnackBar = 'Erro ao salvar configurações: Checar logs.';
      debugPrint("Erro ao salvar configurações: $e");
    } finally {
      // Atualiza o estado para indicar que o carregamento foi concluído
      setState(() {
        _isLoading = false;
      });

      // Exibe uma mensagem de sucesso ou erro
      if (mounted) {
        ScaffoldMessenger.of(context).showSnackBar(
          SnackBar(content: Text(messageSnackBar)),
        );
      }
    }
  }

  /// Função para copiar o certificado para a pasta "cert"
  void _copyCert() async {
    try {
      // 1. Abre o seletor de arquivos
      FilePickerResult? result = await FilePicker.platform.pickFiles(
        type: FileType.custom,
        allowedExtensions: ['pfx'],
      );

      if (result != null && result.files.single.path != null) {
        File sourceFile = File(result.files.single.path!);
        String fileName = result.files.single.name;

        // 2. Obtem o diretório de arquivos internos do aplicativo
        String appDir = await getAppDirByPlatform();

        // 3. Define o caminho da pasta de destino "cert"
        Directory certDir = Directory('$appDir/cert');

        // 4. Cria a pasta "cert" se não existir
        if (!await certDir.exists()) {
          await certDir.create(recursive: true);
        }

        // 5. Define o caminho completo do arquivo de destino
        String destinationPath = '${certDir.path}/certificado.pfx';
        File destinationFile = File(destinationPath);

        // 6. Copia o arquivo
        await sourceFile.copy(destinationFile.path);

        _loadConfig();
        // Arquivo copiado com sucesso
        if (mounted) {
          ScaffoldMessenger.of(context).showSnackBar(
            SnackBar(
              content: Text(
                  'Arquivo PFX "$fileName" copiado para "${certDir.path}" com sucesso!'),
              backgroundColor: Colors.green,
            ),
          );
        }
      } else {
        // Usuário cancelou a seleção
        if (mounted) {
          ScaffoldMessenger.of(context).showSnackBar(
            const SnackBar(
              content: Text('Seleção de arquivo PFX cancelada.'),
              backgroundColor: Colors.orange,
            ),
          );
        }
      }
    } catch (e) {
      // Lida com erros (permissões negadas, erro de cópia, etc.)
      if (mounted) {
        ScaffoldMessenger.of(context).showSnackBar(
          SnackBar(
            content: Text('Erro ao copiar arquivo PFX: Checar logs.'),
            backgroundColor: Colors.red,
          ),
        );
      }
      debugPrint('Erro ao copiar arquivo PFX: $e');
    }
  }

  /// Função para obter os certificados disponíveis
  void _obterCertificados() async {
    String result = '';
    try {
      result = await _acbrNFePlugin.obterCertificados();
      debugPrint('Resultado de obterCertificados: "$result"');
    } catch (e) {
      result = 'ERRO: Erro ao obter certificados.\nChecar logs.';
      debugPrint('Erro ao obterCertificados: $e');
    } finally {
      _retornoController.text = result;
    }
  }

  @override
  Widget build(BuildContext context) {
    // Exibe um indicador de carregamento enquanto as configurações estão sendo carregadas
    if (_isLoading) {
      return const Center(
          child: CircularProgressIndicator(
        color: AppColors.primary,
      ));
    }

    // Retorna a interface de configurações de certificados
    return SingleChildScrollView(
      child: Padding(
        padding: const EdgeInsets.all(12.0),
        child: Column(
          spacing: 15.0,
          children: [
            Row(
              crossAxisAlignment: CrossAxisAlignment.center,
              children: [
                Expanded(
                  child: TextFormField(
                    controller: _pathController,
                    readOnly: true,
                    decoration: const InputDecoration(
                      labelText: 'Caminho do Certificado',
                      border: OutlineInputBorder(),
                    ),
                  ),
                ),
                const SizedBox(width: 8.0),
                IconButton(
                  icon: const Icon(Icons.file_upload),
                  onPressed: () => _copyCert(),
                ),
              ],
            ),
            TextFormField(
              controller: _dadosPfxController,
              decoration: const InputDecoration(
                labelText: 'Dados PFX',
                border: OutlineInputBorder(),
              ),
            ),
            TextFormField(
              controller: _senhaController,
              decoration: const InputDecoration(
                labelText: 'Senha',
                border: OutlineInputBorder(),
              ),
            ),
            TextFormField(
              controller: _numeroSerieController,
              decoration: const InputDecoration(
                labelText: 'Número de Série',
                border: OutlineInputBorder(),
              ),
            ),
            Divider(height: 2.0),
            TextFormField(
              controller: _retornoController,
              readOnly: true,
              minLines: 4,
              maxLines: null,
              decoration: const InputDecoration(
                labelText: 'Retorno',
                border: OutlineInputBorder(),
              ),
            ),
            SizedBox(
              width: double.infinity,
              child: ElevatedButton(
                onPressed: _obterCertificados,
                child: const Text(
                  'Obter Certificados',
                  style: TextStyle(fontWeight: FontWeight.bold),
                ),
              ),
            ),
            Divider(height: 2.0),
            const SizedBox(height: 10.0),
            ConfigButtons(onSave: _onSave),
          ],
        ),
      ),
    );
  }
}
