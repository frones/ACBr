import 'package:acbrlibnfe/ui/configuracoes/widgets/config_buttons.dart';
import 'package:acbrlibnfe/ui/configuracoes/widgets/dropdown_menu.dart';
import 'package:acbrlibnfe/ui/configuracoes/widgets/switch_text.dart';
import 'package:flutter/material.dart';

import '../../plugin/acbrnfe_plugin.dart';
import '../../utils/acbrlib_nfe_helper.dart';
import '../_core/app_colors.dart';

/// Página de Configurações Gerais
class GeralPage extends StatefulWidget {
  const GeralPage({super.key});

  @override
  State<GeralPage> createState() => _GeralPageState();
}

class _GeralPageState extends State<GeralPage> {
  /// Instância do plugin
  late ACBrNFePlugin _acbrNFePlugin;

  /// Lista de opções para o tipo de emissão
  final List<String> _tipoEmissao = [
    "Emissão normal (não em contingência)",
    "Contingência FS-IA, com impressão do DANFE em formulário de segurança",
    "Contingência SCAN(Sistema de Contingência do Ambiente Nacional)",
    "Contingência DPEC(Declaração Prévia da Emissão em Contingência)",
    "Contingência FS-DA, com impressão do DANFE em formulário de segurança",
    "Contingência SVC-AN(SEFAZ Virtual de Contingência do AN)",
    "Contingência SVC-RS(SEFAZ Virtual de Contingência do RS)",
    "Contingência SVC-SP(SEFAZ Virtual de Contingência de SP)",
    "Contingência off-line da NFC-e(as demais opções de contingência são válidas também para a NFC-e)"
  ];

  /// Lista de opções para o modelo do documento fiscal
  final List<String> _modeloDF = [
    "NFe - Nota Fiscal Eletrônica",
    "NFCe - Nota Fiscal do Consumidor Eletrônica"
  ];

  // Variáveis para armazenar as opções selecionadas
  late String _selectedTipoEmissao;
  late String _selectedModeloDF;

  // Variáveis para armazenar as configurações de switch
  bool _atualizarXml = false;
  bool _exibirErroSchemas = false;
  bool _retirarAcentos = false;
  bool _salvarArquivos = false;

  // Controllers para os campos de texto
  final TextEditingController _formatoAlertaController =
      TextEditingController();
  final TextEditingController _pathLogsController = TextEditingController();
  final TextEditingController _pathSchemasController = TextEditingController();
  final TextEditingController _idTokenController = TextEditingController();
  final TextEditingController _tokenController = TextEditingController();

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
    _formatoAlertaController.dispose();
    _pathLogsController.dispose();
    _pathSchemasController.dispose();
    _idTokenController.dispose();
    _tokenController.dispose();
    super.dispose();
  }

  /// Função para carregar as configurações dos campos
  void _loadConfig() async {
    try {
      // Atualiza o estado para indicar que as configurações estão sendo carregadas
      setState(() {
        _isLoading = true;
      });

      _selectedTipoEmissao = _tipoEmissao.first;
      _selectedModeloDF = _modeloDF.first;

      _atualizarXml = await _acbrNFePlugin
          .configLerValor("NFe", "AtualizarXMLCancelado")
          .then((value) => value == '1');

      _exibirErroSchemas = await _acbrNFePlugin
          .configLerValor("NFe", "ExibirErroSchema")
          .then((value) => value == '1');

      _formatoAlertaController.text =
          await _acbrNFePlugin.configLerValor("NFe", "FormatoAlerta");

      _selectedTipoEmissao = await _acbrNFePlugin
          .configLerValor(
            "NFe",
            "FormaEmissao",
          )
          .then((value) => _tipoEmissao[int.parse(value)]);

      _selectedModeloDF = await _acbrNFePlugin
          .configLerValor(
            "NFe",
            "ModeloDF",
          )
          .then((value) => _modeloDF[int.parse(value)]);

      _retirarAcentos = await _acbrNFePlugin
          .configLerValor("NFe", "RetirarAcentos")
          .then((value) => value == '1');

      _salvarArquivos = await _acbrNFePlugin
          .configLerValor("NFe", "SalvarWS")
          .then((value) => value == '1');

      _pathLogsController.text =
          await _acbrNFePlugin.configLerValor("NFe", "PathSalvar");
      _pathSchemasController.text =
          await _acbrNFePlugin.configLerValor("NFe", "PathSchemas");

      _idTokenController.text =
          await _acbrNFePlugin.configLerValor("NFe", "IdCSC");
      _tokenController.text = await _acbrNFePlugin.configLerValor("NFe", "CSC");
    } catch (e) {
      debugPrint('Erro ao carregar configurações: $e');

      // Exibe uma mensagem de erro caso não consiga carregar as configurações
      if (mounted) {
        ScaffoldMessenger.of(context).showSnackBar(
          const SnackBar(
              content: Text('Erro ao carregar configurações: Checar logs')),
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
          "NFe", "AtualizarXMLCancelado", _atualizarXml ? '1' : '0');
      await _acbrNFePlugin.configGravarValor(
          "NFe", "ExibirErroSchema", _exibirErroSchemas ? '1' : '0');
      await _acbrNFePlugin.configGravarValor(
          "NFe", "FormatoAlerta", _formatoAlertaController.text);
      await _acbrNFePlugin.configGravarValor("NFe", "FormaEmissao",
          _tipoEmissao.indexOf(_selectedTipoEmissao).toString());
      await _acbrNFePlugin.configGravarValor(
          "NFe", "ModeloDF", _modeloDF.indexOf(_selectedModeloDF).toString());
      await _acbrNFePlugin.configGravarValor(
          "NFe", "RetirarAcentos", _retirarAcentos ? '1' : '0');
      await _acbrNFePlugin.configGravarValor(
          "NFe", "SalvarWS", _salvarArquivos ? '1' : '0');
      await _acbrNFePlugin.configGravarValor(
          "NFe", "PathSalvar", _pathLogsController.text);
      await _acbrNFePlugin.configGravarValor(
          "NFe", "PathSchemas", _pathSchemasController.text);
      await _acbrNFePlugin.configGravarValor(
          "NFe", "IdCSC", _idTokenController.text);
      await _acbrNFePlugin.configGravarValor(
          "NFe", "CSC", _tokenController.text);

      await _acbrNFePlugin.configGravar();

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

  @override
  Widget build(BuildContext context) {
    // Exibe um indicador de carregamento enquanto as configurações estão sendo carregadas
    if (_isLoading) {
      return const Center(
          child: CircularProgressIndicator(
        color: AppColors.primary,
      ));
    }

    // Retorna a interface de configurações gerais
    return SingleChildScrollView(
      padding: const EdgeInsets.all(12.0),
      child: Column(spacing: 15.0, children: [
        SwitchText(
          value: _atualizarXml,
          text: 'Atualizar XML',
          onChanged: (value) {
            setState(() {
              _atualizarXml = value;
            });
          },
        ),
        SwitchText(
          value: _exibirErroSchemas,
          text: 'Exibir erro Schemas',
          onChanged: (value) {
            setState(() {
              _exibirErroSchemas = value;
            });
          },
        ),
        TextFormField(
          controller: _formatoAlertaController,
          decoration: const InputDecoration(
            labelText: 'Formato Alerta',
            border: OutlineInputBorder(),
          ),
        ),
        MainDropdownMenu(
          text: 'Forma Emissão',
          initialValue: _selectedTipoEmissao,
          list: _tipoEmissao,
          onChanged: (value) {
            _selectedTipoEmissao = value;
          },
        ),
        MainDropdownMenu(
          text: 'Modelo do Documento Fiscal',
          initialValue: _selectedModeloDF,
          list: _modeloDF,
          onChanged: (value) {
            _selectedModeloDF = value;
          },
        ),
        SwitchText(
          value: _retirarAcentos,
          text: 'Retirar acentos dos XMLs enviados',
          onChanged: (value) {
            setState(() {
              _retirarAcentos = value;
            });
          },
        ),
        SwitchText(
          value: _salvarArquivos,
          text: 'Salvar arquivos de Envio e Resposta',
          onChanged: (value) {
            setState(() {
              _salvarArquivos = value;
            });
          },
        ),
        TextFormField(
          controller: _pathLogsController,
          readOnly: true,
          decoration: const InputDecoration(
            labelText: 'Pasta dos Logs',
            border: OutlineInputBorder(),
          ),
        ),
        TextFormField(
          controller: _pathSchemasController,
          readOnly: true,
          decoration: const InputDecoration(
            labelText: 'Pasta dos Schemas',
            border: OutlineInputBorder(),
          ),
        ),
        TextFormField(
          controller: _idTokenController,
          decoration: const InputDecoration(
            labelText: 'IdToken/IdCSC (Somente para NFC-e)',
            border: OutlineInputBorder(),
          ),
        ),
        TextFormField(
          controller: _tokenController,
          decoration: const InputDecoration(
            labelText: 'Token/CSC (Somente para NFC-e)',
            border: OutlineInputBorder(),
          ),
        ),
        const SizedBox(height: 10.0),
        ConfigButtons(onSave: _onSave)
      ]),
    );
  }
}
