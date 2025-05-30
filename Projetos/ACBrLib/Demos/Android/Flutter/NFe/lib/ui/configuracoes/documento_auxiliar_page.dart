import 'package:acbrlibnfe/ui/configuracoes/widgets/config_buttons.dart';
import 'package:acbrlibnfe/ui/configuracoes/widgets/dropdown_menu.dart';
import 'package:acbrlibnfe/ui/configuracoes/widgets/switch_text.dart';
import 'package:flutter/material.dart';

import '../../plugin/acbrnfe_plugin.dart';
import '../../utils/acbrlib_nfe_helper.dart';
import '../_core/app_colors.dart';

/// Página de Configurações do Documento Auxiliar
class DocumentoAuxiliarPage extends StatefulWidget {
  const DocumentoAuxiliarPage({super.key});

  @override
  State<DocumentoAuxiliarPage> createState() => _DocumentoAuxiliarPageState();
}

class _DocumentoAuxiliarPageState extends State<DocumentoAuxiliarPage> {
  /// Instância do plugin
  late ACBrNFePlugin _acbrNFePlugin;

  /// Lista de opções para modelo pos printer
  final List<String> _modelos = [
    "ppTexto",
    "ppEscPosEpson",
    "ppEscBematech",
    "ppEscDaruma",
    "ppEscVox",
    "ppEscDiebold",
    "ppEscEpsonP2",
    "ppCustomPos",
    "ppEscPosStar",
    "ppEscZJiang",
    "ppEscGPrinter",
    "ppEscDatecs",
    "ppEscSunmi",
    "ppExterno"
  ];

  /// Lista de opções para página de código
  final List<String> _paginaDeCodigo = [
    "pcNone",
    "pc437",
    "pc850",
    "pc852",
    "pc860",
    "pcUTF8",
    "pc1252"
  ];

  // Variáveis para armazenar as opções selecionadas
  late String _selectedModelo;
  late String _selectedPagina;

  // Variáveis para armazenar as configurações de switch
  bool _controlePorta = false;
  bool _cortarPapel = false;
  bool _traduzirTags = false;
  bool _ignorarTags = false;

  // Controllers para os campos de texto
  final TextEditingController _logomarcaController = TextEditingController();
  final TextEditingController _portaController = TextEditingController();
  final TextEditingController _colunasController = TextEditingController();
  final TextEditingController _espacosController = TextEditingController();
  final TextEditingController _bufferController = TextEditingController();
  final TextEditingController _linhasPularController = TextEditingController();

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
    _logomarcaController.dispose();
    _portaController.dispose();
    _colunasController.dispose();
    _espacosController.dispose();
    _bufferController.dispose();
    _linhasPularController.dispose();
    super.dispose();
  }

  /// Função para carregar as configurações dos campos
  void _loadConfig() async {
    try {
      // Atualiza o estado para indicar que as configurações estão sendo carregadas
      setState(() {
        _isLoading = true;
      });

      _selectedModelo = _modelos.first;
      _selectedPagina = _paginaDeCodigo.first;

      _logomarcaController.text =
          await _acbrNFePlugin.configLerValor('DANFE', 'PathLogo');

      _selectedModelo = await _acbrNFePlugin
          .configLerValor('PosPrinter', 'Modelo')
          .then((value) => _modelos[int.parse(value)]);

      _selectedPagina = await _acbrNFePlugin
          .configLerValor('PosPrinter', 'PaginaDeCodigo')
          .then((value) => _paginaDeCodigo[int.parse(value)]);

      _portaController.text =
          await _acbrNFePlugin.configLerValor('PosPrinter', 'Porta');
      _colunasController.text = await _acbrNFePlugin.configLerValor(
          'PosPrinter', 'ColunasFonteNormal');
      _espacosController.text = await _acbrNFePlugin.configLerValor(
          'PosPrinter', 'EspacoEntreLinhas');
      _bufferController.text =
          await _acbrNFePlugin.configLerValor('PosPrinter', 'LinhasBuffer');
      _linhasPularController.text = await _acbrNFePlugin.configLerValor(
          'PosPrinter', 'LinhasEntreCupons');

      _controlePorta = await _acbrNFePlugin
          .configLerValor('PosPrinter', 'ControlePorta')
          .then((value) => value == '1');

      _cortarPapel = await _acbrNFePlugin
          .configLerValor('PosPrinter', 'CortaPapel')
          .then((value) => value == '1');

      _traduzirTags = await _acbrNFePlugin
          .configLerValor('PosPrinter', 'TraduzirTags')
          .then((value) => value == '1');

      _ignorarTags = await _acbrNFePlugin
          .configLerValor('PosPrinter', 'IgnorarTags')
          .then((value) => value == '1');
    } catch (e) {
      debugPrint('Erro ao carregar configurações: $e');

      // Exibe uma mensagem de erro caso não consiga carregar as configurações
      if (mounted) {
        ScaffoldMessenger.of(context).showSnackBar(
          const SnackBar(
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
          'DANFE', 'PathLogo', _logomarcaController.text);

      await _acbrNFePlugin.configGravarValor(
          'PosPrinter', 'Modelo', _modelos.indexOf(_selectedModelo).toString());

      await _acbrNFePlugin.configGravarValor('PosPrinter', 'PaginaDeCodigo',
          _paginaDeCodigo.indexOf(_selectedPagina).toString());

      await _acbrNFePlugin.configGravarValor(
          'PosPrinter', 'Porta', _portaController.text);

      await _acbrNFePlugin.configGravarValor(
          'PosPrinter', 'ColunasFonteNormal', _colunasController.text);

      await _acbrNFePlugin.configGravarValor(
          'PosPrinter', 'EspacoEntreLinhas', _espacosController.text);

      await _acbrNFePlugin.configGravarValor(
          'PosPrinter', 'LinhasBuffer', _bufferController.text);

      await _acbrNFePlugin.configGravarValor(
          'PosPrinter', 'LinhasEntreCupons', _linhasPularController.text);

      await _acbrNFePlugin.configGravarValor(
          'PosPrinter', 'ControlePorta', _controlePorta ? '1' : '0');

      await _acbrNFePlugin.configGravarValor(
          'PosPrinter', 'CortaPapel', _cortarPapel ? '1' : '0');

      await _acbrNFePlugin.configGravarValor(
          'PosPrinter', 'TraduzirTags', _traduzirTags ? '1' : '0');

      await _acbrNFePlugin.configGravarValor(
          'PosPrinter', 'IgnorarTags', _ignorarTags ? '1' : '0');

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

    // Retorna a interface de configurações do documento auxiliar
    return SingleChildScrollView(
      padding: const EdgeInsets.all(12.0),
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.start,
        spacing: 15.0,
        children: [
          TextFormField(
            controller: _logomarcaController,
            decoration: InputDecoration(
                labelText: 'Logomarca', border: OutlineInputBorder()),
          ),

          // PosPrinter
          Text(
            'PosPrinter',
            style: TextStyle(fontWeight: FontWeight.bold, fontSize: 24.0),
            textAlign: TextAlign.start,
          ),
          MainDropdownMenu(
            text: 'Modelo',
            initialValue: _selectedModelo,
            list: _modelos,
            onChanged: (value) {
              _selectedModelo = value;
            },
          ),
          MainDropdownMenu(
            text: 'Pág. Código',
            initialValue: _selectedPagina,
            list: _paginaDeCodigo,
            onChanged: (value) {
              _selectedPagina = value;
            },
          ),
          TextFormField(
            controller: _portaController,
            decoration: InputDecoration(
                labelText: 'Porta', border: OutlineInputBorder()),
          ),
          TextFormField(
            controller: _colunasController,
            decoration: InputDecoration(
                labelText: 'Colunas', border: OutlineInputBorder()),
          ),
          TextFormField(
            controller: _espacosController,
            decoration: InputDecoration(
                labelText: 'Espaços', border: OutlineInputBorder()),
          ),
          TextFormField(
            controller: _bufferController,
            decoration: InputDecoration(
                labelText: 'Buffer', border: OutlineInputBorder()),
          ),
          TextFormField(
            controller: _linhasPularController,
            decoration: InputDecoration(
                labelText: 'Linhas Pular', border: OutlineInputBorder()),
          ),
          SwitchText(
            value: _controlePorta,
            text: 'Controle Porta',
            onChanged: (value) {
              setState(() {
                _controlePorta = value;
              });
            },
          ),
          SwitchText(
            value: _cortarPapel,
            text: 'Cortar Papel',
            onChanged: (value) {
              setState(() {
                _cortarPapel = value;
              });
            },
          ),
          SwitchText(
            value: _traduzirTags,
            text: 'Traduzir Tags',
            onChanged: (value) {
              setState(() {
                _traduzirTags = value;
              });
            },
          ),
          SwitchText(
            value: _ignorarTags,
            text: 'Ignorar Tags',
            onChanged: (value) {
              setState(() {
                _ignorarTags = value;
              });
            },
          ),
          const SizedBox(height: 10.0),
          ConfigButtons(onSave: _onSave)
        ],
      ),
    );
  }
}
