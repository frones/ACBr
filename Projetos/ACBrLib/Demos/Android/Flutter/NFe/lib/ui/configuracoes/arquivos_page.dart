import 'package:acbrlibnfe/ui/configuracoes/widgets/config_buttons.dart';
import 'package:acbrlibnfe/ui/configuracoes/widgets/switch_text.dart';
import 'package:flutter/material.dart';

import '../../plugin/acbrnfe_plugin.dart';
import '../../utils/acbrlib_nfe_helper.dart';
import '../_core/app_colors.dart';

/// Página de Configurações de Arquivos
class ArquivosPage extends StatefulWidget {
  const ArquivosPage({super.key});

  @override
  State<ArquivosPage> createState() => _ArquivosPageState();
}

class _ArquivosPageState extends State<ArquivosPage> {
  /// Instância do plugin
  late ACBrNFePlugin _acbrNFePlugin;

  // Variáveis para armazenar as configurações de switch
  bool _salvarArquivosPastaSeparada = false;
  bool _criarPastaMensalmente = false;
  bool _adicionarLiteral = false;
  bool _salvarDocumentos = false;
  bool _salvarArqEventos = false;
  bool _separarPorCNPJ = false;
  bool _separarPorModelo = false;

  // Controllers para os campos de texto
  final TextEditingController _pathArqNfe = TextEditingController();
  final TextEditingController _pathArqInutilizacao = TextEditingController();
  final TextEditingController _pathArqEventos = TextEditingController();

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
    _pathArqNfe.dispose();
    _pathArqInutilizacao.dispose();
    _pathArqEventos.dispose();
    super.dispose();
  }

  /// Função para carregar as configurações dos campos
  void _loadConfig() async {
    try {
      // Atualiza o estado para indicar que as configurações estão sendo carregadas
      setState(() {
        _isLoading = true;
      });

      _salvarArquivosPastaSeparada =
          await _acbrNFePlugin.configLerValor("NFe", "SalvarGer").then(
                (value) => value == '1',
              );
      _criarPastaMensalmente =
          await _acbrNFePlugin.configLerValor("NFe", "SepararPorMes").then(
                (value) => value == '1',
              );
      _adicionarLiteral =
          await _acbrNFePlugin.configLerValor("NFe", "AdicionarLiteral").then(
                (value) => value == '1',
              );
      _salvarDocumentos =
          await _acbrNFePlugin.configLerValor("NFe", "EmissaoPathNFe").then(
                (value) => value == '1',
              );
      _salvarArqEventos =
          await _acbrNFePlugin.configLerValor("NFe", "SalvarArq").then(
                (value) => value == '1',
              );
      _separarPorCNPJ =
          await _acbrNFePlugin.configLerValor("NFe", "SepararPorCNPJ").then(
                (value) => value == '1',
              );
      _separarPorModelo =
          await _acbrNFePlugin.configLerValor("NFe", "SepararPorModelo").then(
                (value) => value == '1',
              );

      _pathArqNfe.text = await _acbrNFePlugin.configLerValor("NFe", "PathNFe");
      _pathArqInutilizacao.text =
          await _acbrNFePlugin.configLerValor("NFe", "PathInu");
      _pathArqEventos.text =
          await _acbrNFePlugin.configLerValor("NFe", "PathEvento");
    } catch (e) {
      debugPrint("Erro ao carregar configurações: $e");

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
        "NFe",
        "SalvarGer",
        _salvarArquivosPastaSeparada ? '1' : '0',
      );
      await _acbrNFePlugin.configGravarValor(
        "NFe",
        "SepararPorMes",
        _criarPastaMensalmente ? '1' : '0',
      );
      await _acbrNFePlugin.configGravarValor(
        "NFe",
        "AdicionarLiteral",
        _adicionarLiteral ? '1' : '0',
      );
      await _acbrNFePlugin.configGravarValor(
        "NFe",
        "EmissaoPathNFe",
        _salvarDocumentos ? '1' : '0',
      );
      await _acbrNFePlugin.configGravarValor(
        "NFe",
        "SalvarArq",
        _salvarArqEventos ? '1' : '0',
      );
      await _acbrNFePlugin.configGravarValor(
        "NFe",
        "SepararPorCNPJ",
        _separarPorCNPJ ? '1' : '0',
      );
      await _acbrNFePlugin.configGravarValor(
        "NFe",
        "SepararPorModelo",
        _separarPorModelo ? '1' : '0',
      );

      await _acbrNFePlugin.configGravarValor(
        "NFe",
        "PathNFe",
        _pathArqNfe.text,
      );
      await _acbrNFePlugin.configGravarValor(
        "NFe",
        "PathInu",
        _pathArqInutilizacao.text,
      );
      await _acbrNFePlugin.configGravarValor(
        "NFe",
        "PathEvento",
        _pathArqEventos.text,
      );

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

    // Retorna a interface de configurações de arquivos
    return SingleChildScrollView(
      child: Padding(
        padding: const EdgeInsets.all(12.0),
        child: Column(
          spacing: 15.0,
          children: [
            SwitchText(
              value: _salvarArquivosPastaSeparada,
              text: 'Salvar arquivos em pastas separadas',
              onChanged: (value) {
                setState(() {
                  _salvarArquivosPastaSeparada = value;
                });
              },
            ),
            SwitchText(
              value: _criarPastaMensalmente,
              text: 'Criar pastas mensalmente',
              onChanged: (value) {
                setState(() {
                  _criarPastaMensalmente = value;
                });
              },
            ),
            SwitchText(
              value: _adicionarLiteral,
              text: 'Adicionar Literal no nome das pastas',
              onChanged: (value) {
                setState(() {
                  _adicionarLiteral = value;
                });
              },
            ),
            SwitchText(
              value: _salvarDocumentos,
              text: 'Salvar documentos pelo campo Data de Emissão',
              onChanged: (value) {
                setState(() {
                  _salvarDocumentos = value;
                });
              },
            ),
            SwitchText(
              value: _salvarArqEventos,
              text: 'Salvar arquivos de Eventos',
              onChanged: (value) {
                setState(() {
                  _salvarArqEventos = value;
                });
              },
            ),
            SwitchText(
              value: _separarPorCNPJ,
              text: 'Separar arquivos pelo CNPJ do Certificado',
              onChanged: (value) {
                setState(() {
                  _separarPorCNPJ = value;
                });
              },
            ),
            SwitchText(
              value: _separarPorModelo,
              text: 'Separar arquivos pelo Modelo do Documento',
              onChanged: (value) {
                setState(() {
                  _separarPorModelo = value;
                });
              },
            ),
            TextFormField(
              controller: _pathArqNfe,
              decoration: const InputDecoration(
                labelText: 'Pasta Arquivos NFe',
                border: OutlineInputBorder(),
              ),
            ),
            TextFormField(
              controller: _pathArqInutilizacao,
              decoration: const InputDecoration(
                labelText: 'Pasta Arquivos Inutilização',
                border: OutlineInputBorder(),
              ),
            ),
            TextFormField(
              controller: _pathArqEventos,
              decoration: const InputDecoration(
                labelText: 'Pasta Arquivos Eventos',
                border: OutlineInputBorder(),
              ),
            ),
            const SizedBox(height: 10.0),
            ConfigButtons(onSave: _onSave),
          ],
        ),
      ),
    );
  }
}
