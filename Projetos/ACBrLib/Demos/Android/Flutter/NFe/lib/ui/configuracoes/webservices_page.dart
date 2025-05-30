import 'package:acbrlibnfe/ui/configuracoes/widgets/config_buttons.dart';
import 'package:acbrlibnfe/ui/configuracoes/widgets/dropdown_menu.dart';
import 'package:acbrlibnfe/ui/configuracoes/widgets/switch_text.dart';
import 'package:flutter/material.dart';

import '../../plugin/acbrnfe_plugin.dart';
import '../../utils/acbrlib_nfe_helper.dart';
import '../_core/app_colors.dart';

/// Página de Configurações de WebServices
class WebservicesPage extends StatefulWidget {
  const WebservicesPage({super.key});

  @override
  State<WebservicesPage> createState() => _WebservicesPageState();
}

class _WebservicesPageState extends State<WebservicesPage> {
  /// Instância do plugin
  late ACBrNFePlugin _acbrNFePlugin;

  /// Lista de opções para uf de destino
  final List<String> _ufsDestino = [
    "Nenhum",
    "AC",
    "AL",
    "AP",
    "AM",
    "BA",
    "CE",
    "DF",
    "ES",
    "GO",
    "MA",
    "MT",
    "MS",
    "MG",
    "PA",
    "PB",
    "PR",
    "PE",
    "PI",
    "RJ",
    "RN",
    "RS",
    "RO",
    "RR",
    "SC",
    "SP",
    "SE",
    "TO"
  ];

  /// Lista de opções para ambiente
  final List<String> _ambientes = ['Produção', 'Homologação'];

  // Variáveis para armazenar as opções selecionadas
  late String _selectedUF;
  late String _selectedAmbiente;

  // Variáveis para armazenar as configurações de switch
  bool _visualizarMensagem = false;
  bool _salvarEnvelope = false;
  bool _ajustarAutomaticamente = false;

  // Controllers para os campos de texto
  final TextEditingController _timeoutController = TextEditingController();
  final TextEditingController _aguardarController = TextEditingController();
  final TextEditingController _tentativasController = TextEditingController();
  final TextEditingController _intervaloController = TextEditingController();
  final TextEditingController _servidorController = TextEditingController();
  final TextEditingController _portaController = TextEditingController();
  final TextEditingController _usuarioController = TextEditingController();
  final TextEditingController _senhaController = TextEditingController();

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
    _timeoutController.dispose();
    _aguardarController.dispose();
    _tentativasController.dispose();
    _intervaloController.dispose();
    _servidorController.dispose();
    _portaController.dispose();
    _usuarioController.dispose();
    _senhaController.dispose();
    super.dispose();
  }

  /// Função para carregar as configurações dos campos
  void _loadConfig() async {
    try {
      // Atualiza o estado para indicar que as configurações estão sendo carregadas
      setState(() {
        _isLoading = true;
      });

      _selectedUF = _ufsDestino.first;
      _selectedAmbiente = _ambientes[1];

      _selectedUF = await _acbrNFePlugin.configLerValor('DFe', 'UF');

      _timeoutController.text =
          await _acbrNFePlugin.configLerValor('NFe', 'TimeOut');

      _selectedAmbiente = await _acbrNFePlugin
          .configLerValor('NFe', 'Ambiente')
          .then((value) => _ambientes[int.parse(value)]);

      _visualizarMensagem = await _acbrNFePlugin
          .configLerValor('NFe', 'Visualizar')
          .then((value) => value == '1');

      _salvarEnvelope = await _acbrNFePlugin
          .configLerValor('NFe', 'SalvarWS')
          .then((value) => value == '1');

      _ajustarAutomaticamente = await _acbrNFePlugin
          .configLerValor('NFe', 'AjustaAguardaConsultaRet')
          .then((value) => value == '1');

      _aguardarController.text =
          await _acbrNFePlugin.configLerValor('NFe', 'AguardarConsultaRet');
      _tentativasController.text =
          await _acbrNFePlugin.configLerValor('NFe', 'Tentativas');

      _intervaloController.text =
          await _acbrNFePlugin.configLerValor('NFe', 'IntervaloTentativas');

      _servidorController.text =
          await _acbrNFePlugin.configLerValor('Proxy', 'Servidor');
      _portaController.text =
          await _acbrNFePlugin.configLerValor('Proxy', 'Porta');
      _usuarioController.text =
          await _acbrNFePlugin.configLerValor('Proxy', 'Usuario');
      _senhaController.text =
          await _acbrNFePlugin.configLerValor('Proxy', 'Senha');
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

      await _acbrNFePlugin.configGravarValor('DFe', 'UF', _selectedUF);

      await _acbrNFePlugin.configGravarValor(
          'NFe', 'TimeOut', _timeoutController.text);

      await _acbrNFePlugin.configGravarValor(
          'NFe', 'Ambiente', _ambientes.indexOf(_selectedAmbiente).toString());

      await _acbrNFePlugin.configGravarValor(
          'NFe', 'Visualizar', _visualizarMensagem ? '1' : '0');

      await _acbrNFePlugin.configGravarValor(
          'NFe', 'SalvarWS', _salvarEnvelope ? '1' : '0');

      await _acbrNFePlugin.configGravarValor('NFe', 'AjustaAguardaConsultaRet',
          _ajustarAutomaticamente ? '1' : '0');

      await _acbrNFePlugin.configGravarValor(
          'NFe', 'AguardarConsultaRet', _aguardarController.text);

      await _acbrNFePlugin.configGravarValor(
          'NFe', 'Tentativas', _tentativasController.text);

      await _acbrNFePlugin.configGravarValor(
          'NFe', 'IntervaloTentativas', _intervaloController.text);

      await _acbrNFePlugin.configGravarValor(
          'Proxy', 'Servidor', _servidorController.text);

      await _acbrNFePlugin.configGravarValor(
          'Proxy', 'Porta', _portaController.text);

      await _acbrNFePlugin.configGravarValor(
          'Proxy', 'Usuario', _usuarioController.text);

      await _acbrNFePlugin.configGravarValor(
          'Proxy', 'Senha', _senhaController.text);

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

    // Retorna a interface de configurações de WebServices
    return SingleChildScrollView(
      padding: const EdgeInsets.all(12.0),
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.start,
        spacing: 15.0,
        children: [
          MainDropdownMenu(
            initialValue: _selectedUF,
            list: _ufsDestino,
            text: 'UF Destino',
            onChanged: (value) {
              setState(() {
                _selectedUF = value;
              });
            },
          ),

          TextFormField(
            controller: _timeoutController,
            keyboardType: TextInputType.number,
            decoration: const InputDecoration(
                labelText: 'TimeOut', border: OutlineInputBorder()),
          ),

          MainDropdownMenu(
            initialValue: _selectedAmbiente,
            list: _ambientes,
            text: 'Ambiente',
            onChanged: (value) {
              setState(() {
                _selectedAmbiente = value;
              });
            },
          ),

          SwitchText(
            value: _visualizarMensagem,
            text: 'Visualizar Mensagem',
            onChanged: (value) {
              setState(() {
                _visualizarMensagem = value;
              });
            },
          ),

          SwitchText(
            value: _salvarEnvelope,
            text: 'Salvar envelope SOAP',
            onChanged: (value) {
              setState(() {
                _salvarEnvelope = value;
              });
            },
          ),

          // Retorno de Envio
          Text(
            'Retorno de Envio',
            style: TextStyle(fontWeight: FontWeight.bold, fontSize: 24.0),
            textAlign: TextAlign.start,
          ),
          SwitchText(
            value: _ajustarAutomaticamente,
            text: 'Ajustar automaticamente prop. Aguardar',
            onChanged: (value) {
              setState(() {
                _ajustarAutomaticamente = value;
              });
            },
          ),
          TextFormField(
            controller: _aguardarController,
            keyboardType: TextInputType.number,
            decoration: const InputDecoration(
                labelText: 'Aguardar', border: OutlineInputBorder()),
          ),
          TextFormField(
            controller: _tentativasController,
            keyboardType: TextInputType.number,
            decoration: const InputDecoration(
                labelText: 'Tentativas', border: OutlineInputBorder()),
          ),
          TextFormField(
            controller: _intervaloController,
            keyboardType: TextInputType.number,
            decoration: const InputDecoration(
                labelText: 'Intervalo', border: OutlineInputBorder()),
          ),

          // Proxy
          Text(
            'Proxy',
            style: TextStyle(fontWeight: FontWeight.bold, fontSize: 24.0),
            textAlign: TextAlign.start,
          ),
          TextFormField(
            controller: _servidorController,
            decoration: const InputDecoration(
                labelText: 'Servidor', border: OutlineInputBorder()),
          ),
          TextFormField(
            controller: _portaController,
            keyboardType: TextInputType.number,
            decoration: const InputDecoration(
                labelText: 'Porta', border: OutlineInputBorder()),
          ),
          TextFormField(
            controller: _usuarioController,
            decoration: const InputDecoration(
                labelText: 'Usuario', border: OutlineInputBorder()),
          ),
          TextFormField(
            controller: _senhaController,
            decoration: const InputDecoration(
                labelText: 'Senha', border: OutlineInputBorder()),
          ),
          const SizedBox(height: 10.0),
          ConfigButtons(onSave: _onSave)
        ],
      ),
    );
  }
}
