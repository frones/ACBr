import 'package:acbrlibnfe/ui/configuracoes/widgets/config_buttons.dart';
import 'package:acbrlibnfe/ui/configuracoes/widgets/switch_text.dart';
import 'package:flutter/material.dart';

import '../../plugin/acbrnfe_plugin.dart';
import '../../utils/acbrlib_nfe_helper.dart';
import '../_core/app_colors.dart';

/// Página de Configurações de Email
class EmailPage extends StatefulWidget {
  const EmailPage({super.key});

  @override
  State<EmailPage> createState() => _EmailPageState();
}

class _EmailPageState extends State<EmailPage> {
  /// Instância do plugin
  late ACBrNFePlugin _acbrNFePlugin;

  // Variáveis para armazenar as configurações de switch
  bool _sslAtivo = false;
  bool _tlsAtivo = false;

  // Controllers para os campos de texto
  final TextEditingController _nomeController = TextEditingController();
  final TextEditingController _emailController = TextEditingController();
  final TextEditingController _hostController = TextEditingController();
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
    _nomeController.dispose();
    _emailController.dispose();
    _hostController.dispose();
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

      _nomeController.text =
          await _acbrNFePlugin.configLerValor("Email", "Nome");
      _emailController.text =
          await _acbrNFePlugin.configLerValor("Email", "Conta");
      _usuarioController.text =
          await _acbrNFePlugin.configLerValor("Email", "Usuario");
      _senhaController.text =
          await _acbrNFePlugin.configLerValor("Email", "Senha");
      _hostController.text =
          await _acbrNFePlugin.configLerValor("Email", "Servidor");
      _portaController.text =
          await _acbrNFePlugin.configLerValor("Email", "Porta");

      _sslAtivo = await _acbrNFePlugin.configLerValor("Email", "SSL").then(
            (value) => value == '1',
          );

      _tlsAtivo = await _acbrNFePlugin.configLerValor("Email", "TLS").then(
            (value) => value == '1',
          );
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
          "Email", "Nome", _nomeController.text);
      await _acbrNFePlugin.configGravarValor(
          "Email", "Conta", _emailController.text);
      await _acbrNFePlugin.configGravarValor(
          "Email", "Usuario", _usuarioController.text);
      await _acbrNFePlugin.configGravarValor(
          "Email", "Senha", _senhaController.text);
      await _acbrNFePlugin.configGravarValor(
          "Email", "Servidor", _hostController.text);
      await _acbrNFePlugin.configGravarValor(
          "Email", "Porta", _portaController.text);

      await _acbrNFePlugin.configGravarValor(
        "Email",
        "SSL",
        _sslAtivo ? '1' : '0',
      );
      await _acbrNFePlugin.configGravarValor(
        "Email",
        "TLS",
        _tlsAtivo ? '1' : '0',
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

    // Retorna a interface de configurações de email
    return SingleChildScrollView(
      child: Padding(
        padding: const EdgeInsets.all(12.0),
        child: Column(
          spacing: 15.0,
          children: [
            TextFormField(
              controller: _nomeController,
              decoration: const InputDecoration(
                labelText: 'Nome',
                border: OutlineInputBorder(),
              ),
            ),
            TextFormField(
              controller: _emailController,
              decoration: const InputDecoration(
                labelText: 'E-mail',
                border: OutlineInputBorder(),
              ),
            ),
            TextFormField(
              controller: _usuarioController,
              decoration: const InputDecoration(
                labelText: 'Usuário',
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
              controller: _hostController,
              decoration: const InputDecoration(
                labelText: 'Host SMTP',
                border: OutlineInputBorder(),
              ),
            ),
            TextFormField(
              controller: _portaController,
              decoration: const InputDecoration(
                labelText: 'Porta',
                border: OutlineInputBorder(),
              ),
              keyboardType: TextInputType.number,
            ),
            SwitchText(
              value: _sslAtivo,
              text: 'SSL',
              onChanged: (value) {
                setState(() {
                  _sslAtivo = value;
                });
              },
            ),
            SwitchText(
              value: _tlsAtivo,
              text: 'TLS',
              onChanged: (value) {
                setState(() {
                  _tlsAtivo = value;
                });
              },
            ),
            const SizedBox(height: 10.0),
            ConfigButtons(onSave: _onSave),
          ],
        ),
      ),
    );
  }
}
