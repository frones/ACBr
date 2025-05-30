import 'package:acbrlibnfe/ui/nfe/nfe_utils.dart';
import 'package:acbrlibnfe/ui/nfe/widgets/painel_resposta.dart';
import 'package:flutter/material.dart';

import '../../plugin/acbrnfe_plugin.dart';
import '../../utils/acbrlib_nfe_helper.dart';
import '../_core/app_colors.dart';

/// Página com comandos de Evento
class EventosPage extends StatefulWidget {
  const EventosPage({super.key});

  @override
  State<EventosPage> createState() => _EventosPageState();
}

class _EventosPageState extends State<EventosPage> {
  /// Instância do plugin
  late ACBrNFePlugin _acbrNFePlugin;

  /// Gerencia respostas dos comandos
  String? _response;

  // Controllers dos inputs
  final TextEditingController _justificativaController =
      TextEditingController();
  final TextEditingController _chaveController = TextEditingController();
  final TextEditingController _cnpjController = TextEditingController();

  final TextEditingController _eventoIniController = TextEditingController();

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
    _justificativaController.dispose();
    _chaveController.dispose();
    _cnpjController.dispose();
    _eventoIniController.dispose();
    _scrollController.dispose();
    super.dispose();
  }

  /// Função para chamar o comando de cancelar nfe
  void _onClickCancelarNFe() async {
    String result = "";
    try {
      result = await _acbrNFePlugin.cancelar(_chaveController.text,
          _justificativaController.text, _cnpjController.text, 1);
      debugPrint('Resultado de cancelar: "$result"');
    } catch (e) {
      result = "ERRO: Erro ao cancelar NFe.\nChecar logs.";
      debugPrint("Erro ao chamar cancelar: '$e'.");
    } finally {
      setState(() {
        _response = result;
      });
      NfeUtils.goToPainelResposta(_scrollController);
    }
  }

  /// Função para chamar o comando de carregar evento INI
  void _onClickCarregarEvento() async {
    String result = '';
    try {
      result =
          await _acbrNFePlugin.carregarEventoINI(_eventoIniController.text);
      debugPrint('Resultado de carregarEventoINI: "$result"');
    } catch (e) {
      result = "ERRO: Erro ao carregar evento.\nChecar logs.";
      debugPrint("Erro ao chamar carregarEventoINI: '$e'.");
    } finally {
      setState(() {
        _response = result;
      });
      NfeUtils.goToPainelResposta(_scrollController);
    }
  }

  /// Função para chamar o comando de enviar evento
  void _onClickEnviarEvento() async {
    String result = '';
    try {
      result = await _acbrNFePlugin.enviarEvento(1);
      debugPrint('Resultado de enviarEvento: "$result"');
    } catch (e) {
      result = "ERRO: Erro ao enviar evento NFe.\nChecar logs.";
      debugPrint("Erro ao chamar enviarEvento: '$e'.");
    } finally {
      setState(() {
        _response = result;
      });
      NfeUtils.goToPainelResposta(_scrollController);
    }
  }

  /// Função para chamar o comando de limpar lista de eventos
  void _onClickLimparListaEventos() async {
    String result = '';
    try {
      result = await _acbrNFePlugin.limparListaEventos();
      debugPrint('Resultado de limparListaEventos: "$result"');
    } catch (e) {
      result = "ERRO: Erro ao limpar lista de eventos.\nChecar logs.";
      debugPrint("Erro ao chamar limparListaEventos: '$e'.");
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
        padding: const EdgeInsets.all(12.0),
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

            ///  Seção Evento NFe INI
            ExpansionTile(
              shape: const Border(),
              title: Text('Eventos NFe INI',
                  style:
                      TextStyle(fontWeight: FontWeight.bold, fontSize: 18.0)),
              children: [
                Column(
                  spacing: 14.0,
                  children: [
                    SizedBox(height: 10.0),
                    TextFormField(
                      controller: _eventoIniController,
                      minLines: 12,
                      maxLines: 12,
                      decoration: InputDecoration(
                        labelText: 'Evento NFe INI',
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
                        onPressed: _onClickCarregarEvento,
                        child: const Text(
                          'Carregar Evento',
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
                        onPressed: _onClickEnviarEvento,
                        child: const Text(
                          'Enviar Evento',
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
                        onPressed: _onClickLimparListaEventos,
                        child: const Text(
                          'Limpar Lista de Eventos',
                          style: TextStyle(fontWeight: FontWeight.bold),
                        ),
                      ),
                    ),
                  ],
                )
              ],
            ),

            Divider(height: 2.0),

            /// Seção Cancelar NFe
            ExpansionTile(
              shape: const Border(),
              title: Text('Cancelar NFe',
                  style:
                      TextStyle(fontWeight: FontWeight.bold, fontSize: 18.0)),
              children: [
                Column(
                  spacing: 14.0,
                  children: [
                    SizedBox(height: 10.0),
                    TextFormField(
                      controller: _justificativaController,
                      decoration: InputDecoration(
                        labelText: 'Justificativa',
                        border: OutlineInputBorder(),
                      ),
                    ),
                    TextFormField(
                      controller: _chaveController,
                      decoration: InputDecoration(
                        labelText: 'Chave',
                        border: OutlineInputBorder(),
                      ),
                    ),
                    TextFormField(
                      controller: _cnpjController,
                      keyboardType: TextInputType.number,
                      decoration: InputDecoration(
                        labelText: 'CNPJ',
                        border: OutlineInputBorder(),
                      ),
                    ),
                    SizedBox(
                      width: double.infinity,
                      child: ElevatedButton(
                        style: ElevatedButton.styleFrom(
                          backgroundColor: AppColors.secondary,
                        ),
                        onPressed: _onClickCancelarNFe,
                        child: const Text(
                          'Cancelar NFe',
                          style: TextStyle(fontWeight: FontWeight.bold),
                        ),
                      ),
                    ),
                  ],
                )
              ],
            )
          ],
        ),
      ),
    );
  }
}
