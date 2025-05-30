import 'package:acbrlibnfe/ui/nfe/nfe_utils.dart';
import 'package:acbrlibnfe/ui/nfe/widgets/painel_resposta.dart';
import 'package:flutter/material.dart';

import '../../plugin/acbrnfe_plugin.dart';
import '../../utils/acbrlib_nfe_helper.dart';
import '../_core/app_colors.dart';

/// Página com comandos de Distribuição DFe
class DistribuicaoDfePage extends StatefulWidget {
  const DistribuicaoDfePage({super.key});

  @override
  State<DistribuicaoDfePage> createState() => _DistribuicaoDfePageState();
}

class _DistribuicaoDfePageState extends State<DistribuicaoDfePage> {
  /// Instância do plugin
  late ACBrNFePlugin _acbrNFePlugin;

  /// Gerencia respostas dos comandos
  String? _response;

  // Controllers dos inputs
  final TextEditingController _ufChaveController = TextEditingController();
  final TextEditingController _cnpjChaveController = TextEditingController();
  final TextEditingController _chaveController = TextEditingController();

  final TextEditingController _ufNsuController = TextEditingController();
  final TextEditingController _cnpjNsuController = TextEditingController();
  final TextEditingController _nsuController = TextEditingController();

  final TextEditingController _ufUltNsuController = TextEditingController();
  final TextEditingController _cnpjUltNsuController = TextEditingController();
  final TextEditingController _ultNsuController = TextEditingController();

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
    _ufChaveController.dispose();
    _cnpjChaveController.dispose();
    _chaveController.dispose();
    _ufNsuController.dispose();
    _cnpjNsuController.dispose();
    _nsuController.dispose();
    _ufUltNsuController.dispose();
    _cnpjUltNsuController.dispose();
    _ultNsuController.dispose();
    _scrollController.dispose();
    super.dispose();
  }

  /// Função para enviar o comando de distribuicao dfe por chave
  void _onClickDfePorChave() async {
    String result = "";
    try {
      result = await _acbrNFePlugin.distribuicaoDFePorChave(
        _ufChaveController.text,
        _cnpjChaveController.text,
        _chaveController.text,
      );
      debugPrint('Resultado de distribuicaoDFePorChave: "$result"');
    } catch (e) {
      result = "ERRO: Erro ao chamar Distribuição DFe Por Chave.\nChecar logs.";
      debugPrint("Erro ao chamar distribuicaoDFePorChave: '$e'.");
    } finally {
      setState(() {
        _response = result;
      });
      NfeUtils.goToPainelResposta(_scrollController);
    }
  }

  /// Função para enviar o comando de distribuicao dfe por NSU
  void _onClickDfePorNsu() async {
    String result = "";
    try {
      result = await _acbrNFePlugin.distribuicaoDFePorNSU(
        _ufNsuController.text,
        _cnpjNsuController.text,
        _nsuController.text,
      );
      debugPrint('Resultado de distribuicaoDFePorNSU: "$result"');
    } catch (e) {
      result = "ERRO: Erro ao chamar Distribuição DFe Por NSU.\nChecar logs.";
      debugPrint("Erro ao chamar distribuicaoDFePorNSU: '$e'.");
    } finally {
      setState(() {
        _response = result;
      });
      NfeUtils.goToPainelResposta(_scrollController);
    }
  }

  /// Função para enviar o comando de distribuicao dfe por Ult. NSU
  void _onClickDfePorUltNsu() async {
    String result = "";
    try {
      result = await _acbrNFePlugin.distribuicaoDFePorUltNSU(
        _ufUltNsuController.text,
        _cnpjUltNsuController.text,
        _ultNsuController.text,
      );
      debugPrint('Resultado de distribuicaoDFePorUltNSU: "$result"');
    } catch (e) {
      result =
          "ERRO: Erro ao chamar Distribuição DFe Por Ult. NSU.\nChecar logs.";
      debugPrint("Erro ao chamar distribuicaoDFePorUltNSU: '$e'.");
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

            /// Seção Por Chave
            ExpansionTile(
              shape: const Border(),
              title: Text('Por Chave',
                  style:
                      TextStyle(fontWeight: FontWeight.bold, fontSize: 18.0)),
              children: [
                Column(
                  spacing: 14.0,
                  children: [
                    SizedBox(height: 10.0),
                    TextFormField(
                      controller: _ufChaveController,
                      decoration: InputDecoration(
                        labelText: 'UF',
                        border: OutlineInputBorder(),
                      ),
                    ),
                    TextFormField(
                      controller: _cnpjChaveController,
                      keyboardType: TextInputType.number,
                      decoration: InputDecoration(
                        labelText: 'CNPJ',
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
                    SizedBox(
                      width: double.infinity,
                      child: ElevatedButton(
                        style: ElevatedButton.styleFrom(
                          backgroundColor: AppColors.secondary,
                        ),
                        onPressed: _onClickDfePorChave,
                        child: const Text(
                          'Por Chave',
                          style: TextStyle(fontWeight: FontWeight.bold),
                        ),
                      ),
                    ),
                  ],
                )
              ],
            ),

            /// Seção Por NSU
            ExpansionTile(
              shape: const Border(),
              title: Text('Por NSU',
                  style:
                      TextStyle(fontWeight: FontWeight.bold, fontSize: 18.0)),
              children: [
                Column(
                  spacing: 14.0,
                  children: [
                    SizedBox(height: 10.0),
                    TextFormField(
                      controller: _ufNsuController,
                      decoration: InputDecoration(
                        labelText: 'UF',
                        border: OutlineInputBorder(),
                      ),
                    ),
                    TextFormField(
                      controller: _cnpjNsuController,
                      keyboardType: TextInputType.number,
                      decoration: InputDecoration(
                        labelText: 'CNPJ',
                        border: OutlineInputBorder(),
                      ),
                    ),
                    TextFormField(
                      controller: _nsuController,
                      decoration: InputDecoration(
                        labelText: 'NSU',
                        border: OutlineInputBorder(),
                      ),
                    ),
                    SizedBox(
                      width: double.infinity,
                      child: ElevatedButton(
                        style: ElevatedButton.styleFrom(
                          backgroundColor: AppColors.secondary,
                        ),
                        onPressed: _onClickDfePorNsu,
                        child: const Text(
                          'Por NSU',
                          style: TextStyle(fontWeight: FontWeight.bold),
                        ),
                      ),
                    ),
                  ],
                )
              ],
            ),

            /// Seção Por Ult. NSU
            ExpansionTile(
              shape: const Border(),
              title: Text('Por Ult. NSU',
                  style:
                      TextStyle(fontWeight: FontWeight.bold, fontSize: 18.0)),
              children: [
                Column(
                  spacing: 14.0,
                  children: [
                    SizedBox(height: 10.0),
                    TextFormField(
                      controller: _ufUltNsuController,
                      decoration: InputDecoration(
                        labelText: 'UF',
                        border: OutlineInputBorder(),
                      ),
                    ),
                    TextFormField(
                      controller: _cnpjUltNsuController,
                      keyboardType: TextInputType.number,
                      decoration: InputDecoration(
                        labelText: 'CNPJ',
                        border: OutlineInputBorder(),
                      ),
                    ),
                    TextFormField(
                      controller: _ultNsuController,
                      decoration: InputDecoration(
                        labelText: 'Ult. NSU',
                        border: OutlineInputBorder(),
                      ),
                    ),
                    SizedBox(
                      width: double.infinity,
                      child: ElevatedButton(
                        style: ElevatedButton.styleFrom(
                          backgroundColor: AppColors.secondary,
                        ),
                        onPressed: _onClickDfePorUltNsu,
                        child: const Text(
                          'Por Ult. NSU',
                          style: TextStyle(fontWeight: FontWeight.bold),
                        ),
                      ),
                    ),
                  ],
                )
              ],
            ),
          ],
        ),
      ),
    );
  }
}
