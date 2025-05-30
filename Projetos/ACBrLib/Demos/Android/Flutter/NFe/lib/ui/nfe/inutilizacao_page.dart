import 'package:acbrlibnfe/ui/nfe/nfe_utils.dart';
import 'package:acbrlibnfe/ui/nfe/widgets/painel_resposta.dart';
import 'package:flutter/material.dart';

import '../../plugin/acbrnfe_plugin.dart';
import '../../utils/acbrlib_nfe_helper.dart';
import '../_core/app_colors.dart';

/// Página com comandos de Inutilização
class InutilizacaoPage extends StatefulWidget {
  const InutilizacaoPage({super.key});

  @override
  State<InutilizacaoPage> createState() => _InutilizacaoPageState();
}

class _InutilizacaoPageState extends State<InutilizacaoPage> {
  /// Instância do plugin
  late ACBrNFePlugin _acbrNFePlugin;

  /// Gerencia respostas dos comandos
  String? _response;

  // Controllers dos inputs
  final TextEditingController _cnpjController = TextEditingController();
  final TextEditingController _justificativaController =
      TextEditingController();
  final TextEditingController _anoController = TextEditingController();
  final TextEditingController _modeloController = TextEditingController();
  final TextEditingController _serieController = TextEditingController();
  final TextEditingController _numInicialController = TextEditingController();
  final TextEditingController _numFinalController = TextEditingController();

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
    _cnpjController.dispose();
    _justificativaController.dispose();
    _anoController.dispose();
    _modeloController.dispose();
    _serieController.dispose();
    _numInicialController.dispose();
    _numFinalController.dispose();
    _scrollController.dispose();
    super.dispose();
  }

  /// Função para chamar comando de inutilizar
  void _onClickInutilizarNumeracao() async {
    String result = '';
    try {
      result = await _acbrNFePlugin.inutilizar(
        _cnpjController.text,
        _justificativaController.text,
        _anoController.text,
        _modeloController.text,
        _serieController.text,
        _numInicialController.text,
        _numFinalController.text,
      );
      debugPrint('Resultado de inutilizar: "$result"');
    } catch (e) {
      result = "ERRO: Erro ao inutilizar numeração.\nChecar logs.";
      debugPrint("Erro ao chamar inutilizar: '$e'.");
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
              PainelResposta(response: _response, onClear: _onResponseClear),
              Divider(height: 2.0, color: Colors.black),

              // CNPJ
              TextFormField(
                controller: _cnpjController,
                keyboardType: TextInputType.number,
                decoration: InputDecoration(
                  labelText: 'CNPJ',
                  border: OutlineInputBorder(),
                ),
              ),

              // Justificativa
              TextFormField(
                controller: _justificativaController,
                decoration: InputDecoration(
                  labelText: 'Justificativa',
                  border: OutlineInputBorder(),
                ),
              ),

              // Ano
              TextFormField(
                controller: _anoController,
                keyboardType: TextInputType.number,
                decoration: InputDecoration(
                  labelText: 'Ano',
                  border: OutlineInputBorder(),
                ),
              ),

              // Modelo
              TextFormField(
                controller: _modeloController,
                decoration: InputDecoration(
                  labelText: 'Modelo',
                  border: OutlineInputBorder(),
                ),
              ),

              // Série
              TextFormField(
                controller: _serieController,
                decoration: InputDecoration(
                  labelText: 'Série',
                  border: OutlineInputBorder(),
                ),
              ),

              // Número Inicial
              TextFormField(
                controller: _numInicialController,
                keyboardType: TextInputType.number,
                decoration: InputDecoration(
                  labelText: 'Número Inicial',
                  border: OutlineInputBorder(),
                ),
              ),

              // Número Final
              TextFormField(
                controller: _numFinalController,
                keyboardType: TextInputType.number,
                decoration: InputDecoration(
                  labelText: 'Número Final',
                  border: OutlineInputBorder(),
                ),
              ),

              SizedBox(height: 5.0),

              // Botão de Inutilização
              SizedBox(
                width: double.infinity,
                child: ElevatedButton(
                  style: ElevatedButton.styleFrom(
                    backgroundColor: AppColors.secondary,
                  ),
                  onPressed: _onClickInutilizarNumeracao,
                  child: const Text(
                    'Inutilizar Numeração',
                    style: TextStyle(fontWeight: FontWeight.bold),
                  ),
                ),
              ),

              SizedBox(height: 10.0),
            ],
          )),
    );
  }
}
