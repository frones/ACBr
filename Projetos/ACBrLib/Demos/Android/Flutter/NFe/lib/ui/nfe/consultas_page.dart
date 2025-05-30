import 'dart:convert';

import 'package:acbrlibnfe/model/status_servico.dart';
import 'package:acbrlibnfe/ui/_core/app_colors.dart';
import 'package:acbrlibnfe/ui/nfe/nfe_utils.dart';
import 'package:acbrlibnfe/ui/nfe/widgets/painel_resposta.dart';
import 'package:flutter/material.dart';

import '../../plugin/acbrnfe_plugin.dart';
import '../../utils/acbrlib_nfe_helper.dart';

/// Página com comandos de Consulta
class ConsultasPage extends StatefulWidget {
  const ConsultasPage({super.key});

  @override
  State<StatefulWidget> createState() => _ConsultasPageState();
}

class _ConsultasPageState extends State<ConsultasPage> {
  /// Instância do plugin
  late ACBrNFePlugin _acbrNFePlugin;

  /// Gerencia respostas dos comandos
  String? _response;

  // Controllers dos inputs
  final TextEditingController _numReciboController = TextEditingController();
  final TextEditingController _xmlController = TextEditingController();
  final TextEditingController _ufController = TextEditingController();
  final TextEditingController _documentoController = TextEditingController();
  final TextEditingController _chaveController = TextEditingController();

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
    _numReciboController.dispose();
    _xmlController.dispose();
    _ufController.dispose();
    _documentoController.dispose();
    _chaveController.dispose();
    _scrollController.dispose();
    super.dispose();
  }

  /// Função para chamar o comando de status do serviço
  ///
  /// Possui exemplo de Serialização do JSON retornado
  void _onClickStatusServico() async {
    String result = "";
    try {
      // Chama o comando de status do serviço
      result = await _acbrNFePlugin.statusServico();

      // Serializa o JSON retornado
      StatusServico statusServico = StatusServico.fromJson(jsonDecode(result));

      // Cria uma string formatada com os dados do status do serviço
      result = "Status Serviço:\n"
          "CStat: ${statusServico.cStat}\n"
          "CUF: ${statusServico.cUF}\n"
          "DhRecbto: ${statusServico.dhRecbto}\n"
          "DhRetorno: ${statusServico.dhRetorno}\n"
          "Msg: ${statusServico.msg}\n"
          "TMed: ${statusServico.tMed}\n"
          "VerAplic: ${statusServico.verAplic}\n"
          "Versao: ${statusServico.versao}\n"
          "XMotivo: ${statusServico.xMotivo}\n"
          "XObs: ${statusServico.xObs}\n"
          "tpAmb: ${statusServico.tpAmb}";

      debugPrint('Resultado de statusServico: "$result"');
    } catch (e) {
      result = "ERRO: Erro ao obter Status Serviço.\nChecar logs.";
      debugPrint("Erro ao chamar statusServico: '$e'.");
    } finally {
      setState(() {
        _response = result;
      });
      NfeUtils.goToPainelResposta(_scrollController);
    }
  }

  /// Função para chamar o comando de consultar recibo
  void _onClickConsultarRecibo() async {
    String result = "";
    try {
      result = await _acbrNFePlugin.consultarRecibo(_numReciboController.text);
      debugPrint('Resultado de consultarRecibo: "$result"');
    } catch (e) {
      result = "ERRO: Erro ao consultar recibo.\nChecar logs.";
      debugPrint("Erro ao chamar consultarRecibo: '$e'.");
    } finally {
      setState(() {
        _response = result;
      });
      NfeUtils.goToPainelResposta(_scrollController);
    }
  }

  /// Função para chamar o comando de consulta com XML
  void _onClickConsultarXML() async {
    String result = "";
    try {
      await _acbrNFePlugin.limparLista();
      result = await _acbrNFePlugin.consultar(_xmlController.text, false);
      debugPrint('Resultado de consultar XML: "$result"');
    } catch (e) {
      result = "ERRO: Erro ao consultar XML.\nChecar logs.";
      debugPrint("Erro ao chamar consultar XML: '$e'.");
    } finally {
      setState(() {
        _response = result;
      });
      NfeUtils.goToPainelResposta(_scrollController);
    }
  }

  /// Função para chamar o comando de consulta de cadastro
  void _onClickConsultarCadastro() async {
    String result = "";
    try {
      result = await _acbrNFePlugin.consultaCadastro(
          _ufController.text, _documentoController.text, false);
      debugPrint('Resultado de consultaCadastro: "$result"');
    } catch (e) {
      result = "ERRO: Erro ao consultar cadastro.\nChecar logs.";
      debugPrint("Erro ao chamar consultaCadastro: '$e'.");
    } finally {
      setState(() {
        _response = result;
      });
      NfeUtils.goToPainelResposta(_scrollController);
    }
  }

  /// Função para chamar o comando de consulta com chave
  void _onClickConsultarChave() async {
    String result = "";
    try {
      result = await _acbrNFePlugin.consultar(_chaveController.text, false);
      debugPrint('Resultado de consultar chave: "$result"');
    } catch (e) {
      result = "ERRO: Erro ao consultar chave.\nChecar logs.";
      debugPrint("Erro ao chamar consultar chave: '$e'.");
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

            /// Seção Status Serviço
            ExpansionTile(
              shape: const Border(),
              title: Text('Status Serviço',
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
                    onPressed: _onClickStatusServico,
                    child: const Text(
                      'Status Serviço',
                      style: TextStyle(fontWeight: FontWeight.bold),
                    ),
                  ),
                ),
              ],
            ),

            Divider(height: 2.0),

            /// Seção Consultar Recibo
            ExpansionTile(
              shape: const Border(),
              title: Text('Consultar Recibo',
                  style:
                      TextStyle(fontWeight: FontWeight.bold, fontSize: 18.0)),
              children: [
                Column(
                  spacing: 14.0,
                  children: [
                    SizedBox(height: 10.0),
                    TextFormField(
                      controller: _numReciboController,
                      keyboardType: TextInputType.number,
                      decoration: InputDecoration(
                        labelText: 'Número Recibo',
                        border: OutlineInputBorder(),
                      ),
                    ),
                    SizedBox(
                      width: double.infinity,
                      child: ElevatedButton(
                        style: ElevatedButton.styleFrom(
                          backgroundColor: AppColors.secondary,
                        ),
                        onPressed: _onClickConsultarRecibo,
                        child: const Text(
                          'Consultar Recibo',
                          style: TextStyle(fontWeight: FontWeight.bold),
                        ),
                      ),
                    ),
                  ],
                )
              ],
            ),

            Divider(height: 2.0),

            /// Seção Consultar com XML
            ExpansionTile(
              shape: const Border(),
              title: Text('Consultar com XML',
                  style:
                      TextStyle(fontWeight: FontWeight.bold, fontSize: 18.0)),
              children: [
                Column(
                  spacing: 14.0,
                  children: [
                    SizedBox(height: 10.0),
                    TextFormField(
                      controller: _xmlController,
                      minLines: 12,
                      maxLines: 12,
                      decoration: InputDecoration(
                        labelText: 'XML NFe',
                        border: OutlineInputBorder(),
                      ),
                    ),
                    SizedBox(
                      width: double.infinity,
                      child: ElevatedButton(
                        style: ElevatedButton.styleFrom(
                          backgroundColor: AppColors.secondary,
                        ),
                        onPressed: _onClickConsultarXML,
                        child: const Text(
                          'Consultar com XML',
                          style: TextStyle(fontWeight: FontWeight.bold),
                        ),
                      ),
                    ),
                  ],
                ),
              ],
            ),

            Divider(height: 2.0),

            /// Seção Consultar Cadastro
            ExpansionTile(
              shape: const Border(),
              title: Text('Consultar Cadastro',
                  style:
                      TextStyle(fontWeight: FontWeight.bold, fontSize: 18.0)),
              children: [
                Column(
                  spacing: 14.0,
                  children: [
                    SizedBox(height: 10.0),
                    TextFormField(
                      controller: _ufController,
                      decoration: InputDecoration(
                        labelText: 'UF',
                        border: OutlineInputBorder(),
                      ),
                    ),
                    TextFormField(
                      controller: _documentoController,
                      decoration: InputDecoration(
                        labelText: 'Documento',
                        border: OutlineInputBorder(),
                      ),
                    ),
                    SizedBox(
                      width: double.infinity,
                      child: ElevatedButton(
                        style: ElevatedButton.styleFrom(
                          backgroundColor: AppColors.secondary,
                        ),
                        onPressed: _onClickConsultarCadastro,
                        child: const Text(
                          'Consultar Cadastro',
                          style: TextStyle(fontWeight: FontWeight.bold),
                        ),
                      ),
                    ),
                  ],
                ),
              ],
            ),

            Divider(height: 2.0),

            /// Seção Consultar com Chave
            ExpansionTile(
              shape: const Border(),
              title: Text('Consultar com Chave',
                  style:
                      TextStyle(fontWeight: FontWeight.bold, fontSize: 18.0)),
              children: [
                Column(
                  spacing: 14.0,
                  children: [
                    SizedBox(height: 10.0),
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
                        onPressed: _onClickConsultarChave,
                        child: const Text(
                          'Consultar com Chave',
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
