import 'dart:convert';
import 'dart:math';

import 'package:flutter/material.dart';
import '../plugin/acbrnfeplugin.dart';
import '../model/statusServico.dart';
import '../screens/widgets/cardResponse.dart';

class MyHomePage extends StatefulWidget {
  const MyHomePage({super.key});

  @override
  State<MyHomePage> createState() => _MyHomePageState();
}

class _MyHomePageState extends State<MyHomePage> {
  late ACBrNFePlugin _acbrNFePlugin;
  late String _result = _getJsonEmptyStatusServico();

  @override
  Widget build(BuildContext context) {
    //layout buildr
    return LayoutBuilder(builder: (context, constraints) {
      return Scaffold(
        appBar: AppBar(
          backgroundColor: Theme.of(context).colorScheme.inversePrimary,
          title: const Text('Programa Exemplo ACBrLibNFe'),
        ),
        body: Center(
          child: SingleChildScrollView(
            child: Column(
              mainAxisAlignment: MainAxisAlignment.start,
              children: <Widget>[
                const SizedBox(height: 8),
                ElevatedButton(
                  onPressed: onClickStatusServico,
                  child: const Text('Consultar Status Serviço'),
                ),
                const Text(
                  'Resultado:',
                ),
                 CardResult(
                   key: ValueKey(_result),
                   result: statusServico.fromJson(jsonDecode(_result)),
                   width: constraints.maxWidth * 0.8,
                 ),
              ],
            ),
          ),
        ),
      );
    });
  }

  String _getJsonEmptyStatusServico() {
    return
      '''
      { 
        "Status" : { 
          "CStat" : "", 
          "CUF" : "", 
          "DhRecbto" : "", 
          "DhRetorno" : "", 
          "Msg" : "", 
          "TMed" : "", 
          "VerAplic" : "", 
          "Versao" : "", 
          "XMotivo" : "", 
          "XObs" : "", 
          "tpAmb" : "" 
          }
      }
      ''';
  }

  void onClickStatusServico() async {
    String result = "";
    try {
      result = await _acbrNFePlugin.statusServico();
    } catch (e) {
      result = _getJsonEmptyStatusServico();
      debugPrint("Erro: '${e.toString()}'.");
    } finally {
      setState(() {
        _result = result;
      });
    }
  }

  //Configurações Básicas no ACBrLib.ini para uso ACBrLibNFe
  void inicializar() async {
    await _acbrNFePlugin.inicializar();
    await _acbrNFePlugin.configGravarValor("Principal", "TipoResposta", "2");
    await _acbrNFePlugin.configGravarValor("DFe", "ArquivoPFX", "../files/cert/certificado.pfx");
    await _acbrNFePlugin.configGravarValor("DFe", "Senha", "SenhaDoCertificado");
    await _acbrNFePlugin.configGravarValor("DFe", "SSLCryptLib", "1");
    await _acbrNFePlugin.configGravarValor("DFe", "SSLHttpLib", "3");
    await _acbrNFePlugin.configGravarValor("DFe", "SSLXmlSignLib", "4");
    await _acbrNFePlugin.configGravarValor("NFe", "PathSchemas", "../Schemas/NFe/");
    await _acbrNFePlugin.configGravarValor("NFe", "PathSalvar", "../files/docs/");
    await _acbrNFePlugin.configGravar();
  }

  @override
  void initState() {
    // TODO: implement initState
    super.initState();
    _acbrNFePlugin = ACBrNFePlugin();
    inicializar();
  }

  @override
  void dispose() {
    super.dispose();
  }
}
