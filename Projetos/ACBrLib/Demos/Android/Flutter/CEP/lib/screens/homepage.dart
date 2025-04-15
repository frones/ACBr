import 'dart:convert';

import 'package:demo_acbrcep_flutter/screens/widgets/CardResponse.dart';
import 'package:flutter/cupertino.dart';
import 'package:flutter/material.dart';

import '../acbrcepaarplugin.dart';
import '../model/Cep.dart';

class MyHomePage extends StatefulWidget {
  const MyHomePage({super.key});

  @override
  State<MyHomePage> createState() => _MyHomePageState();
}

class _MyHomePageState extends State<MyHomePage> {
  late TextEditingController cepTextController;
  late ACBrCepAarPlugin _cepAarPlugin;
  late String _result = _getJsonEmptyCep();

  @override
  Widget build(BuildContext context) {
    //layout buildr
    return LayoutBuilder(builder: (context, constraints) {
      return Scaffold(
        appBar: AppBar(
          backgroundColor: Theme.of(context).colorScheme.inversePrimary,
          title: const Text('Programa Exemplo ACBrCep Flutter'),
        ),
        body: Center(
          child: SingleChildScrollView( // Adicionado para permitir rolagem
            child: Column(
              mainAxisAlignment: MainAxisAlignment.center,
              children: <Widget>[
                Text(
                  'Digite o CEP:',
                ),
                Padding(
                  padding: const EdgeInsets.all(8.0),
                  child: SizedBox(
                    width: constraints.maxWidth * 0.8,
                    child: TextField(
                      controller: cepTextController,
                      keyboardType: TextInputType.number,
                      decoration: const InputDecoration(
                        border: OutlineInputBorder(),
                        hintText: 'Digite o CEP',
                        labelText: 'CEP',
                      ),
                    ),
                  ),
                ),
                const SizedBox(height: 8),
                const Text(
                  'Resultado:',
                ),
                CardResult(
                  key: ValueKey(_result),
                  result: Cep.fromJson(jsonDecode(_result)),
                  width: constraints.maxWidth * 0.8,
                ),
                ElevatedButton(
                  onPressed: onClickBuscarPorCep,
                  child: const Text('Buscar'),
                ),
              ],
            ),
          ),
        ),
      );
    });
  }

  String _getJsonEmptyCep(){
    return
      '''
      {
        "CEP": {
          "Endereco1": {
            "Bairro": "",
            "CEP": "",
            "Complemento": "",
            "IBGE_Municipio": "",
            "IBGE_UF": "",
            "Logradouro": "",
            "Municipio": "",
            "Tipo_Logradouro": "",
            "UF": ""
          },
          "Quantidade": 1
        }
      }
      ''';
  }
  void onClickBuscarPorCep() async {
    String result = "";
    try {
      result = await _cepAarPlugin.buscarPorCep(cepTextController.text);
    } catch (e) {
      result =  _getJsonEmptyCep() ;
      debugPrint("Erro: '${e.toString()}'.");
    } finally {
      setState(() {
        _result = result;
      });
    }
  }

  void inicializar() async {
    await _cepAarPlugin.inicializar();
    await _cepAarPlugin.configGravarValor("CEP","WebService","10");
    await _cepAarPlugin.configGravarValor("Principal","TipoResposta","2");
    await _cepAarPlugin.configGravar();
  }



  @override
  void initState() {
    // TODO: implement initState
    super.initState();
    cepTextController = TextEditingController();
    cepTextController.text = '18270170';
    _cepAarPlugin = ACBrCepAarPlugin();
    inicializar();
  }

  @override
  void finish() {
    cepTextController.dispose();
    super.dispose();
  }


}

