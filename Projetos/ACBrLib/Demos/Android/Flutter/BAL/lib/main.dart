import 'dart:async';

import 'package:flutter/material.dart';
import 'acbrbalplugin.dart';
import 'package:fluttertoast/fluttertoast.dart';

void main() {
  runApp(const MyApp());
}

class MyApp extends StatelessWidget {
  const MyApp({super.key});

  // This widget is the root of your application.
  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'Demo ACBrBal',
      theme: ThemeData(
        colorScheme: ColorScheme.fromSeed(seedColor: Colors.deepPurple),
        useMaterial3: true,
      ),
      home: const MyHomePage(title: 'Flutter Demo Home Page'),
    );
  }
}

class MyHomePage extends StatefulWidget {
  const MyHomePage({super.key, required this.title});

  final String title;

  @override
  State<MyHomePage> createState() => _MyHomePageState();
}

class _MyHomePageState extends State<MyHomePage> {
  String _resultPeso = '0.0';
  bool _ativado = false;
  bool _inicializado = false;
  final acbrbalplugin = Acbrbalplugin();

  void inicializar() async {
    try {
      if (!_inicializado) {
        await acbrbalplugin.inicializar();
        _inicializado = true;
      }
    } on Exception catch (e) {
      print("Erro: '${e.toString()}'.");
    }
  }

  void setResultPeso(double peso) {
    setState(() {
      _resultPeso = peso.toString();
    });
  }

  void toggleAtivado() {
    setState(() {
      if (_ativado) {
        acbrbalplugin.desativar();
      } else {
        acbrbalplugin.ativar();
      }
      _ativado = !_ativado;
    });
  }

  Future<double> getPeso() async {
    try {
      double peso = await acbrbalplugin.lePeso(1000);
      return peso;
    } catch (error) {
      return -1.0;
    }
  }

  void onClickButtonGetPeso() async {
    if (!_ativado) {
      Fluttertoast.showToast(
        msg: "Balança não ativada",
        toastLength: Toast.LENGTH_LONG,
        gravity: ToastGravity.BOTTOM,
      );
      return;
    }
    setResultPeso(await getPeso());
  }

  void onClickButtonResetPeso() {
    setResultPeso(0.0);
  }

 //Metodo exemplo para aplicar configurações
  void onClickConfigurar () async {
    await acbrbalplugin.configGravarValor("Principal", "LogNivel", "4");
    await acbrbalplugin.configGravar();
  }
  @override
  Widget build(BuildContext context) {
    inicializar();

    return Scaffold(
      appBar: AppBar(
        backgroundColor: Theme.of(context).colorScheme.inversePrimary,
        title: Text(widget.title),
      ),
      body: Center(
        child: Column(
          mainAxisAlignment: MainAxisAlignment.center,
          children: <Widget>[
            const Text(
              'Peso Atual:',
            ),
            Text(
              '$_resultPeso',
              style: Theme.of(context).textTheme.headlineMedium,
            ),
            CustomButton(
              text: 'Ler Peso',
              onPressed: onClickButtonGetPeso,
            ),
            CustomButton(
              text: 'Zerar Peso',
              onPressed: onClickButtonResetPeso,
            ),
            CustomButton(
              text: _ativado ? 'Desativar' : 'Ativar',
              onPressed: toggleAtivado,
            ),

            CustomButton(text: "Configurar", onPressed: onClickConfigurar)
          ],
        ),
      ),
    );
  }
}

class CustomButton extends StatelessWidget {
  final String text;
  final  VoidCallback onPressed;

  const CustomButton({super.key, required this.text, required this.onPressed});

  @override
  Widget build(BuildContext context) {
    return MaterialButton(
      onPressed: onPressed,
      child: Text(text),
      color: Theme.of(context).colorScheme.primary, // Cor do botão
      textColor: Colors.white,
      padding: EdgeInsets.symmetric(
          vertical: 10.0, horizontal: 15.0),
      shape: RoundedRectangleBorder(
        borderRadius: BorderRadius.circular(8.0), // Bordas arredondadas
      ),
    );
  }
}
