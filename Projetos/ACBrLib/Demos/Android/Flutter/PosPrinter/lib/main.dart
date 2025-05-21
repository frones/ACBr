import 'dart:convert';
import 'dart:io';
import 'package:demo_acbrlib_posprinter/acbrposprinter/acbrlib_posprinter.dart';
import 'package:demo_acbrlib_posprinter/widgets/simple_button.dart';
import 'package:flutter/material.dart';
import 'package:path_provider/path_provider.dart';

late final String appDir;

void main() {
  runApp(const MyApp());
}

class MyApp extends StatelessWidget {
  const MyApp({super.key});

  // This widget is the root of your application.
  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'Demo PosPrinter',
      theme: ThemeData(
        colorScheme: ColorScheme.fromSeed(seedColor: Colors.deepPurple),
        useMaterial3: true,
      ),
      home: const MyHomePage(title: 'Demo ACBrPosPrinter'),
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
  late ACBrLibPosPrinter _posPrinter;
  late String posprinter_porta_txt = "";
  bool _isActivated = false;

  void aplicarConfiguracoes() {
    try {
      _posPrinter.configGravarValor("Principal", "LogNivel", "4");
      _posPrinter.configGravarValor("Socket", "NivelLog", "4");

      final resultModelo =
          _posPrinter.configGravarValor("PosPrinter", "Modelo", "1");
      final resultDevice = _posPrinter.configGravarValor(
          "PosPrinter", "Porta", posprinter_porta_txt);
      _posPrinter.configGravarValor("PosPrinter", "ControlePorta", "1");
      _posPrinter.configGravar();

      if (resultModelo == 0 && resultDevice == 0) {
        debugPrint("Configuração gravada com sucesso");
      }
    } catch (e) {
      debugPrint("Erro ao gravar configuração: $e");
    }
  }

  void onClickAtivar() async {
    if (!_isActivated) {
      _posPrinter.ativar();
      setState(() {
        _isActivated = true;
      });
    } else {
      _posPrinter.desativar();
      setState(() {
        _isActivated = false;
      });
    }
  }

  Future<void> sendToPrinter(String buffer) async {
    final socket = await Socket.connect('192.168.3.200', 9100);
    final cutPaper = [0x1B, 0x69]; // ESC i

    socket.add(utf8.encode(buffer));
    socket.add(cutPaper);
    await socket.flush();
    await socket.close();
  }

  void _imprimeBuffer() async {
    try {
      final file = File(posprinter_porta_txt);
      if (await file.exists()) {
        String contents = await file.readAsString();
        debugPrint("Conteudo do buffer: $contents");
        await sendToPrinter(contents);
        await file.delete();
      } else {
        debugPrint("Arquivo não encontrado");
      }
    } catch (e) {
      debugPrint("Erro ao ler o buffer: $e");
    }
  }

  void onClickImprimir() {
    if (!_isActivated) {
      debugPrint("Impressora não ativada");
      return;
    }
    try {
      String message  = "lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua" +
          "\nLorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua\n";
      _posPrinter.imprimir(message, 0, 0, 0, 1);
      _imprimeBuffer();
    } catch (e) {
      debugPrint("Erro ao imprimir: $e");

    }
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
        appBar: AppBar(
          backgroundColor: Theme.of(context).colorScheme.inversePrimary,
          title: Text(widget.title),
        ),
        body: Center(
          child: Column(
            mainAxisAlignment: MainAxisAlignment.center,
            children: <Widget>[
              SimpleButton(
                  text: _isActivated ? "Desativar" : "Ativar",
                  onPressed: onClickAtivar),
              SimpleButton(text: "Imprimir", onPressed: onClickImprimir),
            ],
          ),
        ));
  }

  Future<String> getAppDirByPlatform() async {
    if (Platform.isAndroid) {
      return (await getExternalStorageDirectory())!.path;
    }
    return File(Platform.resolvedExecutable).parent.path;
  }

  void carregarBiblioteca() async {
    appDir = await getAppDirByPlatform();
    posprinter_porta_txt = "$appDir/buffer.txt";
    _posPrinter = ACBrLibPosPrinter("$appDir/ACBrPosPrinter.ini", "");
    _posPrinter.inicializar();
    aplicarConfiguracoes();
  }

  @override
  void initState() {
    // TODO: implement initState
    super.initState();
    carregarBiblioteca();
  }

  @override
  void dispose() {
    _posPrinter.finalizar();
    super.dispose();
  }
}
