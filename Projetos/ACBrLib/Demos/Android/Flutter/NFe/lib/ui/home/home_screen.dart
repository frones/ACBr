import 'package:acbrlibnfe/plugin/acbrnfe_plugin.dart';
import 'package:acbrlibnfe/ui/_core/app_colors.dart';
import 'package:acbrlibnfe/ui/acbrlib_ini/ini_file_screen.dart';
import 'package:acbrlibnfe/ui/configuracoes/configuracoes_screen.dart';
import 'package:acbrlibnfe/ui/nfe/nfe_screen.dart';
import 'package:acbrlibnfe/utils/acbrlib_nfe_helper.dart';
import 'package:flutter/material.dart';

import '../../utils/utils.dart';

/// Tela base que controla a navegação entre as telas do App.
///
/// Entre elas estão: NFe, Configurações e ACBrLib.ini.
///
/// Também é responsável pela inicialização da ACBrLibNFe
class HomeScreen extends StatefulWidget {
  const HomeScreen({super.key});

  @override
  State<HomeScreen> createState() => _HomeScreenState();
}

class _HomeScreenState extends State<HomeScreen> {
  int _selectedIndex = 0;
  late ACBrNFePlugin _acbrNFePlugin;

  @override
  void initState() {
    super.initState();
    _acbrNFePlugin = ACBrLibNFeHelper().acbrNFePlugin;
    _inicializarLib();
  }

  @override
  void dispose() {
    // Libera os recursos utilizados pelo ACBrLibNFe
    _acbrNFePlugin.finalizar();
    super.dispose();
  }

  static const List<String> _appBarTitles = <String>[
    'Programa Exemplo ACBrNFe',
    'Configurações',
    'ACBrLib.ini'
  ];

  static const List<Widget> _widgetOptions = <Widget>[
    NfeScreen(),
    ConfiguracoesScreen(),
    IniFileScreen()
  ];

  /// Configurações Básicas no ACBrLib.ini para uso do ACBrLibNFe
  void _inicializarLib() async {
    String appDir = await getAppDirByPlatform();
    pathCertificado = '$appDir/cert/certificado.pfx';
    await _acbrNFePlugin.inicializar();
    await _acbrNFePlugin.configGravarValor("Principal", "TipoResposta", "2");
    await _acbrNFePlugin.configGravarValor(
        "DFe", "ArquivoPFX", pathCertificado);
    await _acbrNFePlugin.configGravarValor("DFe", "SSLCryptLib", "1");
    await _acbrNFePlugin.configGravarValor("DFe", "SSLHttpLib", "3");
    await _acbrNFePlugin.configGravarValor("DFe", "SSLXmlSignLib", "4");
    await _acbrNFePlugin.configGravarValor(
        "NFe", "PathSchemas", "$appDir/Schemas/NFe/");
    await _acbrNFePlugin.configGravarValor(
        "NFe", "PathSalvar", "$appDir/docs/");
    await _acbrNFePlugin.configGravar();
  }

  @override
  Widget build(BuildContext context) {
    return LayoutBuilder(builder: (context, constraints) {
      return Scaffold(
        appBar: AppBar(
          backgroundColor: AppColors.primary,
          foregroundColor: Colors.white,
          title: Text(
            _appBarTitles[_selectedIndex],
            style: TextStyle(fontWeight: FontWeight.bold),
          ),
        ),
        bottomNavigationBar: NavigationBar(
          onDestinationSelected: (int index) {
            setState(() {
              _selectedIndex = index;
            });
          },
          indicatorColor: AppColors.secondary,
          selectedIndex: _selectedIndex,
          destinations: const <Widget>[
            NavigationDestination(
              icon: Icon(Icons.receipt_long),
              label: 'NFe',
            ),
            NavigationDestination(
              icon: Icon(Icons.settings),
              label: 'Configurações',
            ),
            NavigationDestination(
              icon: Icon(Icons.integration_instructions),
              label: 'ACBrLib.ini',
            )
          ],
        ),
        body: _widgetOptions.elementAt(_selectedIndex),
      );
    });
  }
}
