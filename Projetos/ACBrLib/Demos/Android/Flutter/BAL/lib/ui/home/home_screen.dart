import 'package:demoacbrbal/plugin/acbrbal_plugin.dart';
import 'package:demoacbrbal/ui/_core/app_colors.dart';
import 'package:demoacbrbal/ui/acbrlib_ini/acbrlib_ini_screen.dart';
import 'package:demoacbrbal/ui/bal/bal_screen.dart';
import 'package:demoacbrbal/ui/configuracoes/configuracoes_screen.dart';
import 'package:demoacbrbal/utils/acbrlib_bal_helper.dart';
import 'package:flutter/material.dart';

/// Tela base que controla a navegação entre as telas do App.
///
/// Entre elas estão: BAL, Configurações e ACBrLib.ini.
///
/// Também é responsável pela inicialização da ACBrLibBAL
class HomeScreen extends StatefulWidget {
  @override
  State<HomeScreen> createState() => _HomeScreenState();
}

class _HomeScreenState extends State<HomeScreen> {
  int _selectedItemIndex = 0;
  late AcbrbalPlugin _acbrbalplugin;

  List<Widget> _widgets = [
    const BalScreen(),
    const ConfiguracoesScreen(),
    const ACBrLibIniScreen(),
  ];

  List<String> titles = [
    'Programa Exemplo ACBrBAL',
    'Configurações',
    'ACBrLib.ini',
  ];

  // Inicializa ACBrLibBAL
  void inicializarLib() async {
    try {
      await _acbrbalplugin.inicializar();
    } on Exception catch (e) {
      debugPrint("Erro ao inicializar: '$e'.");
    }
  }

  @override
  void initState() {
    super.initState();
    _acbrbalplugin = ACBrLibBalHelper().acbrbalplugin;
    inicializarLib();
  }

  @override
  void dispose() {
    super.dispose();
    // Libera os recursos utilizados pelo ACBrLibBAL
    _acbrbalplugin.finalizar();
  }

  @override
  Widget build(BuildContext context) {
    return LayoutBuilder(builder: (context, constraints) {
      return Scaffold(
          appBar: AppBar(
            backgroundColor: AppColors.primary,
            foregroundColor: Colors.white,
            title: Text(
              titles[_selectedItemIndex],
              style: TextStyle(fontWeight: FontWeight.bold),
            ),
          ),
          bottomNavigationBar: NavigationBar(
            indicatorColor: AppColors.secondary,
            selectedIndex: _selectedItemIndex,
            onDestinationSelected: (int index) {
              setState(() {
                _selectedItemIndex = index;
              });
            },
            destinations: const <Widget>[
              NavigationDestination(
                icon: Icon(Icons.balance),
                label: 'BAL',
              ),
              NavigationDestination(
                icon: Icon(Icons.settings),
                label: 'Configurações',
              ),
              NavigationDestination(
                icon: Icon(Icons.integration_instructions),
                label: 'ACBrLib.Ini',
              ),
            ],
          ),
          body: _widgets[_selectedItemIndex]);
    });
  }
}
