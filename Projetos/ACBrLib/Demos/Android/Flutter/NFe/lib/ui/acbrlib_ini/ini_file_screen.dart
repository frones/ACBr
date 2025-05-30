import 'package:acbrlibnfe/plugin/acbrnfe_plugin.dart';
import 'package:acbrlibnfe/utils/acbrlib_nfe_helper.dart';
import 'package:flutter/material.dart';

/// Tela que exibe o conteúdo do arquivo INI.
class IniFileScreen extends StatefulWidget {
  const IniFileScreen({super.key});

  @override
  State<IniFileScreen> createState() => _IniFileScreenState();
}

class _IniFileScreenState extends State<IniFileScreen> {
  /// Instância do plugin
  late ACBrNFePlugin _acbrNFePlugin;
  late Future<String> _iniFileContent;

  @override
  void initState() {
    super.initState();
    _acbrNFePlugin = ACBrLibNFeHelper().acbrNFePlugin;
    _iniFileContent = getIniFile();
  }

  Future<String> getIniFile() async {
    return await _acbrNFePlugin.configExportar();
  }

  @override
  Widget build(BuildContext context) {
    return FutureBuilder(
        future: _iniFileContent,
        builder: (BuildContext context, AsyncSnapshot<String> snapshot) {
          if (snapshot.connectionState == ConnectionState.waiting) {
            return const Center(child: CircularProgressIndicator());
          } else if (snapshot.hasError) {
            return Center(
                child:
                    Text('Erro ao carregar o arquivo INI: ${snapshot.error}'));
          } else {
            return SingleChildScrollView(
              child: Padding(
                padding: const EdgeInsets.all(16.0),
                child: Text(snapshot.data ?? 'No data available'),
              ),
            );
          }
        });
  }
}
