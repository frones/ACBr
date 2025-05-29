import 'package:flutter/cupertino.dart';

import '../../utils/acbrlib_bal_helper.dart';

/// ACBrLibIniScreen é a tela que exibe o conteúdo do arquivo ACBrLib.ini
/// É utilizada para verificar as configurações atuais da biblioteca ACBrLib
///
class ACBrLibIniScreen extends StatefulWidget {
  const ACBrLibIniScreen({Key? key}) : super(key: key);

  @override
  State<ACBrLibIniScreen> createState() => _ACBrLibIniScreenState();
}

class _ACBrLibIniScreenState extends State<ACBrLibIniScreen> {
  String _acbrLibIniContent = '';

  @override
  void initState() {
    super.initState();
    _loadACBrLibIniContent();
  }

  /// Carrega o conteúdo do arquivo ACBrLib.ini
  void _loadACBrLibIniContent() async {
    try {
      String content = await ACBrLibBalHelper().acbrbalplugin.configExportar();
      setState(() {
        _acbrLibIniContent = content;
      });
    } catch (e) {
      debugPrint("Erro ao carregar ACBrLib.ini: $e");
      setState(() {
        _acbrLibIniContent = "Erro ao carregar ACBrLib.ini: $e";
      });
    }
  }

  @override
  Widget build(BuildContext context) {
    return SingleChildScrollView(
      padding: const EdgeInsets.all(16.0),
      child: Text(
        _acbrLibIniContent,
        style: const TextStyle(fontSize: 16),
      ),
    );
  }
}
