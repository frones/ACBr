import 'package:demoacbrbal/plugin/acbrbal_plugin.dart';

/// Classe Singleton para gerenciar uma intância do plugin ACBrLibBAL
///
/// Essa classe garante que apenas uma instância do plugin seja criada e utilizada em toda a aplicação.
class ACBrLibBalHelper {
  ACBrLibBalHelper._privateConstructor();

  static final ACBrLibBalHelper _instance =
      ACBrLibBalHelper._privateConstructor();

  /// Classe Singleton para gerenciar uma intância do plugin ACBrLibBAL
  factory ACBrLibBalHelper() {
    return _instance;
  }

  final AcbrbalPlugin _acbrbalplugin = AcbrbalPlugin();

  AcbrbalPlugin get acbrbalplugin => _acbrbalplugin;
}
