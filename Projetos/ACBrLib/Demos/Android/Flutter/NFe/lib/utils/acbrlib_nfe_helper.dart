import 'package:acbrlibnfe/plugin/acbrnfe_plugin.dart';

/// Classe Singleton para gerenciar uma intância do plugin ACBrLibNFe
///
/// Essa classe garante que apenas uma instância do plugin seja criada e utilizada em toda a aplicação.
class ACBrLibNFeHelper {
  ACBrLibNFeHelper._privateConstructor();

  static final ACBrLibNFeHelper _instance = ACBrLibNFeHelper
      ._privateConstructor();

  /// Classe Singleton para gerenciar uma intância do plugin ACBrLibNFe
  factory ACBrLibNFeHelper() {
    return _instance;
  }

  final ACBrNFePlugin _acbrNFePlugin = ACBrNFePlugin();

  ACBrNFePlugin get acbrNFePlugin => _acbrNFePlugin;
}