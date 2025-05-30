import 'package:flutter/material.dart';

/// Utilitários para os Comandos NFe
class NfeUtils {

  /// Função para rolar a tela até o painel de resposta
  ///
  /// - [scrollController] é o controlador de rolagem da tela.
  ///
  static void goToPainelResposta(scrollController) {
    if (scrollController.hasClients) {
      scrollController.animateTo(
        0.0,
        duration: const Duration(milliseconds: 300),
        curve: Curves.easeOut,
      );
    }
  }
}
