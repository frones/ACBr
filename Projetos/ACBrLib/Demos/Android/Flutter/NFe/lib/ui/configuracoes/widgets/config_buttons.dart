import 'package:flutter/material.dart';

/// Botão de configuração que permite salvar as configurações atuais.
class ConfigButtons extends StatelessWidget {
  final VoidCallback onSave;

  /// Crie o botão de salvar configurações.
  ///
  /// - [onSave] é a função a ser chamada quando o botão de salvar for pressionado.
  ///
  const ConfigButtons({super.key, required this.onSave});

  @override
  Widget build(BuildContext context) {
    return Row(
      spacing: 12.0,
      mainAxisAlignment: MainAxisAlignment.spaceAround,
      children: [
        Expanded(
          child: ElevatedButton(
              onPressed: onSave,
              child: const Text(
                'Salvar configurações',
                textAlign: TextAlign.center,
                style: TextStyle(fontWeight: FontWeight.bold),
              )),
        ),
      ],
    );
  }
}
