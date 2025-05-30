import 'package:flutter/material.dart';

import '../../_core/app_colors.dart';

/// Widget de Switch com Texto auxiliar
///
/// Possui um texto descritivo ao lado do switch
class SwitchText extends StatefulWidget {
  final bool value;
  final String? text;
  final ValueChanged<bool>? onChanged;

  /// Crie um widget de Switch com Texto
  ///
  /// - [value] é o estado do switch (ligado/desligado)
  /// - [text] é o texto descritivo ao lado do switch
  /// - [onChanged] é a função chamada quando o switch é alterado
  ///
  const SwitchText({
    super.key,
    required this.value,
    required this.text,
    this.onChanged,
  });

  @override
  State<SwitchText> createState() => _SwitchTextState();
}

class _SwitchTextState extends State<SwitchText> {
  @override
  Widget build(BuildContext context) {
    return Container(
      decoration: BoxDecoration(
        color: AppColors.lightGray,
        borderRadius: BorderRadius.circular(12.0),
      ),
      child: Row(
        mainAxisAlignment: MainAxisAlignment.spaceBetween,
        crossAxisAlignment: CrossAxisAlignment.center,
        children: [
          Expanded(
            child: Padding(
              padding: const EdgeInsets.only(left: 10.0),
              child: Text(
                widget.text ?? '',
                style:
                    const TextStyle(fontSize: 16, fontWeight: FontWeight.w500),
              ),
            ),
          ),
          const SizedBox(width: 10.0),
          Switch(
            value: widget.value,
            activeColor: AppColors.primary,
            onChanged: widget.onChanged,
          ),
        ],
      ),
    );
  }
}
