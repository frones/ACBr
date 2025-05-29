import 'package:flutter/material.dart';

/// Configura um menu dropdown para as configurações.
/// Este widget é genérico e pode ser usado para diferentes tipos de valores.
class ConfiguracoesDropdowmMenu<T> extends StatelessWidget {
  final String label;
  final List<T> values;
  final T value;
  final void Function(T?) onChanged;
  final String Function(T) getLabel;

  /// [_getLabel] é uma função que recebe um valor do tipo [T] e retorna uma string que será exibida no menu.
  /// [T] é o tipo genérico que representa os valores do menu dropdown.
  /// [ConfiguracoesDropdowmMenu] é um widget genérico que pode ser usado para qualquer tipo de configuração que precise de um menu dropdown.
  /// [onChanged] é uma função que será chamada quando o valor selecionado for alterado.
  ///
  const ConfiguracoesDropdowmMenu({
    super.key,
    required this.label,
    required this.values,
    required this.value,
    required this.onChanged,
    required this.getLabel,
  });

  @override
  Widget build(BuildContext context) {
    return Column(
      crossAxisAlignment: CrossAxisAlignment.start,
      children: [
        Text(label),
        DropdownButton<T>(
          value: value,
          isExpanded: true,
          items: values.map((T v) {
            return DropdownMenuItem<T>(
              value: v,
              child: Text(getLabel(v)),
            );
          }).toList(),
          onChanged: onChanged,
        ),
      ],
    );
  }
}

/// Extensão para a classe String que adiciona um método `capitalize`.
/// Exemplo de uso: "exemplo".capitalize() retorna "Exemplo".
extension StringExtension on String {
  String capitalize() {
    if (isEmpty) return this;
    return this[0].toUpperCase() + substring(1).toLowerCase();
  }
}

