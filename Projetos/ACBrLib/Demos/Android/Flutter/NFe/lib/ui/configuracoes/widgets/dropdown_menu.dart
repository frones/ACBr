import 'package:flutter/material.dart';

/// DropDownMenu com estilo padrão
///
/// Possui um texto descritivo acima do menu
///
class MainDropdownMenu extends StatefulWidget {
  final String text;
  final String initialValue;
  final List<String> list;
  final ValueChanged<String>? onChanged;

  /// Crie um DropDownMenu
  ///
  /// - [text] é o texto descritivo acima do Menu
  /// - [initialValue] Valor inicial do menu
  /// - [list] lista de itens do menu
  /// - [onChanged] é a função chamada quando um item do menu é selecionado
  ///
  const MainDropdownMenu(
      {super.key,
      this.text = 'Selecione uma opção',
      required this.initialValue,
      required this.list,
      this.onChanged});

  @override
  State<MainDropdownMenu> createState() => _MainDropdownMenuState();
}

class _MainDropdownMenuState extends State<MainDropdownMenu> {
  String? _currentValue;

  @override
  void initState() {
    super.initState();
    _currentValue = widget.initialValue;
    if (_currentValue != null && !widget.list.contains(_currentValue)) {
      _currentValue = null;
    }
  }

  @override
  Widget build(BuildContext context) {
    return Column(
      crossAxisAlignment: CrossAxisAlignment.start,
      children: [
        Text(widget.text),
        DropdownButton<String>(
          isExpanded: true,
          value: _currentValue,
          elevation: 16,
          underline: Container(height: 2, color: Colors.grey),
          onChanged: (String? newValue) {
            setState(() {
              _currentValue = newValue;
              widget.onChanged?.call(newValue!);
            });
          },
          items: widget.list.map<DropdownMenuItem<String>>((String value) {
            return DropdownMenuItem<String>(value: value, child: Text(value));
          }).toList(),
        ),
      ],
    );
  }
}
