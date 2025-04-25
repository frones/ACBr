import 'package:flutter/material.dart';
import 'package:flutter/widgets.dart';

class SimpleButton extends StatelessWidget {
  final String text;
  final VoidCallback onPressed;

  const SimpleButton({
    super.key,
    required this.text,
    required this.onPressed,
  });

  @override
  Widget build(BuildContext context) {
    return ElevatedButton(
      onPressed: onPressed,
      child: Text(text),
    );
  }
}