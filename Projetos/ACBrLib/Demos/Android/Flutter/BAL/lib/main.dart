import 'package:demoacbrbal/ui/_core/app_theme.dart';
import 'package:demoacbrbal/ui/home/home_screen.dart';
import 'package:flutter/material.dart';

void main() {
  runApp(const MyApp());
}

class MyApp extends StatelessWidget {
  const MyApp({super.key});

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      home: HomeScreen(),
      title: 'Demo ACBrBAL',
      theme: AppTheme.appTheme,
    );
  }
}
