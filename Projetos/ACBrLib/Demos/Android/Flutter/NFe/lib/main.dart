import 'package:acbrlibnfe/ui/_core/app_theme.dart';
import 'package:acbrlibnfe/ui/home/home_screen.dart';
import 'package:acbrlibnfe/utils/utils.dart';
import 'package:flutter/material.dart';

void main() async {
  runApp(const MainApp());
}

class MainApp extends StatefulWidget {
  const MainApp({super.key});

  @override
  State<MainApp> createState() => _MainAppState();
}

class _MainAppState extends State<MainApp> {
  @override
  void initState() {
    super.initState();
    // Extrai a pasta Schemas na abertura do APP, se você importou via Device Explorer,
    // comente esta linha.
    _extrairSchemas();
  }

  /// Tenta extrair a pasta Schemas via arquivo `schemas.zip` em `assets/`
  void _extrairSchemas() async {
    try {
      await extrairSchemasParaPastaFiles();
    } catch (e) {
      debugPrint("Erro ao extrair schemas: $e");
    }
  }

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      home: HomeScreen(),
      title: 'Demo ACBrLibNFe',
      theme: AppTheme.appTheme,
    );
  }
}
