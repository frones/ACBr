import 'package:demo_acbrcep_flutter/screens/homepage.dart';
import 'package:flutter/material.dart';

void main() {
  runApp(const MainApp());
}

class MainApp extends StatelessWidget {
  const MainApp({super.key});

  @override
  Widget build(BuildContext context) {
    final colorScheme = ColorScheme.fromSeed(seedColor: Colors.blue);
    final darkColorScheme = ColorScheme.fromSeed(
        seedColor: Colors.blue, brightness: Brightness.dark);

    return MaterialApp(
      home: MyHomePage(),
      title: 'Programa Exemplo ACBrCep Flutter',
      theme: ThemeData(
        useMaterial3: true,
        colorScheme: colorScheme,
      ),
      darkTheme: ThemeData(
        useMaterial3: true,
        colorScheme: darkColorScheme,
      ),
      themeMode: ThemeMode.system, // Use system theme mode
    );
  }
}
