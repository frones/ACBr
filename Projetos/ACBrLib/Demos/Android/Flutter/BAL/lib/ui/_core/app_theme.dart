import 'package:flutter/material.dart';

import 'app_colors.dart';

/// Classe que concentra as configurações de tema do App.
abstract class AppTheme {
  static ThemeData appTheme = ThemeData.light().copyWith(

    // Tema dos botões
    //
    // Todos os botões (ElevatedButton) herdam esse tema.
    elevatedButtonTheme: ElevatedButtonThemeData(
        style: ButtonStyle(
          foregroundColor: WidgetStatePropertyAll(Colors.white),
          backgroundColor: WidgetStateColor.resolveWith((states) {
            if (states.contains(WidgetState.disabled)) {
              return Colors.grey;
            } else if (states.contains(WidgetState.pressed)) {
              return AppColors.secondary;
            }
            return AppColors.primary;
          }),
        )
    ),

    // Tema dos inputs
    //
    // Todos os os inputs (TextField, TextFormField, etc) herdam esse tema.
    inputDecorationTheme: InputDecorationTheme(
      labelStyle: TextStyle(
          color: Colors.grey,
          fontWeight: FontWeight.w500
      ),
      floatingLabelStyle: TextStyle(
          color: AppColors.primary,
          fontWeight: FontWeight.bold
      ),
      border: OutlineInputBorder(
        borderRadius: BorderRadius.circular(8.0),
        borderSide: BorderSide(color: Colors.grey.shade400, width: 1.0),
      ),
      focusedBorder: OutlineInputBorder(
        borderRadius: BorderRadius.circular(8.0),
        borderSide: BorderSide(color: AppColors.primary, width: 2.0),
      ),
      errorBorder: OutlineInputBorder(
        borderRadius: BorderRadius.circular(8.0),
        borderSide: const BorderSide(color: Colors.red, width: 1.5),
      ),
      contentPadding: const EdgeInsets.symmetric(vertical: 12.0, horizontal: 16.0),
      hintStyle: TextStyle(color: Colors.grey.shade500),
    ),
  );
}