import 'package:acbrlibnfe/ui/_core/app_colors.dart';
import 'package:acbrlibnfe/ui/configuracoes/arquivos_page.dart';
import 'package:acbrlibnfe/ui/configuracoes/certificados_page.dart';
import 'package:acbrlibnfe/ui/configuracoes/documento_auxiliar_page.dart';
import 'package:acbrlibnfe/ui/configuracoes/email_page.dart';
import 'package:acbrlibnfe/ui/configuracoes/geral_page.dart';
import 'package:acbrlibnfe/ui/configuracoes/webservices_page.dart';
import 'package:flutter/material.dart';

/// Tela que controla os Tabs de Configurações.
class ConfiguracoesScreen extends StatelessWidget {
  const ConfiguracoesScreen({super.key});

  @override
  Widget build(BuildContext context) {
    return DefaultTabController(
        length: 6,
        child: Column(
          children: [
            TabBar(
              isScrollable: true,
              tabAlignment: TabAlignment.center,
              indicatorColor: AppColors.primary,
              labelColor: AppColors.primary,
              indicatorWeight: 5.0,
              tabs: [
                Tab(child: Padding(padding: EdgeInsets.symmetric(horizontal: 24.0), child: Text('Geral'))),
                Tab(child: Padding(padding: EdgeInsets.symmetric(horizontal: 24.0), child: Text('WebServices'))),
                Tab(child: Padding(padding: EdgeInsets.symmetric(horizontal: 24.0), child: Text('Certificados'))),
                Tab(child: Padding(padding: EdgeInsets.symmetric(horizontal: 24.0), child: Text('Arquivos'))),
                Tab(child: Padding(padding: EdgeInsets.symmetric(horizontal: 24.0), child: Text('Email'))),
                Tab(child: Padding(padding: EdgeInsets.symmetric(horizontal: 24.0), child: Text('Documento Auxiliar'))),
              ],
            ),
            Expanded(
              child: TabBarView(
                children: [
                  const GeralPage(),
                  const WebservicesPage(),
                  const CertificadosPage(),
                  const ArquivosPage(),
                  const EmailPage(),
                  const DocumentoAuxiliarPage(),
                ],
              ),
            ),
          ],
        ));
  }
}
