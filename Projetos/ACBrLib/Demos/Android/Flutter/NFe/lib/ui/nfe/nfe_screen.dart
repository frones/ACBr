import 'package:acbrlibnfe/ui/nfe/consultas_page.dart';
import 'package:acbrlibnfe/ui/nfe/distribuicao_dfe_page.dart';
import 'package:acbrlibnfe/ui/nfe/envio_page.dart';
import 'package:acbrlibnfe/ui/nfe/eventos_page.dart';
import 'package:acbrlibnfe/ui/nfe/inutilizacao_page.dart';
import 'package:flutter/material.dart';

import '../_core/app_colors.dart';

/// Tela que controla os Tabs de Comandos da NFe
class NfeScreen extends StatelessWidget {
  const NfeScreen({super.key});

  @override
  Widget build(BuildContext context) {
    return DefaultTabController(
        length: 5,
        child: Column(
          children: [
            SizedBox(
              width: double.infinity,
              child: TabBar(
                isScrollable: true,
                tabAlignment: TabAlignment.center,
                indicatorColor: AppColors.primary,
                labelColor: AppColors.primary,
                indicatorWeight: 5.0,
                tabs: [
                  Tab(child: Padding(padding: EdgeInsets.symmetric(horizontal: 24.0), child: Text('Envio'))),
                  Tab(child: Padding(padding: EdgeInsets.symmetric(horizontal: 24.0), child: Text('Consultas'))),
                  Tab(child: Padding(padding: EdgeInsets.symmetric(horizontal: 24.0), child: Text('Eventos'))),
                  Tab(child: Padding(padding: EdgeInsets.symmetric(horizontal: 24.0), child: Text('Inutilização'))),
                  Tab(child: Padding(padding: EdgeInsets.symmetric(horizontal: 24.0), child: Text('Distribuição DFe'))),
                ],
              ),
            ),
            Expanded(
              child: TabBarView(
                children: [
                  const EnvioPage(),
                  const ConsultasPage(),
                  const EventosPage(),
                  const InutilizacaoPage(),
                  const DistribuicaoDfePage(),
                ],
              ),
            ),
          ],
        ));
  }
}
