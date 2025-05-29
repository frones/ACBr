import 'package:demoacbrbal/ui/_core/app_colors.dart';
import 'package:demoacbrbal/utils/acbrlib_bal_helper.dart';
import 'package:flutter/material.dart';
import 'package:fluttertoast/fluttertoast.dart';

import '../../plugin/acbrbal_plugin.dart';

/// BalScreen é a tela com os comandos da lib BAL.
class BalScreen extends StatefulWidget {
  const BalScreen({super.key});

  @override
  State<BalScreen> createState() => _BalScreenState();
}

class _BalScreenState extends State<BalScreen> {
  /// Instância do plugin
  late AcbrbalPlugin _acbrbalplugin;

  String _resultPeso = '-9,00';
  bool _ativado = false;

  @override
  void initState() {
    super.initState();
    _acbrbalplugin = ACBrLibBalHelper().acbrbalplugin;
  }

  /// Função para setar peso recebido
  void setResultPeso(double peso) {
    setState(() {
      _resultPeso = peso.toStringAsFixed(3).replaceFirst('.', ',');
    });
  }

  /// Função para ativar e desativar o componente
  void toggleAtivado() {
    if (_ativado) {
      _acbrbalplugin.desativar();
    } else {
      _acbrbalplugin.ativar();
    }
    setState(() {
      _ativado = !_ativado;
    });
  }

  /// Função para ler o peso
  Future<double> getPeso() async {
    try {
      double peso = await _acbrbalplugin.lePeso(1000);
      return peso;
    } catch (e) {
      debugPrint('Erro ao chamar o comando lePeso: $e');
      return -1.0;
    }
  }

  void onClickButtonGetPeso() async {
    if (!_ativado) {
      Fluttertoast.showToast(
        msg: "Balança não ativada",
        toastLength: Toast.LENGTH_LONG,
        gravity: ToastGravity.BOTTOM,
      );
      return;
    }
    setResultPeso(await getPeso());
  }

  /// Função para resetar o peso
  void onClickButtonResetPeso() {
    setResultPeso(0.00);
  }

  @override
  Widget build(BuildContext context) {
    return Center(
        child: Padding(
      padding: const EdgeInsets.all(32.0),
      child: Container(
          width: double.infinity,
          decoration: BoxDecoration(
            color: Colors.white,
            boxShadow: [
              BoxShadow(
                blurRadius: 10,
                color: Color(0x33000000),
                offset: Offset(
                  0.0,
                  2.0,
                ),
              )
            ],
            borderRadius: BorderRadius.circular(16),
          ),
          child: Padding(
              padding: EdgeInsets.all(36),
              child: Column(
                mainAxisSize: MainAxisSize.min,
                mainAxisAlignment: MainAxisAlignment.center,
                children: [
                  Text(
                    'Peso Atual',
                    textAlign: TextAlign.center,
                    style: TextStyle(fontSize: 16.0, fontWeight: FontWeight.bold),
                  ),
                  Align(
                    alignment: AlignmentDirectional(0, 0),
                    child: Padding(
                      padding: EdgeInsets.all(16),
                      child: Container(
                        width: double.infinity,
                        height: 90,
                        decoration: BoxDecoration(
                          color: Color(0xFF121212),
                          borderRadius: BorderRadius.circular(8),
                        ),
                        child: Padding(
                          padding: EdgeInsets.all(8),
                          child: Text(_resultPeso,
                              textAlign: TextAlign.center,
                              style: const TextStyle(
                                fontFamily: 'RobotoMono',
                                fontSize: 48,
                                color: Color(0xFF00FF41),
                                // Verde digital
                                letterSpacing: 2,
                                fontWeight: FontWeight.bold,
                                shadows: [
                                  Shadow(
                                    blurRadius: 10,
                                    color: Color(0xFF00FF41),
                                    offset: Offset(0, 0),
                                  ),
                                ],
                              )),
                        ),
                      ),
                    ),
                  ),
                  Padding(
                    padding: EdgeInsetsDirectional.fromSTEB(0, 8, 0, 0),
                    child: Row(
                      mainAxisSize: MainAxisSize.max,
                      mainAxisAlignment: MainAxisAlignment.end,
                      children: [
                        Text(
                          'Kg',
                          style: TextStyle(
                            fontFamily: 'RobotoMono',
                            fontSize: 24,
                            color: AppColors.primary,
                            letterSpacing: 2,
                            fontWeight: FontWeight.bold,
                          ),
                        ),
                      ],
                    ),
                  ),
                  Padding(
                    padding: EdgeInsetsDirectional.fromSTEB(0, 8, 0, 8),
                    child: Container(
                      width: double.infinity,
                      height: 1,
                      decoration: BoxDecoration(
                        color: Theme.of(context).dividerColor,
                      ),
                    ),
                  ),
                  Padding(
                    padding: EdgeInsetsDirectional.fromSTEB(0, 16, 0, 0),
                    child: Row(
                        mainAxisAlignment: MainAxisAlignment.spaceEvenly,
                        children: [
                          Column(children: [
                            IconButton(
                              icon: const Icon(Icons.power_settings_new,
                                  color: Colors.white, size: 32),
                              onPressed: toggleAtivado,
                              tooltip: _ativado ? 'Desativar' : 'Ativar',
                              style: IconButton.styleFrom(
                                backgroundColor:
                                    _ativado ? Colors.green : Colors.grey,
                                shape: const CircleBorder(),
                                padding: const EdgeInsets.all(12),
                              ),
                            ),
                            Padding(
                              padding: const EdgeInsets.only(top: 8.0),
                              child: Text(
                                _ativado ? 'Ativado' : 'Desativado',
                                style: const TextStyle(
                                  fontSize: 14,
                                  color: Colors.black,
                                  fontWeight: FontWeight.bold,
                                ),
                              ),
                            )
                          ]),
                          Column(
                            children: [
                              IconButton(
                                icon: const Icon(Icons.scale,
                                    color: Colors.white, size: 32),
                                onPressed: onClickButtonGetPeso,
                                tooltip: 'Ler Peso',
                                style: IconButton.styleFrom(
                                  backgroundColor: AppColors.primary,
                                  shape: const CircleBorder(),
                                  padding: const EdgeInsets.all(12),
                                ),
                              ),
                              Padding(
                                padding: const EdgeInsets.only(top: 8.0),
                                child: Text(
                                  'Ler Peso',
                                  style: const TextStyle(
                                    fontSize: 14,
                                    color: Colors.black,
                                    fontWeight: FontWeight.bold,
                                  ),
                                ),
                              )
                            ],
                          ),
                          Column(
                            children: [
                              IconButton(
                                icon: const Icon(Icons.refresh,
                                    color: Colors.white, size: 32),
                                onPressed: onClickButtonResetPeso,
                                tooltip: 'Zerar Peso',
                                style: IconButton.styleFrom(
                                  backgroundColor: AppColors.primary,
                                  shape: const CircleBorder(),
                                  padding: const EdgeInsets.all(12),
                                ),
                              ),
                              Padding(
                                padding: const EdgeInsets.only(top: 8.0),
                                child: Text(
                                  'Zerar Peso',
                                  style: const TextStyle(
                                    fontSize: 14,
                                    color: Colors.black,
                                    fontWeight: FontWeight.bold,
                                  ),
                                ),
                              )
                            ],
                          ),
                        ]),
                  ),
                ],
              ))),
    ));
  }
}
