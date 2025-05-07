import 'package:acbrlibnfe/model/statusServico.dart';
import 'package:flutter/material.dart';
import '../../model/statusServico.dart';

class CardResult extends StatelessWidget {
  final statusServico result;
  final double width;

  CardResult({super.key, required this.result, required this.width});

  @override
  Widget build(BuildContext context) {
    return Padding(
        padding: EdgeInsets.all(8.0),
        child: SizedBox(
          width: width,
          child: Form(
            child: Column(
              mainAxisSize: MainAxisSize.min,
              children: <Widget>[
                SizedBox(height: 8),
                TextFormField(
                  initialValue: result.CStat,
                  decoration: InputDecoration(
                    labelText: 'CStat',
                    border: OutlineInputBorder(),
                  ),
                  readOnly: true,
                ),
                SizedBox(height: 8),
                TextFormField(
                  initialValue: result.CUF,
                  decoration: InputDecoration(
                    labelText: 'CUF',
                    border: OutlineInputBorder(),
                  ),
                  readOnly: true,
                ),
                SizedBox(height: 8),
                TextFormField(
                  initialValue: result.DhRecbto,
                  decoration: InputDecoration(
                    labelText: 'DhRecbto',
                    border: OutlineInputBorder(),
                  ),
                  readOnly: true,
                ),
                SizedBox(height: 8),
                TextFormField(
                  initialValue: result.DhRetorno,
                  decoration: InputDecoration(
                    labelText: 'DhRetorno',
                    border: OutlineInputBorder(),
                  ),
                  readOnly: true,
                ),
                SizedBox(height: 8),
                TextFormField(
                  initialValue: result.Msg,
                  decoration: InputDecoration(
                    labelText: 'Msg',
                    border: OutlineInputBorder(),
                  ),
                  readOnly: true,
                ),
                SizedBox(height: 8),
                TextFormField(
                  initialValue: result.TMed,
                  decoration: InputDecoration(
                    labelText: 'TMed',
                    border: OutlineInputBorder(),
                  ),
                  readOnly: true,
                ),
                SizedBox(height: 8),
                TextFormField(
                  initialValue: result.VerAplic,
                  decoration: InputDecoration(
                    labelText: 'VerAplic',
                    border: OutlineInputBorder(),
                  ),
                  readOnly: true,
                ),
                SizedBox(height: 8),
                TextFormField(
                  initialValue: result.Versao,
                  decoration: InputDecoration(
                    labelText: 'Versao',
                    border: OutlineInputBorder(),
                  ),
                  readOnly: true,
                ),
                SizedBox(height: 8),
                TextFormField(
                  initialValue: result.XMotivo,
                  decoration: InputDecoration(
                    labelText: 'XMotivo',
                    border: OutlineInputBorder(),
                  ),
                  readOnly: true,
                ),
                SizedBox(height: 8),
                TextFormField(
                  initialValue: result.XObs,
                  decoration: InputDecoration(
                    labelText: 'XObs',
                    border: OutlineInputBorder(),
                  ),
                  readOnly: true,
                ),
                SizedBox(height: 8),
                TextFormField(
                  initialValue: result.tpAmb,
                  decoration: InputDecoration(
                    labelText: 'tpAmb',
                    border: OutlineInputBorder(),
                  ),
                  readOnly: true,
                ),
              ],
            ),
          ),
        )
    );
  }

}