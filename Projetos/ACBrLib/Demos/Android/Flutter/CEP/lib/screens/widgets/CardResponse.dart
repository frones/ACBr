import 'package:flutter/material.dart';
import '../../model/Cep.dart';

class CardResult extends StatelessWidget {
  final Cep result;
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
                  initialValue: result.cep,
                  decoration: InputDecoration(
                    labelText: 'CEP',
                    border: OutlineInputBorder(),
                  ),
                  readOnly: true,
                ),
                SizedBox(height: 8),
                TextFormField(
                  initialValue: result.logradouro,
                  decoration: InputDecoration(
                    labelText: 'Logradouro',
                    border: OutlineInputBorder(),
                  ),
                  readOnly: true,
                ),
                SizedBox(height: 8),
                TextFormField(
                  initialValue: result.bairro,
                  decoration: InputDecoration(
                    labelText: 'Bairro',
                    border: OutlineInputBorder(),
                  ),
                  readOnly: true,
                ),
                SizedBox(height: 8),
                TextFormField(
                  initialValue: result.municipio,
                  decoration: InputDecoration(
                    labelText: 'Município',
                    border: OutlineInputBorder(),
                  ),
                  readOnly: true,
                ),
                SizedBox(height: 8),
                TextFormField(
                  initialValue: result.uf,
                  decoration: InputDecoration(
                    labelText: 'UF',
                    border: OutlineInputBorder(),
                  ),
                  readOnly: true,
                ),
                SizedBox(height: 8),
                TextFormField(
                  initialValue: result.codigoIbgeMunicipio,
                  decoration: InputDecoration(
                    labelText: 'Código IBGE Município',
                    border: OutlineInputBorder(),
                  ),
                  readOnly: true,
                ),
                SizedBox(height: 8),
                TextFormField(
                  initialValue: result.codigoIbgeUf,
                  decoration: InputDecoration(
                    labelText: 'Código IBGE UF',
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
