import 'package:flutter/material.dart';

/// Painel para exibir a resposta do servidor.
///
/// Possui um campo de texto que exibe a resposta e um botão para limpar a resposta.
class PainelResposta extends StatefulWidget {
  final String? response;
  final VoidCallback? onClear;

  /// Crie um Painel de Resposta
  ///
  /// - [response] é o texto a ser exibido no campo.
  /// - [onClear] é a função a ser chamada quando o botão de limpar for pressionado.
  ///
  const PainelResposta({super.key, this.response, this.onClear});

  @override
  State<PainelResposta> createState() => _PainelRespostaState();
}

class _PainelRespostaState extends State<PainelResposta> {
  final TextEditingController _responseController = TextEditingController();

  @override
  void initState() {
    super.initState();
    if (widget.response != null) {
      _responseController.text = widget.response ?? '';
    }
  }

  @override
  void didUpdateWidget(covariant PainelResposta oldWidget) {
    super.didUpdateWidget(oldWidget);
    if (widget.response != oldWidget.response) {
      _responseController.text = widget.response ?? '';
      setState(() {});
    }
  }

  @override
  void dispose() {
    _responseController.dispose();
    super.dispose();
  }

  void _clearResponse() {
    setState(() {
      _responseController.clear();
    });
    widget.onClear?.call();
  }

  @override
  Widget build(BuildContext context) {
    return Column(
      children: [
        Container(
          height: 250,
          width: double.infinity,
          decoration: BoxDecoration(
            border: Border.all(color: Colors.grey),
            borderRadius: BorderRadius.circular(4.0),
          ),
          child: _responseController.text.isEmpty
              ? Center(
                  child: Text(
                    'Resposta',
                    textAlign: TextAlign.center,
                    style: const TextStyle(
                      fontSize: 24.0,
                      fontWeight: FontWeight.bold,
                      color: Colors.grey,
                    ),
                  ),
                )
              : SingleChildScrollView(
                  padding: const EdgeInsets.all(8.0),
                  child: TextFormField(
                    controller: _responseController,
                    readOnly: true,
                    maxLines: null,
                    decoration: const InputDecoration(
                      border: InputBorder.none,
                      isDense: true,
                      contentPadding: EdgeInsets.zero,
                    ),
                    style: const TextStyle(
                      fontSize: 16.0,
                      color: Colors.black,
                    ),
                  ),
                ),
        ),
        const SizedBox(height: 10),
        Align(
          alignment: Alignment.centerRight,
          child: ElevatedButton.icon(
            onPressed: _clearResponse,
            icon: const Icon(Icons.clear),
            label: const Text(
              'Limpar Resposta',
              style: TextStyle(fontWeight: FontWeight.bold),
            ),
          ),
        ),
      ],
    );
  }
}
