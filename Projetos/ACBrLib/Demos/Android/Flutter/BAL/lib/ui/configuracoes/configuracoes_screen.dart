import 'package:demoacbrbal/utils/acbrlib_bal_helper.dart';
import 'package:demoacbrbal/utils/configuracoes_balanca.dart';
import 'package:flutter/material.dart';

import '../../plugin/acbrbal_plugin.dart';
import '../../utils/bal_consts.dart';
import 'widgets/configuracoes_dropdown_menu.dart';

/// ConfiguracoesScreen é a tela de configurações da balança
/// Ela permite ao usuário configurar os parâmetros da balança, como paridade, handshake, baud rate, stop bits, data bits e modelo da balança.
///
class ConfiguracoesScreen extends StatefulWidget {
  const ConfiguracoesScreen({Key? key}) : super(key: key);

  @override
  State<ConfiguracoesScreen> createState() => _ConfiguracoesScreenState();
}

class _ConfiguracoesScreenState extends State<ConfiguracoesScreen> {
  /// Instância do plugin
  late AcbrbalPlugin _acbrbalPlugin;

  /// Configurações da balança
  late ConfiguracoesBalanca balDevice = ConfiguracoesBalanca();

  /// Controller da porta
  late TextEditingController portaController;

  @override
  void initState() {
    super.initState();
    _acbrbalPlugin = ACBrLibBalHelper().acbrbalplugin;
    portaController = TextEditingController();
    portaController.value = TextEditingValue(text: balDevice.porta);
    _loadSettings();
  }

  @override
  void dispose() {
    super.dispose();
    // Descartando controller para evitar vazamento de memória
    portaController.dispose();
  }

  /// Carrega as configurações da balança do plugin ACBrBAL
  void _loadSettings() async {
    try {
      String currentParity = "0";
      String currentHandshake = "0";
      String currentBaudRate = "9600";
      String currentStopBits = "1";
      String currentDataBits = "8";
      String currentModeloBalanca = "0";
      String currentPorta = "";
      currentParity =
          await _acbrbalPlugin.configLerValor("BAL_Device", "Parity");
      currentHandshake =
          await _acbrbalPlugin.configLerValor("BAL_Device", "Handshake");
      currentBaudRate =
          await _acbrbalPlugin.configLerValor("BAL_Device", "Baud");
      currentStopBits =
          await _acbrbalPlugin.configLerValor("BAL_Device", "Stop");
      currentDataBits =
          await _acbrbalPlugin.configLerValor("BAL_Device", "Data");

      currentModeloBalanca =
          await _acbrbalPlugin.configLerValor("BAL", "Modelo");

      currentPorta = await _acbrbalPlugin.configLerValor("BAL", "Porta");
      setState(() {
        balDevice.parity = Parity.values[int.parse(currentParity)];
        balDevice.handshake = Handshake.values[int.parse(currentHandshake)];
        balDevice.baudRate = BaudRateExtension.fromValue(currentBaudRate)!;
        balDevice.stopBits = StopBits.values[int.parse(currentStopBits)];
        balDevice.dataBits = DataBitsExtension.fromValue(currentDataBits);
        balDevice.modeloBalanca =
            ModeloBalanca.values[int.parse(currentModeloBalanca)];
        balDevice.porta = currentPorta;
      });
    } catch (e) {
      debugPrint("Erro ao ler configuração: ${e.toString()}");

      // Exibe uma mensagem de erro caso não consiga carregar as configurações
      if (mounted) {
        ScaffoldMessenger.of(context).showSnackBar(
          SnackBar(
              content: Text('Erro ao carregar configurações: Checar logs.')),
        );
      }
    }
  }

  /// Salva as configurações da balança no plugin ACBrBAL
  void _saveConfig() async {
    String messageSnackBar = '';
    try {
      await _acbrbalPlugin.configGravarValor(
          "BAL_Device", "Parity", balDevice.parity.index.toString());
      await _acbrbalPlugin.configGravarValor(
          "BAL_Device", "Handshake", balDevice.handshake.index.toString());
      await _acbrbalPlugin.configGravarValor(
          "BAL_Device", "Baud", balDevice.baudRate.value);
      await _acbrbalPlugin.configGravarValor(
          "BAL_Device", "Stop", balDevice.stopBits.index.toString());
      await _acbrbalPlugin.configGravarValor(
          "BAL_Device", "Data", balDevice.dataBits.value.toString());

      await _acbrbalPlugin.configGravarValor(
          "BAL", "Modelo", balDevice.modeloBalanca.index.toString());

      await _acbrbalPlugin.configGravarValor("BAL", "Porta", balDevice.porta);
      await _acbrbalPlugin.configGravar();

      messageSnackBar = 'Configurações salvas com sucesso!';
    } catch (e) {
      messageSnackBar = 'Erro ao salvar configurações: Checar logs.';
      debugPrint("Erro ao salvar configuração: $e");
    } finally {
      // Exibe uma mensagem de sucesso ou erro
      if (mounted) {
        ScaffoldMessenger.of(context).showSnackBar(
          SnackBar(content: Text(messageSnackBar)),
        );
      }
    }
  }

  void onChangeParity(Parity newParity) {
    setState(() {
      balDevice.parity = newParity;
    });
  }

  void onChangeHandshake(Handshake newHandshake) {
    setState(() {
      balDevice.handshake = newHandshake;
    });
  }

  void onChangeBaudRate(BaudRate newBaudRate) {
    setState(() {
      balDevice.baudRate = newBaudRate;
    });
  }

  void onChangeStopBits(StopBits newStopBits) {
    setState(() {
      balDevice.stopBits = newStopBits;
    });
  }

  void onChangeDataBits(DataBits newDataBits) {
    setState(() {
      balDevice.dataBits = newDataBits;
    });
  }

  void onChangeModeloBalanca(ModeloBalanca newModelo) {
    setState(() {
      balDevice.modeloBalanca = newModelo;
    });
  }

  void onChangePorta(String newPorta) {
    setState(() {
      balDevice.porta = newPorta;
    });
  }

  @override
  Widget build(BuildContext context) {
    return SingleChildScrollView(
      padding: EdgeInsetsDirectional.fromSTEB(24, 16, 24, 24),
      child: Column(
        mainAxisAlignment: MainAxisAlignment.start,
        crossAxisAlignment: CrossAxisAlignment.stretch,
        children: [
          Text(
            "Configurações da Balança",
            style: TextStyle(fontWeight: FontWeight.bold, fontSize: 24.0),
          ),
          const SizedBox(height: 16),
          TextField(
            decoration: const InputDecoration(
              labelText: 'Porta',
            ),
            controller: TextEditingController(text: balDevice.porta),
            onChanged: (value) {
              setState(() {
                balDevice.porta = value;
              });
            },
          ),
          const SizedBox(height: 16),
          ConfiguracoesDropdowmMenu<ModeloBalanca>(
            label: 'Modelo',
            values: ModeloBalanca.values,
            value: balDevice.modeloBalanca,
            onChanged: (newModeloBalanca) =>
                onChangeModeloBalanca(newModeloBalanca!),
            getLabel: (modelo) =>
                modelo.name.replaceAll("BAL", "").capitalize(),
          ),
          const SizedBox(height: 16),
          ConfiguracoesDropdowmMenu<Parity>(
            label: 'Paridade',
            values: Parity.values,
            value: balDevice.parity,
            onChanged: (newParity) => onChangeParity(newParity!),
            getLabel: (parity) => parity.name.capitalize(),
          ),
          const SizedBox(height: 16),
          ConfiguracoesDropdowmMenu<Handshake>(
            label: 'Handshake',
            values: Handshake.values,
            value: balDevice.handshake,
            onChanged: (newHandshake) => onChangeHandshake(newHandshake!),
            getLabel: (handshake) => handshake.name.capitalize(),
          ),
          const SizedBox(height: 16),
          ConfiguracoesDropdowmMenu<BaudRate>(
            label: 'Baud Rate',
            values: BaudRate.values,
            value: balDevice.baudRate,
            onChanged: (newBaudrate) => onChangeBaudRate(newBaudrate!),
            getLabel: (baudRate) => baudRate.value,
          ),
          const SizedBox(height: 16),
          ConfiguracoesDropdowmMenu<StopBits>(
            label: 'Stop Bits',
            values: StopBits.values,
            value: balDevice.stopBits,
            onChanged: (newStopBtis) => onChangeStopBits(newStopBtis!),
            getLabel: (stopBits) => stopBits.value.capitalize(),
          ),
          const SizedBox(height: 16),
          ConfiguracoesDropdowmMenu<DataBits>(
            label: 'Data Bits',
            values: DataBits.values,
            value: balDevice.dataBits,
            onChanged: (newDataBits) => onChangeDataBits(newDataBits!),
            getLabel: (dataBits) => dataBits.value.capitalize(),
          ),
          const SizedBox(height: 16),
          SizedBox(
            width: double.infinity,
            child: ElevatedButton(
              onPressed: _saveConfig,
              child: const Text(
                "Salvar Configurações",
                style: TextStyle(fontWeight: FontWeight.bold),
              ),
            ),
          ),
        ],
      ),
    );
  }
}
