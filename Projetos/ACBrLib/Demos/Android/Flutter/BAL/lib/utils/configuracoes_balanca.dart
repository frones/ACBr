
import 'bal_consts.dart';

/// ConfiguracoesBalanca é uma classe que representa as configurações de uma balança
///
class ConfiguracoesBalanca {
  late Parity _parity;
  late Handshake _handshake;
  late BaudRate _baudRate;
  late DataBits _dataBits;
  late StopBits _stopBits;
  late ModeloBalanca _modeloBalanca;
  late String _porta;

  ConfiguracoesBalanca({
    Parity parity = Parity.NONE,
    Handshake handShake = Handshake.NONE,
    BaudRate baudRate = BaudRate.B9600,
    DataBits dataBits = DataBits.EIGHT,
    StopBits stopBits = StopBits.ONE,
    ModeloBalanca modeloBalanca = ModeloBalanca.BALNENHUM,
    String porta = "",
  })  : _parity = parity,
        _handshake = handShake,
        _baudRate = baudRate,
        _dataBits = dataBits,
        _stopBits = stopBits,
        _modeloBalanca = modeloBalanca,
        _porta = porta;


  Parity get parity => _parity;
  set parity(Parity value) {
    _parity = value;
  }

  Handshake get handshake => _handshake;
  set handshake(Handshake value) {
    _handshake = value;
  }
  BaudRate get baudRate => _baudRate;
  set baudRate(BaudRate value) {
    _baudRate = value;
  }

  DataBits get dataBits => _dataBits;
  set dataBits(DataBits value) {
    _dataBits = value;
  }

  StopBits get stopBits => _stopBits;
  set stopBits(StopBits value) {
    _stopBits = value;
  }
  ModeloBalanca get modeloBalanca => _modeloBalanca;
  set modeloBalanca(ModeloBalanca value) {
    _modeloBalanca = value;
  }

  String get porta => _porta;
  set porta(String value) {
    _porta = value;
  }
}