/// Constantes e Enumerados para Configuração de Balanças

/// Enumerado que define os modelos de balança disponíveis
enum ModeloBalanca {
  BALNENHUM,
  BALFILIZOLA,
  BALTOLEDO,
  BALTOLEDO2090,
  BALTOLEDO2180,
  BALURANO,
  BALLUCASTEC,
  BALMAGNA,
  BALDIGITRON,
  BALMAGELLAN,
  BALURANOPOP,
  BALLIDER,
  BALRINNERT,
  BALMULLER,
  BALSATURNO,
  BALAFTS,
  BALGENERICA,
  BALLIBRATEK,
  BALMICHELETTI,
  BALALFA,
  BALTOLEDO9091_8530_8540,
  BALWEIGHTECHWT1000,
  BALMARELCG62XL,
  BALWEIGHTECHWT3000_ABS,
  BALTOLEDO2090N,
  BALTOLEDOBCS21,
  BALPRECISION,
  BALDIGITRON_UL,
  BALLIBRATEKWT3000IR,
  BALTOLEDOTI420,
  BALWEIGHTECHWT27R_ETH,
  BALCAPITAL,
  BALMARTE,
  BALLENKELK2500,
  BALWEIGHTRUTEST
}

/// Enumerado que define os tipos de paridade disponíveis
enum Parity { NONE, ODD, EVEN, MARK }

/// Enumerado que define os tipos de HandShake disponíveis
enum Handshake {
  NONE,
  XON_XOFF,
  DTR,
  RTS,
}

/// Enumerado que define os tipos de Bits de Parada disponíveis
enum StopBits { ONE, ONE_POINT_FIVE, TWO }

/// Enumerado que define a taxa de transmissão (Baud Rate) disponível
enum BaudRate {
  B110,
  B300,
  B600,
  B1200,
  B2400,
  B4800,
  B9600,
  B14400,
  B19200,
  B38400,
  B56000,
  B57600
}

/// Enumerado que define os Bits de Dados disponíveis
enum DataBits { FIVE, SIX, SEVEN, EIGHT }

/// Extensão para obter o nome do modelo de balança
extension BaudRateExtension on BaudRate {
  String get value {
    switch (this) {
      case BaudRate.B110:
        return "110";
      case BaudRate.B300:
        return "300";
      case BaudRate.B600:
        return "600";
      case BaudRate.B1200:
        return "1200";
      case BaudRate.B2400:
        return "2400";
      case BaudRate.B4800:
        return "4800";
      case BaudRate.B9600:
        return "9600";
      case BaudRate.B14400:
        return "14400";
      case BaudRate.B19200:
        return "19200";
      case BaudRate.B38400:
        return "38400";
      case BaudRate.B56000:
        return "56000";
      case BaudRate.B57600:
        return "57600";
    }
  }

  static BaudRate? fromValue(String value) {
    for (var baud in BaudRate.values) {
      if (baud.value == value) {
        return baud;
      }
    }
    return BaudRate.B9600; // Retorna um valor padrão se não encontrar
  }
}

/// Extensão para obter o nome da paridade
extension DataBitsExtension on DataBits {
  String get value {
    switch (this) {
      case DataBits.FIVE:
        return "5";
      case DataBits.SIX:
        return "6";
      case DataBits.SEVEN:
        return "7";
      case DataBits.EIGHT:
        return "8";
    }
  }

  static fromValue(String value) {
    for (var dataBits in DataBits.values) {
      if (dataBits.value == value) {
        return dataBits;
      }
    }
    return DataBits.EIGHT; // Retorna um valor padrão se não encontrar
  }
}

extension StopBitsExtension on StopBits {
  String get value {
    switch (this) {
      case StopBits.ONE:
        return "1";
      case StopBits.ONE_POINT_FIVE:
        return "1.5";
      case StopBits.TWO:
        return "2";
    }
  }

  static StopBits? fromValue(String value) {
    for (var stopBit in StopBits.values) {
      if (stopBit.value == value) {
        return stopBit;
      }
    }
    return StopBits.ONE; // Retorna um valor padrão se não encontrar
  }
}
