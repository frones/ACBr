/// Classe de dados para armazenar a resposta de Status Serviço
class StatusServico {
  String cStat;
  String cUF;
  String dhRecbto;
  String dhRetorno;
  String msg;
  String tMed;
  String verAplic;
  String versao;
  String xMotivo;
  String xObs;
  String tpAmb;

  StatusServico({
    required this.cStat,
    required this.cUF,
    required this.dhRecbto,
    required this.dhRetorno,
    required this.msg,
    required this.tMed,
    required this.verAplic,
    required this.versao,
    required this.xMotivo,
    required this.xObs,
    required this.tpAmb,
  });

  /// Metodo para converter o JSON em um objeto StatusServico
  ///
  /// - [json] - O JSON a ser convertido
  ///
  StatusServico.fromJson(Map<String, dynamic> json)
      : cStat = json['Status']['CStat'].toString(),
        cUF = json['Status']['CUF'].toString(),
        dhRecbto = json['Status']['DhRecbto'] ?? '',
        dhRetorno = json['Status']['DhRetorno'] ?? '',
        msg = json['Status']['Msg'] ?? '',
        tMed = json['Status']['TMed'].toString(),
        verAplic = json['Status']['VerAplic'] ?? '',
        versao = json['Status']['Versao'] ?? '',
        xMotivo = json['Status']['XMotivo'] ?? '',
        xObs = json['Status']['XObs'] ?? '',
        tpAmb = json['Status']['tpAmb'] ?? '';
}
