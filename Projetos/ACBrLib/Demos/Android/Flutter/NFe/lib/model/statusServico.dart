class statusServico {
  String CStat;
  String CUF;
  String DhRecbto;
  String DhRetorno;
  String Msg;
  String TMed;
  String VerAplic;
  String Versao;
  String XMotivo;
  String XObs;
  String tpAmb;

  statusServico({
    required this.CStat,
    required this.CUF,
    required this.DhRecbto,
    required this.DhRetorno,
    required this.Msg,
    required this.TMed,
    required this.VerAplic,
    required this.Versao,
    required this.XMotivo,
    required this.XObs,
    required this.tpAmb,
  });

  statusServico.fromJson(Map<String, dynamic> json)
      : CStat = json['Status']['CStat'].toString(),
        CUF = json['Status']['CUF'].toString(),
        DhRecbto = json['Status']['DhRecbto'] ?? '',
        DhRetorno = json['Status']['DhRetorno'] ?? '',
        Msg = json['Status']['Msg'] ?? '',
        TMed = json['Status']['TMed'].toString(),
        VerAplic = json['Status']['VerAplic'] ?? '',
        Versao = json['Status']['Versao'] ?? '',
        XMotivo = json['Status']['XMotivo'] ?? '',
        XObs = json['Status']['XObs'] ?? '',
        tpAmb = json['Status']['tpAmb'] ?? '';
}
