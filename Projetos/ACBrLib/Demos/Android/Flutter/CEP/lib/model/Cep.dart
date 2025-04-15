class Cep {
  String bairro;
  String cep;
  String municipio;
  String logradouro;
  String uf;
  String codigoIbgeMunicipio;
  String codigoIbgeUf;

  Cep({
    required this.bairro,
    required this.cep,
    required this.logradouro,
    required this.municipio,
    required this.uf,
    required this.codigoIbgeMunicipio,
    required this.codigoIbgeUf,
  });

  Cep.fromJson(Map<String, dynamic> json)

      : bairro = json['CEP']['Endereco1']['Bairro'],
        cep = json['CEP']['Endereco1']['CEP'],
        logradouro = json['CEP']['Endereco1']['Logradouro'],
        municipio = json['CEP']['Endereco1']['Municipio'],
        uf = json['CEP']['Endereco1']['UF'],
        codigoIbgeMunicipio = json['CEP']['Endereco1']['IBGE_Municipio'],
        codigoIbgeUf = json['CEP']['Endereco1']['IBGE_UF'];

}