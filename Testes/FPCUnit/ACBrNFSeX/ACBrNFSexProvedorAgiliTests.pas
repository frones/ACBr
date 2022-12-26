unit ACBrNFSeXProvedorAgiliTests;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils, ACBrTests.Util,
  ACBrNFSeX;

type

  { ACBrNFSeXProvedorAgiliTests }

  ACBrNFSeXProvedorAgiliTests101 = class(TTestCase)
  private
    FACBrNFSeX : TACBrNFSeX;
  public
    procedure SetUp;override;
    procedure TearDown;override;
  published
    procedure AoCriarComponente_ContadorDeNotas_DeveSerZero;
    procedure Provedor_Eh_Agili;
    procedure LoadFromFile_PassandoXMLValidoNaoAssinado_CampoXML_EhIdenticoAoArquivo;
    procedure LoadFromFile_PassandoXMLValidoNaoAssinado_CamposCarregadosCorretamente;
  end;

implementation

uses
  ACBrNFSeXConversao, ACBrUtil.DateTime;

const
  SArquivoAgili01  = '..\..\..\..\Recursos\NFSe\Provedores\Agili\101\Agili_01-nfse.xml';
  UmMunicipioAgili = 5106257;

{ ACBrNFSeXProvedorAgiliTests101 }

procedure ACBrNFSeXProvedorAgiliTests101.SetUp;
begin
  inherited SetUp;
  FACBrNFSeX := TACBrNFSeX.Create(nil);
  FACBrNFSeX.Configuracoes.Geral.CodigoMunicipio:= UmMunicipioAgili;
end;

procedure ACBrNFSeXProvedorAgiliTests101.TearDown;
begin
  FACBrNFSeX.Free;
  inherited TearDown;
end;

procedure ACBrNFSeXProvedorAgiliTests101.AoCriarComponente_ContadorDeNotas_DeveSerZero;
begin
  CheckEquals(0, FACBrNFSeX.NotasFiscais.Count, 'Contador de NFSe não é zero');
end;

procedure ACBrNFSeXProvedorAgiliTests101.Provedor_Eh_Agili;
begin
  CheckTrue(FACBrNFSeX.Configuracoes.Geral.Provedor = proAgili, 'Provedor não é Agili');
end;

procedure ACBrNFSeXProvedorAgiliTests101.LoadFromFile_PassandoXMLValidoNaoAssinado_CampoXML_EhIdenticoAoArquivo;
var
  lStrList: TStringList;
  sxml : string;
begin
  lStrList := TStringList.Create;
  try
    //lStrList.TrailingLineBreak := False; //Não funciona no Delphi 7
    lStrList.LoadFromFile(SArquivoAgili01);
    sxml := lStrList.Text;
  finally
    lStrList.Free;
  end;

  //remove o linebreak que fica no final da string por ter vindo do "TStringList.Text"
  if Length(sxml) >= Length(sLineBreak)then
  begin
    sxml := Copy(sxml, 0, Length(sxml) - Length(sLineBreak));
  end;

  FACBrNFSeX.NotasFiscais.LoadFromFile(SArquivoAgili01, False);
  CheckEquals(sxml, FACBrNFSeX.NotasFiscais[0].XmlNfse, 'Campo XmlNfse não corresponde ao conteúdo do arquivo');

end;

procedure ACBrNFSeXProvedorAgiliTests101.LoadFromFile_PassandoXMLValidoNaoAssinado_CamposCarregadosCorretamente;
begin
  FACBrNFSeX.NotasFiscais.LoadFromFile(SArquivoAgili01, False);

  CheckTrue(FACBrNFSeX.NotasFiscais.Count > 0, '');

  CheckEquals('17', FACBrNFSeX.NotasFiscais[0].NFSe.Numero, 'NFSe.Numero valor incorreto');
  CheckEquals('aa11bb11cc11dd11ee11ff1111111111', FACBrNFSeX.NotasFiscais[0].NFSe.CodigoVerificacao, 'NFSe.CodigoVerificacao valor incorreto');
  CheckEquals(EncodeDataHora('2015-06-09T00:00:00'), FACBrNFSeX.NotasFiscais[0].NFSe.DataEmissao, 'NFSe.DataEmissao valor incorreto');

  CheckEquals('11111111111111', FACBrNFSeX.NotasFiscais[0].NFSe.Prestador.IdentificacaoPrestador.CpfCnpj, 'NFSe.Prestador.IdentificacaoPrestador.CpfCnpj valor incorreto');
  CheckEquals('1443', FACBrNFSeX.NotasFiscais[0].NFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal, 'NFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal valor incorreto');
  CheckEquals('RAZAO SOCIAL DE TESTE DO PRESTADOR DE SERVICOS1', FACBrNFSeX.NotasFiscais[0].NFSe.Prestador.RazaoSocial, 'NFSe.Prestador.RazaoSocial valor incorreto');
  CheckEquals('FANTASIA', FACBrNFSeX.NotasFiscais[0].NFSe.Prestador.NomeFantasia, 'NFSe.Prestador.NomeFantasia valor incorreto');
  CheckEquals('AVENIDA', FACBrNFSeX.NotasFiscais[0].NFSe.Prestador.Endereco.TipoLogradouro, 'NFSe.Prestador.Endereco.TipoLogradouro valor incorreto');
  CheckEquals('LOGRADOURO DE TESTE', FACBrNFSeX.NotasFiscais[0].NFSe.Prestador.Endereco.Endereco, 'NFSe.Prestador.Endereco.Endereco valor incorreto');
  CheckEquals('1295', FACBrNFSeX.NotasFiscais[0].NFSe.Prestador.Endereco.Numero, 'NFSe.Prestador.Endereco.Numero valor incorreto');
  CheckEquals('BAIRRO TESTE', FACBrNFSeX.NotasFiscais[0].NFSe.Prestador.Endereco.Bairro, 'NFSe.Prestador.Endereco.Bairro valor incorreto');

  CheckEquals('5106257', FACBrNFSeX.NotasFiscais[0].NFSe.Prestador.Endereco.CodigoMunicipio, 'NFSe.Prestador.Endereco.CodigoMunicipio valor incorreto');
  CheckEquals('MT', FACBrNFSeX.NotasFiscais[0].NFSe.Prestador.Endereco.UF, 'NFSe.Prestador.Endereco.UF valor incorreto');
  CheckEquals(1058, FACBrNFSeX.NotasFiscais[0].NFSe.Prestador.Endereco.CodigoPais, 'NFSe.Prestador.Endereco.CodigoPais valor incorreto');
  CheckEquals('11111111', FACBrNFSeX.NotasFiscais[0].NFSe.Prestador.Endereco.CEP, 'NFSe.Prestador.Endereco.CEP valor incorreto');
  CheckEquals('11111111', FACBrNFSeX.NotasFiscais[0].NFSe.Prestador.Contato.Telefone, 'NFSe.Prestador.Contato.Telefone valor incorreto');
  CheckEquals('emailteste01@provedor.com.br', FACBrNFSeX.NotasFiscais[0].NFSe.Prestador.Contato.Email, 'NFSe.Prestador.Contato.Email valor incorreto');

  CheckEquals('5106257', FACBrNFSeX.NotasFiscais[0].NFSe.OrgaoGerador.CodigoMunicipio, 'NFSe.OrgaoGerador.CodigoMunicipio valor incorreto');
  CheckEquals('MT', FACBrNFSeX.NotasFiscais[0].NFSe.OrgaoGerador.Uf, 'NFSe.OrgaoGerador.Uf valor incorreto');

  CheckEquals('100', FACBrNFSeX.NotasFiscais[0].NFSe.IdentificacaoRps.Numero, 'NFSe.IdentificacaoRps.Numero valor incorreto');
  CheckEquals('U', FACBrNFSeX.NotasFiscais[0].NFSe.IdentificacaoRps.Serie, 'NFSe.IdentificacaoRps.Serie valor incorreto');
  CheckTrue(FACBrNFSeX.NotasFiscais[0].NFSe.IdentificacaoRps.Tipo = trRPS, 'NFSe.IdentificacaoRps.Tipo valor incorreto');
  CheckEquals(EncodeDataHora('2015-06-09'), FACBrNFSeX.NotasFiscais[0].NFSe.DataEmissaoRps, 'NFSe.DataEmissaoRps valor incorreto');

  CheckEquals('11111111111', FACBrNFSeX.NotasFiscais[0].NFSe.Tomador.IdentificacaoTomador.CpfCnpj, 'NFSe.Tomador.IdentificacaoTomador.CpfCnpj valor incorreto');
  CheckEquals('0', FACBrNFSeX.NotasFiscais[0].NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal, 'NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal valor incorreto');
  CheckEquals('RAZAO SOCIAL', FACBrNFSeX.NotasFiscais[0].NFSe.Tomador.RazaoSocial, 'NFSe.Tomador.RazaoSocial valor incorreto');
  CheckEquals('RUA', FACBrNFSeX.NotasFiscais[0].NFSe.Tomador.Endereco.TipoLogradouro, 'NFSe.Tomador.Endereco.TipoLogradouro valor incorreto');
  CheckEquals('LOGRADOURO DE TESTE', FACBrNFSeX.NotasFiscais[0].NFSe.Tomador.Endereco.Endereco, 'NFSe.Tomador.Endereco.Endereco valor incorreto');
  CheckEquals('129', FACBrNFSeX.NotasFiscais[0].NFSe.Tomador.Endereco.Numero, 'NFSe.Tomador.Endereco.Numero valor incorreto');
  CheckEquals('COMPLEMENTO', FACBrNFSeX.NotasFiscais[0].NFSe.Tomador.Endereco.Complemento, 'NFSe.Tomador.Endereco.Complemento valor incorreto');
  CheckEquals('PROGRESSO', FACBrNFSeX.NotasFiscais[0].NFSe.Tomador.Endereco.Bairro, 'NFSe.Tomador.Endereco.Bairro valor incorreto');
  CheckEquals('2407203', FACBrNFSeX.NotasFiscais[0].NFSe.Tomador.Endereco.CodigoMunicipio, 'NFSe.Tomador.Endereco.CodigoMunicipio valor incorreto');
  CheckEquals('MT', FACBrNFSeX.NotasFiscais[0].NFSe.Tomador.Endereco.UF, 'NFSe.Tomador.Endereco.UF valor incorreto');
  CheckEquals(1058, FACBrNFSeX.NotasFiscais[0].NFSe.Tomador.Endereco.CodigoPais, 'NFSe.Tomador.Endereco.CodigoPais valor incorreto');
  CheckEquals('59500000', FACBrNFSeX.NotasFiscais[0].NFSe.Tomador.Endereco.Cep, 'NFSe.Tomador.Endereco.Cep valor incorreto');
  CheckEquals('1111111111', FACBrNFSeX.NotasFiscais[0].NFSe.Tomador.Contato.Telefone, 'NFSe.Tomador.Contato.Telefone valor incorreto');
  CheckEquals('enderecoemail@provedor.com.br', FACBrNFSeX.NotasFiscais[0].NFSe.Tomador.Contato.Email, 'NFSe.Tomador.Contato.Email valor incorreto');
  CheckEquals('0', FACBrNFSeX.NotasFiscais[0].NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual, 'NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual valor incorreto');

  CheckTrue(FACBrNFSeX.NotasFiscais[0].NFSe.RegimeEspecialTributacao = retMicroempresarioEmpresaPP, 'NFSe.RegimeEspecialTributacao valor incorreto');
  CheckTrue(FACBrNFSeX.NotasFiscais[0].NFSe.OptanteSimplesNacional = snNao, 'NFSe.OptanteSimplesNacional valor incorreto');
  CheckTrue(FACBrNFSeX.NotasFiscais[0].NFSe.Servico.Valores.IssRetido = stNormal, 'NFSe.Servico.Valores.ISSRetido valor incorreto');
  CheckTrue(FACBrNFSeX.NotasFiscais[0].NFSe.Servico.ResponsavelRetencao = rtTomador, 'NFSe.Servico.ResponsavelRetencao valor incorreto');
  CheckEquals('45.41203', FACBrNFSeX.NotasFiscais[0].NFSe.Servico.CodigoTributacaoMunicipio, 'NFSe.Servico.ItemsListaServico valor incorreto');
  CheckEquals('4541-203', FACBrNFSeX.NotasFiscais[0].NFSe.Servico.CodigoCnae, 'NFSe.Servico.CodigoCnae valor incorreto');
  CheckTrue(FACBrNFSeX.NotasFiscais[0].NFSe.Servico.ExigibilidadeISS = exiExigivel, 'NFSe.Servico.ExigibilidadeISS valor incorreto');
  CheckEquals(5106257, FACBrNFSeX.NotasFiscais[0].NFSe.Servico.MunicipioIncidencia, 'NFSe.Servico.MunicipioIncidencia valor incorreto');
  CheckEquals(10, FACBrNFSeX.NotasFiscais[0].NFSe.Servico.Valores.ValorServicos, 'NFSe.Servico.Valores.ValorServicos valor incorreto');
  CheckEquals(0, FACBrNFSeX.NotasFiscais[0].NFSe.Servico.Valores.DescontoIncondicionado, 'NFSe.Servico.Valores.DescontoIncondicionado valor incorreto');
  CheckEquals(0, FACBrNFSeX.NotasFiscais[0].NFSe.Servico.Valores.ValorPis, 'NFSe.Servico.Valores.ValorPis valor incorreto');
  CheckEquals(0, FACBrNFSeX.NotasFiscais[0].NFSe.Servico.Valores.ValorCofins, 'NFse.Servico.Valores.ValorCofins valor incorreto');
  CheckEquals(0, FACBrNFSeX.NotasFiscais[0].NFSe.Servico.Valores.ValorInss, 'NFSe.Servico.Valores.ValorInss valor incorreto');
  CheckEquals(0, FACBrNFSeX.NotasFiscais[0].NFSe.Servico.Valores.ValorIr, 'NFSe.Servico.Valores.ValorIr valor incorreto');
  CheckEquals(0, FACBrNFSeX.NotasFiscais[0].NFSe.Servico.Valores.ValorCsll, 'NFse.Servico.Valores.ValorCsll valor incorreto');
  CheckEquals(0, FACBrNFSeX.NotasFiscais[0].NFSe.Servico.Valores.ValorOutrasRetencoes, 'NFSe.Valores.ValorOutrasRetencoes valor incorreto');
  CheckEquals(10, FACBrNFSeX.NotasFiscais[0].NFse.Servico.Valores.BaseCalculo, 'NFSe.Servico.Valores.BaseCalculo valor incorreto');
  CheckEquals(5, FACBrNFSeX.NotasFiscais[0].NFSe.Servico.Valores.Aliquota, 'NFSe.Servico.Valores.Aliquota valor incorreto');
  CheckEquals(0.5, FACBrNFSeX.NotasFiscais[0].NFSe.Servico.Valores.ValorIss, 'NFSe.Servico.Valores.ValorISS valor incorreto');
  CheckEquals(10, FACBrNFSeX.NotasFiscais[0].NFSe.Servico.Valores.ValorLiquidoNfse, 'NFse.Servico.Valores.ValorLiquidoNfse valor incorreto');

  CheckEquals('000', FACBrNFSeX.NotasFiscais[0].NFSe.Servico.ItemServico[0].Descricao, 'NFSe.Servico.ItemServico.Descricao valor incorreto');
  CheckEquals(1, FACBrNFSeX.NotasFiscais[0].NFSe.Servico.ItemServico[0].Quantidade, 'NFSe.Servico.ItemServico.Quantidade valor incorreto');
  CheckEquals(10, FACBrNFSeX.NotasFiscais[0].NFSe.Servico.ItemServico[0].ValorUnitario, 'NFSe.Servico.ItemServico.ValorUnitario valor incorreto');
  CheckEquals(0, FACBrNFSeX.NotasFiscais[0].NFSe.Servico.ItemServico[0].DescontoIncondicionado, 'NFSe.Servico.ItemServico.DescontoIncondicionado valor incorreto');

end;

//initialization
//  _RegisterTest('ACBrNFSeXProvedorAgiliTests', ACBrNFSeXProvedorAgiliTests101);

end.

