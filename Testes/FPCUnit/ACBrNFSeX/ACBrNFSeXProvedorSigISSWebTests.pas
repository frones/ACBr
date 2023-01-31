unit ACBrNFSeXProvedorSigISSWebTests;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils, ACBrTests.Util,
  ACBrNFSeX;

type

  { ACBrNFSeXProvedorSigISSWebTest }

  ACBrNFSeXProvedorSigISSWebTest = class(TTestCase)
  private
    FACBrNFSeX1: TACBrNFSeX;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure AoCriarComponente_ContadorDeNotas_DeveSerZero;
    procedure Proverdor_Eh_SigISSWeb;
    procedure LoadFromFile_PassandoXMLValidoNaoAssinado_CampoXml_EhIdenticoAoArquivo;
    procedure LoadFromFile_PassandoXMLValidoNaoAssinado_CamposCarregadosCorretamente;

  end;

implementation

uses
  synautil,
  ACBrConsts,
  ACBrUtil.Strings, ACBrUtil.Math, ACBrUtil.DateTime,
  ACBrNFSeXConversao;

const
  SArquivoSigISSWeb01  = '..\..\..\..\Recursos\NFSe\Provedores\SigISSWeb\SigISSWeb_01-nfse.xml';
  UmMunicipioSigISSWeb = 3545803; // Usado o XML da NFS-e de Santa Barba do Oeste - SP para o teste

{ ACBrNFSeXProvedorSigISSWebTest }

procedure ACBrNFSeXProvedorSigISSWebTest.SetUp;
begin
  inherited SetUp;
  FACBrNFSeX1 := TACBrNFSeX.Create(nil);
  FACBrNFSeX1.Configuracoes.Geral.CodigoMunicipio := UmMunicipioSigISSWeb;
end;

procedure ACBrNFSeXProvedorSigISSWebTest.TearDown;
begin
  FACBrNFSeX1.Free;
  inherited TearDown;
end;

procedure ACBrNFSeXProvedorSigISSWebTest.AoCriarComponente_ContadorDeNotas_DeveSerZero;
begin
  CheckEquals(0, FACBrNFSeX1.NotasFiscais.Count, 'Contador de NFSe Não é zero');
end;

procedure ACBrNFSeXProvedorSigISSWebTest.Proverdor_Eh_SigISSWeb;
begin
  CheckTrue((FACBrNFSeX1.Configuracoes.Geral.Provedor = proSigISSWeb), 'Provedor não é SigISSWeb');
end;

procedure ACBrNFSeXProvedorSigISSWebTest.LoadFromFile_PassandoXMLValidoNaoAssinado_CampoXml_EhIdenticoAoArquivo;
var
  lStrList: TStringList;
  sxml: string;
begin
  lStrList := TStringList.Create;;
  try
    //lStrList.TrailingLineBreak := False; //Não funciona no Delphi 7
    lStrList.LoadFromFile(SArquivoSigISSWeb01);
    sxml := lStrList.Text;
  finally
    lStrList.Free;
  end;

  //remove o linebreak que fica no final da string por ter vindo do "TStringList.Text"
  if Length(sxml) >= Length(sLineBreak) then
  begin
    sxml := Copy(sxml, 0, Length(sxml) - Length(sLineBreak));
  end;

  FACBrNFSeX1.NotasFiscais.LoadFromFile(SArquivoSigISSWeb01, False);
  CheckEquals(sxml, FACBrNFSeX1.NotasFiscais[0].XmlNfse, 'Campo XmlNfse não corresponde ao conteúdo do arquivo');
end;

procedure ACBrNFSeXProvedorSigISSWebTest.LoadFromFile_PassandoXMLValidoNaoAssinado_CamposCarregadosCorretamente;
begin
  FACBrNFSeX1.NotasFiscais.LoadFromFile(SArquivoSigISSWeb01, False);

  CheckTrue(FACBrNFSeX1.NotasFiscais.Count > 0, '');

  CheckEquals('11111111111111', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.IdentificacaoPrestador.CpfCnpj, 'NFSe.Prestador.IdentificacaoPrestador.CpfCnpj valor incorreto') ;
  CheckEquals('111111111', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.IdentificacaoPrestador.InscricaoEstadual, 'NFSe.Prestador.IdentificacaoPrestador.InscricaoEstadual valor incorreto') ;
  CheckEquals('11111', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal, 'NFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal valor incorreto') ;
  CheckEquals('RAZÃO SOCIAL', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.RazaoSocial, 'NFSe.Prestador.RazaoSocial valor incorreto') ;
  CheckEquals('RUA', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Endereco.Endereco, 'NFSe.Prestador.Endereco.Endereco valor incorreto') ;
  CheckEquals('123', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Endereco.Numero, 'NFSe.Prestador.Endereco.Numero valor incorreto') ;
  CheckEquals('DISTRITO INDÚSTRIAL', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Endereco.Bairro, 'NFSe.Prestador.Endereco.Bairro valor incorreto') ;
  CheckEquals('13456167', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Endereco.CEP, 'NFSe.Prestador.Endereco.CEP valor incorreto') ;
  CheckEquals('SANTA BARBARA D OEST', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Endereco.xMunicipio, 'NFSe.Prestador.Endereco.xMunicipio valor incorreto') ;
  CheckEquals('SP', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Endereco.UF, 'NFSe.Prestador.Endereco.UF valor incorreto') ;
  CheckEquals('nome@provedor.com.br', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Contato.Email, 'NFSe.Prestador.Contato.Email valor incorreto') ;

  CheckEquals('11111111111', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.IdentificacaoTomador.CpfCnpj, 'NFSe.Tomador.IdentificacaoTomador.CpfCnpj valor incorreto') ;
  CheckEquals('INSCRICAO DE TESTE & TESTE', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.RazaoSocial, 'NFSe.Tomador.RazaoSocial valor incorreto') ;
  CheckEquals('RUA PRINCIPAL', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.Endereco, 'NFSe.Tomador.Endereco.Endereco valor incorreto') ;
  CheckEquals('100', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.Numero, 'NFSe.Tomador.Endereco.Numero valor incorreto') ;
  CheckEquals('APTO 11', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.Complemento, 'NFSe.Tomador.Endereco.Complemento valor incorreto') ;
  CheckEquals('CENTRO', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.Bairro, 'NFSe.Tomador.Endereco.Bairro valor incorreto') ;
  CheckEquals('14800000', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.CEP, 'NFSe.Tomador.Endereco.CEP valor incorreto') ;
  CheckEquals('Santa Barbara d Oeste', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.xMunicipio, 'NFSe.Tomador.Endereco.xMunicipio valor incorreto') ;
  CheckEquals('SP', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.UF, 'NFSe.Tomador.Endereco.UF valor incorreto') ;
  CheckEquals('BRASIL', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.xPais, 'NFSe.Tomador.Endereco.xPais valor incorreto') ;
  CheckEquals('1622223333', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Contato.Telefone, 'NFSe.Tomador.Contato.Telefone valor incorreto') ;
  CheckEquals('nome@provedor.com.br', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Contato.Email, 'NFSe.Tomador.Contato.Email valor incorreto') ;

  CheckEquals('8', FACBrNFSeX1.NotasFiscais[0].NFSe.Numero, 'NFSe.Numero valor incorreto') ;
  CheckEquals('E', FACBrNFSeX1.NotasFiscais[0].NFSe.SeriePrestacao, 'NFSe.SeriePrestacao valor incorreto') ;

  CheckEquals(100.35, SimpleRoundToEX(FACBrNFSeX1.NotasFiscais[0].NFSe.ValoresNfse.ValorLiquidoNfse), 'NFSe.ValoresNfse.ValorLiquidoNfse valor incorreto') ;
  CheckEquals(0.00, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorDeducoes, 'NFSe.Servico.Valores.ValorDeducoes valor incorreto') ;
  CheckEquals(100.35, SimpleRoundToEX(FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorServicos), 'NFSe.Servico.Valores.ValorServicos valor incorreto') ;
  CheckEquals(EncodeDataHora('28/01/2023', 'DD/MM/YYYY'), FACBrNFSeX1.NotasFiscais[0].NFSe.DataEmissao, 'NFSe.DataEmissao valor incorreto');
  CheckEquals('discriminacao I; discriminacao II', FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Discriminacao, 'NFSe.Servico.Discriminacao valor incorreto') ;
  CheckEquals('14.01', FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.ItemListaServico, 'NFSe.Servico.ItemListaServico valor incorreto') ;

  CheckEquals('2', FACBrNFSeX1.Provider.SituacaoTributariaToStr(FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.IssRetido), 'NFSe.Servico.Valores.IssRetido valor incorreto') ;
  CheckEquals(3.00, SimpleRoundToEX(FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.Aliquota), 'NFSe.Servico.Valores.Aliquota valor incorreto') ;
  CheckEquals(3.0105, SimpleRoundToEX(FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorIss, -4), 'NFSe.Servico.Valores.ValorIss valor incorreto') ;
  CheckEquals(100.35, SimpleRoundToEX(FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.BaseCalculo), 'NFSe.Servico.Valores.BaseCalculo valor incorreto') ;
  CheckEquals(0.00, SimpleRoundToEX(FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.AliquotaPis), 'NFSe.ValoresNfse.AliquotaPis valor incorreto') ;
  CheckEquals(0.00, SimpleRoundToEX(FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorPis), 'NFSe.ValoresNfse.ValorPis valor incorreto') ;
  CheckEquals(2.00, SimpleRoundToEX(FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.AliquotaCofins), 'NFSe.ValoresNfse.AliquotaCofins valor incorreto') ;
  CheckEquals(2.00, SimpleRoundToEX(FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorCofins), 'NFSe.ValoresNfse.ValorCofins valor incorreto') ;
  CheckEquals(0.00, SimpleRoundToEX(FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.AliquotaCsll), 'NFSe.ValoresNfse.AliquotaCsll valor incorreto') ;
  CheckEquals(0.00, SimpleRoundToEX(FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorCsll), 'NFSe.ValoresNfse.ValorCsll valor incorreto') ;
  CheckEquals(0.00, SimpleRoundToEX(FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.AliquotaIr), 'NFSe.ValoresNfse.AliquotaIr valor incorreto') ;
  CheckEquals(0.00, SimpleRoundToEX(FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorIr), 'NFSe.ValoresNfse.ValorIr valor incorreto') ;
  CheckEquals(0.00, SimpleRoundToEX(FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.AliquotaInss), 'NFSe.ValoresNfse.AliquotaInss valor incorreto') ;
  CheckEquals(0.00, SimpleRoundToEX(FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorInss), 'NFSe.ValoresNfse.ValorInss valor incorreto') ;

  CheckEquals('ACBrNFSeX-1.00', FACBrNFSeX1.NotasFiscais[0].NFSe.verAplic, 'NFSe.verAplic valor incorreto') ;
  CheckEquals('85', FACBrNFSeX1.NotasFiscais[0].NFSe.IdentificacaoRps.Serie, 'NFSe.IdentificacaoRps.Serie valor incorreto') ;
  CheckEquals('8', FACBrNFSeX1.NotasFiscais[0].NFSe.IdentificacaoRps.Numero, 'NFSe.IdentificacaoRps.Numero valor incorreto') ;
  CheckEquals('000004', FACBrNFSeX1.NotasFiscais[0].NFSe.CodigoVerificacao, 'NFSe.CodigoVerificacao valor incorreto');
end;

initialization

  _RegisterTest('ACBrNFSeXProvedorSigISSWebTests', ACBrNFSeXProvedorSigISSWebTest);

end.
