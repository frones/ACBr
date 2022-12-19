unit ACBrNFSeXProvedorSoftPlanTests;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils, ACBrTests.Util,
  ACBrNFSeX;

type

  { ACBrNFSeXProvedorSoftPlanTest }

  ACBrNFSeXProvedorSoftPlanTest = class(TTestCase)
  private
    FACBrNFSeX1: TACBrNFSeX;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure AoCriarComponente_ContadorDeNotas_DeveSerZero;
    procedure Proverdor_Eh_SoftPlan;
    procedure LoadFromFile_PassandoXMLValidoNaoAssinado_CampoXml_EhIdenticoAoArquivo;
    procedure LoadFromFile_PassandoXMLValidoNaoAssinado_CamposCarregadosCorretamente;

  end;

implementation

uses
  ACBrConsts,
  ACBrUtil.Strings, ACBrUtil.DateTime, ACBrUtil.Math,
  ACBrNFSeXConversao;

const
  SArquivoSoftPlan01  = '..\..\..\..\Recursos\NFSe\Provedores\SoftPlan\SoftPlan_01-nfse.xml';
  UmMunicipioSoftPlan = 4205407; // Usado o XML da NFS-e de Florianopolis - SC para o teste

{ ACBrNFSeXProvedorSoftPlanTest }

procedure ACBrNFSeXProvedorSoftPlanTest.SetUp;
begin
  inherited SetUp;
  FACBrNFSeX1 := TACBrNFSeX.Create(nil);
  FACBrNFSeX1.Configuracoes.Geral.CodigoMunicipio := UmMunicipioSoftPlan;
end;

procedure ACBrNFSeXProvedorSoftPlanTest.TearDown;
begin
  FACBrNFSeX1.Free;
  inherited TearDown;
end;

procedure ACBrNFSeXProvedorSoftPlanTest.AoCriarComponente_ContadorDeNotas_DeveSerZero;
begin
  CheckEquals(0, FACBrNFSeX1.NotasFiscais.Count, 'Contador de NFSe Não é zero');
end;

procedure ACBrNFSeXProvedorSoftPlanTest.Proverdor_Eh_SoftPlan;
begin
  CheckTrue((FACBrNFSeX1.Configuracoes.Geral.Provedor = proSoftPlan), 'Provedor não é SoftPlan');
end;

procedure ACBrNFSeXProvedorSoftPlanTest.LoadFromFile_PassandoXMLValidoNaoAssinado_CampoXml_EhIdenticoAoArquivo;
var
  lStrList: TStringList;
  sxml: string;
begin
  lStrList := TStringList.Create;;
  try
    //lStrList.TrailingLineBreak := False; //Não funciona no Delphi 7
    lStrList.LoadFromFile(SArquivoSoftPlan01);
    sxml := lStrList.Text;
  finally
    lStrList.Free;
  end;

  //remove o linebreak que fica no final da string por ter vindo do "TStringList.Text"
  if Length(sxml) >= Length(sLineBreak) then
  begin
    sxml := Copy(sxml, 0, Length(sxml) - Length(sLineBreak));
  end;

  FACBrNFSeX1.NotasFiscais.LoadFromFile(SArquivoSoftPlan01, False);
  CheckEquals(sxml, FACBrNFSeX1.NotasFiscais[0].XmlNfse, 'Campo XmlNfse não corresponde ao conteúdo do arquivo');
end;

procedure ACBrNFSeXProvedorSoftPlanTest.LoadFromFile_PassandoXMLValidoNaoAssinado_CamposCarregadosCorretamente;
begin
  FACBrNFSeX1.NotasFiscais.LoadFromFile(SArquivoSoftPlan01, False);

  CheckTrue(FACBrNFSeX1.NotasFiscais.Count > 0, '');

  CheckEquals('94', FACBrNFSeX1.NotasFiscais[0].NFSe.Numero, 'NFSe.Numero valor incorreto') ;
  CheckEquals('1111222F1234FE2A', FACBrNFSeX1.NotasFiscais[0].NFSe.CodigoVerificacao, 'NFSe.CodigoVerificacao valor incorreto');
  CheckEquals(EncodeDataHora('2022-11-24T00:00:00-03:00'), FACBrNFSeX1.NotasFiscais[0].NFSe.DataEmissao, 'NFSe.DataEmissao valor incorreto');
  CheckEquals(EncodeDataHora('2022-11-24T10:26:39.313-03:00'), FACBrNFSeX1.NotasFiscais[0].NFSe.NfseCancelamento.DataHora, 'NFSe.NfseCancelamento.DataHora valor incorreto');
  CheckEquals('Consulta de NFS-e cancelada', FACBrNFSeX1.NotasFiscais[0].NFSe.MotivoCancelamento, 'NFSe.MotivoCancelamento valor incorreto');
  CheckEquals('9203', FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.CodigoCnae, 'NFSe.Servico.CodigoCnae valor incorreto');
  CheckEquals(1, FACBrNFSeX1.NotasFiscais[0].NFSe.Situacao, 'NFSe.Situacao valor incorreto') ;
  CheckEquals(0, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.BaseCalculo, 'NFSe.Servico.Valores.BaseCalculo valor incorreto');
  CheckEquals(0, SimpleRoundToEX(FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorIss), 0.01, 'NFSe.Servico.Valores.ValorIss valor incorreto') ;
  CheckEquals('99999999999999', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.IdentificacaoPrestador.CpfCnpj, 'NFSe.Prestador.IdentificacaoPrestador.CpfCnpj valor incorreto') ;
  CheckEquals('9999999', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal, 'NFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal valor incorreto') ;
  CheckEquals('RAZÃO SOCIAL', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.RazaoSocial, 'NFSe.Prestador.RazaoSocial valor incorreto') ;
  CheckEquals('AVN PEQUENO PRINCIPE', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Endereco.Endereco, 'NFSe.Prestador.Endereco.Endereco valor incorreto') ;
  CheckEquals('CAMPECHE', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Endereco.Bairro, 'NFSe.Prestador.Endereco.Bairro valor incorreto') ;
  CheckEquals('FLORIANOPOLIS', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Endereco.xMunicipio, 'NFSe.Prestador.Endereco.xMunicipio valor incorreto') ;
  CheckEquals('88063000', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Endereco.CEP, 'NFSe.Prestador.Endereco.CEP valor incorreto') ;
  CheckEquals('SC', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Endereco.UF, 'NFSe.Prestador.Endereco.UF valor incorreto') ;
  CheckEquals('NOME@PROVEDOR.COM.BR', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Contato.Email, 'NFSe.Prestador.Contato.Email valor incorreto') ;
  CheckEquals('4811223344', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Contato.Telefone, 'NFSe.Prestador.Contato.Telefone valor incorreto') ;

  CheckEquals('99', FACBrNFSeX1.NotasFiscais[0].NFSe.IdentificacaoRps.Numero, 'NFSe.IdentificacaoRps.Numero valor incorreto') ;

  CheckEquals('99999999999999', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.IdentificacaoTomador.CpfCnpj, 'NFSe.Tomador.IdentificacaoTomador.CpfCnpj valor incorreto') ;
  CheckEquals('9999999', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal, 'NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal valor incorreto') ;
  CheckEquals('RAZÃO SOCIAL', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.RazaoSocial, 'NFSe.Tomador.RazaoSocial valor incorreto') ;
  CheckEquals('R. Francisco', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.Endereco, 'NFSe.Tomador.Endereco.Endereco valor incorreto') ;
  CheckEquals('1234', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.Numero, 'NFSe.Tomador.Endereco.Numero valor incorreto') ;
  CheckEquals('Sala 3', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.Complemento, 'NFSe.Tomador.Endereco.Complemento valor incorreto') ;
  CheckEquals('Bairro Coqueiral', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.Bairro, 'NFSe.Tomador.Endereco.Bairro valor incorreto') ;
  CheckEquals('4104808', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.CodigoMunicipio, 'NFSe.Tomador.Endereco.CodigoMunicipio valor incorreto') ;
  CheckEquals('85807550', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.CEP, 'NFSe.Tomador.Endereco.CEP valor incorreto') ;
  CheckEquals('PR', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.UF, 'NFSe.Tomador.Endereco.UF valor incorreto') ;
  CheckEquals(1058, FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.CodigoPais, 'NFSe.Tomador.Endereco.CodigoPais valor incorreto') ;
  CheckEquals('nome@provedor.com.br', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Contato.Email, 'NFSe.Tomador.Contato.Email valor incorreto') ;
  CheckEquals('45112233444', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Contato.Telefone, 'NFSe.Tomador.Contato.Telefone valor incorreto') ;

  CheckEquals(0, SimpleRoundToEX(FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.ItemServico[0].Aliquota), 'NFSe.Servico.ItemServico[0].Aliquota valor incorreto') ;
  CheckEquals(0, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.ItemServico[0].BaseCalculo, 'NFSe.Servico.ItemServico[0].BaseCalculo valor incorreto') ;
  CheckEquals('8219901', FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.ItemServico[0].CodigoCnae, 'NFSe.Servico.ItemServico[0].CodigoCnae valor incorreto') ;
  CheckEquals('1', FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.ItemServico[0].CodServ, 'NFSe.Servico.ItemServico[0].CodServ valor incorreto') ;
  CheckEquals('Fotocopias - R$ 10,00||Forma de Pagamento: Venda a Vista (Dinheiro)', FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.ItemServico[0].Descricao, 'NFSe.Servico.ItemServico[0].Descricao valor incorreto') ;
  CheckEquals(1, SimpleRoundToEX(FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.ItemServico[0].Quantidade), 'NFSe.Servico.ItemServico[0].Quantidade valor incorreto') ;
  CheckEquals(10, SimpleRoundToEX(FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.ItemServico[0].ValorTotal), 'NFSe.Servico.ItemServico[0].ValorTotal valor incorreto') ;
  CheckEquals(10, SimpleRoundToEX(FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.ItemServico[0].ValorUnitario), 'NFSe.Servico.ItemServico[0].ValorUnitario valor incorreto') ;
end;

initialization

  _RegisterTest('ACBrNFSeXProvedorSoftPlanTests', ACBrNFSeXProvedorSoftPlanTest);

end.
