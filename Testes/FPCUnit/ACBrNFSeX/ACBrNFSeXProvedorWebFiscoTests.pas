unit ACBrNFSeXProvedorWebFiscoTests;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils, ACBrTests.Util,
  ACBrNFSeX;

type

  { ACBrNFSeXProvedorWebFiscoTest }

  ACBrNFSeXProvedorWebFiscoTest = class(TTestCase)
  private
    FACBrNFSeX1: TACBrNFSeX;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure AoCriarComponente_ContadorDeNotas_DeveSerZero;
    procedure Proverdor_Eh_WebFisco;
    procedure LoadFromFile_PassandoXMLValidoNaoAssinado_CampoXml_EhIdenticoAoArquivo;
    procedure LoadFromFile_PassandoXMLValidoNaoAssinado_CamposCarregadosCorretamente;

  end;

implementation

uses
  ACBrConsts,
  ACBrUtil.Strings, ACBrUtil.DateTime, ACBrUtil.Math,
  ACBrNFSeXConversao;

const
  SArquivoWebFisco01  = '..\..\..\..\Recursos\NFSe\Provedores\WebFisco\WebFisco_01-nfse.xml';
  UmMunicipioWebFisco = 3527603; // Usado o XML da NFS-e de Luis Antonio - SP para o teste

{ ACBrNFSeXProvedorWebFiscoTest }

procedure ACBrNFSeXProvedorWebFiscoTest.SetUp;
begin
  inherited SetUp;
  FACBrNFSeX1 := TACBrNFSeX.Create(nil);
  FACBrNFSeX1.Configuracoes.Geral.CodigoMunicipio := UmMunicipioWebFisco;
end;

procedure ACBrNFSeXProvedorWebFiscoTest.TearDown;
begin
  FACBrNFSeX1.Free;
  inherited TearDown;
end;

procedure ACBrNFSeXProvedorWebFiscoTest.AoCriarComponente_ContadorDeNotas_DeveSerZero;
begin
  CheckEquals(0, FACBrNFSeX1.NotasFiscais.Count, 'Contador de NFSe Não é zero');
end;

procedure ACBrNFSeXProvedorWebFiscoTest.Proverdor_Eh_WebFisco;
begin
  CheckTrue((FACBrNFSeX1.Configuracoes.Geral.Provedor = proWebFisco), 'Provedor não é WebFisco');
end;

procedure ACBrNFSeXProvedorWebFiscoTest.LoadFromFile_PassandoXMLValidoNaoAssinado_CampoXml_EhIdenticoAoArquivo;
var
  lStrList: TStringList;
  sxml: string;
begin
  lStrList := TStringList.Create;;
  try
    //lStrList.TrailingLineBreak := False; //Não funciona no Delphi 7
    lStrList.LoadFromFile(SArquivoWebFisco01);
    sxml := lStrList.Text;
  finally
    lStrList.Free;
  end;

  //remove o linebreak que fica no final da string por ter vindo do "TStringList.Text"
  if Length(sxml) >= Length(sLineBreak) then
  begin
    sxml := Copy(sxml, 0, Length(sxml) - Length(sLineBreak));
  end;

  FACBrNFSeX1.NotasFiscais.LoadFromFile(SArquivoWebFisco01, False);
  CheckEquals(sxml, FACBrNFSeX1.NotasFiscais[0].XmlNfse, 'Campo XmlNfse não corresponde ao conteúdo do arquivo');
end;

procedure ACBrNFSeXProvedorWebFiscoTest.LoadFromFile_PassandoXMLValidoNaoAssinado_CamposCarregadosCorretamente;
begin
  FACBrNFSeX1.NotasFiscais.LoadFromFile(SArquivoWebFisco01, False);

  CheckTrue(FACBrNFSeX1.NotasFiscais.Count > 0, '');

  CheckEquals('99.999.999/0001-99', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.IdentificacaoPrestador.CpfCnpj, 'NFSe.Prestador.IdentificacaoPrestador.CpfCnpj valor incorreto') ;
  CheckEquals('39', FACBrNFSeX1.NotasFiscais[0].NFSe.Numero, 'NFSe.Numero valor incorreto') ;
  CheckEquals(EncodeDataHora('2022-09-01T12:57'), FACBrNFSeX1.NotasFiscais[0].NFSe.DataEmissao, 'NFSe.DataEmissao valor incorreto');
  CheckEquals('AAAA-1111-BBBB-2222-CCCC', FACBrNFSeX1.NotasFiscais[0].NFSe.CodigoVerificacao, 'NFSe.CodigoVerificacao valor incorreto');
  CheckEquals('999.999.999-99', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.IdentificacaoTomador.CpfCnpj, 'NFSe.Tomador.IdentificacaoTomador.CpfCnpj valor incorreto') ;
  CheckEquals('', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal, 'NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal valor incorreto') ;
  CheckEquals('', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual, 'NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual valor incorreto') ;
  CheckEquals('TESTE IMPRIME', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.RazaoSocial, 'NFSe.Tomador.RazaoSocial valor incorreto') ;
  CheckEquals('AV TESTE', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.Endereco, 'NFSe.Tomador.Endereco.Endereco valor incorreto') ;
  CheckEquals('888', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.Numero, 'NFSe.Tomador.Endereco.Numero valor incorreto') ;
  CheckEquals('CASA TESTE', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.Complemento, 'NFSe.Tomador.Endereco.Complemento valor incorreto') ;
  CheckEquals('JARDIM TESTE', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.Bairro, 'NFSe.Tomador.Endereco.Bairro valor incorreto') ;
  CheckEquals('LUIS ANTONIO', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.xMunicipio, 'NFSe.Tomador.Endereco.xMunicipio valor incorreto') ;
  CheckEquals('SP', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.UF, 'NFSe.Tomador.Endereco.UF valor incorreto') ;
  CheckEquals('14210-000', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.CEP, 'NFSe.Tomador.Endereco.CEP valor incorreto') ;

  CheckEquals('52.00', FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.ItemServico[0].ItemListaServico, 'NFSe.Servico.ItemServico[0].ItemListaServico valor incorreto') ;
  CheckEquals(2.01, SimpleRoundToEX(FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.ItemServico[0].Aliquota), 'NFSe.Servico.ItemServico[0].Aliquota valor incorreto') ;
  CheckEquals(10.00, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.ItemServico[0].ValorUnitario, 'NFSe.Servico.ItemServico[0].ValorUnitario valor incorreto') ;
  CheckEquals('TESTE DE SISTEMA', FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.ItemServico[0].Descricao, 'NFSe.Servico.ItemServico[0].Descricao valor incorreto') ;

  CheckEquals(10.00, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorServicos, 'NFSe.Servico.Valores.ValorServicos valor incorreto') ;
  CheckEquals(0.2, SimpleRoundToEX(FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorIss), 0.01, 'NFSe.Servico.Valores.ValorIss valor incorreto') ;
  CheckEquals(0, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorIssRetido, 'NFSe.Servico.Valores.ValorIssRetido valor incorreto') ;
  CheckEquals(0, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.DescontoIncondicionado, 'NFSe.Servico.Valores.DescontoIncondicionado valor incorreto') ;
  CheckEquals(0, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.DescontoCondicionado, 'NFSe.Servico.Valores.DescontoCondicionado valor incorreto') ;
  CheckEquals(0, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.AliquotaPis, 'NFSe.Servico.Valores.AliquotaPis valor incorreto') ;
  CheckEquals(0, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorPis, 'NFSe.Servico.Valores.ValorPis valor incorreto') ;
  CheckEquals(0, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.AliquotaCofins, 'NFSe.Servico.Valores.AliquotaCofins valor incorreto') ;
  CheckEquals(0, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorCofins, 'NFSe.Servico.Valores.ValorCofins valor incorreto') ;
  CheckEquals(0, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.AliquotaCsll, 'NFSe.Servico.Valores.AliquotaCsll valor incorreto') ;
  CheckEquals(0, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorCsll, 'NFSe.Servico.Valores.ValorCsll valor incorreto') ;
  CheckEquals(0, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.AliquotaInss, 'NFSe.Servico.Valores.AliquotaInss valor incorreto') ;
  CheckEquals(0, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorInss, 'NFSe.Servico.Valores.ValorInss valor incorreto') ;
  CheckEquals(0, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.AliquotaIr, 'NFSe.Servico.Valores.AliquotaIr valor incorreto') ;
  CheckEquals(0, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorIr, 'NFSe.Servico.Valores.ValorIr valor incorreto') ;

  CheckEquals('00002338', FACBrNFSeX1.NotasFiscais[0].NFSe.IdentificacaoRps.Numero, 'NFSe.IdentificacaoRps.Numero valor incorreto') ;
  CheckEquals('www.webfiscotecnologia.com.br/issqn/nfea/index.php?lv=prn&k=123', FACBrNFSeX1.NotasFiscais[0].NFSe.Link, 'NFSe.Link valor incorreto') ;
end;

initialization

  _RegisterTest('ACBrNFSeXProvedorWebFiscoTests', ACBrNFSeXProvedorWebFiscoTest);

end.
