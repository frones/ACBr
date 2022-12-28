unit ACBrNFSeXProvedorEquiplanoTests;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils, ACBrTests.Util,
  ACBrNFSeX;

type

  { ACBrNFSeXProvedorEquiplanoTest }

  ACBrNFSeXProvedorEquiplanoTest = class(TTestCase)
  private
    FACBrNFSeX1: TACBrNFSeX;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure AoCriarComponente_ContadorDeNotas_DeveSerZero;
    procedure Proverdor_Eh_Equiplano;
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
  SArquivoEquiplano01  = '..\..\..\..\Recursos\NFSe\Provedores\Equiplano\Equiplano_01-nfse.xml';
  UmMunicipioEquiplano = 4108403; // Usado o XML da NFS-e de Francisco Beltrao - PR para o teste

{ ACBrNFSeXProvedorEquiplanoTest }

procedure ACBrNFSeXProvedorEquiplanoTest.SetUp;
begin
  inherited SetUp;
  FACBrNFSeX1 := TACBrNFSeX.Create(nil);
  FACBrNFSeX1.Configuracoes.Geral.CodigoMunicipio := UmMunicipioEquiplano;
end;

procedure ACBrNFSeXProvedorEquiplanoTest.TearDown;
begin
  FACBrNFSeX1.Free;
  inherited TearDown;
end;

procedure ACBrNFSeXProvedorEquiplanoTest.AoCriarComponente_ContadorDeNotas_DeveSerZero;
begin
  CheckEquals(0, FACBrNFSeX1.NotasFiscais.Count, 'Contador de NFSe Não é zero');
end;

procedure ACBrNFSeXProvedorEquiplanoTest.Proverdor_Eh_Equiplano;
begin
  CheckTrue((FACBrNFSeX1.Configuracoes.Geral.Provedor = proEquiplano), 'Provedor não é Equiplano');
end;

procedure ACBrNFSeXProvedorEquiplanoTest.LoadFromFile_PassandoXMLValidoNaoAssinado_CampoXml_EhIdenticoAoArquivo;
var
  lStrList: TStringList;
  sxml: string;
begin
  lStrList := TStringList.Create;;
  try
    //lStrList.TrailingLineBreak := False; //Não funciona no Delphi 7
    lStrList.LoadFromFile(SArquivoEquiplano01);
    sxml := lStrList.Text;
  finally
    lStrList.Free;
  end;

  //remove o linebreak que fica no final da string por ter vindo do "TStringList.Text"
  if Length(sxml) >= Length(sLineBreak) then
  begin
    sxml := Copy(sxml, 0, Length(sxml) - Length(sLineBreak));
  end;

  FACBrNFSeX1.NotasFiscais.LoadFromFile(SArquivoEquiplano01, False);
  CheckEquals(sxml, FACBrNFSeX1.NotasFiscais[0].XmlNfse, 'Campo XmlNfse não corresponde ao conteúdo do arquivo');
end;

procedure ACBrNFSeXProvedorEquiplanoTest.LoadFromFile_PassandoXMLValidoNaoAssinado_CamposCarregadosCorretamente;
begin
  FACBrNFSeX1.NotasFiscais.LoadFromFile(SArquivoEquiplano01, False);

  CheckTrue(FACBrNFSeX1.NotasFiscais.Count > 0, '');

  CheckEquals('39', FACBrNFSeX1.NotasFiscais[0].NFSe.Numero, 'NFSe.Numero valor incorreto') ;
  CheckEquals('341DA', FACBrNFSeX1.NotasFiscais[0].NFSe.CodigoVerificacao, 'NFSe.CodigoVerificacao valor incorreto');
  CheckEquals(EncodeDataHora('2022-11-16T11:36:58'), FACBrNFSeX1.NotasFiscais[0].NFSe.DataEmissao, 'NFSe.DataEmissao valor incorreto');
  CheckEquals('38', FACBrNFSeX1.NotasFiscais[0].NFSe.IdentificacaoRps.Numero, 'NFSe.IdentificacaoRps.Numero valor incorreto') ;
  CheckEquals('1', FACBrNFSeX1.NotasFiscais[0].NFSe.IdentificacaoRps.Serie, 'NFSe.IdentificacaoRps.Serie valor incorreto') ;
  CheckEquals(EncodeDataHora('2022-11-16T00:00:00'), FACBrNFSeX1.NotasFiscais[0].NFSe.DataEmissaoRps, 'NFSe.DataEmissaoRps valor incorreto');
  CheckEquals('1', NaturezaOperacaoToStr(FACBrNFSeX1.NotasFiscais[0].NFSe.NaturezaOperacao), 'NFSe.NaturezaOperacao valor incorreto') ;
  CheckEquals('2', FACBrNFSeX1.Provider.SituacaoTributariaToStr(FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.IssRetido), 'NFSe.Servico.Valores.IssRetido valor incorreto') ;

  CheckEquals('11111111111111', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.IdentificacaoTomador.CpfCnpj, 'NFSe.Tomador.IdentificacaoTomador.CpfCnpj valor incorreto') ;
  CheckEquals('RAZÃO SOCIAL', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.RazaoSocial, 'NFSe.Tomador.RazaoSocial valor incorreto') ;
  CheckEquals('nome@provedor.com.br', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Contato.Email, 'NFSe.Tomador.Contato.Email valor incorreto') ;
  CheckEquals('RUA CENTRAL', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.Endereco, 'NFSe.Tomador.Endereco.Endereco valor incorreto') ;
  CheckEquals('420', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.Numero, 'NFSe.Tomador.Endereco.Numero valor incorreto') ;
  CheckEquals('CENTRO', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.Bairro, 'NFSe.Tomador.Endereco.Bairro valor incorreto') ;
  CheckEquals('4204202', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.CodigoMunicipio, 'NFSe.Tomador.Endereco.CodigoMunicipio valor incorreto') ;
  CheckEquals('SC', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.UF, 'NFSe.Tomador.Endereco.UF valor incorreto') ;
  CheckEquals('BRASIL', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.xPais, 'NFSe.Tomador.Endereco.xPais valor incorreto') ;
  CheckEquals('89804141', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.CEP, 'NFSe.Tomador.Endereco.CEP valor incorreto') ;
  CheckEquals('9999-9999', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Contato.Telefone, 'NFSe.Tomador.Contato.Telefone valor incorreto') ;

  CheckEquals('14.01', FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.ItemServico[0].ItemListaServico, 'NFSe.Servico.ItemServico[0].ItemListaServico valor incorreto') ;
  CheckEquals(410.00, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.ItemServico[0].ValorUnitario, 'NFSe.Servico.ItemServico[0].ValorUnitario valor incorreto') ;
  CheckEquals(3.00, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.ItemServico[0].Aliquota, 'NFSe.Servico.ItemServico[0].Aliquota valor incorreto') ;
  CheckEquals(410.00, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.ItemServico[0].BaseCalculo, 'NFSe.Servico.ItemServico[0].BaseCalculo valor incorreto') ;
  CheckEquals(12.30, SimpleRoundToEX(FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.ItemServico[0].ValorISS), 'NFSe.Servico.ItemServico[0].ValorISS valor incorreto') ;
  CheckEquals('Item Descricao', FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.ItemServico[0].Descricao, 'NFSe.Servico.ItemServico[0].Descricao valor incorreto') ;
  CheckEquals(410.00, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorLiquidoNfse, 'NFSe.Servico.Valores.ValorLiquidoNfse valor incorreto') ;

  CheckEquals(0.00, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorCofins, 'NFSe.Servico.Valores.ValorCofins valor incorreto') ;
  CheckEquals(0.00, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorCsll, 'NFSe.Servico.Valores.ValorCsll valor incorreto') ;
  CheckEquals(0.00, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorInss, 'NFSe.Servico.Valores.ValorInss valor incorreto') ;
  CheckEquals(0.00, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorIr, 'NFSe.Servico.Valores.ValorIr valor incorreto') ;
  CheckEquals(0.00, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorPis, 'NFSe.Servico.Valores.ValorPis valor incorreto') ;
  CheckEquals(12.30, SimpleRoundToEX(FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorIss), 'NFSe.Servico.Valores.ValorIss valor incorreto') ;

  CheckEquals(0.00, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.AliquotaCofins, 'NFSe.Servico.Valores.AliquotaCofins valor incorreto') ;
  CheckEquals(0.00, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.AliquotaCsll, 'NFSe.Servico.Valores.AliquotaCsll valor incorreto') ;
  CheckEquals(0.00, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.AliquotaInss, 'NFSe.Servico.Valores.AliquotaInss valor incorreto') ;
  CheckEquals(0.00, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.AliquotaIr, 'NFSe.Servico.Valores.AliquotaIr valor incorreto') ;
  CheckEquals(0.00, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.AliquotaPis, 'NFSe.Servico.Valores.AliquotaPis valor incorreto') ;
end;

initialization

  _RegisterTest('ACBrNFSeXProvedorEquiplanoTests', ACBrNFSeXProvedorEquiplanoTest);

end.
