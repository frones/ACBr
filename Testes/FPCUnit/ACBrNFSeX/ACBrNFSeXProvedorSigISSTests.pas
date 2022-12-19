unit ACBrNFSeXProvedorSigISSTests;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils, ACBrTests.Util,
  ACBrNFSeX;

type

  { ACBrNFSeXProvedorSigISSTest }

  ACBrNFSeXProvedorSigISSTest = class(TTestCase)
  private
    FACBrNFSeX1: TACBrNFSeX;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure AoCriarComponente_ContadorDeNotas_DeveSerZero;
    procedure Proverdor_Eh_SigISS;
    procedure LoadFromFile_PassandoXMLValidoNaoAssinado_CampoXml_EhIdenticoAoArquivo;
    procedure LoadFromFile_PassandoXMLValidoNaoAssinado_CamposCarregadosCorretamente;

  end;

implementation

uses
  synautil,
  ACBrConsts,
  ACBrUtil.Strings, ACBrUtil.Math,
  ACBrNFSeXConversao;

const
  SArquivoSigISS01  = '..\..\..\..\Recursos\NFSe\Provedores\SigISS\SigISS_01-nfse.xml';
  UmMunicipioSigISS = 3529005; // Usado o XML da NFS-e de Marilia - SP para o teste

{ ACBrNFSeXProvedorSigISSTest }

procedure ACBrNFSeXProvedorSigISSTest.SetUp;
begin
  inherited SetUp;
  FACBrNFSeX1 := TACBrNFSeX.Create(nil);
  FACBrNFSeX1.Configuracoes.Geral.CodigoMunicipio := UmMunicipioSigISS;
end;

procedure ACBrNFSeXProvedorSigISSTest.TearDown;
begin
  FACBrNFSeX1.Free;
  inherited TearDown;
end;

procedure ACBrNFSeXProvedorSigISSTest.AoCriarComponente_ContadorDeNotas_DeveSerZero;
begin
  CheckEquals(0, FACBrNFSeX1.NotasFiscais.Count, 'Contador de NFSe Não é zero');
end;

procedure ACBrNFSeXProvedorSigISSTest.Proverdor_Eh_SigISS;
begin
  CheckTrue((FACBrNFSeX1.Configuracoes.Geral.Provedor = proSigISS), 'Provedor não é SigISS');
end;

procedure ACBrNFSeXProvedorSigISSTest.LoadFromFile_PassandoXMLValidoNaoAssinado_CampoXml_EhIdenticoAoArquivo;
var
  lStrList: TStringList;
  sxml: string;
begin
  lStrList := TStringList.Create;;
  try
    //lStrList.TrailingLineBreak := False; //Não funciona no Delphi 7
    lStrList.LoadFromFile(SArquivoSigISS01);
    sxml := lStrList.Text;
  finally
    lStrList.Free;
  end;

  //remove o linebreak que fica no final da string por ter vindo do "TStringList.Text"
  if Length(sxml) >= Length(sLineBreak) then
  begin
    sxml := Copy(sxml, 0, Length(sxml) - Length(sLineBreak));
  end;

  FACBrNFSeX1.NotasFiscais.LoadFromFile(SArquivoSigISS01, False);
  CheckEquals(sxml, FACBrNFSeX1.NotasFiscais[0].XmlNfse, 'Campo XmlNfse não corresponde ao conteúdo do arquivo');
end;

procedure ACBrNFSeXProvedorSigISSTest.LoadFromFile_PassandoXMLValidoNaoAssinado_CamposCarregadosCorretamente;
begin
  FACBrNFSeX1.NotasFiscais.LoadFromFile(SArquivoSigISS01, False);

  CheckTrue(FACBrNFSeX1.NotasFiscais.Count > 0, '');

  CheckEquals('687', FACBrNFSeX1.NotasFiscais[0].NFSe.Numero, 'NFSe.Numero valor incorreto') ;
  CheckEquals('XXX123XX', FACBrNFSeX1.NotasFiscais[0].NFSe.CodigoVerificacao, 'NFSe.CodigoVerificacao valor incorreto');
  CheckEquals(DecodeRfcDateTime('May 11 2022'), FACBrNFSeX1.NotasFiscais[0].NFSe.DataEmissao, 'NFSe.DataEmissao valor incorreto');
  CheckEquals(DecodeRfcDateTime('May 11 2022'), FACBrNFSeX1.NotasFiscais[0].NFSe.DataEmissaoRps, 'NFSe.DataEmissaoRps valor incorreto');

  CheckEquals('NOME DO PRESTADOR', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.RazaoSocial, 'NFSe.Prestador.RazaoSocial valor incorreto') ;
  CheckEquals('RUA CENTRAL', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Endereco.Endereco, 'NFSe.Prestador.Endereco.Endereco valor incorreto') ;
  CheckEquals('', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Endereco.Numero, 'NFSe.Prestador.Endereco.Numero valor incorreto') ;
  CheckEquals('', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Endereco.Complemento, 'NFSe.Prestador.Endereco.Complemento valor incorreto') ;
  CheckEquals('CENTRO', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Endereco.Bairro, 'NFSe.Prestador.Endereco.Bairro valor incorreto') ;
  CheckEquals('MARILIA', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Endereco.xMunicipio, 'NFSe.Prestador.Endereco.xMunicipio valor incorreto') ;
  CheckEquals('SP', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Endereco.UF, 'NFSe.Prestador.Endereco.UF valor incorreto') ;
  CheckEquals('17519340', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Endereco.CEP, 'NFSe.Prestador.Endereco.CEP valor incorreto') ;
  CheckEquals('nome@provedor.com.br', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Contato.Email, 'NFSe.Prestador.Contato.Email valor incorreto') ;

  CheckEquals(1.50, FACBrNFSeX1.NotasFiscais[0].NFSe.ValoresNfse.ValorLiquidoNfse, 'NFSe.ValoresNfse.ValorLiquidoNfse valor incorreto') ;
  CheckEquals(1.50, FACBrNFSeX1.NotasFiscais[0].NFSe.ValoresNfse.BaseCalculo, 'NFSe.ValoresNfse.BaseCalculo valor incorreto') ;
  CheckEquals('2401', FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.ItemListaServico, 'NFSe.Servico.ItemListaServico valor incorreto') ;
  CheckEquals(4.23, SimpleRoundToEX(FACBrNFSeX1.NotasFiscais[0].NFSe.ValoresNfse.Aliquota), 'NFSe.ValoresNfse.Aliquota valor incorreto') ;
  CheckEquals(0.06, SimpleRoundToEX(FACBrNFSeX1.NotasFiscais[0].NFSe.ValoresNfse.ValorIss), 'NFSe.ValoresNfse.ValorIss valor incorreto') ;

  CheckEquals('', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.IdentificacaoTomador.CpfCnpj, 'NFSe.Tomador.IdentificacaoTomador.CpfCnpj valor incorreto') ;
  CheckEquals('', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.RazaoSocial, 'NFSe.Tomador.RazaoSocial valor incorreto') ;
  CheckEquals('', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.Endereco, 'NFSe.Tomador.Endereco.Endereco valor incorreto') ;
  CheckEquals('', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.Numero, 'NFSe.Tomador.Endereco.Numero valor incorreto') ;
  CheckEquals('', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.Complemento, 'NFSe.Tomador.Endereco.Complemento valor incorreto') ;
  CheckEquals('', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.Bairro, 'NFSe.Tomador.Endereco.Bairro valor incorreto') ;
  CheckEquals('', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.xMunicipio, 'NFSe.Tomador.Endereco.xMunicipio valor incorreto') ;
  CheckEquals('', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.UF, 'NFSe.Tomador.Endereco.UF valor incorreto') ;
  CheckEquals('', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.CEP, 'NFSe.Tomador.Endereco.CEP valor incorreto') ;
  CheckEquals('', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Contato.Email, 'NFSe.Tomador.Contato.Email valor incorreto') ;

  CheckEquals('Observação:;;Pedido(s): 42-3', FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Discriminacao, 'NFSe.Servico.Discriminacao valor incorreto') ;
  CheckEquals('2', FACBrNFSeX1.Provider.SituacaoTributariaToStr(FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.IssRetido), 'NFSe.Servico.Valores.IssRetido valor incorreto') ;
  CheckEquals('1', StatusNFSeToStr(FACBrNFSeX1.NotasFiscais[0].NFSe.SituacaoNfse), 'NFSe.SituacaoNfse incorreto') ;
  CheckEquals('1', FACBrNFSeX1.Provider.SimNaoToStr(FACBrNFSeX1.NotasFiscais[0].NFSe.OptanteSimplesNacional), 'NFSe.OptanteSimplesNacional incorreto') ;
  CheckEquals('https://marilia.sigiss.com.br/nfe_base.php?id=1234&hash=XXX123XX&nota=687&ccm=', FACBrNFSeX1.NotasFiscais[0].NFSe.Link, 'NFSe.Link valor incorreto') ;
end;

initialization

  _RegisterTest('ACBrNFSeXProvedorSigISSTests', ACBrNFSeXProvedorSigISSTest);

end.
