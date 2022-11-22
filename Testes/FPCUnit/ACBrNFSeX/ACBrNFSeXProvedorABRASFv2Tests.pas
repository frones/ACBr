unit ACBrNFSeXProvedorABRASFv2Tests;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils, ACBrTests.Util,
  ACBrNFSeX;

type

  { ACBrNFSeXProvedorABRASFv2Test }

  ACBrNFSeXProvedorABRASFv2Test = class(TTestCase)
  private
    FACBrNFSeX1: TACBrNFSeX;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure AoCriarComponente_ContadorDeNotas_DeveSerZero;
    procedure Proverdor_Eh_ABRASFv2;
    procedure LoadFromFile_PassandoXMLValidoNaoAssinado_CampoXml_EhIdenticoAoArquivo;
    procedure LoadFromFile_PassandoXMLValidoNaoAssinado_CamposCarregadosCorretamente;

  end;

implementation

uses
  ACBrConsts, ACBrUtil.Strings, ACBrUtil.DateTime, ACBrNFSeXConversao;

const
  SArquivoABRASFv201  = '..\..\..\..\Recursos\NFSe\Provedores\ABRASFv2\ABRASFv2_01-nfse.xml';
  UmMunicipioABRASFv2 = 5300108; // Usado o XML da NFS-e de Brasília - DF para o teste

{ ACBrNFSeXProvedorABRASFv2Test }

procedure ACBrNFSeXProvedorABRASFv2Test.SetUp;
begin
  inherited SetUp;
  FACBrNFSeX1 := TACBrNFSeX.Create(nil);
  FACBrNFSeX1.Configuracoes.Geral.CodigoMunicipio := UmMunicipioABRASFv2;
end;

procedure ACBrNFSeXProvedorABRASFv2Test.TearDown;
begin
  FACBrNFSeX1.Free;
  inherited TearDown;
end;

procedure ACBrNFSeXProvedorABRASFv2Test.AoCriarComponente_ContadorDeNotas_DeveSerZero;
begin
  CheckEquals(0, FACBrNFSeX1.NotasFiscais.Count, 'Contador de NFSe Não é zero');
end;

procedure ACBrNFSeXProvedorABRASFv2Test.Proverdor_Eh_ABRASFv2;
begin
  CheckTrue((FACBrNFSeX1.Configuracoes.Geral.Provedor = proISSNet), 'Provedor não é ISSNet');
end;

procedure ACBrNFSeXProvedorABRASFv2Test.LoadFromFile_PassandoXMLValidoNaoAssinado_CampoXml_EhIdenticoAoArquivo;
var
  lStrList: TStringList;
  sxml: string;

begin
  lStrList := TStringList.Create;;
  try
    //lStrList.TrailingLineBreak := False; //Não funciona no Delphi 7
    lStrList.LoadFromFile(SArquivoABRASFv201);
    sxml := lStrList.Text;
  finally
    lStrList.Free;
  end;

  //remove o linebreak que fica no final da string por ter vindo do "TStringList.Text"
  if Length(sxml) >= Length(sLineBreak) then
  begin
    sxml := Copy(sxml, 0, Length(sxml) - Length(sLineBreak));
  end;

  FACBrNFSeX1.NotasFiscais.LoadFromFile(SArquivoABRASFv201);
  CheckEquals(sxml, FACBrNFSeX1.NotasFiscais[0].XmlNfse, 'Campo XmlNfse não corresponde ao conteúdo do arquivo');
end;

procedure ACBrNFSeXProvedorABRASFv2Test.LoadFromFile_PassandoXMLValidoNaoAssinado_CamposCarregadosCorretamente;
begin
  FACBrNFSeX1.NotasFiscais.LoadFromFile(SArquivoABRASFv201);

  CheckTrue(FACBrNFSeX1.NotasFiscais.Count > 0, '');

  CheckEquals('5', FACBrNFSeX1.NotasFiscais[0].NFSe.Numero, 'NFSe.Numero valor incorreto') ;
  CheckEquals('B01234A5F', FACBrNFSeX1.NotasFiscais[0].NFSe.CodigoVerificacao, 'CodigoVerificacao não é B01234A5F');
  //CheckEquals(Iso8601ToDateTime('2022-10-19T17:21:01.017'), FACBrNFSeX1.NotasFiscais[0].NFSe.DataEmissao, 'NFSe.DataEmissao valor incorreto');
  //CheckEquals(' .', FACBrNFSeX1.NotasFiscais[0].NFSe.OutrasInformacoes, 'NFSe.OutrasInformacoes valor incorreto');

  CheckEquals(15.50, FACBrNFSeX1.NotasFiscais[0].NFSe.ValoresNfse.BaseCalculo, '.NFSe.ValoresNfse.BaseCalculo valor incorreto') ;
  CheckEquals(15.50, FACBrNFSeX1.NotasFiscais[0].NFSe.ValoresNfse.ValorLiquidoNfse, 'NFSe.ValoresNfse.ValorLiquidoNfse valor incorreto') ;

  //CheckEquals('Prestação de Serviços', FACBrNFSeX1.NotasFiscais[0].NFSe.infNFSe.d..., 'xxxxx valor incorreto') ;
  CheckEquals(0, FACBrNFSeX1.NotasFiscais[0].NFSe.ValorCredito, 'NFSe.ValorCredito valor incorreto') ;
  CheckEquals('EMITENTE TESTE', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.RazaoSocial, 'NFSe.Prestador.RazaoSocial valor incorreto') ;
  CheckEquals('EMITENTE TESTE', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.NomeFantasia, 'NFSe.Prestador.NomeFantasia valor incorreto') ;
  CheckEquals('ENDEREÇO TESTE', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Endereco.Endereco, 'NFSe.Prestador.Endereco.Endereco valor incorreto');
  CheckEquals('11', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Endereco.Numero, 'NFSe.Prestador.Endereco.Numero valor incorreto') ;
  CheckEquals('SALA', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Endereco.Complemento, 'NFSe.Prestador.Endereco.Complemento valor incorreto') ;
  CheckEquals('Recanto das Emas', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Endereco.Bairro, 'NFSe.Prestador.Endereco.Bairro valor incorreto') ;
  CheckEquals('5300108', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Endereco.CodigoMunicipio, 'NFSe.Prestador.Endereco.CodigoMunicipio valor incorreto') ;
  CheckEquals('DF', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Endereco.UF, 'NFSe.Prestador.Endereco.UF valor incorreto') ;
  CheckEquals('72610321', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Endereco.CEP, 'NFSe.Prestador.Endereco.CEP valor incorreto') ;

  //CheckEquals('', FACBrNFSeX1.NotasFiscais[0].NFSe., 'xxxxx valor incorreto') ;
  //CheckEquals('', FACBrNFSeX1.NotasFiscais[0].NFSe., 'xxxxx valor incorreto') ;
  //CheckEquals('', FACBrNFSeX1.NotasFiscais[0].NFSe., 'xxxxx valor incorreto') ;
  //CheckEquals('', FACBrNFSeX1.NotasFiscais[0].NFSe., 'xxxxx valor incorreto') ;
  //CheckEquals('', FACBrNFSeX1.NotasFiscais[0].NFSe., 'xxxxx valor incorreto') ;
  //CheckEquals('', FACBrNFSeX1.NotasFiscais[0].NFSe., 'xxxxx valor incorreto') ;
  //CheckEquals('', FACBrNFSeX1.NotasFiscais[0].NFSe., 'xxxxx valor incorreto') ;
  //CheckEquals('', FACBrNFSeX1.NotasFiscais[0].NFSe., 'xxxxx valor incorreto') ;
  //CheckEquals('', FACBrNFSeX1.NotasFiscais[0].NFSe., 'xxxxx valor incorreto') ;
  //CheckEquals('', FACBrNFSeX1.NotasFiscais[0].NFSe., 'xxxxx valor incorreto') ;
  //CheckEquals('', FACBrNFSeX1.NotasFiscais[0].NFSe., 'xxxxx valor incorreto') ;
  //CheckEquals('', FACBrNFSeX1.NotasFiscais[0].NFSe., 'xxxxx valor incorreto') ;
  //CheckEquals('', FACBrNFSeX1.NotasFiscais[0].NFSe., 'xxxxx valor incorreto') ;
  //CheckEquals('', FACBrNFSeX1.NotasFiscais[0].NFSe., 'xxxxx valor incorreto') ;
  //CheckEquals('', FACBrNFSeX1.NotasFiscais[0].NFSe., 'xxxxx valor incorreto') ;
  //CheckEquals('', FACBrNFSeX1.NotasFiscais[0].NFSe., 'xxxxx valor incorreto') ;
  //CheckEquals('', FACBrNFSeX1.NotasFiscais[0].NFSe., 'xxxxx valor incorreto') ;
  //CheckEquals('', FACBrNFSeX1.NotasFiscais[0].NFSe., 'xxxxx valor incorreto') ;
  //CheckEquals('', FACBrNFSeX1.NotasFiscais[0].NFSe., 'xxxxx valor incorreto') ;
  //CheckEquals('', FACBrNFSeX1.NotasFiscais[0].NFSe., 'xxxxx valor incorreto') ;
  //CheckEquals('', FACBrNFSeX1.NotasFiscais[0].NFSe., 'xxxxx valor incorreto') ;
  //CheckEquals('', FACBrNFSeX1.NotasFiscais[0].NFSe., 'xxxxx valor incorreto') ;

end;

initialization

  _RegisterTest('ACBrNFSeXProvedorABRASFv2Tests', ACBrNFSeXProvedorABRASFv2Test);

end.
