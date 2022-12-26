unit ACBrNFSeXProvedorBauhausTests;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils, ACBrTests.Util,
  ACBrNFSeX;

type

  { ACBrNFSeXProvedorBauhausTest }

  ACBrNFSeXProvedorBauhausTest = class(TTestCase)
  private
    FACBrNFSeX1: TACBrNFSeX;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure AoCriarComponente_ContadorDeNotas_DeveSerZero;
    procedure Proverdor_Eh_Bauhaus;
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
  SArquivoBauhaus01  = '..\..\..\..\Recursos\NFSe\Provedores\Bauhaus\Bauhaus_01-nfse.json';
  UmMunicipioBauhaus = 4208302; // Usado o XML da NFS-e de Itapema - SC para o teste

{ ACBrNFSeXProvedorBauhausTest }

procedure ACBrNFSeXProvedorBauhausTest.SetUp;
begin
  inherited SetUp;
  FACBrNFSeX1 := TACBrNFSeX.Create(nil);
  FACBrNFSeX1.Configuracoes.Geral.CodigoMunicipio := UmMunicipioBauhaus;
end;

procedure ACBrNFSeXProvedorBauhausTest.TearDown;
begin
  FACBrNFSeX1.Free;
  inherited TearDown;
end;

procedure ACBrNFSeXProvedorBauhausTest.AoCriarComponente_ContadorDeNotas_DeveSerZero;
begin
  CheckEquals(0, FACBrNFSeX1.NotasFiscais.Count, 'Contador de NFSe Não é zero');
end;

procedure ACBrNFSeXProvedorBauhausTest.Proverdor_Eh_Bauhaus;
begin
  CheckTrue((FACBrNFSeX1.Configuracoes.Geral.Provedor = proBauhaus), 'Provedor não é Bauhaus');
end;

procedure ACBrNFSeXProvedorBauhausTest.LoadFromFile_PassandoXMLValidoNaoAssinado_CampoXml_EhIdenticoAoArquivo;
var
  lStrList: TStringList;
  sxml: string;
begin
  lStrList := TStringList.Create;;
  try
    //lStrList.TrailingLineBreak := False; //Não funciona no Delphi 7
    lStrList.LoadFromFile(SArquivoBauhaus01);
    sxml := lStrList.Text;
  finally
    lStrList.Free;
  end;

  //remove o linebreak que fica no final da string por ter vindo do "TStringList.Text"
  if Length(sxml) >= Length(sLineBreak) then
  begin
    sxml := Copy(sxml, 0, Length(sxml) - Length(sLineBreak));
  end;

  FACBrNFSeX1.NotasFiscais.LoadFromFile(SArquivoBauhaus01, False);
  CheckEquals(sxml, FACBrNFSeX1.NotasFiscais[0].XmlNfse, 'Campo XmlNfse não corresponde ao conteúdo do arquivo');
end;

procedure ACBrNFSeXProvedorBauhausTest.LoadFromFile_PassandoXMLValidoNaoAssinado_CamposCarregadosCorretamente;
begin
  FACBrNFSeX1.NotasFiscais.LoadFromFile(SArquivoBauhaus01, False);

  CheckTrue(FACBrNFSeX1.NotasFiscais.Count > 0, '');

  CheckEquals('653', FACBrNFSeX1.NotasFiscais[0].NFSe.Numero, 'NFSe.Numero valor incorreto') ;
  CheckEquals(EncodeDataHora('2021-12-06'), FACBrNFSeX1.NotasFiscais[0].NFSe.DataEmissao, 'NFSe.DataEmissao valor incorreto');
  CheckEquals(EncodeDataHora('202112'), FACBrNFSeX1.NotasFiscais[0].NFSe.Competencia, 'NFSe.Competencia valor incorreto');
  CheckEquals('aa11bb22cc33dd44ee55ff66gg77hh88', FACBrNFSeX1.NotasFiscais[0].NFSe.CodigoVerificacao, 'NFSe.CodigoVerificacao valor incorreto');
  CheckEquals('4208302', FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.CodigoMunicipio, 'NFSe.Servico.CodigoMunicipio valor incorreto') ;
  CheckEquals('2', FACBrNFSeX1.Provider.SituacaoTributariaToStr(FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.IssRetido), 'NFSe.Servico.Valores.IssRetido valor incorreto') ;
  CheckEquals('', FACBrNFSeX1.NotasFiscais[0].NFSe.OutrasInformacoes, 'NFSe.OutrasInformacoes valor incorreto') ;

  CheckEquals('670', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal, 'NFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal valor incorreto') ;
  CheckEquals('11111111111111', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.IdentificacaoPrestador.CpfCnpj, 'NFSe.Prestador.IdentificacaoPrestador.CpfCnpj valor incorreto') ;
  CheckEquals('EMPRESA TESTE', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.RazaoSocial, 'NFSe.Prestador.RazaoSocial valor incorreto') ;
  CheckEquals('EMPRESA TESTE', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.NomeFantasia, 'NFSe.Prestador.NomeFantasia valor incorreto') ;
  CheckEquals('LOGRADOURO DE TESTE1', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Endereco.Endereco, 'NFSe.Prestador.Endereco.Endereco valor incorreto') ;
  CheckEquals('1010', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Endereco.Numero, 'NFSe.Prestador.Endereco.Numero valor incorreto') ;
  CheckEquals('CASA', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Endereco.Complemento, 'NFSe.Prestador.Endereco.Complemento valor incorreto') ;
  CheckEquals('CENTRO', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Endereco.Bairro, 'NFSe.Prestador.Endereco.Bairro valor incorreto') ;
  CheckEquals('4210803', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Endereco.CodigoMunicipio, 'NFSe.Prestador.Endereco.CodigoMunicipio valor incorreto') ;
  CheckEquals('11111111', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Endereco.CEP, 'NFSe.Prestador.Endereco.CEP valor incorreto') ;
  CheckEquals('', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Contato.Telefone, 'NFSe.Prestador.Contato.Telefone valor incorreto') ;
  CheckEquals('', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Contato.Email, 'NFSe.Prestador.Contato.Email valor incorreto') ;

  CheckEquals('11111111000111', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.IdentificacaoTomador.CpfCnpj, 'NFSe.Tomador.IdentificacaoTomador.CpfCnpj valor incorreto') ;
  CheckEquals('RAZAO SOCIAL DE TESTE DO TOM', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.RazaoSocial, 'NFSe.Tomador.RazaoSocial valor incorreto') ;
  CheckEquals('R TESTE1', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.Endereco, 'NFSe.Tomador.Endereco.Endereco valor incorreto') ;
  CheckEquals('1669', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.Numero, 'NFSe.Tomador.Endereco.Numero valor incorreto') ;
  CheckEquals('********', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.Complemento, 'NFSe.Tomador.Endereco.Complemento valor incorreto') ;
  CheckEquals('NITEROI', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.Bairro, 'NFSe.Tomador.Endereco.Bairro valor incorreto') ;
  CheckEquals('4208302', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.CodigoMunicipio, 'NFSe.Tomador.Endereco.CodigoMunicipio valor incorreto') ;
  CheckEquals('11111111', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.CEP, 'NFSe.Tomador.Endereco.CEP valor incorreto') ;
  CheckEquals('', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Contato.Telefone, 'NFSe.Tomador.Contato.Telefone valor incorreto') ;
  CheckEquals('enderecoemails@provedor.com.br', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Contato.Email, 'NFSe.Tomador.Contato.Email valor incorreto') ;

  CheckEquals('500040', FACBrNFSeX1.NotasFiscais[0].NFSe.IdentificacaoRps.Numero, 'NFSe.IdentificacaoRps.Numero valor incorreto') ;
  CheckEquals('1', FACBrNFSeX1.NotasFiscais[0].NFSe.IdentificacaoRps.Serie, 'NFSe.IdentificacaoRps.Serie valor incorreto') ;
  CheckEquals(EncodeDataHora('2021-12-06'), FACBrNFSeX1.NotasFiscais[0].NFSe.DataEmissaoRps, 'NFSe.DataEmissaoRps valor incorreto');

  CheckEquals('UN', FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.ItemServico[0].Unidade, 'NFSe.Servico.ItemServico[0].Unidade valor incorreto') ;
  CheckEquals(1, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.ItemServico[0].Quantidade, 'NFSe.Servico.ItemServico[0].Quantidade valor incorreto') ;
  CheckEquals('ASSESSORIA - R$ 1,00', FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.ItemServico[0].Descricao, 'NFSe.Servico.ItemServico[0].Descricao valor incorreto') ;
  CheckEquals(1, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.ItemServico[0].ValorUnitario, 'NFSe.Servico.ItemServico[0].ValorUnitario valor incorreto') ;

  CheckEquals(1, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorServicos, 'NFSe.Servico.Valores.ValorServicos valor incorreto') ;
  CheckEquals(0, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorDeducoes, 'NFSe.Servico.Valores.ValorDeducoes valor incorreto') ;
  CheckEquals(0, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorPis, 'NFSe.Servico.Valores.ValorPis valor incorreto') ;
  CheckEquals(0, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorCofins, 'NFSe.Servico.Valores.ValorCofins valor incorreto') ;
  CheckEquals(0, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorInss, 'NFSe.Servico.Valores.ValorInss valor incorreto') ;
  CheckEquals(0, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorIr, 'NFSe.Servico.Valores.ValorIr valor incorreto') ;
  CheckEquals(0, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorCsll, 'NFSe.Servico.Valores.ValorCsll valor incorreto') ;
  CheckEquals(0, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.OutrasRetencoes, 'NFSe.Servico.Valores.OutrasRetencoes valor incorreto') ;
  CheckEquals(0, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.DescontoIncondicionado, 'NFSe.Servico.Valores.DescontoIncondicionado valor incorreto') ;
  CheckEquals(0, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.DescontoCondicionado, 'NFSe.Servico.Valores.DescontoCondicionado valor incorreto') ;
  CheckEquals(1, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.BaseCalculo, 'NFSe.Servico.Valores.BaseCalculo valor incorreto') ;
  CheckEquals(4, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.Aliquota, 'NFSe.Servico.Valores.Aliquota valor incorreto') ;
  CheckEquals(0.04, SimpleRoundToEX(FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorIss), 'NFSe.Servico.Valores.ValorIss valor incorreto') ;
  CheckEquals(1, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorLiquidoNfse, 'NFSe.Servico.Valores.ValorLiquidoNfse valor incorreto') ;
  CheckEquals(0.18, SimpleRoundToEX(FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorTotalTributos, -4), 'NFSe.Servico.Valores.ValorTotalTributos valor incorreto') ;
  CheckEquals(0, FACBrNFSeX1.NotasFiscais[0].NFSe.ValorCredito, 'NFSe.ValorCredito valor incorreto') ;
end;

initialization

  _RegisterTest('ACBrNFSeXProvedorBauhausTests', ACBrNFSeXProvedorBauhausTest);

end.
