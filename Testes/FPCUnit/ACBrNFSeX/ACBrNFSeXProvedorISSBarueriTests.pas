unit ACBrNFSeXProvedorISSBarueriTests;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils, ACBrTests.Util,
  ACBrNFSeX;

type

  { ACBrNFSeXProvedorISSBarueriTest }

  ACBrNFSeXProvedorISSBarueriTest = class(TTestCase)
  private
    FACBrNFSeX1: TACBrNFSeX;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure AoCriarComponente_ContadorDeNotas_DeveSerZero;
    procedure Proverdor_Eh_ISSBarueri;
    procedure LoadFromFile_PassandoXMLValidoNaoAssinado_CampoXml_EhIdenticoAoArquivo;
    procedure LoadFromFile_PassandoXMLValidoNaoAssinado_CamposCarregadosCorretamente;

  end;

implementation

uses
  ACBrConsts,
  ACBrUtil.Strings, ACBrUtil.DateTime, ACBrUtil.Math,
  ACBrNFSeXConversao;

const
  SArquivoISSBarueri01  = '..\..\..\..\Recursos\NFSe\Provedores\ISSBarueri\ISSBarueri_01-nfse.xml';
  UmMunicipioISSBarueri = 3505708; // Usado o XML da NFS-e de Barueri - SP para o teste

{ ACBrNFSeXProvedorISSBarueriTest }

procedure ACBrNFSeXProvedorISSBarueriTest.SetUp;
begin
  inherited SetUp;
  FACBrNFSeX1 := TACBrNFSeX.Create(nil);
  FACBrNFSeX1.Configuracoes.Geral.CodigoMunicipio := UmMunicipioISSBarueri;
end;

procedure ACBrNFSeXProvedorISSBarueriTest.TearDown;
begin
  FACBrNFSeX1.Free;
  inherited TearDown;
end;

procedure ACBrNFSeXProvedorISSBarueriTest.AoCriarComponente_ContadorDeNotas_DeveSerZero;
begin
  CheckEquals(0, FACBrNFSeX1.NotasFiscais.Count, 'Contador de NFSe Não é zero');
end;

procedure ACBrNFSeXProvedorISSBarueriTest.Proverdor_Eh_ISSBarueri;
begin
  CheckTrue((FACBrNFSeX1.Configuracoes.Geral.Provedor = proISSBarueri), 'Provedor não é ISSBarueri');
end;

procedure ACBrNFSeXProvedorISSBarueriTest.LoadFromFile_PassandoXMLValidoNaoAssinado_CampoXml_EhIdenticoAoArquivo;
var
  lStrList: TStringList;
  sxml: string;
begin
  lStrList := TStringList.Create;;
  try
    //lStrList.TrailingLineBreak := False; //Não funciona no Delphi 7
    lStrList.LoadFromFile(SArquivoISSBarueri01);
    sxml := lStrList.Text;
  finally
    lStrList.Free;
  end;

  //remove o linebreak que fica no final da string por ter vindo do "TStringList.Text"
  if Length(sxml) >= Length(sLineBreak) then
  begin
    sxml := Copy(sxml, 0, Length(sxml) - Length(sLineBreak));
  end;

  FACBrNFSeX1.NotasFiscais.LoadFromFile(SArquivoISSBarueri01, False);
  CheckEquals(sxml, FACBrNFSeX1.NotasFiscais[0].XmlNfse, 'Campo XmlNfse não corresponde ao conteúdo do arquivo');
end;

procedure ACBrNFSeXProvedorISSBarueriTest.LoadFromFile_PassandoXMLValidoNaoAssinado_CamposCarregadosCorretamente;
begin
  FACBrNFSeX1.NotasFiscais.LoadFromFile(SArquivoISSBarueri01, False);

  CheckTrue(FACBrNFSeX1.NotasFiscais.Count > 0, '');

  CheckEquals('12596', FACBrNFSeX1.NotasFiscais[0].NFSe.Numero, 'NFSe.Numero valor incorreto') ;
  CheckEquals('191S', FACBrNFSeX1.NotasFiscais[0].NFSe.CodigoVerificacao, 'NFSe.CodigoVerificacao valor incorreto');
  CheckEquals(EncodeDataHora('2023-01-02T14:14:57'), FACBrNFSeX1.NotasFiscais[0].NFSe.DataEmissao, 'NFSe.DataEmissao valor incorreto');

  CheckEquals(4256.75, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.BaseCalculo, 'NFSe.Servico.Valores.BaseCalculo valor incorreto') ;
  CheckEquals(2, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.Aliquota, 'NFSe.Servico.Valores.Aliquota valor incorreto') ;
  CheckEquals(85.14, SimpleRoundToEX(FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorIss), 'NFSe.Servico.Valores.ValorIss valor incorreto') ;
  CheckEquals(4256.75, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorLiquidoNfse, 'NFSe.Servico.Valores.ValorLiquidoNfse valor incorreto') ;

  CheckEquals('11111111111111', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.IdentificacaoPrestador.CpfCnpj, 'NFSe.Prestador.IdentificacaoPrestador.CpfCnpj valor incorreto') ;
  CheckEquals('1111111', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal, 'NFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal valor incorreto') ;
  CheckEquals('RAZÃO SOCIAL', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.RazaoSocial, 'NFSe.Prestador.RazaoSocial valor incorreto') ;
  CheckEquals('AVENIDA', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Endereco.Endereco, 'NFSe.Prestador.Endereco.Endereco valor incorreto') ;
  CheckEquals('641', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Endereco.Numero, 'NFSe.Prestador.Endereco.Numero valor incorreto') ;
  CheckEquals('CENTRO', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Endereco.Bairro, 'NFSe.Prestador.Endereco.Bairro valor incorreto') ;
  CheckEquals('BARUERI', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Endereco.xMunicipio, 'NFSe.Prestador.Endereco.xMunicipio valor incorreto') ;
  CheckEquals('SP', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Endereco.UF, 'NFSe.Prestador.Endereco.UF valor incorreto') ;
  CheckEquals('06460010', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Endereco.CEP, 'NFSe.Prestador.Endereco.CEP valor incorreto') ;

  CheckEquals('0000000985', FACBrNFSeX1.NotasFiscais[0].NFSe.IdentificacaoRps.Numero, 'NFSe.IdentificacaoRps.Numero valor incorreto') ;
  CheckEquals('RPS', FACBrNFSeX1.NotasFiscais[0].NFSe.IdentificacaoRps.Serie, 'NFSe.IdentificacaoRps.Serie valor incorreto') ;
  CheckEquals(EncodeDataHora('2023-01-02T00:00:00'), FACBrNFSeX1.NotasFiscais[0].NFSe.DataEmissaoRps, 'NFSe.DataEmissaoRps valor incorreto');
  CheckEquals(EncodeDataHora('2023-01-02T00:00:00'), FACBrNFSeX1.NotasFiscais[0].NFSe.Competencia, 'NFSe.Competencia valor incorreto');

  CheckEquals('140114213', FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.CodigoMunicipio, 'NFSe.Servico.CodigoMunicipio valor incorreto') ;
  CheckEquals('MANUTENÇÃO', FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Descricao, 'NFSe.Servico.Descricao valor incorreto') ;
  CheckEquals('SERVICO DE MAO DE OBRA', FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Discriminacao, 'NFSe.Servico.Discriminacao valor incorreto') ;
  CheckEquals('ISSQN devido a: BARUERI-SP', FACBrNFSeX1.NotasFiscais[0].NFSe.InformacoesComplementares, 'NFSe.InformacoesComplementares valor incorreto') ;

  CheckEquals('2', FACBrNFSeX1.Provider.SituacaoTributariaToStr(FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.IssRetido), 'NFSe.Servico.Valores.IssRetido valor incorreto') ;

  CheckEquals(85.14, SimpleRoundToEX(FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorIss), 'NFSe.Servico.Valores.ValorIss valor incorreto') ;
  CheckEquals(2, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.Aliquota, 'NFSe.Servico.Valores.Aliquota valor incorreto') ;
  CheckEquals(4256.75, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorServicos, 'NFSe.Servico.Valores.ValorServicos valor incorreto') ;
  CheckEquals(0, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorPis, 'NFSe.Servico.Valores.ValorPis valor incorreto') ;
  CheckEquals(0, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorCofins, 'NFSe.Servico.Valores.ValorCofins valor incorreto') ;
  CheckEquals(0, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorIr, 'NFSe.Servico.Valores.ValorIr valor incorreto') ;
  CheckEquals(0, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorCsll, 'NFSe.Servico.Valores.ValorCsll valor incorreto') ;
  CheckEquals(0, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorTotalTributos, 'NFSe.Servico.Valores.ValorTotalTributos valor incorreto') ;

  CheckEquals('11111111111111', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.IdentificacaoTomador.CpfCnpj, 'NFSe.Tomador.IdentificacaoTomador.CpfCnpj valor incorreto') ;
  CheckEquals('', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal, 'NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal valor incorreto') ;

  CheckEquals('RAZÃO SOCIAL', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.RazaoSocial, 'NFSe.Tomador.RazaoSocial valor incorreto') ;
  CheckEquals('AV.', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.Endereco, 'NFSe.Tomador.Endereco.Endereco valor incorreto') ;
  CheckEquals('453', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.Numero, 'NFSe.Tomador.Endereco.Numero valor incorreto') ;
  CheckEquals('APTO 16', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.Complemento, 'NFSe.Tomador.Endereco.Complemento valor incorreto') ;
  CheckEquals('CERQUEIRA CESAR', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.Bairro, 'NFSe.Tomador.Endereco.Bairro valor incorreto') ;
  CheckEquals('Sao Paulo', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.xMunicipio, 'NFSe.Tomador.Endereco.xMunicipio valor incorreto') ;
  CheckEquals('SP', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.UF, 'NFSe.Tomador.Endereco.UF valor incorreto') ;
  CheckEquals('01311907', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.CEP, 'NFSe.Tomador.Endereco.CEP valor incorreto') ;

  CheckEquals('nome@provedor.com.br', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Contato.Email, 'NFSe.Tomador.Contato.Email valor incorreto') ;

  CheckEquals('7', FACBrNFSeX1.Provider.RegimeEspecialTributacaoToStr(FACBrNFSeX1.NotasFiscais[0].NFSe.RegimeEspecialTributacao), 'NFSe.RegimeEspecialTributacao valor incorreto') ;

  {

  CheckEquals('52.00', FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.ItemServico[0].ItemListaServico, 'NFSe.Servico.ItemServico[0].ItemListaServico valor incorreto') ;
  CheckEquals(2.01, SimpleRoundToEX(FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.ItemServico[0].Aliquota), 'NFSe.Servico.ItemServico[0].Aliquota valor incorreto') ;
  CheckEquals(10.00, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.ItemServico[0].ValorUnitario, 'NFSe.Servico.ItemServico[0].ValorUnitario valor incorreto') ;
  CheckEquals('TESTE DE SISTEMA', FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.ItemServico[0].Descricao, 'NFSe.Servico.ItemServico[0].Descricao valor incorreto') ;

  CheckEquals(0, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorIssRetido, 'NFSe.Servico.Valores.ValorIssRetido valor incorreto') ;
  CheckEquals(0, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.DescontoIncondicionado, 'NFSe.Servico.Valores.DescontoIncondicionado valor incorreto') ;
  CheckEquals(0, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.DescontoCondicionado, 'NFSe.Servico.Valores.DescontoCondicionado valor incorreto') ;
  CheckEquals(0, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.AliquotaPis, 'NFSe.Servico.Valores.AliquotaPis valor incorreto') ;
  CheckEquals(0, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.AliquotaCofins, 'NFSe.Servico.Valores.AliquotaCofins valor incorreto') ;
  CheckEquals(0, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.AliquotaCsll, 'NFSe.Servico.Valores.AliquotaCsll valor incorreto') ;
  CheckEquals(0, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.AliquotaInss, 'NFSe.Servico.Valores.AliquotaInss valor incorreto') ;
  CheckEquals(0, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorInss, 'NFSe.Servico.Valores.ValorInss valor incorreto') ;
  CheckEquals(0, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.AliquotaIr, 'NFSe.Servico.Valores.AliquotaIr valor incorreto') ;

  CheckEquals('www.ISSBarueritecnologia.com.br/issqn/nfea/index.php?lv=prn&k=123', FACBrNFSeX1.NotasFiscais[0].NFSe.Link, 'NFSe.Link valor incorreto') ;
  }
end;

initialization

  _RegisterTest('ACBrNFSeXProvedorISSBarueriTests', ACBrNFSeXProvedorISSBarueriTest);

end.
