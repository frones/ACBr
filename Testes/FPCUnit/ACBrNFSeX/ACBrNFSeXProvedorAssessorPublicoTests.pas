unit ACBrNFSeXProvedorAssessorPublicoTests;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils, ACBrTests.Util,
  ACBrNFSeX;

type

  { ACBrNFSeXProvedorAssessorPublicoTest }

  ACBrNFSeXProvedorAssessorPublicoTest = class(TTestCase)
  private
    FACBrNFSeX1: TACBrNFSeX;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure AoCriarComponente_ContadorDeNotas_DeveSerZero;
    procedure Proverdor_Eh_AssessorPublico;
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
  SArquivoAssessorPublico01  = '..\..\..\..\Recursos\NFSe\Provedores\AssessorPublico\AssessorPublico_01-nfse.xml';
  UmMunicipioAssessorPublico = 3557006; // Usado o XML da NFS-e de Votorantim - SP para o teste

{ ACBrNFSeXProvedorAssessorPublicoTest }

procedure ACBrNFSeXProvedorAssessorPublicoTest.SetUp;
begin
  inherited SetUp;
  FACBrNFSeX1 := TACBrNFSeX.Create(nil);
  FACBrNFSeX1.Configuracoes.Geral.CodigoMunicipio := UmMunicipioAssessorPublico;
end;

procedure ACBrNFSeXProvedorAssessorPublicoTest.TearDown;
begin
  FACBrNFSeX1.Free;
  inherited TearDown;
end;

procedure ACBrNFSeXProvedorAssessorPublicoTest.AoCriarComponente_ContadorDeNotas_DeveSerZero;
begin
  CheckEquals(0, FACBrNFSeX1.NotasFiscais.Count, 'Contador de NFSe Não é zero');
end;

procedure ACBrNFSeXProvedorAssessorPublicoTest.Proverdor_Eh_AssessorPublico;
begin
  CheckTrue((FACBrNFSeX1.Configuracoes.Geral.Provedor = proAssessorPublico), 'Provedor não é AssessorPublico');
end;

procedure ACBrNFSeXProvedorAssessorPublicoTest.LoadFromFile_PassandoXMLValidoNaoAssinado_CampoXml_EhIdenticoAoArquivo;
var
  lStrList: TStringList;
  sxml: string;
begin
  lStrList := TStringList.Create;;
  try
    //lStrList.TrailingLineBreak := False; //Não funciona no Delphi 7
    lStrList.LoadFromFile(SArquivoAssessorPublico01);
    sxml := lStrList.Text;
  finally
    lStrList.Free;
  end;

  //remove o linebreak que fica no final da string por ter vindo do "TStringList.Text"
  if Length(sxml) >= Length(sLineBreak) then
  begin
    sxml := Copy(sxml, 0, Length(sxml) - Length(sLineBreak));
  end;

  FACBrNFSeX1.NotasFiscais.LoadFromFile(SArquivoAssessorPublico01, False);
  CheckEquals(sxml, FACBrNFSeX1.NotasFiscais[0].XmlNfse, 'Campo XmlNfse não corresponde ao conteúdo do arquivo');
end;

procedure ACBrNFSeXProvedorAssessorPublicoTest.LoadFromFile_PassandoXMLValidoNaoAssinado_CamposCarregadosCorretamente;
begin
  FACBrNFSeX1.NotasFiscais.LoadFromFile(SArquivoAssessorPublico01, False);

  CheckTrue(FACBrNFSeX1.NotasFiscais.Count > 0, '');

  CheckEquals('http://sql.sefvotorantim.sp.gov.br:80/issonline/servlet/hgeranfe?acbdefg', FACBrNFSeX1.NotasFiscais[0].NFSe.Link, 'NFSe.Link valor incorreto') ;
  CheckEquals('4891', FACBrNFSeX1.NotasFiscais[0].NFSe.Numero, 'NFSe.Numero valor incorreto') ;
  CheckEquals('27044617', FACBrNFSeX1.NotasFiscais[0].NFSe.NumeroLote, 'NFSe.NumeroLote valor incorreto') ;
  CheckEquals('4892', FACBrNFSeX1.NotasFiscais[0].NFSe.IdentificacaoRps.Numero, 'NFSe.IdentificacaoRps.Numero valor incorreto') ;
  CheckEquals('8777-2200-2931', FACBrNFSeX1.NotasFiscais[0].NFSe.CodigoVerificacao, 'NFSe.CodigoVerificacao valor incorreto');
  CheckEquals(EncodeDataHora('27/04/2022 12:46:17', 'DD/MM/YYYY'), FACBrNFSeX1.NotasFiscais[0].NFSe.DataEmissao, 'NFSe.DataEmissao valor incorreto');
  CheckEquals('NENHUMA INFORMAÇÃO COMPLEMENTAR REGISTRADA', FACBrNFSeX1.NotasFiscais[0].NFSe.OutrasInformacoes, 'NFSe.OutrasInformacoes valor incorreto') ;

  CheckEquals('01.07', FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.ItemListaServico, 'NFSe.Servico.ItemListaServico valor incorreto') ;
  CheckEquals('SUPORTE TÉCNICO EM INFORMÁTICA, INCLUSIVE INSTALAÇÃO, CONFIGURAÇÃO E MANUTENÇÃO DE PROGRAMAS DE COMP', FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.xItemListaServico, 'NFSe.Servico.xItemListaServico valor incorreto') ;
  CheckEquals('3557006', FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.CodigoMunicipio, 'NFSe.Servico.CodigoMunicipio valor incorreto') ;

  CheckEquals('3557006', FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.CodigoMunicipio, 'NFSe.Servico.CodigoMunicipio valor incorreto') ;
  CheckEquals('2', FACBrNFSeX1.Provider.SituacaoTributariaToStr(FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.IssRetido), 'NFSe.Servico.Valores.IssRetido valor incorreto') ;
  CheckEquals(240.00, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.BaseCalculo, 'NFSe.Servico.Valores.BaseCalculo valor incorreto') ;
  CheckEquals(0.00, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorIss, 'NFSe.Servico.Valores.ValorIss valor incorreto') ;
  CheckEquals(0.00, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorDeducoes, 'NFSe.Servico.Valores.ValorDeducoes valor incorreto') ;
  CheckEquals(0.00, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorOutrasRetencoes, 'NFSe.Servico.Valores.ValorOutrasRetencoes valor incorreto') ;
  CheckEquals(0.00, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorPis, 'NFSe.Servico.Valores.ValorPis valor incorreto') ;
  CheckEquals(0.00, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorCofins, 'NFSe.Servico.Valores.ValorCofins valor incorreto') ;
  CheckEquals(0.00, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorInss, 'NFSe.Servico.Valores.ValorInss valor incorreto') ;
  CheckEquals(0.00, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorIr, 'NFSe.Servico.Valores.ValorIr valor incorreto') ;
  CheckEquals(0.00, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorCsll, 'NFSe.Servico.Valores.ValorCsll valor incorreto') ;
  CheckEquals(0.00, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.OutrasRetencoes, 'NFSe.Servico.Valores.OutrasRetencoes valor incorreto') ;
  CheckEquals(2.2600, SimpleRoundToEX(FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.Aliquota), 'NFSe.Servico.Valores.Aliquota valor incorreto') ;
  CheckEquals(240.00, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorServicos, 'NFSe.Servico.Valores.ValorServicos valor incorreto') ;

  CheckEquals('99999', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal, 'NFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal valor incorreto') ;
  CheckEquals('99999999999999', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.IdentificacaoPrestador.CpfCnpj, 'NFSe.Prestador.IdentificacaoPrestador.CpfCnpj valor incorreto') ;
  CheckEquals('RAZÃO SOCIAL', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.RazaoSocial, 'NFSe.Prestador.RazaoSocial valor incorreto') ;
  CheckEquals('RUA RUA ARRUDA', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Endereco.Endereco, 'NFSe.Prestador.Endereco.Endereco valor incorreto') ;
  CheckEquals('40', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Endereco.Numero, 'NFSe.Prestador.Endereco.Numero valor incorreto') ;
  CheckEquals('PARQUE', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Endereco.Bairro, 'NFSe.Prestador.Endereco.Bairro valor incorreto') ;
  CheckEquals('3557006', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Endereco.CodigoMunicipio, 'NFSe.Prestador.Endereco.CodigoMunicipio valor incorreto') ;
  CheckEquals('VOTORANTIM', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Endereco.xMunicipio, 'NFSe.Prestador.Endereco.xMunicipio valor incorreto') ;
  CheckEquals('SP', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Endereco.UF, 'NFSe.Prestador.Endereco.UF valor incorreto') ;
  CheckEquals('18117236', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Endereco.CEP, 'NFSe.Prestador.Endereco.CEP valor incorreto') ;

  CheckEquals('99999', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal, 'NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal valor incorreto') ;
  CheckEquals('99999999999999', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.IdentificacaoTomador.CpfCnpj, 'NFSe.Tomador.IdentificacaoTomador.CpfCnpj valor incorreto') ;
  CheckEquals('RAZÃO SOCIAL', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.RazaoSocial, 'NFSe.Tomador.RazaoSocial valor incorreto') ;
  CheckEquals('AVENIDA AMAZONAS', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.Endereco, 'NFSe.Tomador.Endereco.Endereco valor incorreto') ;
  CheckEquals('88', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.Numero, 'NFSe.Tomador.Endereco.Numero valor incorreto') ;
  CheckEquals('SANTA', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.Bairro, 'NFSe.Tomador.Endereco.Bairro valor incorreto') ;
  CheckEquals('3557006', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.CodigoMunicipio, 'NFSe.Tomador.Endereco.CodigoMunicipio valor incorreto') ;
  CheckEquals('VOTORANTIM', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.xMunicipio, 'NFSe.Tomador.Endereco.xMunicipio valor incorreto') ;
  CheckEquals('SP', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.UF, 'NFSe.Tomador.Endereco.UF valor incorreto') ;
  CheckEquals('18117740', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.CEP, 'NFSe.Tomador.Endereco.CEP valor incorreto') ;

  CheckEquals('141', FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.ItemServico[0].CodServ, 'NFSe.Servico.ItemServico[0].CodServ valor incorreto') ;
  CheckEquals('SERVICOS PRESTADOS SERVCS. PRESTADOS ANTERIORMENTE', FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.ItemServico[0].Descricao, 'NFSe.Servico.ItemServico[0].Descricao valor incorreto') ;
  CheckEquals(1.00, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.ItemServico[0].Quantidade, 'NFSe.Servico.ItemServico[0].Quantidade valor incorreto') ;
  CheckEquals(0.00, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.ItemServico[0].DescontoIncondicionado, 'NFSe.Servico.ItemServico[0].DescontoIncondicionado valor incorreto') ;
  CheckEquals(240.000000, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.ItemServico[0].ValorUnitario, 'NFSe.Servico.ItemServico[0].ValorUnitario valor incorreto') ;
end;

initialization

  _RegisterTest('ACBrNFSeXProvedorAssessorPublicoTests', ACBrNFSeXProvedorAssessorPublicoTest);

end.
