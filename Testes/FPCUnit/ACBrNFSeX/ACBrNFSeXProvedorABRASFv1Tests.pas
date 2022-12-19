unit ACBrNFSeXProvedorABRASFv1Tests;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils, ACBrTests.Util,
  ACBrNFSeX;

type

  { ACBrNFSeXProvedorABRASFv1Test }

  ACBrNFSeXProvedorABRASFv1Test = class(TTestCase)
  private
    FACBrNFSeX1: TACBrNFSeX;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure AoCriarComponente_ContadorDeNotas_DeveSerZero;
    procedure Proverdor_Eh_ABRASFv1;
    procedure LoadFromFile_PassandoXMLValidoNaoAssinado_CampoXml_EhIdenticoAoArquivo;
    procedure LoadFromFile_PassandoXMLValidoNaoAssinado_CamposCarregadosCorretamente;

  end;

implementation

uses
  ACBrConsts, ACBrUtil.Strings, ACBrUtil.DateTime, ACBrNFSeXConversao;

const
  SArquivoABRASFv101  = '..\..\..\..\Recursos\NFSe\Provedores\ABRASFv1\ABRASFv1_01-nfse.xml';
  UmMunicipioABRASFv1 = 3549805; // Usado o XML da NFS-e de São José do Rio Preto- SP para o teste

{ ACBrNFSeXProvedorABRASFv1Test }

procedure ACBrNFSeXProvedorABRASFv1Test.SetUp;
begin
  inherited SetUp;

  FACBrNFSeX1 := TACBrNFSeX.Create(nil);
  FACBrNFSeX1.Configuracoes.Geral.CodigoMunicipio := UmMunicipioABRASFv1;
end;

procedure ACBrNFSeXProvedorABRASFv1Test.TearDown;
begin
  FACBrNFSeX1.Free;

  inherited TearDown;
end;

procedure ACBrNFSeXProvedorABRASFv1Test.AoCriarComponente_ContadorDeNotas_DeveSerZero;
begin
  CheckEquals(0, FACBrNFSeX1.NotasFiscais.Count, 'Contador de NFSe Não é zero');
end;

procedure ACBrNFSeXProvedorABRASFv1Test.Proverdor_Eh_ABRASFv1;
begin
  CheckTrue((FACBrNFSeX1.Configuracoes.Geral.Provedor = proGinfes), 'Provedor não é Ginfes');
end;

procedure ACBrNFSeXProvedorABRASFv1Test.LoadFromFile_PassandoXMLValidoNaoAssinado_CampoXml_EhIdenticoAoArquivo;
var
  lStrList: TStringList;
  sxml: string;
begin
  lStrList := TStringList.Create;;
  try
    //lStrList.TrailingLineBreak := False; //Não funciona no Delphi 7
    lStrList.LoadFromFile(SArquivoABRASFv101);
    sxml := lStrList.Text;
  finally
    lStrList.Free;
  end;

  //remove o linebreak que fica no final da string por ter vindo do "TStringList.Text"
  if Length(sxml) >= Length(sLineBreak) then
  begin
    sxml := Copy(sxml, 0, Length(sxml) - Length(sLineBreak));
  end;

  FACBrNFSeX1.NotasFiscais.LoadFromFile(SArquivoABRASFv101, False);
  CheckEquals(sxml, FACBrNFSeX1.NotasFiscais[0].XmlNfse, 'Campo XmlNfse não corresponde ao conteúdo do arquivo');
end;

procedure ACBrNFSeXProvedorABRASFv1Test.LoadFromFile_PassandoXMLValidoNaoAssinado_CamposCarregadosCorretamente;
begin
  FACBrNFSeX1.NotasFiscais.LoadFromFile(SArquivoABRASFv101, False);

  CheckTrue(FACBrNFSeX1.NotasFiscais.Count > 0, '');

  // InfNfse
  CheckEquals('3104', FACBrNFSeX1.NotasFiscais[0].NFSe.Numero, 'NFSe.Numero valor incorreto') ;
  CheckEquals('D12345EFG', FACBrNFSeX1.NotasFiscais[0].NFSe.CodigoVerificacao, 'NFSe.CodigoVerificacao valor incorreto');
  CheckEquals(EncodeDataHora('2022-08-22T16:07:23'), FACBrNFSeX1.NotasFiscais[0].NFSe.DataEmissao, 'NFSe.DataEmissao valor incorreto');

  // IdentificacaoRps
  CheckEquals('357', FACBrNFSeX1.NotasFiscais[0].NFSe.IdentificacaoRps.Numero, 'NFSe.IdentificacaoRps.Numero valor incorreto') ;
  CheckEquals('0', FACBrNFSeX1.NotasFiscais[0].NFSe.IdentificacaoRps.Serie, 'NFSe.IdentificacaoRps.Serie valor incorreto') ;
  CheckEquals('1', FACBrNFSeX1.Provider.TipoRPSToStr(FACBrNFSeX1.NotasFiscais[0].NFSe.IdentificacaoRps.Tipo), 'NFSe.IdentificacaoRps.Tipo valor incorreto') ;

  // InfNfse
  CheckEquals(EncodeDataHora('2022-08-22'), FACBrNFSeX1.NotasFiscais[0].NFSe.DataEmissaoRps, 'NFSe.DataEmissaoRps valor incorreto');
  CheckEquals('1', NaturezaOperacaoToStr(FACBrNFSeX1.NotasFiscais[0].NFSe.NaturezaOperacao), 'NFSe.NaturezaOperacao valor incorreto') ;
  CheckEquals('1', FACBrNFSeX1.Provider.RegimeEspecialTributacaoToStr(FACBrNFSeX1.NotasFiscais[0].NFSe.RegimeEspecialTributacao), 'NFSe.RegimeEspecialTributacao valor incorreto') ;
  CheckEquals('1', FACBrNFSeX1.Provider.SimNaoToStr(FACBrNFSeX1.NotasFiscais[0].NFSe.OptanteSimplesNacional), 'NFSe.OptanteSimplesNacional valor incorreto') ;
  CheckEquals('2', FACBrNFSeX1.Provider.SimNaoToStr(FACBrNFSeX1.NotasFiscais[0].NFSe.IncentivadorCultural), 'NFSe.IncentivadorCultural valor incorreto') ;
  CheckEquals(EncodeDataHora('2022-08-22'), FACBrNFSeX1.NotasFiscais[0].NFSe.Competencia, 'NFSe.Competencia valor incorreto');
  CheckEquals('2022', FACBrNFSeX1.NotasFiscais[0].NFSe.NfseSubstituida, 'NFSe.NfseSubstituida valor incorreto') ;
  CheckEquals('Outras Informações', FACBrNFSeX1.NotasFiscais[0].NFSe.OutrasInformacoes, 'NFSe.OutrasInformacoes valor incorreto') ;

  // Servico.Valores
  CheckEquals(65, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorServicos, 'NFSe.Servico.Valores.ValorServicos valor incorreto');
  CheckEquals(1, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorDeducoes, 'NFSe.Servico.Valores.ValorDeducoes valor incorreto');
  CheckEquals(1, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorPis, 'NFSe.Servico.Valores.ValorPis valor incorreto');
  CheckEquals(1, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorCofins, 'NFSe.Servico.Valores.ValorCofins valor incorreto');
  CheckEquals(1, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorInss, 'NFSe.Servico.Valores.ValorInss valor incorreto');
  CheckEquals(1, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorIr, 'NFSe.Servico.Valores.ValorIr valor incorreto');
  CheckEquals(1, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorCsll, 'NFSe.Servico.Valores.ValorCsll valor incorreto');
  CheckEquals('2', FACBrNFSeX1.Provider.SituacaoTributariaToStr(FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.IssRetido), 'NFSe.Servico.Valores.IssRetido valor incorreto');
  CheckEquals(1, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorIss, 'NFSe.Servico.Valores.ValorIss valor incorreto');
  CheckEquals(65, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.BaseCalculo, 'NFSe.Servico.Valores.BaseCalculo valor incorreto');
  CheckEquals(2.5, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.Aliquota, 'NFSe.Servico.Valores.Aliquota valor incorreto');
  CheckEquals(39, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorLiquidoNfse, 'NFSe.Servico.Valores.ValorLiquidoNfse valor incorreto');
  CheckEquals(10, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorIssRetido, 'NFSe.Servico.Valores.ValorIssRetido valor incorreto');
  CheckEquals(5, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.DescontoCondicionado, 'NFSe.Servico.Valores.DescontoCondicionado valor incorreto');
  CheckEquals(5, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.DescontoIncondicionado, 'NFSe.Servico.Valores.DescontoIncondicionado valor incorreto');

  // Servico
  CheckEquals('01.02', FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.ItemListaServico, 'NFSe.Servico.ItemListaServico valor incorreto') ;
  CheckEquals('12345', FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.CodigoCnae, 'NFSe.Servico.CodigoCnae valor incorreto') ;
  CheckEquals('01.02.00', FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.CodigoTributacaoMunicipio, 'NFSe.Servico.CodigoTributacaoMunicipio valor incorreto') ;
  CheckEquals('SERVIÇO TESTE Qtde :1,000 Total R$65,00\nDinheiro. R$ 65,00 \nMENSAGEM PADRÃO DA NFSE',
    FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Discriminacao, 'NFSe.Servico.Discriminacao valor incorreto') ;
  CheckEquals('3549805', FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.CodigoMunicipio, 'NFSe.Servico.CodigoMunicipio valor incorreto') ;

  // InfNfse
  CheckEquals(10, FACBrNFSeX1.NotasFiscais[0].NFSe.ValorCredito, 'NFSe.ValorCredito valor incorreto');

  // PrestadorServico.IdentificacaoPrestador
  CheckEquals('09922226000158', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.IdentificacaoPrestador.CpfCnpj, 'NFSe.Prestador.IdentificacaoPrestador.CpfCnpj valor incorreto') ;
  CheckEquals('3063260', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal, 'NFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal valor incorreto') ;

  // PrestadorServico
  CheckEquals('EMITENTE TESTE LTDA', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.RazaoSocial, 'NFSe.Prestador.RazaoSocial valor incorreto') ;
  CheckEquals('EMITENTE TESTE LTDA', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.NomeFantasia, 'NFSe.Prestador.NomeFantasia valor incorreto') ;

  // PrestadorServico.Endereco
  CheckEquals('R  TESTE', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Endereco.Endereco, 'NFSe.Prestador.Endereco.Endereco valor incorreto') ;
  CheckEquals('1', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Endereco.Numero, 'NFSe.Prestador.Endereco.Numero valor incorreto') ;
  CheckEquals('Apto 10', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Endereco.Complemento, 'NFSe.Prestador.Endereco.Complemento valor incorreto') ;
  CheckEquals('Centro', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Endereco.Bairro, 'NFSe.Prestador.Endereco.Bairro valor incorreto') ;
  CheckEquals('3549805', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Endereco.CodigoMunicipio, 'NFSe.Prestador.Endereco.CodigoMunicipio valor incorreto') ;
  CheckEquals('SP', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Endereco.UF, 'NFSe.Prestador.Endereco.UF valor incorreto') ;
  CheckEquals('15014380', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Endereco.CEP, 'NFSe.Prestador.Endereco.CEP valor incorreto') ;

  // PrestadorServico.Contato
  CheckEquals('22334455', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Contato.Telefone, 'NFSe.Prestador.Contato.Telefone valor incorreto') ;
  CheckEquals('emitente@emitenteteste.com.br', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Contato.Email, 'NFSe.Prestador.Contato.Email valor incorreto') ;

  // TomadorServico.IdentificacaoTomador
  CheckEquals('12345678909', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.IdentificacaoTomador.CpfCnpj, 'NFSe.Tomador.IdentificacaoTomador.CpfCnpj valor incorreto') ;

  // TomadorServico
  CheckEquals('PANDA TESTES TESTES TESTES TESTES TESTES', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.RazaoSocial, 'NFSe.Tomador.RazaoSocial valor incorreto') ;

  // TomadorServico.Endereco
  CheckEquals('RUA DOS CLIENTES', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.Endereco, 'NFSe.Tomador.Endereco.Endereco valor incorreto') ;
  CheckEquals('100', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.Numero, 'NFSe.Tomador.Endereco.Numero valor incorreto') ;
  CheckEquals('Apto 10', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.Complemento, 'NFSe.Tomador.Endereco.Complemento valor incorreto') ;
  CheckEquals('JARDIM DOS CLIENTES', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.Bairro, 'NFSe.Tomador.Endereco.Bairro valor incorreto') ;
  CheckEquals('4115200', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.CodigoMunicipio, 'NFSe.Tomador.Endereco.CodigoMunicipio valor incorreto') ;
  CheckEquals('PR', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.UF, 'NFSe.Tomador.Endereco.UF valor incorreto') ;
  CheckEquals('87000100', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.CEP, 'NFSe.Tomador.Endereco.CEP valor incorreto') ;

  // TomadorServico.Contato
  CheckEquals('22334455', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Contato.Telefone, 'NFSe.Tomador.Contato.Telefone valor incorreto') ;
  CheckEquals('npd@provedor.com.br', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Contato.Email, 'NFSe.Tomador.Contato.Email valor incorreto') ;

  // Intermediario
  CheckEquals('Nome do Intermediario', FACBrNFSeX1.NotasFiscais[0].NFSe.Intermediario.RazaoSocial, 'NFSe.Intermediario.RazaoSocial valor incorreto') ;
  CheckEquals('12345678909', FACBrNFSeX1.NotasFiscais[0].NFSe.Intermediario.Identificacao.CpfCnpj, 'NFSe.Intermediario.Identificacao.CpfCnpj valor incorreto') ;
  CheckEquals('1234', FACBrNFSeX1.NotasFiscais[0].NFSe.Intermediario.Identificacao.InscricaoMunicipal, 'NFSe.Intermediario.Identificacao.InscricaoMunicipal valor incorreto') ;

  // OrgaoGerador
  CheckEquals('3549805', FACBrNFSeX1.NotasFiscais[0].NFSe.OrgaoGerador.CodigoMunicipio, 'NFSe.OrgaoGerador.CodigoMunicipio valor incorreto') ;
  CheckEquals('SP', FACBrNFSeX1.NotasFiscais[0].NFSe.OrgaoGerador.Uf, 'NFSe.OrgaoGerador.Uf valor incorreto') ;

  // ConstrucaoCivil
  CheckEquals('354', FACBrNFSeX1.NotasFiscais[0].NFSe.ConstrucaoCivil.CodigoObra, 'NFSe.ConstrucaoCivil.CodigoObra valor incorreto') ;
  CheckEquals('354', FACBrNFSeX1.NotasFiscais[0].NFSe.ConstrucaoCivil.Art, 'NFSe.ConstrucaoCivil.Art valor incorreto') ;

  // NfseCancelamento.Confirmacao.Pedido.InfPedidoCancelamento.IdentificacaoNfse
  CheckEquals('10', FACBrNFSeX1.NotasFiscais[0].NFSe.NfseCancelamento.Pedido.IdentificacaoNfse.Numero, 'NFSe.NfseCancelamento.Pedido.IdentificacaoNfse.Numero valor incorreto') ;
  CheckEquals('12345678000179', FACBrNFSeX1.NotasFiscais[0].NFSe.NfseCancelamento.Pedido.IdentificacaoNfse.Cnpj, 'NFSe.NfseCancelamento.Pedido.IdentificacaoNfse.Cnpj valor incorreto') ;
  CheckEquals('1234', FACBrNFSeX1.NotasFiscais[0].NFSe.NfseCancelamento.Pedido.IdentificacaoNfse.InscricaoMunicipal, 'NFSe.NfseCancelamento.Pedido.IdentificacaoNfse.InscricaoMunicipal valor incorreto') ;
  CheckEquals('3549805', FACBrNFSeX1.NotasFiscais[0].NFSe.NfseCancelamento.Pedido.IdentificacaoNfse.CodigoMunicipio, 'NFSe.NfseCancelamento.Pedido.IdentificacaoNfse.CodigoMunicipio valor incorreto') ;

  // NfseCancelamento.Confirmacao.Pedido.InfPedidoCancelamento
  CheckEquals('1', FACBrNFSeX1.NotasFiscais[0].NFSe.NfseCancelamento.Pedido.CodigoCancelamento, 'NFSe.NfseCancelamento.Pedido.CodigoCancelamento valor incorreto') ;

  // NfseCancelamento.Confirmacao.InfConfirmacaoCancelamento
  CheckEquals(True, FACBrNFSeX1.NotasFiscais[0].NFSe.NfseCancelamento.Sucesso, 'NFSe.NfseCancelamento.Sucesso valor incorreto') ;
  CheckEquals(EncodeDataHora('2022-08-22T16:07:23'), FACBrNFSeX1.NotasFiscais[0].NFSe.NfseCancelamento.DataHora, 'NFSe.NfseCancelamento.DataHora valor incorreto');

  // NfseSubstituicao.SubstituicaoNfse
  CheckEquals('300', FACBrNFSeX1.NotasFiscais[0].NFSe.NfseSubstituidora, 'NFSe.NfseSubstituidora valor incorreto') ;
end;

initialization

  _RegisterTest('ACBrNFSeXProvedorABRASFv1Tests', ACBrNFSeXProvedorABRASFv1Test);

end.
