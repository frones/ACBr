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
  synautil, ACBrConsts,
  ACBrUtil.Strings, ACBrUtil.DateTime, ACBrUtil.Math,
  ACBrNFSeXConversao;

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

  FACBrNFSeX1.NotasFiscais.LoadFromFile(SArquivoABRASFv201, False);
  CheckEquals(sxml, FACBrNFSeX1.NotasFiscais[0].XmlNfse, 'Campo XmlNfse não corresponde ao conteúdo do arquivo');
end;

procedure ACBrNFSeXProvedorABRASFv2Test.LoadFromFile_PassandoXMLValidoNaoAssinado_CamposCarregadosCorretamente;
begin
  FACBrNFSeX1.NotasFiscais.LoadFromFile(SArquivoABRASFv201, False);

  CheckTrue(FACBrNFSeX1.NotasFiscais.Count > 0, '');

  // InfNfse
  CheckEquals('5', FACBrNFSeX1.NotasFiscais[0].NFSe.Numero, 'NFSe.Numero valor incorreto') ;
  CheckEquals('B01234A5F', FACBrNFSeX1.NotasFiscais[0].NFSe.CodigoVerificacao, 'NFSe.CodigoVerificacao valor incorreto');
  CheckEquals(EncodeDataHora('2022-10-19T17:21:01.017'), FACBrNFSeX1.NotasFiscais[0].NFSe.DataEmissao, 'NFSe.DataEmissao valor incorreto');
  CheckEquals('2022', FACBrNFSeX1.NotasFiscais[0].NFSe.NfseSubstituida, 'NFSe.NfseSubstituida valor incorreto') ;
  CheckEquals('.', FACBrNFSeX1.NotasFiscais[0].NFSe.OutrasInformacoes, 'NFSe.OutrasInformacoes valor incorreto');

  // ValoresNfse
  CheckEquals(15.50, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.BaseCalculo, 'NFSe.Servico.Valores.BaseCalculo valor incorreto');
  CheckEquals(5.00, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.Aliquota, 'NFSe.Servico.Valores.Aliquota valor incorreto');
  CheckEquals(0.39, SimpleRoundToEX(FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorIss), 'NFSe.Servico.Valores.ValorIss valor incorreto');
  CheckEquals(7.50, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorLiquidoNfse, 'NFSe.Servico.Valores.ValorLiquidoNfse valor incorreto');

  // InfNfse
  CheckEquals('Prestação de Serviços', FACBrNFSeX1.NotasFiscais[0].NFSe.DescricaoCodigoTributacaoMunicipio, 'NFSe.DescricaoCodigoTributacaoMunicipio valor incorreto') ;
  CheckEquals(10, FACBrNFSeX1.NotasFiscais[0].NFSe.ValorCredito, 'NFSe.ValorCredito valor incorreto');

  // PrestadorServico
  CheckEquals('EMITENTE TESTE', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.RazaoSocial, 'NFSe.Prestador.RazaoSocial valor incorreto') ;
  CheckEquals('EMITENTE TESTE', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.NomeFantasia, 'NFSe.Prestador.NomeFantasia valor incorreto') ;

  // PrestadorServico.Endereco
  CheckEquals('ENDEREÇO TESTE', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Endereco.Endereco, 'NFSe.Prestador.Endereco.Endereco valor incorreto') ;
  CheckEquals('11', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Endereco.Numero, 'NFSe.Prestador.Endereco.Numero valor incorreto') ;
  CheckEquals('SALA', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Endereco.Complemento, 'NFSe.Prestador.Endereco.Complemento valor incorreto') ;
  CheckEquals('Recanto das Emas', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Endereco.Bairro, 'NFSe.Prestador.Endereco.Bairro valor incorreto') ;
  CheckEquals('5300108', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Endereco.CodigoMunicipio, 'NFSe.Prestador.Endereco.CodigoMunicipio valor incorreto') ;
  CheckEquals('DF', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Endereco.UF, 'NFSe.Prestador.Endereco.UF valor incorreto') ;
  CheckEquals('72610321', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Endereco.CEP, 'NFSe.Prestador.Endereco.CEP valor incorreto') ;

  // PrestadorServico.Contato
  CheckEquals('61999999999', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Contato.Telefone, 'NFSe.Prestador.Contato.Telefone valor incorreto') ;
  CheckEquals('email@gmail.com', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.Contato.Email, 'NFSe.Prestador.Contato.Email valor incorreto') ;

  // OrgaoGerador
  CheckEquals('5002704', FACBrNFSeX1.NotasFiscais[0].NFSe.OrgaoGerador.CodigoMunicipio, 'NFSe.OrgaoGerador.CodigoMunicipio valor incorreto') ;
  CheckEquals('MS', FACBrNFSeX1.NotasFiscais[0].NFSe.OrgaoGerador.Uf, 'NFSe.OrgaoGerador.Uf valor incorreto') ;

  // DeclaracaoPrestacaoServico.InfDeclaracaoPrestacaoServico.Rps.IdentificacaoRps
  CheckEquals('4', FACBrNFSeX1.NotasFiscais[0].NFSe.IdentificacaoRps.Numero, 'NFSe.IdentificacaoRps.Numero valor incorreto') ;
  CheckEquals('8', FACBrNFSeX1.NotasFiscais[0].NFSe.IdentificacaoRps.Serie, 'NFSe.IdentificacaoRps.Serie valor incorreto') ;
  CheckEquals('1', FACBrNFSeX1.Provider.TipoRPSToStr(FACBrNFSeX1.NotasFiscais[0].NFSe.IdentificacaoRps.Tipo), 'NFSe.IdentificacaoRps.Tipo valor incorreto') ;

  // DeclaracaoPrestacaoServico.InfDeclaracaoPrestacaoServico.Rps
  CheckEquals(EncodeDataHora('2022-10-19'), FACBrNFSeX1.NotasFiscais[0].NFSe.DataEmissaoRps, 'NFSe.DataEmissaoRps valor incorreto');
  CheckEquals('1', StatusRPSToStr(FACBrNFSeX1.NotasFiscais[0].NFSe.StatusRps), 'NFSe.DataEmissaoRps valor incorreto');

  // DeclaracaoPrestacaoServico.InfDeclaracaoPrestacaoServico.Rps.RpsSubstituido
  CheckEquals('4', FACBrNFSeX1.NotasFiscais[0].NFSe.RpsSubstituido.Numero, 'NFSe.RpsSubstituido.Numero valor incorreto') ;
  CheckEquals('8', FACBrNFSeX1.NotasFiscais[0].NFSe.RpsSubstituido.Serie, 'NFSe.RpsSubstituido.Serie valor incorreto') ;
  CheckEquals('1', FACBrNFSeX1.Provider.TipoRPSToStr(FACBrNFSeX1.NotasFiscais[0].NFSe.RpsSubstituido.Tipo), 'NFSe.RpsSubstituido.Tipo valor incorreto') ;

  // DeclaracaoPrestacaoServico.InfDeclaracaoPrestacaoServico
  CheckEquals(EncodeDataHora('2022-10-19'), FACBrNFSeX1.NotasFiscais[0].NFSe.Competencia, 'NFSe.Competencia valor incorreto');

  // DeclaracaoPrestacaoServico.InfDeclaracaoPrestacaoServico.Servico.Valores
  CheckEquals(15.50, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorServicos, 'NFSe.Servico.Valores.ValorServicos valor incorreto');
  CheckEquals(1, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorDeducoes, 'NFSe.Servico.Valores.ValorDeducoes valor incorreto');
  CheckEquals(1, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorPis, 'NFSe.Servico.Valores.ValorPis valor incorreto');
  CheckEquals(1, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorCofins, 'NFSe.Servico.Valores.ValorCofins valor incorreto');
  CheckEquals(1, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorInss, 'NFSe.Servico.Valores.ValorInss valor incorreto');
  CheckEquals(1, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorIr, 'NFSe.Servico.Valores.ValorIr valor incorreto');
  CheckEquals(1, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorCsll, 'NFSe.Servico.Valores.ValorCsll valor incorreto');
  CheckEquals(1, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.OutrasRetencoes, 'NFSe.Servico.Valores.OutrasRetencoes valor incorreto');
  CheckEquals(1, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorTotalTributos, 'NFSe.Servico.Valores.ValorTotalTributos valor incorreto');
  CheckEquals(0.39, SimpleRoundToEX(FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorIss), 'NFSe.Servico.Valores.ValorIss valor incorreto');
  CheckEquals(5.00, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.Aliquota, 'NFSe.Servico.Valores.Aliquota valor incorreto');
  CheckEquals(1, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.DescontoIncondicionado, 'NFSe.Servico.Valores.DescontoIncondicionado valor incorreto');
  CheckEquals(1, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.DescontoCondicionado, 'NFSe.Servico.Valores.DescontoCondicionado valor incorreto');

  // DeclaracaoPrestacaoServico.InfDeclaracaoPrestacaoServico.Servico
  CheckEquals('2', FACBrNFSeX1.Provider.SituacaoTributariaToStr(FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.IssRetido), 'NFSe.Servico.Valores.IssRetido valor incorreto');
  CheckEquals('1', FACBrNFSeX1.Provider.ResponsavelRetencaoToStr(FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.ResponsavelRetencao), 'NFSe.Servico.ResponsavelRetencao valor incorreto');
  CheckEquals('17.01', FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.ItemListaServico, 'NFSe.Servico.ItemListaServico valor incorreto') ;
  CheckEquals('6399200', FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.CodigoCnae, 'NFSe.Servico.CodigoCnae valor incorreto') ;
  CheckEquals('170', FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.CodigoTributacaoMunicipio, 'NFSe.Servico.CodigoTributacaoMunicipio valor incorreto') ;
  CheckEquals('170', FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.CodigoNBS, 'NFSe.Servico.CodigoNBS valor incorreto') ;
  CheckEquals('PLANEJAMENTO E PRODUCAO DE PAGINAS ELETRONICAS Obs: - ; Informacoes dos tributos totais incidentes (Lei Federal 12.741/2012). Total R$ = 2,08;',
    FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Discriminacao, 'NFSe.Servico.Discriminacao valor incorreto') ;
  CheckEquals('5300108', FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.CodigoMunicipio, 'NFSe.Servico.CodigoMunicipio valor incorreto') ;
  CheckEquals('1', FACBrNFSeX1.Provider.ExigibilidadeISSToStr(FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.ExigibilidadeISS), 'NFSe.Servico.ExigibilidadeISS valor incorreto');
  CheckEquals('1', FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.IdentifNaoExigibilidade, 'NFSe.Servico.IdentifNaoExigibilidade valor incorreto');
  CheckEquals(5300108, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.MunicipioIncidencia, 'NFSe.Servico.MunicipioIncidencia valor incorreto') ;
  CheckEquals('53', FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.NumeroProcesso, 'NFSe.Servico.NumeroProcesso valor incorreto');

  // DeclaracaoPrestacaoServico.InfDeclaracaoPrestacaoServico.Prestador
  CheckEquals('12345678000123', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.IdentificacaoPrestador.CpfCnpj, 'NFSe.Prestador.IdentificacaoPrestador.CpfCnpj valor incorreto') ;
  CheckEquals('1234567', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal, 'NFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal valor incorreto') ;

  // DeclaracaoPrestacaoServico.InfDeclaracaoPrestacaoServico.TomadorServico.IdentificacaoTomador
  CheckEquals('12345678901', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.IdentificacaoTomador.CpfCnpj, 'NFSe.Tomador.IdentificacaoTomador.CpfCnpj valor incorreto') ;
  CheckEquals('1234567', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal, 'NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal valor incorreto') ;

  // DeclaracaoPrestacaoServico.InfDeclaracaoPrestacaoServico.TomadorServico
  CheckEquals('123', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.IdentificacaoTomador.Nif, 'NFSe.Tomador.IdentificacaoTomador.Nif valor incorreto') ;
  CheckEquals('TOMADOR TESTE', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.RazaoSocial, 'NFSe.Tomador.RazaoSocial valor incorreto') ;

  // DeclaracaoPrestacaoServico.InfDeclaracaoPrestacaoServico.TomadorServico.Endereco
  CheckEquals('ENDEREÇO TESTE', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.Endereco, 'NFSe.Tomador.Endereco.Endereco valor incorreto') ;
  CheckEquals('11', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.Numero, 'NFSe.Tomador.Endereco.Numero valor incorreto') ;
  CheckEquals('LOTE', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.Complemento, 'NFSe.Tomador.Endereco.Complemento valor incorreto') ;
  CheckEquals('RECANTO DAS EMAS', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.Bairro, 'NFSe.Tomador.Endereco.Bairro valor incorreto') ;
  CheckEquals('5300108', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.CodigoMunicipio, 'NFSe.Tomador.Endereco.CodigoMunicipio valor incorreto') ;
  CheckEquals('DF', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.UF, 'NFSe.Tomador.Endereco.UF valor incorreto') ;
  CheckEquals('72600407', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.CEP, 'NFSe.Tomador.Endereco.CEP valor incorreto') ;

  // DeclaracaoPrestacaoServico.InfDeclaracaoPrestacaoServico.TomadorServico.Contato
  CheckEquals('61999999999', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Contato.Telefone, 'NFSe.Tomador.Contato.Telefone valor incorreto') ;
  CheckEquals('email@gmai.com', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Contato.Email, 'NFSe.Tomador.Contato.Email valor incorreto') ;

  // DeclaracaoPrestacaoServico.InfDeclaracaoPrestacaoServico.Intermediario.IdentificacaoIntermediario
  CheckEquals('12345678909', FACBrNFSeX1.NotasFiscais[0].NFSe.Intermediario.Identificacao.CpfCnpj, 'NFSe.Intermediario.Identificacao.CpfCnpj valor incorreto') ;
  CheckEquals('1234', FACBrNFSeX1.NotasFiscais[0].NFSe.Intermediario.Identificacao.InscricaoMunicipal, 'NFSe.Intermediario.Identificacao.InscricaoMunicipal valor incorreto') ;

  // DeclaracaoPrestacaoServico.InfDeclaracaoPrestacaoServico.Intermediario
  CheckEquals('Nome do Intermediario', FACBrNFSeX1.NotasFiscais[0].NFSe.Intermediario.RazaoSocial, 'NFSe.Intermediario.RazaoSocial valor incorreto') ;
  CheckEquals('5300108', FACBrNFSeX1.NotasFiscais[0].NFSe.Intermediario.CodigoMunicipio, 'NFSe.Intermediario.CodigoMunicipio valor incorreto') ;

  // DeclaracaoPrestacaoServico.InfDeclaracaoPrestacaoServico.ConstrucaoCivil
  CheckEquals('354', FACBrNFSeX1.NotasFiscais[0].NFSe.ConstrucaoCivil.CodigoObra, 'NFSe.ConstrucaoCivil.CodigoObra valor incorreto') ;
  CheckEquals('354', FACBrNFSeX1.NotasFiscais[0].NFSe.ConstrucaoCivil.Art, 'NFSe.ConstrucaoCivil.Art valor incorreto') ;

  // DeclaracaoPrestacaoServico.InfDeclaracaoPrestacaoServico
  CheckEquals('1', FACBrNFSeX1.Provider.RegimeEspecialTributacaoToStr(FACBrNFSeX1.NotasFiscais[0].NFSe.RegimeEspecialTributacao), 'NFSe.RegimeEspecialTributacao valor incorreto') ;
  CheckEquals('2', FACBrNFSeX1.Provider.SimNaoToStr(FACBrNFSeX1.NotasFiscais[0].NFSe.OptanteSimplesNacional), 'NFSe.OptanteSimplesNacional valor incorreto') ;
  CheckEquals('2', FACBrNFSeX1.Provider.SimNaoToStr(FACBrNFSeX1.NotasFiscais[0].NFSe.IncentivadorCultural), 'NFSe.IncentivadorCultural valor incorreto') ;
  CheckEquals('Informações Complementares', FACBrNFSeX1.NotasFiscais[0].NFSe.InformacoesComplementares, 'NFSe.InformacoesComplementares valor incorreto') ;

  // DeclaracaoPrestacaoServico.InfDeclaracaoPrestacaoServico.Deducao

     {Falta implementar as classes, a leitura das tags caso presente no XML}






  // NfseCancelamento.Confirmacao.Pedido.InfPedidoCancelamento.IdentificacaoNfse
  CheckEquals('10', FACBrNFSeX1.NotasFiscais[0].NFSe.NfseCancelamento.Pedido.IdentificacaoNfse.Numero, 'NFSe.NfseCancelamento.Pedido.IdentificacaoNfse.Numero valor incorreto') ;
  CheckEquals('12345678000123', FACBrNFSeX1.NotasFiscais[0].NFSe.NfseCancelamento.Pedido.IdentificacaoNfse.Cnpj, 'NFSe.NfseCancelamento.Pedido.IdentificacaoNfse.Cnpj valor incorreto') ;
  CheckEquals('1234', FACBrNFSeX1.NotasFiscais[0].NFSe.NfseCancelamento.Pedido.IdentificacaoNfse.InscricaoMunicipal, 'NFSe.NfseCancelamento.Pedido.IdentificacaoNfse.InscricaoMunicipal valor incorreto') ;
  CheckEquals('3549805', FACBrNFSeX1.NotasFiscais[0].NFSe.NfseCancelamento.Pedido.IdentificacaoNfse.CodigoMunicipio, 'NFSe.NfseCancelamento.Pedido.IdentificacaoNfse.CodigoMunicipio valor incorreto') ;

  // NfseCancelamento.Confirmacao.Pedido.InfPedidoCancelamento
  CheckEquals('1', FACBrNFSeX1.NotasFiscais[0].NFSe.NfseCancelamento.Pedido.CodigoCancelamento, 'NFSe.NfseCancelamento.Pedido.CodigoCancelamento valor incorreto') ;

  // NfseCancelamento.Confirmacao.InfConfirmacaoCancelamento
  CheckEquals(EncodeDataHora('2022-08-22T16:07:23'), FACBrNFSeX1.NotasFiscais[0].NFSe.NfseCancelamento.DataHora, 'NFSe.NfseCancelamento.DataHora valor incorreto');

  // NfseSubstituicao.SubstituicaoNfse
  CheckEquals('300', FACBrNFSeX1.NotasFiscais[0].NFSe.NfseSubstituidora, 'NFSe.NfseSubstituidora valor incorreto') ;
end;

initialization

  _RegisterTest('ACBrNFSeXProvedorABRASFv2Tests', ACBrNFSeXProvedorABRASFv2Test);

end.
