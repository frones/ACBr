unit ACBrNFSeXProvedorPadraoNacionalTests;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils, ACBrTests.Util,
  ACBrNFSeX;

type

  { ACBrNFSeXProvedorPadraoNacionalTest }

  ACBrNFSeXProvedorPadraoNacionalTest = class(TTestCase)
  private
    FACBrNFSeX1: TACBrNFSeX;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure AoCriarComponente_ContadorDeNotas_DeveSerZero;
    procedure Proverdor_Eh_PadraoNacional;
    procedure LoadFromFile_PassandoXMLValidoNaoAssinado_CampoXml_EhIdenticoAoArquivo;
    procedure LoadFromFile_PassandoXMLValidoNaoAssinado_CamposCarregadosCorretamente;

  end;

implementation

uses
  ACBrConsts,
  ACBrUtil.Strings, ACBrUtil.DateTime, ACBrUtil.Math,
  ACBrNFSeXConversao;

const
  SArquivoPadraoNacional01  = '..\..\..\..\Recursos\NFSe\Provedores\PadraoNacional\PadraoNacional_01-nfse.xml';
  UmMunicipioPadraoNacional = 1400159; // Usado o XML da NFS-e de Bonfim - RR para o teste

{ ACBrNFSeXProvedorPadraoNacionalTest }

procedure ACBrNFSeXProvedorPadraoNacionalTest.SetUp;
begin
  inherited SetUp;
  FACBrNFSeX1 := TACBrNFSeX.Create(nil);
  FACBrNFSeX1.Configuracoes.Geral.CodigoMunicipio := UmMunicipioPadraoNacional;
end;

procedure ACBrNFSeXProvedorPadraoNacionalTest.TearDown;
begin
  FACBrNFSeX1.Free;
  inherited TearDown;
end;

procedure ACBrNFSeXProvedorPadraoNacionalTest.AoCriarComponente_ContadorDeNotas_DeveSerZero;
begin
  CheckEquals(0, FACBrNFSeX1.NotasFiscais.Count, 'Contador de NFSe Não é zero');
end;

procedure ACBrNFSeXProvedorPadraoNacionalTest.Proverdor_Eh_PadraoNacional;
begin
  CheckTrue((FACBrNFSeX1.Configuracoes.Geral.Provedor = proPadraoNacional), 'Provedor não é PadraoNacional');
end;

procedure ACBrNFSeXProvedorPadraoNacionalTest.LoadFromFile_PassandoXMLValidoNaoAssinado_CampoXml_EhIdenticoAoArquivo;
var
  lStrList: TStringList;
  sxml: string;
begin
  lStrList := TStringList.Create;;
  try
    //lStrList.TrailingLineBreak := False; //Não funciona no Delphi 7
    lStrList.LoadFromFile(SArquivoPadraoNacional01);
    sxml := lStrList.Text;
  finally
    lStrList.Free;
  end;

  //remove o linebreak que fica no final da string por ter vindo do "TStringList.Text"
  if Length(sxml) >= Length(sLineBreak) then
  begin
    sxml := Copy(sxml, 0, Length(sxml) - Length(sLineBreak));
  end;

  FACBrNFSeX1.NotasFiscais.LoadFromFile(SArquivoPadraoNacional01, False);
  CheckEquals(sxml, FACBrNFSeX1.NotasFiscais[0].XmlNfse, 'Campo XmlNfse não corresponde ao conteúdo do arquivo');
end;

procedure ACBrNFSeXProvedorPadraoNacionalTest.LoadFromFile_PassandoXMLValidoNaoAssinado_CamposCarregadosCorretamente;
begin
  FACBrNFSeX1.NotasFiscais.Clear;
  FACBrNFSeX1.NotasFiscais.LoadFromFile(SArquivoPadraoNacional01, False);

  CheckTrue(FACBrNFSeX1.NotasFiscais.Count > 0, '');

  CheckEquals('NFS14001591299999999999999000000000000022097579757684', FACBrNFSeX1.NotasFiscais[0].NFSe.CodigoVerificacao, 'NFSe.CodigoVerificacao valor incorreto');
  CheckEquals('Bonfim', FACBrNFSeX1.NotasFiscais[0].NFSe.infNFSe.xLocEmi, 'NFSe.infNFSe.xLocEmi valor incorreto') ;
  CheckEquals('Bonfim', FACBrNFSeX1.NotasFiscais[0].NFSe.infNFSe.xLocPrestacao, 'NFSe.infNFSe.xLocPrestacao valor incorreto') ;
  CheckEquals('25', FACBrNFSeX1.NotasFiscais[0].NFSe.Numero, 'NFSe.Numero valor incorreto') ;
  CheckEquals(1400159, FACBrNFSeX1.NotasFiscais[0].NFSe.infNFSe.cLocIncid, 'NFSe.infNFSe.cLocIncid valor incorreto') ;
  CheckEquals('Bonfim', FACBrNFSeX1.NotasFiscais[0].NFSe.infNFSe.xLocIncid, 'NFSe.infNFSe.xLocIncid valor incorreto') ;
  CheckEquals('Processamento de dados', FACBrNFSeX1.NotasFiscais[0].NFSe.infNFSe.xTribNac, 'NFSe.infNFSe.xTribNac valor incorreto') ;
  CheckEquals('Testes_0.1.0', FACBrNFSeX1.NotasFiscais[0].NFSe.infNFSe.verAplic, 'NFSe.infNFSe.verAplic valor incorreto') ;
  CheckEquals('2', ambGerToStr(FACBrNFSeX1.NotasFiscais[0].NFSe.infNFSe.ambGer), 'NFSe.infNFSe.ambGer valor incorreto') ;
  CheckEquals('1', tpEmisToStr(FACBrNFSeX1.NotasFiscais[0].NFSe.infNFSe.tpEmis), 'NFSe.infNFSe.tpEmis valor incorreto') ;
  CheckEquals('1', procEmiToStr(FACBrNFSeX1.NotasFiscais[0].NFSe.infNFSe.procEmi), 'NFSe.infNFSe.procEmi valor incorreto') ;
  CheckEquals(100, FACBrNFSeX1.NotasFiscais[0].NFSe.infNFSe.cStat, 'NFSe.infNFSe.cStat valor incorreto') ;
  CheckEquals(EncodeDataHora('2022-09-23T10:20:19-03:00'), FACBrNFSeX1.NotasFiscais[0].NFSe.infNFSe.dhProc, 'NFSe.infNFSe.dhProc valor incorreto') ;
  CheckEquals('345', FACBrNFSeX1.NotasFiscais[0].NFSe.infNFSe.nDFSe, 'NFSe.infNFSe.nDFSe valor incorreto') ;

  CheckEquals('99999999999999', FACBrNFSeX1.NotasFiscais[0].NFSe.infNFSe.emit.Identificacao.CpfCnpj, 'NFSe.infNFSe.emit.Identificacao.CpfCnpj valor incorreto') ;
  CheckEquals('99999999999999', FACBrNFSeX1.NotasFiscais[0].NFSe.infNFSe.emit.Identificacao.InscricaoMunicipal, 'NFSe.infNFSe.emit.Identificacao.InscricaoMunicipal valor incorreto') ;
  CheckEquals('RAZÃO SOCIAL', FACBrNFSeX1.NotasFiscais[0].NFSe.infNFSe.emit.RazaoSocial, 'NFSe.infNFSe.emit.RazaoSocial valor incorreto') ;
  CheckEquals('RUA A', FACBrNFSeX1.NotasFiscais[0].NFSe.infNFSe.emit.Endereco.Endereco, 'NFSe.infNFSe.emit.Endereco.Endereco valor incorreto') ;
  CheckEquals('10', FACBrNFSeX1.NotasFiscais[0].NFSe.infNFSe.emit.Endereco.Numero, 'NFSe.infNFSe.emit.Endereco.Numero valor incorreto') ;
  CheckEquals('CENTRO', FACBrNFSeX1.NotasFiscais[0].NFSe.infNFSe.emit.Endereco.Bairro, 'NFSe.infNFSe.emit.Endereco.Bairro valor incorreto') ;
  CheckEquals('RR', FACBrNFSeX1.NotasFiscais[0].NFSe.infNFSe.emit.Endereco.UF, 'NFSe.infNFSe.emit.Endereco.UF valor incorreto') ;
  CheckEquals('69380000', FACBrNFSeX1.NotasFiscais[0].NFSe.infNFSe.emit.Endereco.CEP, 'NFSe.infNFSe.emit.Endereco.CEP valor incorreto') ;

  CheckEquals(12.45, SimpleRoundToEX(FACBrNFSeX1.NotasFiscais[0].NFSe.infNFSe.Valores.BaseCalculo), 'NFSe.infNFSe.Valores.BaseCalculo valor incorreto');
  CheckEquals(5.00, FACBrNFSeX1.NotasFiscais[0].NFSe.infNFSe.Valores.Aliquota, 'NFSe.infNFSe.Valores.Aliquota valor incorreto');
  CheckEquals(0.62, SimpleRoundToEX(FACBrNFSeX1.NotasFiscais[0].NFSe.infNFSe.Valores.ValorIss), 'NFSe.infNFSe.Valores.ValorIss valor incorreto');
  CheckEquals(0, FACBrNFSeX1.NotasFiscais[0].NFSe.infNFSe.Valores.vTotalRet, 'NFSe.infNFSe.Valores.vTotalRet valor incorreto');
  CheckEquals(12.45, SimpleRoundToEX(FACBrNFSeX1.NotasFiscais[0].NFSe.infNFSe.Valores.ValorLiquidoNfse), 'NFSe.infNFSe.Valores.ValorLiquidoNfse valor incorreto');

  CheckEquals('DPS140015929999999999999900900000001559999999', FACBrNFSeX1.NotasFiscais[0].NFSe.infID.ID, 'NFSe.infID.ID valor incorreto');
  CheckEquals(EncodeDataHora('2022-09-23T10:20:17-03:00'), FACBrNFSeX1.NotasFiscais[0].NFSe.DataEmissao, 'NFSe.DataEmissao valor incorreto');
  CheckEquals('POC_0.0.0', FACBrNFSeX1.NotasFiscais[0].NFSe.verAplic, 'NFSe.verAplic valor incorreto');
  CheckEquals('900', FACBrNFSeX1.NotasFiscais[0].NFSe.IdentificacaoRps.Serie, 'NFSe.IdentificacaoRps.Serie valor incorreto');
  CheckEquals('1559999999', FACBrNFSeX1.NotasFiscais[0].NFSe.IdentificacaoRps.Numero, 'NFSe.IdentificacaoRps.Numero valor incorreto');
  CheckEquals(EncodeDataHora('2022-09-23'), FACBrNFSeX1.NotasFiscais[0].NFSe.Competencia, 'NFSe.Competencia valor incorreto');
  CheckEquals('1', tpEmitToStr(FACBrNFSeX1.NotasFiscais[0].NFSe.tpEmit), 'NFSe.tpEmit valor incorreto');

  CheckEquals('99999999999999', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.IdentificacaoPrestador.CpfCnpj, 'NFSe.Prestador.IdentificacaoPrestador.CpfCnpj valor incorreto') ;
  CheckEquals('99999999999999', FACBrNFSeX1.NotasFiscais[0].NFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal, 'NFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal valor incorreto') ;
  CheckEquals('1', OptanteSNToStr(FACBrNFSeX1.NotasFiscais[0].NFSe.OptanteSN), 'NFSe.OptanteSN valor incorreto') ;
  CheckEquals('0', FACBrNFSeX1.Provider.RegimeEspecialTributacaoToStr(FACBrNFSeX1.NotasFiscais[0].NFSe.RegimeEspecialTributacao), 'NFSe.RegimeEspecialTributacao valor incorreto') ;

  CheckEquals('99999999999999', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.IdentificacaoTomador.CpfCnpj, 'NFSe.Tomador.IdentificacaoTomador.CpfCnpj valor incorreto') ;
  CheckEquals('99999999999999', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal, 'NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal valor incorreto') ;
  CheckEquals('RAZÃO SOCIAL', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.RazaoSocial, 'NFSe.Tomador.RazaoSocial valor incorreto') ;
  CheckEquals('Rua', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.Endereco, 'NFSe.Tomador.Endereco.Endereco valor incorreto') ;
  CheckEquals('2176', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.Numero, 'NFSe.Tomador.Endereco.Numero valor incorreto') ;
  CheckEquals('COMPLEMENTO', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.Complemento, 'NFSe.Tomador.Endereco.Complemento valor incorreto') ;
  CheckEquals('Centro', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.Bairro, 'NFSe.Tomador.Endereco.Bairro valor incorreto') ;
  CheckEquals('3503208', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.CodigoMunicipio, 'NFSe.Tomador.Endereco.CodigoMunicipio valor incorreto') ;
  CheckEquals('05422030', FACBrNFSeX1.NotasFiscais[0].NFSe.Tomador.Endereco.CEP, 'NFSe.Tomador.Endereco.CEP valor incorreto') ;

  CheckEquals('1400159', FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.CodigoMunicipio, 'NFSe.Servico.CodigoMunicipio valor incorreto') ;
  CheckEquals('010301', FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.ItemListaServico, 'NFSe.Servico.ItemListaServico valor incorreto') ;
  CheckEquals('Descrição do Serviço', FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Discriminacao, 'NFSe..Servico.Discriminacao valor incorreto') ;

  CheckEquals(12.45, SimpleRoundToEX(FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.ValorServicos), 'NFSe.Servico.Valores.ValorServicos valor incorreto') ;

  CheckEquals('1', tribISSQNToStr(FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.tribMun.tribISSQN), 'NFSe.Servico.Valores.tribMun.tribISSQN valor incorreto') ;
  CheckEquals('1', tpRetISSQNToStr(FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.tribMun.tpRetISSQN), 'NFSe.Servico.Valores.tribMun.tpRetISSQN valor incorreto') ;

  CheckEquals(1, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.totTrib.pTotTribFed, 'NFSe.Servico.Valores.totTrib.pTotTribFed valor incorreto') ;
  CheckEquals(1, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.totTrib.pTotTribEst, 'NFSe.Servico.Valores.totTrib.pTotTribEst valor incorreto') ;
  CheckEquals(1, FACBrNFSeX1.NotasFiscais[0].NFSe.Servico.Valores.totTrib.pTotTribMun, 'NFSe.Servico.Valores.totTrib.pTotTribMun valor incorreto') ;
end;

initialization

  _RegisterTest('ACBrNFSeXProvedorPadraoNacionalTests', ACBrNFSeXProvedorPadraoNacionalTest);

end.
