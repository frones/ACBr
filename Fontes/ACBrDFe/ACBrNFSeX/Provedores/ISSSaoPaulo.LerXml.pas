{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Giurizzato Junior                         }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{                                                                              }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

{$I ACBr.inc}

unit ISSSaoPaulo.LerXml;

interface

uses
  SysUtils, Classes, StrUtils,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXConversao, ACBrNFSeXLerXml;

type
  { TNFSeR_ISSSaoPaulo }

  TNFSeR_ISSSaoPaulo = class(TNFSeRClass)
  protected

    procedure LerChaveNFe(const ANode: TACBrXmlNode);
    procedure LerChaveRPS(const ANode: TACBrXmlNode);
    procedure LerCPFCNPJPrestador(const ANode: TACBrXmlNode);
    procedure LerEnderecoPrestador(const ANode: TACBrXmlNode);
    procedure LerCPFCNPJTomador(const ANode: TACBrXmlNode);
    procedure LerEnderecoTomador(const ANode: TACBrXmlNode);
    procedure LerCPFCNPJIntermediario(const ANode: TACBrXmlNode);
  public
    function LerXml: Boolean; override;
    function LerXmlRps(const ANode: TACBrXmlNode): Boolean;
    function LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
  end;

implementation

uses
  ACBrUtil.Base, ACBrUtil.Strings;

//==============================================================================
// Essa unit tem por finalidade exclusiva ler o XML do provedor:
//     ISSSaoPaulo
//==============================================================================

{ TNFSeR_ISSSaoPaulo }

procedure TNFSeR_ISSSaoPaulo.LerChaveNFe(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('ChaveNFe');

  if AuxNode <> nil then
  begin
    NFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal := ObterConteudo(AuxNode.Childrens.FindAnyNs('InscricaoPrestador'), tcStr);

    NFSe.Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('NumeroNFe'), tcStr);
    NFSe.CodigoVerificacao := ObterConteudo(AuxNode.Childrens.FindAnyNs('CodigoVerificacao'), tcStr);
  end;
end;

procedure TNFSeR_ISSSaoPaulo.LerChaveRPS(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('ChaveRPS');

  if AuxNode <> nil then
  begin
    NFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal := ObterConteudo(AuxNode.Childrens.FindAnyNs('InscricaoPrestador'), tcStr);

    NFSe.IdentificacaoRps.Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('NumeroRPS'), tcStr);
    NFSe.IdentificacaoRps.Serie := ObterConteudo(AuxNode.Childrens.FindAnyNs('SerieRPS'), tcStr);
    NFSe.IdentificacaoRps.Tipo := trRPS;

    if NFSe.InfID.ID = '' then
      NFSe.InfID.ID := OnlyNumber(NFSe.IdentificacaoRps.Numero) +
                       NFSe.IdentificacaoRps.Serie;
  end;
end;

procedure TNFSeR_ISSSaoPaulo.LerCPFCNPJIntermediario(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('CPFCNPJIntermediario');

  if AuxNode <> nil then
  begin
    NFSe.Intermediario.Identificacao.CpfCnpj := ObterConteudo(AuxNode.Childrens.FindAnyNs('CNPJ'), tcStr);

    if NFSe.Intermediario.Identificacao.CpfCnpj = '' then
      NFSe.Intermediario.Identificacao.CpfCnpj := ObterConteudo(AuxNode.Childrens.FindAnyNs('CPF'), tcStr);
  end;
end;

procedure TNFSeR_ISSSaoPaulo.LerCPFCNPJPrestador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('CPFCNPJPrestador');

  if AuxNode <> nil then
  begin
    NFSe.Prestador.IdentificacaoPrestador.CpfCnpj := ObterConteudo(AuxNode.Childrens.FindAnyNs('CNPJ'), tcStr);

    if NFSe.Prestador.IdentificacaoPrestador.CpfCnpj = '' then
      NFSe.Prestador.IdentificacaoPrestador.CpfCnpj := ObterConteudo(AuxNode.Childrens.FindAnyNs('CPF'), tcStr);
  end;
end;

procedure TNFSeR_ISSSaoPaulo.LerCPFCNPJTomador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('CPFCNPJTomador');

  if AuxNode <> nil then
    NFSe.Tomador.IdentificacaoTomador.CpfCnpj := ObterCNPJCPF(AuxNode);
end;

procedure TNFSeR_ISSSaoPaulo.LerEnderecoPrestador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  xUF: string;
begin
  AuxNode := ANode.Childrens.FindAnyNs('EnderecoPrestador');

  if AuxNode <> nil then
  begin
    with NFSe.Prestador.Endereco do
    begin
      TipoLogradouro  := ObterConteudo(AuxNode.Childrens.FindAnyNs('TipoLogradouro'), tcStr);
      Endereco := ObterConteudo(AuxNode.Childrens.FindAnyNs('Logradouro'), tcStr);
      Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('NumeroEndereco'), tcStr);
      Complemento := ObterConteudo(AuxNode.Childrens.FindAnyNs('ComplementoEndereco'), tcStr);
      Bairro := ObterConteudo(AuxNode.Childrens.FindAnyNs('Bairro'), tcStr);
      CodigoMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('Cidade'), tcStr);
      UF := ObterConteudo(AuxNode.Childrens.FindAnyNs('UF'), tcStr);
      CEP := ObterConteudo(AuxNode.Childrens.FindAnyNs('CEP'), tcStr);
      xMunicipio := ObterNomeMunicipioUF(StrToIntDef(CodigoMunicipio, 0), xUF);

      if UF = '' then
        UF := xUF;
    end;
  end;
end;

procedure TNFSeR_ISSSaoPaulo.LerEnderecoTomador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  xUF: string;
begin
  AuxNode := ANode.Childrens.FindAnyNs('EnderecoTomador');

  if AuxNode <> nil then
  begin
    with NFSe.Tomador.Endereco do
    begin
      TipoLogradouro  := ObterConteudo(AuxNode.Childrens.FindAnyNs('TipoLogradouro'), tcStr);
      Endereco := ObterConteudo(AuxNode.Childrens.FindAnyNs('Logradouro'), tcStr);
      Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('NumeroEndereco'), tcStr);
      Complemento := ObterConteudo(AuxNode.Childrens.FindAnyNs('ComplementoEndereco'), tcStr);
      Bairro := ObterConteudo(AuxNode.Childrens.FindAnyNs('Bairro'), tcStr);
      CodigoMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('Cidade'), tcStr);
      UF := ObterConteudo(AuxNode.Childrens.FindAnyNs('UF'), tcStr);
      CEP := ObterConteudo(AuxNode.Childrens.FindAnyNs('CEP'), tcStr);
      xMunicipio := ObterNomeMunicipioUF(StrToIntDef(CodigoMunicipio, 0), xUF);

      if UF = '' then
        UF := xUF;
    end;
  end;
end;

function TNFSeR_ISSSaoPaulo.LerXml: Boolean;
var
  XmlNode: TACBrXmlNode;
begin
  FpQuebradeLinha := FpAOwner.ConfigGeral.QuebradeLinha;

  if EstaVazio(Arquivo) then
    raise Exception.Create('Arquivo xml não carregado.');

  LerParamsTabIni(True);

  Arquivo := NormatizarXml(Arquivo);

  if FDocument = nil then
    FDocument := TACBrXmlDocument.Create();

  Document.Clear();
  Document.LoadFromXml(Arquivo);

  if (Pos('NFe', Arquivo) > 0) then
    tpXML := txmlNFSe
  else
    tpXML := txmlRPS;

  XmlNode := Document.Root;

  if XmlNode = nil then
    raise Exception.Create('Arquivo xml vazio.');

  if tpXML = txmlNFSe then
    Result := LerXmlNfse(XmlNode)
  else
    Result := LerXmlRps(XmlNode);

  if NFSe.Tomador.RazaoSocial = '' then
    NFSe.Tomador.RazaoSocial := 'Tomador Não Identificado';

  FreeAndNil(FDocument);
end;

function TNFSeR_ISSSaoPaulo.LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
var
  aValor: string;
  Ok :Boolean;
begin
  Result := True;

  if not Assigned(ANode) then Exit;

  NFSe.dhRecebimento := Now;
  NFSe.SituacaoNfse := snNormal;
  NFSe.NumeroLote := ObterConteudo(ANode.Childrens.FindAnyNs('NumeroLote'), tcStr);
  NFSe.DataEmissao := ObterConteudo(ANode.Childrens.FindAnyNs('DataEmissaoNFe'), tcDatHor);
  NFSe.DataEmissaoRps := ObterConteudo(ANode.Childrens.FindAnyNs('DataEmissaoRPS'), tcDat);
  NFSe.Competencia := NFSe.DataEmissao;

  aValor := ObterConteudo(ANode.Childrens.FindAnyNs('StatusNFe'), tcStr);

  if aValor = 'C' then
  begin
    NFSe.SituacaoNfse := snCancelado;
    NFSe.NfseCancelamento.DataHora := ObterConteudo(ANode.Childrens.FindAnyNs('DataCancelamento'), tcDat);
  end;

  NFSe.TipoTributacaoRPS := FpAOwner.StrToTipoTributacaoRPS(Ok, ObterConteudo(ANode.Childrens.FindAnyNs('TributacaoNFe'), tcStr));

  aValor := ObterConteudo(ANode.Childrens.FindAnyNs('OpcaoSimples'), tcStr);

  if aValor = '0' then
    NFSe.OptanteSimplesNacional := snNao
  else
    NFSe.OptanteSimplesNacional := snSim;

  aValor := ObterConteudo(ANode.Childrens.FindAnyNs('CodigoServico'), tcStr);

  NFSe.Servico.ItemListaServico := NormatizarItemListaServico(aValor);
  NFSe.Servico.xItemListaServico := ItemListaServicoDescricao(NFSe.Servico.ItemListaServico);
  NFSe.Servico.ItemListaServico := '0' + NFSe.Servico.ItemListaServico;

  NFSe.Servico.Discriminacao := ObterConteudo(ANode.Childrens.FindAnyNs('Discriminacao'), tcStr);
  NFSe.Servico.Discriminacao := StringReplace(NFSe.Servico.Discriminacao, FpQuebradeLinha,
                                      sLineBreak, [rfReplaceAll, rfIgnoreCase]);

  VerificarSeConteudoEhLista(NFSe.Servico.Discriminacao);

  aValor := ObterConteudo(ANode.Childrens.FindAnyNs('ISSRetido'), tcStr);

  with NFSe.Servico.Valores do
  begin
    ValorServicos := ObterConteudo(ANode.Childrens.FindAnyNs('ValorServicos'), tcDe2);
    BaseCalculo := ObterConteudo(ANode.Childrens.FindAnyNs('ValorServicos'), tcDe2);
    Aliquota := ObterConteudo(ANode.Childrens.FindAnyNs('AliquotaServicos'), tcDe2);
    Aliquota := (Aliquota * 100);
    ValorIss := ObterConteudo(ANode.Childrens.FindAnyNs('ValorISS'), tcDe2);
    ValorPis := ObterConteudo(ANode.Childrens.FindAnyNs('ValorPIS'), tcDe2);
    ValorCofins := ObterConteudo(ANode.Childrens.FindAnyNs('ValorCOFINS'), tcDe2);
    ValorInss := ObterConteudo(ANode.Childrens.FindAnyNs('ValorINSS'), tcDe2);
    ValorIr := ObterConteudo(ANode.Childrens.FindAnyNs('ValorIR'), tcDe2);
    ValorCsll := ObterConteudo(ANode.Childrens.FindAnyNs('ValorCSLL'), tcDe2);

    if aValor = 'false' then
      IssRetido := stNormal
    else
      IssRetido := stRetencao;

    RetencoesFederais := ValorPis + ValorCofins + ValorInss + ValorIr + ValorCsll;

    ValorLiquidoNfse := ValorServicos -
                        (RetencoesFederais + ValorDeducoes + ValorIssRetido +
                         DescontoCondicionado + DescontoIncondicionado);

    ValorTotalNotaFiscal := ValorServicos - DescontoCondicionado -
                            DescontoIncondicionado;
  end;

  with NFSe.ValoresNfse do
  begin
    ValorLiquidoNfse := NFSe.Servico.Valores.ValorLiquidoNfse;
    BaseCalculo := NFSe.Servico.Valores.BaseCalculo;
    Aliquota := NFSe.Servico.Valores.Aliquota;
    ValorIss := NFSe.Servico.Valores.ValorIss;
//    Aliquota := (NFSe.ValoresNfse.Aliquota * 100);
  end;

  NFSe.Prestador.RazaoSocial := ObterConteudo(ANode.Childrens.FindAnyNs('RazaoSocialPrestador'), tcStr);
  NFSe.Prestador.Contato.Email := ObterConteudo(ANode.Childrens.FindAnyNs('EmailPrestador'), tcStr);

  NFSe.Tomador.RazaoSocial := ObterConteudo(ANode.Childrens.FindAnyNs('RazaoSocialTomador'), tcStr);
  NFSe.Tomador.Contato.Email := ObterConteudo(ANode.Childrens.FindAnyNs('EmailTomador'), tcStr);

  LerChaveNFe(ANode);
  LerChaveRPS(ANode);
  LerCPFCNPJPrestador(ANode);
  LerEnderecoPrestador(ANode);
  LerCPFCNPJTomador(ANode);
  LerEnderecoTomador(ANode);

  {
   TipoTributacaoRPS = ttTribnoMun, ttTribforaMun, ttTribnoMunIsento,
                       ttTribforaMunIsento, ttTribnoMunImune, ttTribforaMunImune,
                       ttTribnoMunSuspensa, ttTribforaMunSuspensa, ttExpServicos,
                       ttSimplesNacional, ttRetidonoMun
  }

  if NFSe.TipoTributacaoRPS in [ttTribnoMun, ttTribnoMunIsento,
                                ttTribnoMunImune, ttTribnoMunSuspensa] then
    NFSe.Servico.MunicipioIncidencia := StrToIntDef(NFSe.Prestador.Endereco.CodigoMunicipio, 0)
  else
    NFSe.Servico.MunicipioIncidencia := StrToIntDef(NFSe.Tomador.Endereco.CodigoMunicipio, 0);

  NFSe.Servico.CodigoMunicipio := ObterConteudo(ANode.Childrens.FindAnyNs('MunicipioPrestacao'), tcStr);

  if NFSe.Servico.CodigoMunicipio = '' then
    NFSe.Servico.CodigoMunicipio := NFSe.Prestador.Endereco.CodigoMunicipio;

  NFSe.ConstrucaoCivil.nNumeroEncapsulamento := ObterConteudo(ANode.Childrens.FindAnyNs('NumeroEncapsulamento'), tcStr);
  NFSe.Servico.ValorTotalRecebido := ObterConteudo(ANode.Childrens.FindAnyNs('ValorTotalRecebido'), tcDe2);

  LerCampoLink;
end;

function TNFSeR_ISSSaoPaulo.LerXmlRps(const ANode: TACBrXmlNode): Boolean;
var
  aValor: string;
  Ok: Boolean;
begin
  Result := True;

  with NFSe do
  begin
    Assinatura := ObterConteudo(ANode.Childrens.FindAnyNs('Assinatura'), tcStr);
    DataEmissao := ObterConteudo(ANode.Childrens.FindAnyNs('DataEmissao'), tcDat);

    aValor := ObterConteudo(ANode.Childrens.FindAnyNs('StatusRPS'), tcStr);

    if aValor = 'N' then
      StatusRps := srNormal
    else
      StatusRps := srCancelado;

    TipoTributacaoRPS := FPAOwner.StrToTipoTributacaoRPS(Ok, ObterConteudo(ANode.Childrens.FindAnyNs('TributacaoNFe'), tcStr));

    LerChaveRPS(ANode);

    Servico.ItemListaServico := ObterConteudo(ANode.Childrens.FindAnyNs('CodigoServico'), tcStr);
    Servico.Discriminacao := ObterConteudo(ANode.Childrens.FindAnyNs('Discriminacao'), tcStr);
    Servico.Discriminacao := StringReplace(Servico.Discriminacao, FpQuebradeLinha,
                                    sLineBreak, [rfReplaceAll, rfIgnoreCase]);

    VerificarSeConteudoEhLista(Servico.Discriminacao);

    ValorCargaTributaria := ObterConteudo(ANode.Childrens.FindAnyNs('ValorCargaTributaria'), tcDe2);
    PercentualCargaTributaria := ObterConteudo(ANode.Childrens.FindAnyNs('PercentualCargaTributaria'), tcDe4);
    Servico.FonteCargaTributaria := ObterConteudo(ANode.Childrens.FindAnyNs('FonteCargaTributaria'), tcStr);
    Servico.MunicipioIncidencia := ObterConteudo(ANode.Childrens.FindAnyNs('MunicipioPrestacao'), tcInt);
    Servico.ValorTotalRecebido := ObterConteudo(ANode.Childrens.FindAnyNs('ValorTotalRecebido'), tcDe2);

    Servico.Valores.ValorServicos := ObterConteudo(ANode.Childrens.FindAnyNs('ValorServicos'), tcDe2);
    Servico.Valores.ValorDeducoes := ObterConteudo(ANode.Childrens.FindAnyNs('ValorDeducoes'), tcDe2);
    Servico.Valores.ValorPis := ObterConteudo(ANode.Childrens.FindAnyNs('ValorPIS'), tcDe2);
    Servico.Valores.ValorCofins := ObterConteudo(ANode.Childrens.FindAnyNs('ValorCOFINS'), tcDe2);
    Servico.Valores.ValorInss := ObterConteudo(ANode.Childrens.FindAnyNs('ValorINSS'), tcDe2);
    Servico.Valores.ValorIr := ObterConteudo(ANode.Childrens.FindAnyNs('ValorIR'), tcDe2);
    Servico.Valores.ValorCsll := ObterConteudo(ANode.Childrens.FindAnyNs('ValorCSLL'), tcDe2);
    Servico.Valores.Aliquota := ObterConteudo(ANode.Childrens.FindAnyNs('AliquotaServicos'), tcDe4);

    aValor := ObterConteudo(ANode.Childrens.FindAnyNs('ISSRetido'), tcStr);

    if aValor = 'true' then
      Servico.Valores.IssRetido := stRetencao
    else
      Servico.Valores.IssRetido := stNormal;

    LerCPFCNPJTomador(ANode);

    Tomador.RazaoSocial := ObterConteudo(ANode.Childrens.FindAnyNs('RazaoSocialTomador'), tcStr);

    Tomador.IdentificacaoTomador.InscricaoMunicipal := ObterConteudo(ANode.Childrens.FindAnyNs('InscricaoMunicipalTomador'), tcStr);
    Tomador.IdentificacaoTomador.InscricaoEstadual := ObterConteudo(ANode.Childrens.FindAnyNs('InscricaoEstadualTomador'), tcStr);

    Tomador.Contato.Email := ObterConteudo(ANode.Childrens.FindAnyNs('EmailTomador'), tcStr);

    LerEnderecoTomador(ANode);
    LerCPFCNPJIntermediario(ANode);

    Intermediario.Identificacao.InscricaoMunicipal := ObterConteudo(ANode.Childrens.FindAnyNs('InscricaoMunicipalIntermediario'), tcStr);
    Intermediario.Contato.EMail := ObterConteudo(ANode.Childrens.FindAnyNs('EmailIntermediario'), tcStr);

    aValor := ObterConteudo(ANode.Childrens.FindAnyNs('ISSRetidoIntermediario'), tcStr);

    if aValor = 'true' then
      Intermediario.IssRetido := stRetencao
    else
      Intermediario.IssRetido := stNormal;

    ConstrucaoCivil.nCei := ObterConteudo(ANode.Childrens.FindAnyNs('CodigoCEI'), tcStr);
    ConstrucaoCivil.nMatri := ObterConteudo(ANode.Childrens.FindAnyNs('MatriculaObra'), tcStr);
    ConstrucaoCivil.nNumeroEncapsulamento := ObterConteudo(ANode.Childrens.FindAnyNs('NumeroEncapsulamento'), tcStr);
  end;
end;

end.
