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

unit Simple.LerXml;

interface

uses
  SysUtils, Classes, StrUtils,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXConversao, ACBrNFSeXLerXml;

type
  { TNFSeR_Simple }

  TNFSeR_Simple = class(TNFSeRClass)
  protected
    procedure LerTomador(const ANode: TACBrXmlNode);
    procedure LerTItens(const ANode: TACBrXmlNode);
    procedure LerItens(const ANode: TACBrXmlNode);
  public
    function LerXml: Boolean; override;
    function LerXmlRps(const ANode: TACBrXmlNode): Boolean;
    function LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
  end;

implementation

uses
  ACBrUtil.Base,
  ACBrUtil.Strings;

//==============================================================================
// Essa unit tem por finalidade exclusiva ler o XML do provedor:
//     Simple
//==============================================================================

{ TNFSeR_Simple }

procedure TNFSeR_Simple.LerItens(const ANode: TACBrXmlNode);
var
  ANodes: TACBrXmlNodeArray;
  i: Integer;
begin
  ANodes := ANode.Childrens.FindAllAnyNs('Itens');

  for i := 0 to Length(ANodes) - 1 do
  begin
    NFSe.Servico.ItemServico.New;

    with NFSe.Servico.ItemServico[i] do
    begin
      ItemListaServico := ObterConteudo(ANodes[i].Childrens.FindAnyNs('iServico'), tcStr);
      ItemListaServico := NormatizarItemListaServico(ItemListaServico);
      xItemListaServico := ItemListaServicoDescricao(ItemListaServico);
      ValorUnitario := ObterConteudo(ANodes[i].Childrens.FindAnyNs('nValorServico'), tcDe2);
      Descricao := ObterConteudo(ANodes[i].Childrens.FindAnyNs('sDescricao'), tcStr);
      Descricao := StringReplace(Descricao, FpQuebradeLinha,
                                      sLineBreak, [rfReplaceAll, rfIgnoreCase]);
      Aliquota := ObterConteudo(ANodes[i].Childrens.FindAnyNs('nAliquota'), tcDe2);
      ValorISS := ObterConteudo(ANodes[i].Childrens.FindAnyNs('nValorIss'), tcDe2);
      ValorTotal := ObterConteudo(ANodes[i].Childrens.FindAnyNs('nValorTotal'), tcDe2);

      Quantidade := ValorTotal / ValorUnitario;
    end;

    NFSe.Servico.CodigoCnae := ObterConteudo(ANodes[i].Childrens.FindAnyNs('sCNAE'), tcStr);
  end;
end;

procedure TNFSeR_Simple.LerTItens(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('tItens');

  if AuxNode = nil then Exit;

  LerItens(AuxNode);
end;

procedure TNFSeR_Simple.LerTomador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('tTomador');

  if AuxNode <> nil then
  begin
    with NFSe.Tomador do
    begin
      IdentificacaoTomador.CpfCnpj := ObterConteudo(AuxNode.Childrens.FindAnyNs('sCPFTomador'), tcStr);
      RazaoSocial := ObterConteudo(AuxNode.Childrens.FindAnyNs('sNomeTomador'), tcStr);

      Endereco.xMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('sCidadeTomador'), tcStr);
      Endereco.Endereco := ObterConteudo(AuxNode.Childrens.FindAnyNs('sEnderecoTomador'), tcStr);
      Endereco.UF := ObterConteudo(AuxNode.Childrens.FindAnyNs('sUfTomador'), tcStr);

      Contato.Email := ObterConteudo(AuxNode.Childrens.FindAnyNs('sEmailTomador'), tcStr);
    end;
  end;
end;

function TNFSeR_Simple.LerXml: Boolean;
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

  if (Pos('Nota', Arquivo) > 0) then
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

function TNFSeR_Simple.LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
var
  aValor: string;
  Ok :Boolean;
  i: Integer;
begin
  Result := True;

  if not Assigned(ANode) then Exit;

  with NFSe do
  begin
    Prestador.IdentificacaoPrestador.CpfCnpj := ObterConteudo(ANode.Childrens.FindAnyNs('sContribuinte'), tcStr);
    IdentificacaoRps.Numero := ObterConteudo(ANode.Childrens.FindAnyNs('iRecibo'), tcStr);
    dhRecebimento := ObterConteudo(ANode.Childrens.FindAnyNs('dDataRecibo'), tcDatHor);
    Numero := ObterConteudo(ANode.Childrens.FindAnyNs('iNota'), tcStr);
    SeriePrestacao := ObterConteudo(ANode.Childrens.FindAnyNs('sSerie'), tcStr);
    DataEmissao := ObterConteudo(ANode.Childrens.FindAnyNs('dDataEmissao'), tcDatHor);
    CodigoVerificacao := ObterConteudo(ANode.Childrens.FindAnyNs('sCodigoVerificador'), tcStr);
    SituacaoNfse := StrToStatusNFSe(Ok, ObterConteudo(ANode.Childrens.FindAnyNs('sSituacao'), tcStr));
    Competencia := DataEmissao;

    Servico.Valores.ValorServicos := ObterConteudo(ANode.Childrens.FindAnyNs('nValorTotal'), tcDe2);
    Servico.Valores.ValorIss := ObterConteudo(ANode.Childrens.FindAnyNs('nValorIss'), tcDe2);
    Servico.Valores.BaseCalculo := ObterConteudo(ANode.Childrens.FindAnyNs('nValorBaseCalculo'), tcDe2);

    Servico.Valores.ValorLiquidoNfse := Servico.Valores.ValorServicos;

    Servico.Valores.ValorTotalNotaFiscal := Servico.Valores.ValorServicos -
                                            Servico.Valores.DescontoCondicionado -
                                            Servico.Valores.DescontoIncondicionado;
  end;

  LerTomador(ANode);

  with NFSe.Servico.Valores do
  begin
    AliquotaIr := ObterConteudo(ANode.Childrens.FindAnyNs('nIrAliquota'), tcDe2);
    ValorIr := ObterConteudo(ANode.Childrens.FindAnyNs('nIrValor'), tcDe2);
    ValorPis := ObterConteudo(ANode.Childrens.FindAnyNs('nPisPasep'), tcDe2);
    ValorCofins := ObterConteudo(ANode.Childrens.FindAnyNs('nCofins'), tcDe2);
    ValorInss := ObterConteudo(ANode.Childrens.FindAnyNs('nInss'), tcDe2);
    ValorCsll := ObterConteudo(ANode.Childrens.FindAnyNs('nCsll'), tcDe2);

    RetencoesFederais := ValorPis + ValorCofins + ValorInss + ValorIr + ValorCsll;
  end;

  LerTItens(ANode);

  aValor := '';

  for i := 1 to 10 do
    aValor := aValor +
      ObterConteudo(ANode.Childrens.FindAnyNs('sObservacao' + IntToStr(i)), tcStr){ +
      ';'};

  NFSe.OutrasInformacoes := aValor;
  NFSe.OutrasInformacoes := StringReplace(NFSe.OutrasInformacoes, FpQuebradeLinha,
                                      sLineBreak, [rfReplaceAll, rfIgnoreCase]);

  LerCampoLink;
end;

function TNFSeR_Simple.LerXmlRps(const ANode: TACBrXmlNode): Boolean;
//var
//  aValor: string;
begin
  if not Assigned(ANode) then Exit;

  Result := True;
  (*
  with NFSe do
  begin
    Assinatura := ObterConteudo(ANode.Childrens.FindAnyNs('Assinatura'), tcStr);
    DataEmissao := ObterConteudo(ANode.Childrens.FindAnyNs('DataEmissao'), tcDat);

    aValor := ObterConteudo(ANode.Childrens.FindAnyNs('StatusRPS'), tcStr);

    if aValor = 'N' then
      Status := srNormal
    else
      Status := srCancelado;

    TipoTributacaoRPS := ObterConteudo(ANode.Childrens.FindAnyNs('TributacaoRPS'), tcStr);

    LerChaveRPS(ANode);

    with Servico do
    begin
      ItemListaServico := ObterConteudo(ANode.Childrens.FindAnyNs('CodigoServico'), tcStr);

      Discriminacao := ObterConteudo(ANode.Childrens.FindAnyNs('Discriminacao'), tcStr);

      ValorCargaTributaria := ObterConteudo(ANode.Childrens.FindAnyNs('ValorCargaTributaria'), tcDe2);

      PercentualCargaTributaria := ObterConteudo(ANode.Childrens.FindAnyNs('PercentualCargaTributaria'), tcDe4);

      FonteCargaTributaria := ObterConteudo(ANode.Childrens.FindAnyNs('FonteCargaTributaria'), tcStr);

      MunicipioIncidencia := ObterConteudo(ANode.Childrens.FindAnyNs('MunicipioPrestacao'), tcInt);

      with Valores do
      begin
        ValorServicos := ObterConteudo(ANode.Childrens.FindAnyNs('ValorServicos'), tcDe2);

        ValorDeducoes := ObterConteudo(ANode.Childrens.FindAnyNs('ValorDeducoes'), tcDe2);

        ValorPis := ObterConteudo(ANode.Childrens.FindAnyNs('ValorPIS'), tcDe2);

        ValorCofins := ObterConteudo(ANode.Childrens.FindAnyNs('ValorCOFINS'), tcDe2);

        ValorInss := ObterConteudo(ANode.Childrens.FindAnyNs('ValorINSS'), tcDe2);

        ValorIr := ObterConteudo(ANode.Childrens.FindAnyNs('ValorIR'), tcDe2);

        ValorCsll := ObterConteudo(ANode.Childrens.FindAnyNs('ValorCSLL'), tcDe2);

        Aliquota := ObterConteudo(ANode.Childrens.FindAnyNs('AliquotaServicos'), tcDe4);

        aValor := ObterConteudo(ANode.Childrens.FindAnyNs('ISSRetido'), tcStr);

        if aValor = 'true' then
          IssRetido := stRetencao
        else
          IssRetido := stNormal;
      end;
    end;

    LerCPFCNPJTomador(ANode);

    with Tomador do
    begin
      RazaoSocial := ObterConteudo(ANode.Childrens.FindAnyNs('RazaoSocialTomador'), tcStr);

      with IdentificacaoTomador do
      begin
        InscricaoMunicipal := ObterConteudo(ANode.Childrens.FindAnyNs('InscricaoMunicipalTomador'), tcStr);

        InscricaoEstadual := ObterConteudo(ANode.Childrens.FindAnyNs('InscricaoEstadualTomador'), tcStr);
      end;

      with Contato do
      begin
        Email := ObterConteudo(ANode.Childrens.FindAnyNs('EmailTomador'), tcStr);
      end;
    end;

    LerEnderecoTomador(ANode);
    LerCPFCNPJIntermediario(ANode);

    with IntermediarioServico do
    begin
      InscricaoMunicipal := ObterConteudo(ANode.Childrens.FindAnyNs('InscricaoMunicipalIntermediario'), tcStr);

      EMail := ObterConteudo(ANode.Childrens.FindAnyNs('EmailIntermediario'), tcStr);

      aValor := ObterConteudo(ANode.Childrens.FindAnyNs('ISSRetidoIntermediario'), tcStr);

      if aValor = 'true' then
        IssRetido := stRetencao
      else
        IssRetido := stNormal;
    end;

    with ConstrucaoCivil do
    begin
      nCei := ObterConteudo(ANode.Childrens.FindAnyNs('CodigoCEI'), tcStr);

      nMatri := ObterConteudo(ANode.Childrens.FindAnyNs('MatriculaObra'), tcStr);

      nNumeroEncapsulamento := ObterConteudo(ANode.Childrens.FindAnyNs('NumeroEncapsulamento'), tcStr);
    end;
  end;
  *)
end;

end.
