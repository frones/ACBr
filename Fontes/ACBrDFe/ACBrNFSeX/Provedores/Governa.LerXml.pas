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

unit Governa.LerXml;

interface

uses
  SysUtils, Classes, StrUtils,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXConversao, ACBrNFSeXLerXml;

type
  { Provedor com layout próprio }
  { TNFSeR_Governa }

  TNFSeR_Governa = class(TNFSeRClass)
  protected

    procedure LerItensRps(const ANode: TACBrXmlNode);
  public
    function LerXml: Boolean; override;
    function LerXmlRps(const ANode: TACBrXmlNode): Boolean;
    function LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
  end;

implementation

uses
  ACBrUtil.Base;

//==============================================================================
// Essa unit tem por finalidade exclusiva ler o XML do provedor:
//     Governa
//==============================================================================

{ TNFSeR_Governa }

procedure TNFSeR_Governa.LerItensRps(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  ANodes: TACBrXmlNodeArray;
  i: Integer;
  xValorServico: Extended;
begin
  xValorServico := 0;
  AuxNode := ANode.Childrens.FindAnyNs('tcItensRps');

  if AuxNode <> nil then
  begin
    ANodes := AuxNode.Childrens.FindAllAnyNs('tcItemRps');

    for i := 0 to Length(ANodes) - 1 do
    begin
      NFSe.Servico.ItemServico.New;

      with NFSe.Servico.ItemServico[i] do
      begin
        Descricao     := ObterConteudo(ANodes[i].Childrens.FindAnyNs('tsDesSvc'), tcStr);
        Descricao := StringReplace(Descricao, FpQuebradeLinha,
                                      sLineBreak, [rfReplaceAll, rfIgnoreCase]);
        ValorUnitario := ObterConteudo(ANodes[i].Childrens.FindAnyNs('tsVlrUnt'), tcDe2);
        Quantidade    := 1;
        ValorTotal    := Quantidade * ValorUnitario;
        xValorServico := xValorServico + ValorTotal;
      end;
    end;

    with NFSe.Servico.Valores do
    begin
      ValorServicos := xValorServico;
      ValorLiquidoNfse := xValorServico;
      BaseCalculo := xValorServico;
      ValorIss := (BaseCalculo * (Aliquota/100));
    end;
  end;
end;

function TNFSeR_Governa.LerXml: Boolean;
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

  if (Pos('tcRps', Arquivo) > 0) then
    tpXML := txmlRPS
  else
    tpXML := txmlNFSe;

  XmlNode := Document.Root;

  if XmlNode = nil then
    raise Exception.Create('Arquivo xml vazio.');

  if tpXML = txmlNFSe then
    Result := LerXmlNfse(XmlNode)
  else
    Result := LerXmlRps(XmlNode);

  FreeAndNil(FDocument);
end;

function TNFSeR_Governa.LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
var
  AuxNode: TACBrXmlNode;
  ANodes: TACBrXmlNodeArray;
  i: integer;
begin
  Result := True;

  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('Nfse');

  if AuxNode = nil then Exit;

  with NFSe do
  begin
    Numero            := ObterConteudo(AuxNode.Childrens.FindAnyNs('NumNot'), tcStr);
    CodigoVerificacao := ObterConteudo(AuxNode.Childrens.FindAnyNs('CodVer'), tcStr);
    DataEmissao       := ObterConteudo(AuxNode.Childrens.FindAnyNs('DtemiNfse'), tcDat);
    Competencia       := DataEmissao;
    TipoRecolhimento  := ObterConteudo(AuxNode.Childrens.FindAnyNs('TipRec'), tcStr);
    OutrasInformacoes := ObterConteudo(AuxNode.Childrens.FindAnyNs('Obs'), tcStr);
    OutrasInformacoes := StringReplace(OutrasInformacoes, FpQuebradeLinha,
                                      sLineBreak, [rfReplaceAll, rfIgnoreCase]);

    IdentificacaoRps.Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('NumRps'), tcStr);

    Prestador.RazaoSocial := ObterConteudo(AuxNode.Childrens.FindAnyNs('RzSocialPr'), tcStr);

    Prestador.IdentificacaoPrestador.CpfCnpj := ObterConteudo(AuxNode.Childrens.FindAnyNs('CNPJPr'), tcStr);
    Prestador.IdentificacaoPrestador.InscricaoEstadual := ObterConteudo(AuxNode.Childrens.FindAnyNs('IEPr'), tcStr);
    Prestador.IdentificacaoPrestador.InscricaoMunicipal := ObterConteudo(AuxNode.Childrens.FindAnyNs('CodCadBic'), tcStr);

    Prestador.Endereco.Endereco := ObterConteudo(AuxNode.Childrens.FindAnyNs('EndLogradouroPr'), tcStr);
    Prestador.Endereco.Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('EndNumeroPr'), tcStr);
    Prestador.Endereco.Bairro := ObterConteudo(AuxNode.Childrens.FindAnyNs('EndBairroPr'), tcStr);
    Prestador.Endereco.Complemento := ObterConteudo(AuxNode.Childrens.FindAnyNs('EndComplPr'), tcStr);
    Prestador.Endereco.xMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('EndCidadePr'), tcStr);
    Prestador.Endereco.CEP := ObterConteudo(AuxNode.Childrens.FindAnyNs('EndCEPPr'), tcStr);
    Prestador.Endereco.UF := ObterConteudo(AuxNode.Childrens.FindAnyNs('EndUFPr'), tcStr);

    Tomador.RazaoSocial := ObterConteudo(AuxNode.Childrens.FindAnyNs('NomTmd'), tcStr);

    Tomador.IdentificacaoTomador.CpfCnpj := ObterConteudo(AuxNode.Childrens.FindAnyNs('NumDocTmd'), tcStr);
    Tomador.IdentificacaoTomador.InscricaoEstadual := ObterConteudo(AuxNode.Childrens.FindAnyNs('InscricaoEstadual'), tcStr);

    Tomador.Endereco.Endereco := ObterConteudo(AuxNode.Childrens.FindAnyNs('DesEndTmd'), tcStr);
//    Tomador.Endereco.Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('EndNumeroPr'), tcStr);
    Tomador.Endereco.Bairro := ObterConteudo(AuxNode.Childrens.FindAnyNs('NomBaiTmd'), tcStr);
//    Tomador.Endereco.Complemento := ObterConteudo(AuxNode.Childrens.FindAnyNs('EndComplPr'), tcStr);
    Tomador.Endereco.xMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('NomCidTmd'), tcStr);
    Tomador.Endereco.CEP := ObterConteudo(AuxNode.Childrens.FindAnyNs('CEPTmd'), tcStr);
    Tomador.Endereco.UF := ObterConteudo(AuxNode.Childrens.FindAnyNs('CodEstTmd'), tcStr);

    Servico.CodigoCnae := ObterConteudo(AuxNode.Childrens.FindAnyNs('CodAti'), tcStr);
    Servico.Discriminacao := ObterConteudo(AuxNode.Childrens.FindAnyNs('DesSvc'), tcStr);
    Servico.Discriminacao := StringReplace(Servico.Discriminacao, FpQuebradeLinha,
                                    sLineBreak, [rfReplaceAll, rfIgnoreCase]);

    Servico.Descricao := ObterConteudo(AuxNode.Childrens.FindAnyNs('DescricaoServ'), tcStr);
    Servico.Descricao := StringReplace(Servico.Descricao, FpQuebradeLinha,
                                    sLineBreak, [rfReplaceAll, rfIgnoreCase]);

    Servico.Valores.ValorServicos := ObterConteudo(AuxNode.Childrens.FindAnyNs('VlrUnt'), tcDe2);
    Servico.Valores.ValorPis := ObterConteudo(AuxNode.Childrens.FindAnyNs('VlrPIS'), tcDe2);
    Servico.Valores.ValorCofins := ObterConteudo(AuxNode.Childrens.FindAnyNs('VlrCofins'), tcDe2);
    Servico.Valores.ValorInss := ObterConteudo(AuxNode.Childrens.FindAnyNs('VlrINSS'), tcDe2);
    Servico.Valores.ValorIr := ObterConteudo(AuxNode.Childrens.FindAnyNs('VlrIR'), tcDe2);

    Servico.Valores.RetencoesFederais := Servico.Valores.ValorPis +
      Servico.Valores.ValorCofins + Servico.Valores.ValorInss +
      Servico.Valores.ValorIr + Servico.Valores.ValorCsll;

    Servico.Valores.ValorTotalNotaFiscal := Servico.Valores.ValorServicos -
      Servico.Valores.DescontoCondicionado - Servico.Valores.DescontoIncondicionado;

    Servico.Valores.ValorLiquidoNfse := 0;

    ANodes := AuxNode.Childrens.FindAllAnyNs('ItemNfse');

    for i := 0 to Length(ANodes) - 1 do
    begin
      Servico.ItemServico.New;

      Servico.ItemServico[i].Descricao := ObterConteudo(ANodes[i].Childrens.FindAnyNs('DesSvc'), tcStr);
      Servico.ItemServico[i].Descricao := StringReplace(Servico.ItemServico[i].Descricao, FpQuebradeLinha,
                                sLineBreak, [rfReplaceAll, rfIgnoreCase]);
      Servico.ItemServico[i].Quantidade    := ObterConteudo(ANodes[i].Childrens.FindAnyNs('QdeSvc'), tcDe2);
      Servico.ItemServico[i].ValorUnitario := ObterConteudo(ANodes[i].Childrens.FindAnyNs('VlrUnt'), tcDe2);
      Servico.ItemServico[i].ValorTotal    := Servico.ItemServico[i].Quantidade *
                                              Servico.ItemServico[i].ValorUnitario;

      Servico.Valores.ValorLiquidoNfse := Servico.Valores.ValorLiquidoNfse +
                                          Servico.ItemServico[i].ValorTotal;
    end;
  end;

  LerCampoLink;
end;

function TNFSeR_Governa.LerXmlRps(const ANode: TACBrXmlNode): Boolean;
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
begin
  Result := True;

  AuxNode := ANode.Childrens.FindAnyNs('tcRps');

  if AuxNode = nil then
    AuxNode := ANode.Childrens.FindAnyNs('tcInfRps')
  else
    AuxNode := AuxNode.Childrens.FindAnyNs('tcInfRps');

  if AuxNode = nil then Exit;

  with NFSe do
  begin
    IdentificacaoRps.Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('tsNumRps'), tcStr);

    CodigoVerificacao := ObterConteudo(AuxNode.Childrens.FindAnyNs('tsCodVer'), tcStr);

    Tomador.IdentificacaoTomador.CpfCnpj := ObterConteudo(AuxNode.Childrens.FindAnyNs('tsNumDocTmd'), tcStr);
    Tomador.IdentificacaoTomador.InscricaoEstadual := ObterConteudo(AuxNode.Childrens.FindAnyNs('tsInsEstTmd'), tcStr);
    Tomador.IdentificacaoTomador.InscricaoMunicipal := ObterConteudo(AuxNode.Childrens.FindAnyNs('tsInsMunTmd'), tcStr);

    Tomador.RazaoSocial := ObterConteudo(AuxNode.Childrens.FindAnyNs('tsNomTmd'), tcStr);

    Tomador.Endereco.Endereco := ObterConteudo(AuxNode.Childrens.FindAnyNs('tsDesEndTmd'), tcStr);
    Tomador.Endereco.Bairro := ObterConteudo(AuxNode.Childrens.FindAnyNs('tsNomBaiTmd'), tcStr);
    Tomador.Endereco.xMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('tsNomCidTmd'), tcStr);
    Tomador.Endereco.UF := ObterConteudo(AuxNode.Childrens.FindAnyNs('tsCodEstTmd'), tcStr);
    Tomador.Endereco.CEP := ObterConteudo(AuxNode.Childrens.FindAnyNs('tsCEPTmd'), tcStr);
    Tomador.Endereco.Endereco := ObterConteudo(AuxNode.Childrens.FindAnyNs('tsDesEndTmd'), tcStr);

    Tomador.Contato.Email := ObterConteudo(AuxNode.Childrens.FindAnyNs('tsEmlTmd'), tcStr);

    Servico.CodigoTributacaoMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('tsCodAti'), tcStr);

    Servico.Valores.Aliquota := ObterConteudo(AuxNode.Childrens.FindAnyNs('tsPerAlq'), tcDe2);
    Servico.Valores.ValorRepasse := ObterConteudo(AuxNode.Childrens.FindAnyNs('tsVlrRep'), tcDe2);
    Servico.Valores.ValorDeducoes := ObterConteudo(AuxNode.Childrens.FindAnyNs('tsVlrDed'), tcDe2);
    Servico.Valores.DescontoCondicionado := ObterConteudo(AuxNode.Childrens.FindAnyNs('tsVlrDsc'), tcDe2);
    Servico.Valores.ValorPis := ObterConteudo(AuxNode.Childrens.FindAnyNs('tsVlrPIS'), tcDe2);
    Servico.Valores.ValorCofins := ObterConteudo(AuxNode.Childrens.FindAnyNs('tsVlrCOFINS'), tcDe2);
    Servico.Valores.ValorInss := ObterConteudo(AuxNode.Childrens.FindAnyNs('tsVlrINSS'), tcDe2);
    Servico.Valores.ValorIr := ObterConteudo(AuxNode.Childrens.FindAnyNs('tsVlrIR'), tcDe2);
    Servico.Valores.ValorCsll := ObterConteudo(AuxNode.Childrens.FindAnyNs('tsVlrCSLL'), tcDe2);
    Servico.Valores.valorOutrasRetencoes := ObterConteudo(AuxNode.Childrens.FindAnyNs('tsVlrOtrRtn'), tcDe2);
    Servico.Valores.DescricaoOutrasRetencoes := ObterConteudo(AuxNode.Childrens.FindAnyNs('tsDesOtrRtn'), tcStr);

    Servico.Valores.RetencoesFederais := Servico.Valores.ValorPis +
      Servico.Valores.ValorCofins + Servico.Valores.ValorInss +
      Servico.Valores.ValorIr + Servico.Valores.ValorCsll;

    Servico.UFPrestacao := ObterConteudo(AuxNode.Childrens.FindAnyNs('tsEstServ'), tcStr);
    Servico.CodigoMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('tsMunSvc'), tcStr);

    RegRec := StrToRegRec(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('tsRegRec'), tcStr));
    FrmRec := StrToFrmRec(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('tsFrmRec'), tcStr));
    DataEmissao := ObterConteudo(AuxNode.Childrens.FindAnyNs('tsDatEmsRps'), tcDat);
    TipoRecolhimento := ObterConteudo(AuxNode.Childrens.FindAnyNs('tsTipRec'), tcStr);
    OutrasInformacoes := ObterConteudo(AuxNode.Childrens.FindAnyNs('tsObs'), tcStr);
    OutrasInformacoes := StringReplace(OutrasInformacoes, FpQuebradeLinha,
                                      sLineBreak, [rfReplaceAll, rfIgnoreCase]);

    LerItensRps(AuxNode);

    if FrmRec = frmRetidoNaFonte then
    begin
      Servico.Valores.IssRetido := stRetencao;
      Servico.Valores.ValorIssRetido := Servico.Valores.ValorIss;
    end;

    Servico.Valores.ValorLiquidoNfse := Servico.Valores.ValorServicos -
      Servico.Valores.DescontoCondicionado - Servico.Valores.RetencoesFederais -
      Servico.Valores.ValorOutrasRetencoes - Servico.Valores.ValorIssRetido;

    Servico.Valores.ValorTotalNotaFiscal := Servico.Valores.ValorServicos -
      Servico.Valores.DescontoCondicionado;
  end;
end;

end.
