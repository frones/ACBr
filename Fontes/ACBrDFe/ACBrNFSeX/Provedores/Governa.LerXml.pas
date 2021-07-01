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
{$IFDEF FPC}
  LResources, Controls, Graphics, Dialogs,
{$ENDIF}
  SysUtils, Classes, StrUtils,
  ACBrUtil,
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
begin
  AuxNode := ANode.Childrens.FindAnyNs('tcItensRps');

  if AuxNode <> nil then
  begin
    ANodes := AuxNode.Childrens.FindAllAnyNs('tcItemRps');

    for i := 0 to Length(ANodes) - 1 do
    begin
      NFSe.Servico.ItemServico.New;

      with NFSe.Servico.ItemServico[i] do
      begin
        Descricao  := ProcessarConteudo(ANodes[i].Childrens.FindAnyNs('tsDesSvc'), tcStr);
        ValorUnitario := ProcessarConteudo(ANodes[i].Childrens.FindAnyNs('tsVlrUnt'), tcDe2);
      end;
    end;
  end;
end;

function TNFSeR_Governa.LerXml: Boolean;
var
  XmlNode: TACBrXmlNode;
  xRetorno: string;
begin
  xRetorno := TratarRetorno(Arquivo);

  if EstaVazio(xRetorno) then
    raise Exception.Create('Arquivo xml não carregado.');

  if FDocument = nil then
    FDocument := TACBrXmlDocument.Create();

  Document.Clear();
  Document.LoadFromXml(xRetorno);

  if (Pos('tcRps', xRetorno) > 0) then
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
    Numero            := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('NumNot'), tcStr);
    CodigoVerificacao := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('CodVer'), tcStr);
    DataEmissao       := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('DtemiNfse'), tcDat);
    Competencia       := DataEmissao;
    TipoRecolhimento  := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('TipRec'), tcStr);
    OutrasInformacoes := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Obs'), tcStr);

    IdentificacaoRps.Numero := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('NumRps'), tcStr);

    with Prestador do
    begin
      RazaoSocial := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('RzSocialPr'), tcStr);

      with IdentificacaoPrestador do
      begin
        Cnpj               := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('CNPJPr'), tcStr);
        InscricaoEstadual  := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('IEPr'), tcStr);
        InscricaoMunicipal := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('CodCadBic'), tcStr);
      end;

      with Endereco do
      begin
        Endereco    := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('EndLogradouroPr'), tcStr);
        Numero      := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('EndNumeroPr'), tcStr);
        Bairro      := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('EndBairroPr'), tcStr);
        Complemento := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('EndComplPr'), tcStr);
        xMunicipio  := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('EndCidadePr'), tcStr);
        CEP         := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('EndCEPPr'), tcStr);
        UF          := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('EndUFPr'), tcStr);
      end;
    end;

    with Tomador do
    begin
      RazaoSocial := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('NomTmd'), tcStr);

      with IdentificacaoTomador do
      begin
        CpfCnpj           := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('NumDocTmd'), tcStr);
        InscricaoEstadual := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('InscricaoEstadual'), tcStr);
      end;

      with Endereco do
      begin
        Endereco    := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('DesEndTmd'), tcStr);
//        Numero      := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('EndNumeroPr'), tcStr);
        Bairro      := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('NomBaiTmd'), tcStr);
//        Complemento := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('EndComplPr'), tcStr);
        xMunicipio  := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('NomCidTmd'), tcStr);
        CEP         := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('CEPTmd'), tcStr);
        UF          := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('CodEstTmd'), tcStr);
      end;
    end;

    with Servico do
    begin
      CodigoCnae := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('CodAti'), tcStr);
      Discriminacao := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('DesSvc'), tcStr);
      Descricao := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('DescricaoServ'), tcStr);

      with Valores do
      begin
        ValorServicos := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('VlrUnt'), tcDe2);
        ValorPis := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('VlrPIS'), tcDe2);
        ValorCofins := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('VlrCofins'), tcDe2);
        ValorInss := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('VlrINSS'), tcDe2);
        ValorIr := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('VlrIR'), tcDe2);

        ValorLiquidoNfse := 0;

        ANodes := AuxNode.Childrens.FindAllAnyNs('ItemNfse');

        for i := 0 to Length(ANodes) - 1 do
        begin
          ItemServico.New;
          with ItemServico[i] do
          begin
            Descricao     := ProcessarConteudo(ANodes[i].Childrens.FindAnyNs('DesSvc'), tcStr);
            Quantidade    := ProcessarConteudo(ANodes[i].Childrens.FindAnyNs('QdeSvc'), tcDe2);
            ValorUnitario := ProcessarConteudo(ANodes[i].Childrens.FindAnyNs('VlrUnt'), tcDe2);
            ValorTotal    := Quantidade + ValorUnitario;
          end;

          ValorLiquidoNfse := ValorLiquidoNfse + ItemServico[i].ValorTotal;
        end;
      end;
    end;
  end;
end;

function TNFSeR_Governa.LerXmlRps(const ANode: TACBrXmlNode): Boolean;
var
  AuxNode: TACBrXmlNode;
begin
  Result := True;

  AuxNode := ANode.Childrens.FindAnyNs('tcInfRps');

  if AuxNode = nil then Exit;

  with NFSe do
  begin
    with IdentificacaoRps do
    begin
      Numero := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('tsNumRps'), tcStr);
    end;

    CodigoVerificacao := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('tsCodVer'), tcStr);

    with Tomador do
    begin
      with IdentificacaoTomador do
      begin
        CpfCnpj := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('tsNumDocTmd'), tcStr);
        InscricaoEstadual := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('tsInsEstTmd'), tcStr);
        InscricaoMunicipal := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('tsInsMunTmd'), tcStr);
      end;

      RazaoSocial := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('tsNomTmd'), tcStr);

      with Endereco do
      begin
        Endereco := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('tsDesEndTmd'), tcStr);
        Bairro := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('tsNomBaiTmd'), tcStr);
        xMunicipio := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('tsNomCidTmd'), tcStr);
        UF := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('tsCodEstTmd'), tcStr);
        CEP := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('tsCEPTmd'), tcStr);
        Endereco := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('tsDesEndTmd'), tcStr);
      end;

      with Contato do
      begin
        Email := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('tsEmlTmd'), tcStr);
      end;
    end;

    with Servico do
    begin
      CodigoTributacaoMunicipio := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('tsCodAti'), tcStr);

      with Valores do
      begin
        Aliquota := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('tsPerAlq'), tcDe2);
        ValorRepasse := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('tsVlrRep'), tcDe2);
        ValorDeducoes := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('tsVlrDed'), tcDe2);
        DescontoCondicionado := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('tsVlrDsc'), tcDe2);
        ValorPis := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('tsVlrPIS'), tcDe2);
        ValorCofins := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('tsVlrCOFINS'), tcDe2);
        ValorInss := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('tsVlrINSS'), tcDe2);
        ValorIr := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('tsVlrIR'), tcDe2);
        ValorCsll := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('tsVlrCSLL'), tcDe2);
        valorOutrasRetencoes := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('tsVlrOtrRtn'), tcDe2);
        DescricaoOutrasRetencoes := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('tsDesOtrRtn'), tcStr);
      end;

      UFPrestacao := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('tsEstServ'), tcStr);
      CodigoMunicipio := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('tsMunSvc'), tcStr);
    end;

    RegRec := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('tsRegRec'), tcStr);
    FrmRec := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('tsFrmRec'), tcStr);
    DataEmissao := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('tsDatEmsRps'), tcDat);
    TipoRecolhimento := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('tsTipRec'), tcStr);
    OutrasInformacoes := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('tsObs'), tcStr);

    LerItensRps(AuxNode);
  end;
end;

end.
