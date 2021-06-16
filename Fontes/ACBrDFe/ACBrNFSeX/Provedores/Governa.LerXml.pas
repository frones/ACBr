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
  AuxNode := ANode.Childrens.Find('tcItensRps');

  if AuxNode <> nil then
  begin
    ANodes := AuxNode.Childrens.FindAll('tcItemRps');

    for i := 0 to Length(ANodes) - 1 do
    begin
      NFSe.Servico.ItemServico.New;

      with NFSe.Servico.ItemServico[i] do
      begin
        Descricao  := ProcessarConteudo(ANodes[i].Childrens.Find('tsDesSvc'), tcStr);
        ValorUnitario := ProcessarConteudo(ANodes[i].Childrens.Find('tsVlrUnt'), tcDe2);
      end;
    end;
  end;
end;

function TNFSeR_Governa.LerXml: Boolean;
var
  XmlNode: TACBrXmlNode;
begin
  if EstaVazio(Arquivo) then
    raise Exception.Create('Arquivo xml não carregado.');

  Document.Clear();
  Document.LoadFromXml(Arquivo);

  if (Pos('tcRps', Arquivo) > 0) then
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

  AuxNode := ANode.Childrens.Find('Nfse');

  if AuxNode = nil then Exit;

  with NFSe do
  begin
    Numero            := ProcessarConteudo(AuxNode.Childrens.Find('NumNot'), tcStr);
    CodigoVerificacao := ProcessarConteudo(AuxNode.Childrens.Find('CodVer'), tcStr);
    DataEmissao       := ProcessarConteudo(AuxNode.Childrens.Find('DtemiNfse'), tcDat);
    Competencia       := DataEmissao;
    TipoRecolhimento  := ProcessarConteudo(AuxNode.Childrens.Find('TipRec'), tcStr);
    OutrasInformacoes := ProcessarConteudo(AuxNode.Childrens.Find('Obs'), tcStr);

    IdentificacaoRps.Numero := ProcessarConteudo(AuxNode.Childrens.Find('NumRps'), tcStr);

    with Prestador do
    begin
      RazaoSocial := ProcessarConteudo(AuxNode.Childrens.Find('RzSocialPr'), tcStr);

      with IdentificacaoPrestador do
      begin
        Cnpj               := ProcessarConteudo(AuxNode.Childrens.Find('CNPJPr'), tcStr);
        InscricaoEstadual  := ProcessarConteudo(AuxNode.Childrens.Find('IEPr'), tcStr);
        InscricaoMunicipal := ProcessarConteudo(AuxNode.Childrens.Find('CodCadBic'), tcStr);
      end;

      with Endereco do
      begin
        Endereco    := ProcessarConteudo(AuxNode.Childrens.Find('EndLogradouroPr'), tcStr);
        Numero      := ProcessarConteudo(AuxNode.Childrens.Find('EndNumeroPr'), tcStr);
        Bairro      := ProcessarConteudo(AuxNode.Childrens.Find('EndBairroPr'), tcStr);
        Complemento := ProcessarConteudo(AuxNode.Childrens.Find('EndComplPr'), tcStr);
        xMunicipio  := ProcessarConteudo(AuxNode.Childrens.Find('EndCidadePr'), tcStr);
        CEP         := ProcessarConteudo(AuxNode.Childrens.Find('EndCEPPr'), tcStr);
        UF          := ProcessarConteudo(AuxNode.Childrens.Find('EndUFPr'), tcStr);
      end;
    end;

    with Tomador do
    begin
      RazaoSocial := ProcessarConteudo(AuxNode.Childrens.Find('NomTmd'), tcStr);

      with IdentificacaoTomador do
      begin
        CpfCnpj           := ProcessarConteudo(AuxNode.Childrens.Find('NumDocTmd'), tcStr);
        InscricaoEstadual := ProcessarConteudo(AuxNode.Childrens.Find('InscricaoEstadual'), tcStr);
      end;

      with Endereco do
      begin
        Endereco    := ProcessarConteudo(AuxNode.Childrens.Find('DesEndTmd'), tcStr);
//        Numero      := ProcessarConteudo(AuxNode.Childrens.Find('EndNumeroPr'), tcStr);
        Bairro      := ProcessarConteudo(AuxNode.Childrens.Find('NomBaiTmd'), tcStr);
//        Complemento := ProcessarConteudo(AuxNode.Childrens.Find('EndComplPr'), tcStr);
        xMunicipio  := ProcessarConteudo(AuxNode.Childrens.Find('NomCidTmd'), tcStr);
        CEP         := ProcessarConteudo(AuxNode.Childrens.Find('CEPTmd'), tcStr);
        UF          := ProcessarConteudo(AuxNode.Childrens.Find('CodEstTmd'), tcStr);
      end;
    end;

    with Servico do
    begin
      CodigoCnae := ProcessarConteudo(AuxNode.Childrens.Find('CodAti'), tcStr);
      Discriminacao := ProcessarConteudo(AuxNode.Childrens.Find('DesSvc'), tcStr);
      Descricao := ProcessarConteudo(AuxNode.Childrens.Find('DescricaoServ'), tcStr);

      with Valores do
      begin
        ValorServicos := ProcessarConteudo(AuxNode.Childrens.Find('VlrUnt'), tcDe2);
        ValorPis := ProcessarConteudo(AuxNode.Childrens.Find('VlrPIS'), tcDe2);
        ValorCofins := ProcessarConteudo(AuxNode.Childrens.Find('VlrCofins'), tcDe2);
        ValorInss := ProcessarConteudo(AuxNode.Childrens.Find('VlrINSS'), tcDe2);
        ValorIr := ProcessarConteudo(AuxNode.Childrens.Find('VlrIR'), tcDe2);

        ValorLiquidoNfse := 0;

        ANodes := AuxNode.Childrens.FindAll('ItemNfse');

        for i := 0 to Length(ANodes) - 1 do
        begin
          ItemServico.New;
          with ItemServico[i] do
          begin
            Descricao     := ProcessarConteudo(ANodes[i].Childrens.Find('DesSvc'), tcStr);
            Quantidade    := ProcessarConteudo(ANodes[i].Childrens.Find('QdeSvc'), tcDe2);
            ValorUnitario := ProcessarConteudo(ANodes[i].Childrens.Find('VlrUnt'), tcDe2);
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

  AuxNode := ANode.Childrens.Find('tcInfRps');

  if AuxNode = nil then Exit;

  with NFSe do
  begin
    with IdentificacaoRps do
    begin
      Numero := ProcessarConteudo(AuxNode.Childrens.Find('tsNumRps'), tcStr);
    end;

    CodigoVerificacao := ProcessarConteudo(AuxNode.Childrens.Find('tsCodVer'), tcStr);

    with Tomador do
    begin
      with IdentificacaoTomador do
      begin
        CpfCnpj := ProcessarConteudo(AuxNode.Childrens.Find('tsNumDocTmd'), tcStr);
        InscricaoEstadual := ProcessarConteudo(AuxNode.Childrens.Find('tsInsEstTmd'), tcStr);
        InscricaoMunicipal := ProcessarConteudo(AuxNode.Childrens.Find('tsInsMunTmd'), tcStr);
      end;

      RazaoSocial := ProcessarConteudo(AuxNode.Childrens.Find('tsNomTmd'), tcStr);

      with Endereco do
      begin
        Endereco := ProcessarConteudo(AuxNode.Childrens.Find('tsDesEndTmd'), tcStr);
        Bairro := ProcessarConteudo(AuxNode.Childrens.Find('tsNomBaiTmd'), tcStr);
        xMunicipio := ProcessarConteudo(AuxNode.Childrens.Find('tsNomCidTmd'), tcStr);
        UF := ProcessarConteudo(AuxNode.Childrens.Find('tsCodEstTmd'), tcStr);
        CEP := ProcessarConteudo(AuxNode.Childrens.Find('tsCEPTmd'), tcStr);
        Endereco := ProcessarConteudo(AuxNode.Childrens.Find('tsDesEndTmd'), tcStr);
      end;

      with Contato do
      begin
        Email := ProcessarConteudo(AuxNode.Childrens.Find('tsEmlTmd'), tcStr);
      end;
    end;

    with Servico do
    begin
      CodigoTributacaoMunicipio := ProcessarConteudo(AuxNode.Childrens.Find('tsCodAti'), tcStr);

      with Valores do
      begin
        Aliquota := ProcessarConteudo(AuxNode.Childrens.Find('tsPerAlq'), tcDe2);
        ValorRepasse := ProcessarConteudo(AuxNode.Childrens.Find('tsVlrRep'), tcDe2);
        ValorDeducoes := ProcessarConteudo(AuxNode.Childrens.Find('tsVlrDed'), tcDe2);
        DescontoCondicionado := ProcessarConteudo(AuxNode.Childrens.Find('tsVlrDsc'), tcDe2);
        ValorPis := ProcessarConteudo(AuxNode.Childrens.Find('tsVlrPIS'), tcDe2);
        ValorCofins := ProcessarConteudo(AuxNode.Childrens.Find('tsVlrCOFINS'), tcDe2);
        ValorInss := ProcessarConteudo(AuxNode.Childrens.Find('tsVlrINSS'), tcDe2);
        ValorIr := ProcessarConteudo(AuxNode.Childrens.Find('tsVlrIR'), tcDe2);
        ValorCsll := ProcessarConteudo(AuxNode.Childrens.Find('tsVlrCSLL'), tcDe2);
        valorOutrasRetencoes := ProcessarConteudo(AuxNode.Childrens.Find('tsVlrOtrRtn'), tcDe2);
        DescricaoOutrasRetencoes := ProcessarConteudo(AuxNode.Childrens.Find('tsDesOtrRtn'), tcStr);
      end;

      UFPrestacao := ProcessarConteudo(AuxNode.Childrens.Find('tsEstServ'), tcStr);
      CodigoMunicipio := ProcessarConteudo(AuxNode.Childrens.Find('tsMunSvc'), tcStr);
    end;

    RegRec := ProcessarConteudo(AuxNode.Childrens.Find('tsRegRec'), tcStr);
    FrmRec := ProcessarConteudo(AuxNode.Childrens.Find('tsFrmRec'), tcStr);
    DataEmissao := ProcessarConteudo(AuxNode.Childrens.Find('tsDatEmsRps'), tcDat);
    TipoRecolhimento := ProcessarConteudo(AuxNode.Childrens.Find('tsTipRec'), tcStr);
    OutrasInformacoes := ProcessarConteudo(AuxNode.Childrens.Find('tsObs'), tcStr);

    LerItensRps(AuxNode);
  end;
end;

end.
