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
{$IFDEF FPC}
  LResources, Controls, Graphics, Dialogs,
{$ENDIF}
  SysUtils, Classes, StrUtils,
  ACBrUtil,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXConversao, ACBrNFSeXLerXml;

type
  { TNFSeR_Simple }

  TNFSeR_Simple = class(TNFSeRClass)
  protected
    procedure LerTomador(const ANode: TACBrXmlNode);
    procedure LerTItens(const ANode: TACBrXmlNode);
    procedure LerItens(const ANode: TACBrXmlNode);

    procedure SetxItemListaServico(Codigo: string);
  public
    function LerXml: Boolean; override;
    function LerXmlRps(const ANode: TACBrXmlNode): Boolean;
    function LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
  end;

implementation

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
      ItemListaServico := ProcessarConteudo(ANodes[i].Childrens.FindAnyNs('iServico'), tcStr);
      ValorUnitario := ProcessarConteudo(ANodes[i].Childrens.FindAnyNs('nValorServico'), tcDe2);
      Descricao := ProcessarConteudo(ANodes[i].Childrens.FindAnyNs('sDescricao'), tcStr);
      Aliquota := ProcessarConteudo(ANodes[i].Childrens.FindAnyNs('nAliquota'), tcDe2);
      ValorISS := ProcessarConteudo(ANodes[i].Childrens.FindAnyNs('nValorIss'), tcDe2);
      ValorTotal := ProcessarConteudo(ANodes[i].Childrens.FindAnyNs('nValorTotal'), tcDe2);
    end;

    NFSe.Servico.CodigoCnae := ProcessarConteudo(ANodes[i].Childrens.FindAnyNs('sCNAE'), tcStr);
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
      IdentificacaoTomador.CpfCnpj := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('sCPFTomador'), tcStr);
      RazaoSocial := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('sNomeTomador'), tcStr);

      with Endereco do
      begin
        xMunicipio := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('sCidadeTomador'), tcStr);
        Endereco := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('sEnderecoTomador'), tcStr);
        UF := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('sUfTomador'), tcStr);
      end;

      Contato.Email := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('sEmailTomador'), tcStr);
    end;
  end;
end;

function TNFSeR_Simple.LerXml: Boolean;
var
  XmlNode: TACBrXmlNode;
  xRetorno: string;
begin
  xRetorno := TratarXmlRetorno(Arquivo);

  if EstaVazio(xRetorno) then
    raise Exception.Create('Arquivo xml não carregado.');

  if FDocument = nil then
    FDocument := TACBrXmlDocument.Create();

  Document.Clear();
  Document.LoadFromXml(xRetorno);

  if (Pos('Nota', xRetorno) > 0) then
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

function TNFSeR_Simple.LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
var
  AuxNode: TACBrXmlNode;
  aValor: string;
  Ok :Boolean;
  i: Integer;
begin
  Result := True;

  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode;

  with NFSe do
  begin
    Prestador.IdentificacaoPrestador.Cnpj := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('sContribuinte'), tcStr);
    IdentificacaoRps.Numero := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('iRecibo'), tcStr);
    dhRecebimento := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('dDataRecibo'), tcDatHor);
    Numero := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('iNota'), tcStr);
    SeriePrestacao := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('sSerie'), tcStr);
    DataEmissao := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('dDataEmissao'), tcDatHor);
    CodigoVerificacao := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('sCodigoVerificador'), tcStr);
    SituacaoNfse := StrToStatusNFSe(Ok, ProcessarConteudo(AuxNode.Childrens.FindAnyNs('sSituacao'), tcStr));

    Servico.Valores.ValorServicos := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('nValorTotal'), tcDe2);
    Servico.Valores.ValorIss := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('nValorIss'), tcDe2);
    Servico.Valores.BaseCalculo := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('nValorBaseCalculo'), tcDe2);
  end;

  LerTomador(AuxNode);

  with NFSe.Servico.Valores do
  begin
    AliquotaIr := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('nIrAliquota'), tcDe2);
    ValorIr := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('nIrValor'), tcDe2);
    ValorPis := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('nPisPasep'), tcDe2);
    ValorCofins := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('nCofins'), tcDe2);
    ValorInss := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('nInss'), tcDe2);
    ValorCsll := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('nCsll'), tcDe2);
  end;

  LerTItens(AuxNode);

  aValor := '';

  for i := 1 to 10 do
    aValor := aValor +
      ProcessarConteudo(AuxNode.Childrens.FindAnyNs('sObservacao' + IntToStr(i)), tcStr);

  NFSe.OutrasInformacoes := aValor;
end;

function TNFSeR_Simple.LerXmlRps(const ANode: TACBrXmlNode): Boolean;
//var
//  aValor: string;
begin
  Result := True;
  (*
  with NFSe do
  begin
    Assinatura := ProcessarConteudo(ANode.Childrens.FindAnyNs('Assinatura'), tcStr);
    DataEmissao := ProcessarConteudo(ANode.Childrens.FindAnyNs('DataEmissao'), tcDat);

    aValor := ProcessarConteudo(ANode.Childrens.FindAnyNs('StatusRPS'), tcStr);

    if aValor = 'N' then
      Status := srNormal
    else
      Status := srCancelado;

    TipoTributacaoRPS := ProcessarConteudo(ANode.Childrens.FindAnyNs('TributacaoRPS'), tcStr);

    LerChaveRPS(ANode);

    with Servico do
    begin
      ItemListaServico := ProcessarConteudo(ANode.Childrens.FindAnyNs('CodigoServico'), tcStr);

      Discriminacao := ProcessarConteudo(ANode.Childrens.FindAnyNs('Discriminacao'), tcStr);

      ValorCargaTributaria := ProcessarConteudo(ANode.Childrens.FindAnyNs('ValorCargaTributaria'), tcDe2);

      PercentualCargaTributaria := ProcessarConteudo(ANode.Childrens.FindAnyNs('PercentualCargaTributaria'), tcDe4);

      FonteCargaTributaria := ProcessarConteudo(ANode.Childrens.FindAnyNs('FonteCargaTributaria'), tcStr);

      MunicipioIncidencia := ProcessarConteudo(ANode.Childrens.FindAnyNs('MunicipioPrestacao'), tcInt);

      with Valores do
      begin
        ValorServicos := ProcessarConteudo(ANode.Childrens.FindAnyNs('ValorServicos'), tcDe2);

        ValorDeducoes := ProcessarConteudo(ANode.Childrens.FindAnyNs('ValorDeducoes'), tcDe2);

        ValorPis := ProcessarConteudo(ANode.Childrens.FindAnyNs('ValorPIS'), tcDe2);

        ValorCofins := ProcessarConteudo(ANode.Childrens.FindAnyNs('ValorCOFINS'), tcDe2);

        ValorInss := ProcessarConteudo(ANode.Childrens.FindAnyNs('ValorINSS'), tcDe2);

        ValorIr := ProcessarConteudo(ANode.Childrens.FindAnyNs('ValorIR'), tcDe2);

        ValorCsll := ProcessarConteudo(ANode.Childrens.FindAnyNs('ValorCSLL'), tcDe2);

        Aliquota := ProcessarConteudo(ANode.Childrens.FindAnyNs('AliquotaServicos'), tcDe4);

        aValor := ProcessarConteudo(ANode.Childrens.FindAnyNs('ISSRetido'), tcStr);

        if aValor = 'true' then
          IssRetido := stRetencao
        else
          IssRetido := stNormal;
      end;
    end;

    LerCPFCNPJTomador(ANode);

    with Tomador do
    begin
      RazaoSocial := ProcessarConteudo(ANode.Childrens.FindAnyNs('RazaoSocialTomador'), tcStr);

      with IdentificacaoTomador do
      begin
        InscricaoMunicipal := ProcessarConteudo(ANode.Childrens.FindAnyNs('InscricaoMunicipalTomador'), tcStr);

        InscricaoEstadual := ProcessarConteudo(ANode.Childrens.FindAnyNs('InscricaoEstadualTomador'), tcStr);
      end;

      with Contato do
      begin
        Email := ProcessarConteudo(ANode.Childrens.FindAnyNs('EmailTomador'), tcStr);
      end;
    end;

    LerEnderecoTomador(ANode);
    LerCPFCNPJIntermediario(ANode);

    with IntermediarioServico do
    begin
      InscricaoMunicipal := ProcessarConteudo(ANode.Childrens.FindAnyNs('InscricaoMunicipalIntermediario'), tcStr);

      EMail := ProcessarConteudo(ANode.Childrens.FindAnyNs('EmailIntermediario'), tcStr);

      aValor := ProcessarConteudo(ANode.Childrens.FindAnyNs('ISSRetidoIntermediario'), tcStr);

      if aValor = 'true' then
        IssRetido := stRetencao
      else
        IssRetido := stNormal;
    end;

    with ConstrucaoCivil do
    begin
      nCei := ProcessarConteudo(ANode.Childrens.FindAnyNs('CodigoCEI'), tcStr);

      nMatri := ProcessarConteudo(ANode.Childrens.FindAnyNs('MatriculaObra'), tcStr);

      nNumeroEncapsulamento := ProcessarConteudo(ANode.Childrens.FindAnyNs('NumeroEncapsulamento'), tcStr);
    end;
  end;
  *)
end;

procedure TNFSeR_Simple.SetxItemListaServico(Codigo: string);
var
  Item: Integer;
  ItemServico: string;
begin
  NFSe.Servico.ItemListaServico := Codigo;

  Item := StrToIntDef(OnlyNumber(Nfse.Servico.ItemListaServico), 0);
  if Item < 100 then
    Item := Item * 100 + 1;

  ItemServico := FormatFloat('0000', Item);

  NFSe.Servico.ItemListaServico := Copy(ItemServico, 1, 2) + '.' +
                                     Copy(ItemServico, 3, 2);

  if FAOwner.ConfigGeral.TabServicosExt then
    NFSe.Servico.xItemListaServico := ObterDescricaoServico(ItemServico)
  else
    NFSe.Servico.xItemListaServico := CodItemServToDesc(ItemServico);
end;

end.
