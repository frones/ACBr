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

unit Equiplano.LerXml;

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
  { TNFSeR_Equiplano }

  TNFSeR_Equiplano = class(TNFSeRClass)
  protected
    procedure LerListaServico(const ANode: TACBrXmlNode);
    procedure LerRetencoes(const ANode: TACBrXmlNode);
  public
    function LerXml: Boolean; override;
    function LerXmlRps(const ANode: TACBrXmlNode): Boolean;
    function LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
  end;

implementation

//==============================================================================
// Essa unit tem por finalidade exclusiva de ler o XML do provedor:
//     Equiplano
//==============================================================================

procedure TNFSeR_Equiplano.LerListaServico(const ANode: TACBrXmlNode);
var
  AuxNode, AuxNodeDed: TACBrXmlNode;
  ANodes: TACBrXmlNodeArray;
  i: Integer;
begin
  AuxNode := ANode.Childrens.FindAnyNs('listaServicos');

  if AuxNode <> nil then
  begin
    ANodes := AuxNode.Childrens.FindAllAnyNs('servico');

    for i := 0 to Length(ANodes) - 1 do
    begin
      NFSe.Servico.ItemServico.New;
      with NFSe.Servico.ItemServico[i] do
      begin
        ItemListaServico := ProcessarConteudo(ANodes[i].Childrens.FindAnyNs('nrServicoItem'), tcStr) +
                            ProcessarConteudo(ANodes[i].Childrens.FindAnyNs('nrServicoSubItem'), tcStr);

        ValorUnitario := ProcessarConteudo(ANodes[i].Childrens.FindAnyNs('vlServico'), tcDe2);

        Aliquota := ProcessarConteudo(ANodes[i].Childrens.FindAnyNs('vlAliquota'), tcDe2);

        AuxNodeDed := ANodes[i].Childrens.FindAnyNs('deducao');

        if AuxNodeDed <> nil then
        begin
          ValorDeducoes := ProcessarConteudo(AuxNodeDed.Childrens.FindAnyNs('vlDeducao'), tcDe2);

          JustificativaDeducao := ProcessarConteudo(AuxNodeDed.Childrens.FindAnyNs('dsJustificativaDeducao'), tcStr);
        end;

        BaseCalculo := ProcessarConteudo(ANodes[i].Childrens.FindAnyNs('vlBaseCalculo'), tcDe2);

        ValorIss := ProcessarConteudo(ANodes[i].Childrens.FindAnyNs('vlIssServico'), tcDe2);

        Descricao := ProcessarConteudo(ANodes[i].Childrens.FindAnyNs('dsDiscriminacaoServico'), tcStr);
      end;
    end;
  end;
end;

procedure TNFSeR_Equiplano.LerRetencoes(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('retencoes');

  if AuxNode <> nil then
  begin
    with NFSe.Servico.Valores do
    begin
      ValorCofins := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('vlCofins'), tcDe2);
      ValorCsll   := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('vlCsll'), tcDe2);
      ValorInss   := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('vlInss'), tcDe2);
      ValorIr     := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('vlIrrf'), tcDe2);
      ValorPis    := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('vlPis'), tcDe2);
      ValorIssRetido := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('vlIss'), tcDe2);
      AliquotaCofins := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('vlAliquotaCofins'), tcDe2);
      AliquotaCsll := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('vlAliquotaCsll'), tcDe2);
      AliquotaInss := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('vlAliquotaInss'), tcDe2);
      AliquotaIr  := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('vlAliquotaIrrf'), tcDe2);
      AliquotaPis := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('vlAliquotaPis'), tcDe2);
    end;
  end;
end;

function TNFSeR_Equiplano.LerXml: Boolean;
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

  if (Pos('nfse', xRetorno) > 0) then
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

function TNFSeR_Equiplano.LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
var
  AuxNode: TACBrXmlNode;
  aValor: string;
begin
  Result := True;

  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('nfse');

  if AuxNode <> nil then
  begin
    NFSe.Numero            := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('nrNfse'), tcStr);
    NFSe.CodigoVerificacao := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('cdAutenticacao'), tcStr);
    NFSe.DataEmissao       := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('dtEmissaoNfs'), tcDatHor);

    NFSe.IdentificacaoRps.Numero := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('nrRps'), tcStr);

    AuxNode := AuxNode.Childrens.FindAnyNs('cancelamento');

    if AuxNode <> nil then
    begin
      NFSe.NfseCancelamento.DataHora := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('dtCancelamento'), tcDatHor);
      NFSe.MotivoCancelamento        := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('dsCancelamento'), tcStr);

      NFSe.Status    := srCancelado;
      NFSe.Cancelada := snSim;
    end;
  end;

  AuxNode := ANode.Childrens.FindAnyNs('nfs');

  if AuxNode = nil then
  begin
    NFSe.Numero                  := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('nrNfs'), tcStr);
    NFSe.CodigoVerificacao       := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('cdAutenticacao'), tcStr);
    NFSe.DataEmissao             := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('dtEmissaoNfs'), tcDatHor);
    NFSe.IdentificacaoRps.Numero := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('nrRps'), tcStr);

    with NFSe.Prestador.IdentificacaoPrestador do
    begin
      Cnpj               := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('nrDocumento'), tcStr);
      InscricaoMunicipal := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('nrInscricaoMunicipal'), tcStr);
    end;

    with NFSe.Prestador do
    begin
      RazaoSocial  := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('nmPrestador'), tcStr);
      NomeFantasia := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('nmPrestador'), tcStr);
    end;

    with NFSe.Prestador.Endereco do
    begin
      Endereco   := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('dsEndereco'), tcStr);
      Numero     := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('nrEndereco'), tcStr);
      Bairro     := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('nmBairro'), tcStr);
      xMunicipio := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('nmCidade'), tcStr);
      UF         := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('nmUf'), tcStr);
      CEP        := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('nrCEP'), tcStr);
      xPais      := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('nmPais'), tcStr);
    end;

    with NFSe.Servico do
    begin
      Discriminacao := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('dsDiscriminacaoServico'), tcStr);
    end;

    with NFSe.Servico.Valores do
    begin
      ValorServicos := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('vlServico'), tcDe2);
      Aliquota      := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('vlAliquota'), tcDe2);
      ValorIss      := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('vlImposto'), tcDe2);
      BaseCalculo   := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('vlBaseCalculo'), tcDe2);

      aValor := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('isIssRetido'), tcStr);

      if aValor = 'Sim' then
        ValorISSRetido := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('vlImposto'), tcDe2);

      ValorPis      := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('vlPis'), tcDe2);
      ValorCofins   := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('vlCofins'), tcDe2);
      ValorIr       := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('vlAliquotaIrpj'), tcDe2);
      ValorCsll     := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('vlCsll'), tcDe2);
      ValorInss     := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('vlInss'), tcDe2);
    end;

    AuxNode := AuxNode.Childrens.FindAnyNs('cancelamento');

    if AuxNode <> nil then
    begin
      NFSe.NfseCancelamento.DataHora := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('dtCancelamento'), tcDatHor);
      NFSe.MotivoCancelamento        := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('dsCancelamento'), tcStr);

      NFSe.Status    := srCancelado;
      NFSe.Cancelada := snSim;
    end;
  end;

  AuxNode := ANode.Childrens.FindAnyNs('tomadorServico');

  if AuxNode <> nil then
  begin
    with NFSe.Tomador do
    begin
      RazaoSocial := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('nmTomador'), tcStr);
    end;

    with NFSe.Tomador.IdentificacaoTomador do
    begin
      CpfCnpj := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('nrDocumento'), tcStr);
    end;

    with NFSe.Tomador.Endereco do
    begin
      Endereco := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('dsEndereco'), tcStr);
      Numero   := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('nrEndereco'), tcStr);

      Bairro          := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('nmBairro'), tcStr);
      CodigoMunicipio := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('cdIbge'), tcStr);
      xMunicipio      := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('nmCidade'), tcStr);
      UF              := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('nmUf'), tcStr);
      CEP             := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('nrCep'), tcStr);
      xPais           := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('nmPais'), tcStr);
    end;
  end;
end;

function TNFSeR_Equiplano.LerXmlRps(const ANode: TACBrXmlNode): Boolean;
var
  AuxNode, AuxNodeDoc: TACBrXmlNode;
  Ok: Boolean;
begin
  Result := True;

  with NFSe do
  begin
    with IdentificacaoRps do
    begin
      Numero := ProcessarConteudo(ANode.Childrens.FindAnyNs('nrRps'), tcStr);
      Serie := ProcessarConteudo(ANode.Childrens.FindAnyNs('nrEmissorRps'), tcStr);
    end;

    DataEmissao := ProcessarConteudo(ANode.Childrens.FindAnyNs('dtEmissaoRps'), tcDatHor);
    NaturezaOperacao := ProcessarConteudo(ANode.Childrens.FindAnyNs('tpTributacao'), tcStr);

    with Servico.Valores do
    begin
      IssRetido := StrToSituacaoTributaria(Ok, ProcessarConteudo(ANode.Childrens.FindAnyNs('isIssRetido'), tcStr));
      ValorServicos := ProcessarConteudo(ANode.Childrens.FindAnyNs('vlTotalRps'), tcDe2);
      ValorLiquidoNfse := ProcessarConteudo(ANode.Childrens.FindAnyNs('vlLiquidoRps'), tcDe2);
    end;

    AuxNode := ANode.Childrens.FindAnyNs('tomador');

    if AuxNode <> nil then
    begin
      with Tomador do
      begin
        RazaoSocial := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('nmTomador'), tcStr);

        with Contato do
        begin
          Email := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('dsEmail'), tcStr);
          Telefone := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('nrTelefone'), tcStr);
        end;

        with IdentificacaoTomador do
        begin
          InscricaoEstadual := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('nrInscricaoEstadual'), tcStr);
        end;

        with Endereco do
        begin
          Endereco := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('dsEndereco'), tcStr);
          Numero := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('nrEndereco'), tcStr);
          Complemento := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('dsComplemento'), tcStr);
          Bairro := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('nmBairro'), tcStr);
          CodigoMunicipio := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('nrCidadeIbge'), tcStr);
          UF := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('nmUf'), tcStr);
          xPais := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('nmPais'), tcStr);
          CEP := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('nrCep'), tcStr);
        end;

        AuxNodeDoc := AuxNode.Childrens.FindAnyNs('documento');

        if AuxNodeDoc <> nil then
        begin
          with IdentificacaoTomador do
          begin
            CpfCnpj := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('nrDocumento'), tcStr);
            DocTomadorEstrangeiro := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('dsDocumentoEstrangeiro'), tcStr);
          end;
        end;
      end;
    end;

    LerListaServico(ANode);
    LerRetencoes(ANode);
  end;
end;

end.
