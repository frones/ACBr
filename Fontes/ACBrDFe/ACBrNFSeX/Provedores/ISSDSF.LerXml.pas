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

unit ISSDSF.LerXml;

interface

uses
{$IFDEF FPC}
  LResources, Controls, Graphics, Dialogs,
{$ENDIF}
  SysUtils, Classes, StrUtils, synacode,
  ACBrUtil,
  ACBrXmlBase, ACBrXmlDocument,
  pcnAuxiliar,
  ACBrNFSeXConversao, ACBrNFSeXLerXml;

type
  { Provedor com layout próprio }
  { TNFSeR_ISSDSF }

  TNFSeR_ISSDSF = class(TNFSeRClass)
  protected

    procedure LerDeducoes(const ANode: TACBrXmlNode);
    procedure LerItens(const ANode: TACBrXmlNode);
  public
    function LerXml: Boolean; override;
    function LerXmlRps(const ANode: TACBrXmlNode): Boolean;
    function LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
  end;

implementation

//==============================================================================
// Essa unit tem por finalidade exclusiva ler o XML do provedor:
//     ISSDSF
//==============================================================================

{ TNFSeR_ISSDSF }

procedure TNFSeR_ISSDSF.LerDeducoes(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  ANodes: TACBrXmlNodeArray;
  i: Integer;
  aValor: string;
  Ok: Boolean;
begin
  AuxNode := ANode.Childrens.FindAnyNs('Deducoes');

  if AuxNode <> nil then
  begin
    ANodes := AuxNode.Childrens.FindAllAnyNs('Deducao');

    for i := 0 to Length(ANodes) - 1 do
    begin
      NFSe.Servico.Deducao.New;
      with NFSe.Servico.Deducao[i] do
      begin
        aValor := ProcessarConteudo(ANodes[i].Childrens.FindAnyNs('DeducaoPor'), tcStr);

        DeducaoPor := StrToEnumerado(Ok, aValor, ['','Percentual','Valor'],
                                             [dpNenhum, dpPercentual, dpValor]);

        aValor := ProcessarConteudo(ANodes[i].Childrens.FindAnyNs('TipoDeducao'), tcStr);

        TipoDeducao := StrToEnumerado(Ok, aValor,
                  ['', 'Despesas com Materiais', 'Despesas com Sub-empreitada'],
                                      [tdNenhum, tdMateriais, tdSubEmpreitada]);

        CpfCnpjReferencia    := ProcessarConteudo(ANodes[i].Childrens.FindAnyNs('CPFCNPJReferencia'), tcStr);
        NumeroNFReferencia   := ProcessarConteudo(ANodes[i].Childrens.FindAnyNs('NumeroNFReferencia'), tcStr);
        ValorTotalReferencia := ProcessarConteudo(ANodes[i].Childrens.FindAnyNs('ValorTotalReferencia'), tcDe2);
        PercentualDeduzir    := ProcessarConteudo(ANodes[i].Childrens.FindAnyNs('PercentualDeduzir'), tcDe2);
        ValorDeduzir         := ProcessarConteudo(ANodes[i].Childrens.FindAnyNs('ValorDeduzir'), tcDe2);
      end;
    end;
  end;
end;

procedure TNFSeR_ISSDSF.LerItens(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  ANodes: TACBrXmlNodeArray;
  i: Integer;
  aValor: string;
  Ok: Boolean;
begin
  AuxNode := ANode.Childrens.FindAnyNs('Itens');

  if AuxNode <> nil then
  begin
    ANodes := AuxNode.Childrens.FindAllAnyNs('Item');

    for i := 0 to Length(ANodes) - 1 do
    begin
      NFSe.Servico.ItemServico.New;
      with NFSe.Servico.ItemServico[i] do
      begin
        Descricao     := ProcessarConteudo(ANodes[i].Childrens.FindAnyNs('DiscriminacaoServico'), tcStr);
        Quantidade    := ProcessarConteudo(ANodes[i].Childrens.FindAnyNs('Quantidade'), tcDe2);
        ValorUnitario := ProcessarConteudo(ANodes[i].Childrens.FindAnyNs('ValorUnitario'), tcDe2);
        ValorTotal    := ProcessarConteudo(ANodes[i].Childrens.FindAnyNs('ValorTotal'), tcDe2);

        aValor := ProcessarConteudo(ANodes[i].Childrens.FindAnyNs('Tributavel'), tcStr);

        Tributavel := StrToEnumerado(Ok, aValor, ['N','S'], [snNao, snSim]);

        NFSe.Servico.Valores.ValorServicos := (NFSe.Servico.Valores.ValorServicos +
                                                                    ValorTotal);
      end;
    end;
  end;
end;

function TNFSeR_ISSDSF.LerXml: Boolean;
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

function TNFSeR_ISSDSF.LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
var
  AuxNode: TACBrXmlNode;
  aValor: string;
  Ok :Boolean;
  sOperacao, sTributacao: String;
begin
  Result := True;

  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('Notas');

  if AuxNode = nil then
    AuxNode := ANode.Childrens.FindAnyNs('Nota');

  if AuxNode = nil then
    AuxNode := ANode.Childrens.FindAnyNs('ConsultaNFSe');

  if AuxNode = nil then Exit;

  NFSe.Numero := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('NumeroNota'), tcStr);

  if (NFSe.Numero = '') then
    NFSe.Numero := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('NumeroNFe'), tcStr);

  NFSe.NumeroLote        := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('NumeroLote'), tcStr);
  NFSe.CodigoVerificacao := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('CodigoVerificacao'), tcStr);

  if NFSe.CodigoVerificacao = '' then
    NFSe.CodigoVerificacao := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('CodigoVerificao'), tcStr);

  NFSe.DataEmissaoRps := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('DataEmissaoRPS'), tcDat);
  NFSe.DataEmissao    := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('DataProcessamento'), tcDatHor);

  if (NFSe.DataEmissao = 0) then
    NFSe.DataEmissao  := NFSe.DataEmissaoRps;

  NFSe.Competencia := NFSe.DataEmissaoRps;

  aValor := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('SituacaoRPS'), tcStr);

  NFSe.Status := StrToEnumerado(ok, aValor, ['N','C'], [srNormal, srCancelado]);

  with NFSe.IdentificacaoRps do
  begin
    Numero := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('NumeroRPS'), tcStr);
    Serie  := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('SerieRPS'), tcStr);
    Tipo   := trRPS;
  end;

  if NFSe.InfID.ID = '' then
    NFSe.InfID.ID := OnlyNumber(NFSe.IdentificacaoRps.Numero);

  NFSe.SeriePrestacao := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('SeriePrestacao'), tcStr);

  with NFSe.Tomador do
  begin
    RazaoSocial := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('RazaoSocialTomador'), tcStr);

    with IdentificacaoTomador do
    begin
      InscricaoMunicipal := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('InscricaoMunicipalTomador'), tcStr);
      CpfCnpj            := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('CPFCNPJTomador'), tcStr);
    end;

    with Endereco do
    begin
      TipoLogradouro := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('TipoLogradouroTomador'), tcStr);
      Endereco       := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('LogradouroTomador'), tcStr);
      Numero         := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('NumeroEnderecoTomador'), tcStr);
      Complemento    := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('ComplementoEnderecoTomador'), tcStr);
      TipoBairro     := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('TipoBairroTomador'), tcStr);
      Bairro         := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('BairroTomador'), tcStr);
      CEP            := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('CEPTomador'), tcStr);

      aValor := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('CidadeTomador'), tcStr);

      if aValor <> '' then
      begin
        CodigoMunicipio := CodTOMToCodIBGE(aValor);
        xMunicipio      := CodIBGEToCidade(StrToInt(CodigoMunicipio));
        UF              := CodigoParaUF(StrToInt(Copy(CodigoMunicipio, 1, 2)));
      end;
    end;

    with Contato do
    begin
      Telefone := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('DDDTomador'), tcStr) +
            ProcessarConteudo(AuxNode.Childrens.FindAnyNs('TelefoneTomador'), tcStr);
      Email  := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('EmailTomador'), tcStr);
    end;
  end;

  NFSe.TipoRecolhimento := AnsiUpperCase(ProcessarConteudo(AuxNode.Childrens.FindAnyNs('TipoRecolhimento'), tcStr));

  with NFSe.Servico do
  begin
    CodigoCnae                := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('CodigoAtividade'), tcStr);
    CodigoTributacaoMunicipio := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('CodigoAtividade'), tcStr);

    aValor := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('MunicipioPrestacao'), tcStr);

    if aValor <> '' then
      CodigoMunicipio := CodTOMToCodIBGE(aValor);

    with Valores do
    begin
      Aliquota       := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('AliquotaAtividade'), tcDe3);
      ValorPis       := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('ValorPIS'), tcDe2);
      ValorCofins    := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('ValorCOFINS'), tcDe2);
      ValorInss      := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('ValorINSS'), tcDe2);
      ValorIr        := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('ValorIR'), tcDe2);
      ValorCsll      := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('ValorCSLL'), tcDe2);
      AliquotaPIS    := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('AliquotaPIS'), tcDe2);
      AliquotaCOFINS := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('AliquotaCOFINS'), tcDe2);
      AliquotaINSS   := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('AliquotaINSS'), tcDe2);
      AliquotaIR     := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('AliquotaIR'), tcDe2);
      AliquotaCSLL   := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('AliquotaCSLL'), tcDe2);

      aValor   := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('TipoRecolhimento'), tcStr);

      IssRetido := StrToEnumerado(Ok, aValor, ['A','R'], [stNormal, stRetencao]);
    end;
  end;

  sOperacao   := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Operacao'), tcStr);
  sTributacao := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Tributacao'), tcStr);

  if (sOperacao <> '') then
  begin
    if (sOperacao = 'A') or (sOperacao = 'B') then
    begin
      if NFSe.Servico.CodigoMunicipio = NFSe.Prestador.Endereco.CodigoMunicipio then
        NFSe.NaturezaOperacao := no1
      else
        NFSe.NaturezaOperacao := no2;
    end
    else
    begin
      if (sOperacao = 'C') and (sTributacao = 'C') then
        NFSe.NaturezaOperacao := no3
      else
      begin
        if (sOperacao = 'C') and (sTributacao = 'F') then
          NFSe.NaturezaOperacao := no4
        else
        begin
          if (sOperacao = 'A') and (sTributacao = 'N') then
            NFSe.NaturezaOperacao := no7;
        end;
      end;
    end;
  end;

  NFSe.Servico.Operacao   := StrToOperacao(Ok, sOperacao);
  NFSe.Servico.Tributacao := StrToTributacao(Ok, sTributacao);

  NFSe.NaturezaOperacao := StrToEnumerado(Ok, sTributacao, ['T','K'], [NFSe.NaturezaOperacao, no5]);

  NFSe.OptanteSimplesNacional := StrToEnumerado(Ok, sTributacao, ['T','H'], [snNao, snSim]);

  NFSe.DeducaoMateriais := StrToEnumerado(Ok, sOperacao, ['A','B'], [snNao, snSim]);

  NFse.RegimeEspecialTributacao := StrToEnumerado(Ok, sTributacao, ['T','M'], [retNenhum, retMicroempresarioIndividual]);

  NFSe.OutrasInformacoes := '';

  NFSE.MotivoCancelamento := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('MotCancelamento'), tcStr);

  with NFSe.Prestador.Contato do
    Telefone := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('DDDPrestador'), tcStr) +
          ProcessarConteudo(AuxNode.Childrens.FindAnyNs('TelefonePrestador'), tcStr);

  with NFSe.IntermediarioServico do
    CpfCnpj := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('CPFCNPJIntermediario'), tcStr);

  LerDeducoes(AuxNode);
  LerItens(AuxNode);

  with NFSe.Servico.Valores do
  begin
    if ((ValorIssRetido = 0) or (ValorIss = 0)) and (Aliquota > 0) then
    begin
      if IssRetido = stRetencao then
         ValorIssRetido := (ValorServicos * (Aliquota / 100));

      ValorIss := (ValorServicos * (Aliquota / 100));
    end;

    ValorLiquidoNfse := ValorServicos -
                        (ValorPis + ValorCofins + ValorInss + ValorIr +
                         ValorCsll + ValorDeducoes + DescontoCondicionado+
                         DescontoIncondicionado + ValorIssRetido);

    BaseCalculo := ValorLiquidoNfse;
  end;
end;

function TNFSeR_ISSDSF.LerXmlRps(const ANode: TACBrXmlNode): Boolean;
var
  aValor: string;
begin
  Result := True;

  with NFSe do
  begin
    with Prestador do
    begin
      RazaoSocial := ProcessarConteudo(ANode.Childrens.FindAnyNs('RazaoSocialPrestador'), tcStr);

      with IdentificacaoPrestador do
      begin
        InscricaoMunicipal := ProcessarConteudo(ANode.Childrens.FindAnyNs('InscricaoMunicipalPrestador'), tcStr);
      end;

      with Contato do
      begin
        aValor := ProcessarConteudo(ANode.Childrens.FindAnyNs('DDDPrestador'), tcStr);
        Telefone := aValor + ProcessarConteudo(ANode.Childrens.FindAnyNs('TelefonePrestador'), tcStr);
      end;
    end;

    with IdentificacaoRps do
    begin
      Serie := ProcessarConteudo(ANode.Childrens.FindAnyNs('SerieRPS'), tcStr);
      Numero := ProcessarConteudo(ANode.Childrens.FindAnyNs('NumeroRPS'), tcStr);
    end;

    DataEmissaoRps := ProcessarConteudo(ANode.Childrens.FindAnyNs('DataEmissaoRPS'), tcDatHor);
    aValor := ProcessarConteudo(ANode.Childrens.FindAnyNs('SituacaoRPS'), tcStr);

    if aValor = 'N' then
      Status := srNormal
    else
      Status := srCancelado;

    with RpsSubstituido do
    begin
      Serie := ProcessarConteudo(ANode.Childrens.FindAnyNs('SerieRPSSubstituido'), tcStr);
      Numero := ProcessarConteudo(ANode.Childrens.FindAnyNs('NumeroRPSSubstituido'), tcStr);
    end;

    SeriePrestacao := ProcessarConteudo(ANode.Childrens.FindAnyNs('SeriePrestacao'), tcStr);

    with Tomador do
    begin
      with IdentificacaoTomador do
      begin
        InscricaoMunicipal := ProcessarConteudo(ANode.Childrens.FindAnyNs('InscricaoMunicipalTomador'), tcStr);
        CpfCnpj := ProcessarConteudo(ANode.Childrens.FindAnyNs('CPFCNPJTomador'), tcStr);
        DocTomadorEstrangeiro := ProcessarConteudo(ANode.Childrens.FindAnyNs('DocTomadorEstrangeiro'), tcStr);
      end;

      RazaoSocial := ProcessarConteudo(ANode.Childrens.FindAnyNs('RazaoSocialTomador'), tcStr);

      with Endereco do
      begin
        TipoLogradouro := ProcessarConteudo(ANode.Childrens.FindAnyNs('TipoLogradouroTomador'), tcStr);
        Endereco := ProcessarConteudo(ANode.Childrens.FindAnyNs('LogradouroTomador'), tcStr);
        Numero := ProcessarConteudo(ANode.Childrens.FindAnyNs('NumeroEnderecoTomador'), tcStr);
        Complemento := ProcessarConteudo(ANode.Childrens.FindAnyNs('ComplementoEnderecoTomador'), tcStr);
        TipoBairro := ProcessarConteudo(ANode.Childrens.FindAnyNs('TipoBairroTomador'), tcStr);
        Bairro := ProcessarConteudo(ANode.Childrens.FindAnyNs('BairroTomador'), tcStr);
        CodigoMunicipio := ProcessarConteudo(ANode.Childrens.FindAnyNs('CidadeTomador'), tcStr);
        xMunicipio := ProcessarConteudo(ANode.Childrens.FindAnyNs('CidadeTomadorDescricao'), tcStr);
        CEP := ProcessarConteudo(ANode.Childrens.FindAnyNs('CEPTomador'), tcStr);
      end;

      with Contato do
      begin
        Email := ProcessarConteudo(ANode.Childrens.FindAnyNs('EmailTomador'), tcStr);
        aValor := ProcessarConteudo(ANode.Childrens.FindAnyNs('DDDTomador'), tcStr);
        Telefone := aValor + ProcessarConteudo(ANode.Childrens.FindAnyNs('TelefoneTomador'), tcStr);
      end;
    end;

    with Servico do
    begin
      CodigoCnae := ProcessarConteudo(ANode.Childrens.FindAnyNs('CodigoAtividade'), tcStr);
      CodigoMunicipio := ProcessarConteudo(ANode.Childrens.FindAnyNs('MunicipioPrestacao'), tcStr);
      Operacao := ProcessarConteudo(ANode.Childrens.FindAnyNs('Operacao'), tcStr);
      Tributacao := ProcessarConteudo(ANode.Childrens.FindAnyNs('Tributacao'), tcStr);

      with Valores do
      begin
        Aliquota := ProcessarConteudo(ANode.Childrens.FindAnyNs('AliquotaAtividade'), tcDe4);
        aValor := ProcessarConteudo(ANode.Childrens.FindAnyNs('TipoRecolhimento'), tcStr);

        if aValor = 'A' then
          IssRetido := stNormal
        else
          IssRetido := stRetencao;

        ValorPis := ProcessarConteudo(ANode.Childrens.FindAnyNs('ValorPIS'), tcDe2);
        ValorCofins := ProcessarConteudo(ANode.Childrens.FindAnyNs('ValorCOFINS'), tcDe2);
        ValorInss := ProcessarConteudo(ANode.Childrens.FindAnyNs('ValorINSS'), tcDe2);
        ValorIr := ProcessarConteudo(ANode.Childrens.FindAnyNs('ValorIR'), tcDe2);
        ValorCsll := ProcessarConteudo(ANode.Childrens.FindAnyNs('ValorCSLL'), tcDe2);
        AliquotaPIS := ProcessarConteudo(ANode.Childrens.FindAnyNs('AliquotaPIS'), tcDe4);
        AliquotaCOFINS := ProcessarConteudo(ANode.Childrens.FindAnyNs('AliquotaCOFINS'), tcDe4);
        AliquotaINSS := ProcessarConteudo(ANode.Childrens.FindAnyNs('AliquotaINSS'), tcDe4);
        AliquotaIR := ProcessarConteudo(ANode.Childrens.FindAnyNs('AliquotaIR'), tcDe4);
        AliquotaCSLL := ProcessarConteudo(ANode.Childrens.FindAnyNs('AliquotaCSLL'), tcDe4);
      end;
    end;

    OutrasInformacoes := ProcessarConteudo(ANode.Childrens.FindAnyNs('DescricaoRPS'), tcStr);
    MotivoCancelamento := ProcessarConteudo(ANode.Childrens.FindAnyNs('MotCancelamento'), tcStr);
    IntermediarioServico.CpfCnpj := ProcessarConteudo(ANode.Childrens.FindAnyNs('CPFCNPJIntermediario'), tcStr);

    LerDeducoes(ANode);
    LerItens(ANode);
  end;
end;

end.
