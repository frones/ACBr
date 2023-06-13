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

unit Infisc.LerXml;

interface

uses
  SysUtils, Classes, StrUtils, DateUtils,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXConversao, ACBrNFSeXLerXml,
  ACBrNFSeXLerXml_ABRASFv2;

type
  { Provedor com layout próprio }
  { TNFSeR_Infisc }

  TNFSeR_Infisc = class(TNFSeRClass)
  private

  protected

    // versão 1.0
    procedure LerId(const ANode: TACBrXmlNode);
    procedure LerEmitente(const ANode: TACBrXmlNode);
    procedure LerEnderecoEmitente(const ANode: TACBrXmlNode);
    procedure LerTomador(const ANode: TACBrXmlNode);
    procedure LerEnderecoTomador(const ANode: TACBrXmlNode);
    procedure LerServicos(const ANode: TACBrXmlNode);
    procedure LerTotal(const ANode: TACBrXmlNode);
    procedure LerFatura(const ANode: TACBrXmlNode);
    procedure LerISS(const ANode: TACBrXmlNode);
    procedure LerCobranca(const ANode: TACBrXmlNode);
    procedure LerObservacoes(const ANode: TACBrXmlNode);
    procedure LerReembolso(const ANode: TACBrXmlNode);
    procedure LerISSST(const ANode: TACBrXmlNode);
    // versão 1.1
    procedure LerDespesas(const ANode: TACBrXmlNode);
    procedure LerRetencao(const ANode: TACBrXmlNode);
    procedure LerTransportadora(const ANode: TACBrXmlNode);
    procedure LerFaturas(const ANode: TACBrXmlNode);
    procedure LerInformacoesAdic(const ANode: TACBrXmlNode);
  public
    function LerXml: Boolean; override;
    function LerXmlRps(const ANode: TACBrXmlNode): Boolean;
    function LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
  end;

  { TNFSeR_Infisc201 }

  TNFSeR_Infisc201 = class(TNFSeR_ABRASFv2)
  protected

  public

  end;

  { TNFSeR_Infisc203 }

  TNFSeR_Infisc203 = class(TNFSeR_ABRASFv2)
  protected

  public

  end;

implementation

uses
  ACBrUtil.Base, ACBrUtil.Strings,
  ACBrDFeUtil;

//==============================================================================
// Essa unit tem por finalidade exclusiva ler o XML do provedor:
//     Infisc
//==============================================================================

{ TNFSeR_Infisc }

procedure TNFSeR_Infisc.LerCobranca(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('cobr');

  if AuxNode <> nil then
  begin
    // Cobrança - Falta implementar
  end;
end;

procedure TNFSeR_Infisc.LerDespesas(const ANode: TACBrXmlNode);
var
  ANodes: TACBrXmlNodeArray;
  i: integer;
begin
  ANodes := ANode.Childrens.FindAllAnyNs('despesas');

  for i := 0 to Length(ANodes) - 1 do
  begin
    NFSe.Despesa.New;
    with NFSe.Despesa.Items[i] do
    begin
      nItemDesp := ObterConteudo(ANodes[i].Childrens.FindAnyNs('nItemDesp'), tcStr);
      xDesp     := ObterConteudo(ANodes[i].Childrens.FindAnyNs('xDesp'), tcStr);
      dDesp     := ObterConteudo(ANodes[i].Childrens.FindAnyNs('dDesp'), tcDat);
      vDesp     := ObterConteudo(ANodes[i].Childrens.FindAnyNs('vDesp'), tcDe2);
    end;
  end;
end;

procedure TNFSeR_Infisc.LerEmitente(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  aValor: string;
begin
  AuxNode := ANode.Childrens.FindAnyNs('emit');

  if AuxNode = nil then
    AuxNode := ANode.Childrens.FindAnyNs('prest');

  if AuxNode <> nil then
  begin
    with NFSe.Prestador do
    begin
      RazaoSocial  := ObterConteudo(AuxNode.Childrens.FindAnyNs('xNome'), tcStr);
      NomeFantasia := ObterConteudo(AuxNode.Childrens.FindAnyNs('xFant'), tcStr);

      with IdentificacaoPrestador do
      begin
        CpfCnpj := ObterConteudo(AuxNode.Childrens.FindAnyNs('CNPJ'), tcStr);
        InscricaoMunicipal := ObterConteudo(AuxNode.Childrens.FindAnyNs('IM'), tcStr);

        // versão 1.1
        InscricaoEstadual := ObterConteudo(AuxNode.Childrens.FindAnyNs('IE'), tcStr);
      end;
    end;

    LerEnderecoEmitente(AuxNode);

    // versao 1.1
    aValor := ObterConteudo(AuxNode.Childrens.FindAnyNs('regimeTrib'), tcStr);

    case StrToIntDef(aValor, 1) of
      1: NFSe.RegimeEspecialTributacao := retSimplesNacional;
      2: NFSe.RegimeEspecialTributacao := retMicroempresarioEmpresaPP;
    else
      NFSe.RegimeEspecialTributacao := retLucroReal;
    end;

    with NFSe.Prestador.Contato do
    begin
      Telefone := ObterConteudo(AuxNode.Childrens.FindAnyNs('fone'), tcStr);
      Email    := ObterConteudo(AuxNode.Childrens.FindAnyNs('xEmail'), tcStr);
      xSite    := ObterConteudo(AuxNode.Childrens.FindAnyNs('xSite'), tcStr);
    end;
  end;
end;

procedure TNFSeR_Infisc.LerEnderecoEmitente(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  xUF: string;
begin
  AuxNode := ANode.Childrens.FindAnyNs('end');

  if AuxNode <> nil then
  begin
    with NFSe.Prestador.Endereco do
    begin
      Endereco        := ObterConteudo(AuxNode.Childrens.FindAnyNs('xLgr'), tcStr);
      Numero          := ObterConteudo(AuxNode.Childrens.FindAnyNs('nro'), tcStr);
      Complemento     := ObterConteudo(AuxNode.Childrens.FindAnyNs('xCpl'), tcStr);
      Bairro          := ObterConteudo(AuxNode.Childrens.FindAnyNs('xBairro'), tcStr);
      CodigoMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('cMun'), tcStr);
      xMunicipio      := ObterConteudo(AuxNode.Childrens.FindAnyNs('xMun'), tcStr);
      UF              := ObterConteudo(AuxNode.Childrens.FindAnyNs('UF'), tcStr);
      CEP             := ObterConteudo(AuxNode.Childrens.FindAnyNs('CEP'), tcStr);

      if xMunicipio = '' then
      begin
        xMunicipio := ObterNomeMunicipio(StrToIntDef(CodigoMunicipio, 0), xUF, '', False);

        if UF = '' then
          UF := xUF;
      end;

      // versão 1.1
      CodigoPais := ObterConteudo(AuxNode.Childrens.FindAnyNs('cPais'), tcInt);
      xPais      := ObterConteudo(AuxNode.Childrens.FindAnyNs('xPais'), tcStr);
    end;

    with NFSe.Prestador.Contato do
    begin
      Telefone := ObterConteudo(AuxNode.Childrens.FindAnyNs('fone'), tcStr);
      Email    := ObterConteudo(AuxNode.Childrens.FindAnyNs('xEmail'), tcStr);
    end;
  end;
end;

procedure TNFSeR_Infisc.LerEnderecoTomador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  xUF: string;
begin
  AuxNode := ANode.Childrens.FindAnyNs('ender');

  if AuxNode <> nil then
  begin
    with NFSe.Tomador.Endereco do
    begin
      Endereco        := ObterConteudo(AuxNode.Childrens.FindAnyNs('xLgr'), tcStr);
      Numero          := ObterConteudo(AuxNode.Childrens.FindAnyNs('nro'), tcStr);
      Complemento     := ObterConteudo(AuxNode.Childrens.FindAnyNs('xCpl'), tcStr);
      Bairro          := ObterConteudo(AuxNode.Childrens.FindAnyNs('xBairro'), tcStr);
      CodigoMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('cMun'), tcStr);
      xMunicipio      := ObterConteudo(AuxNode.Childrens.FindAnyNs('xMun'), tcStr);
      UF              := ObterConteudo(AuxNode.Childrens.FindAnyNs('UF'), tcStr);
      CEP             := ObterConteudo(AuxNode.Childrens.FindAnyNs('CEP'), tcStr);

      if xMunicipio = '' then
      begin
        xMunicipio := ObterNomeMunicipio(StrToIntDef(CodigoMunicipio, 0), xUF, '', False);

        if UF = '' then
          UF := xUF;
      end;

      // versão 1.1
      CodigoPais := ObterConteudo(AuxNode.Childrens.FindAnyNs('cPais'), tcInt);
      xPais      := ObterConteudo(AuxNode.Childrens.FindAnyNs('xPais'), tcStr);
    end;

    with NFSe.Tomador.Contato do
    begin
      Telefone := ObterConteudo(AuxNode.Childrens.FindAnyNs('fone'), tcStr);
      Email    := ObterConteudo(AuxNode.Childrens.FindAnyNs('xEmail'), tcStr);
    end;
  end;
end;

procedure TNFSeR_Infisc.LerFatura(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('fat');

  if AuxNode <> nil then
  begin
    // Fatura - Falta implementar
  end;
end;

procedure TNFSeR_Infisc.LerFaturas(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  ANodes: TACBrXmlNodeArray;
  i: Integer;
begin
  AuxNode := ANode.Childrens.FindAnyNs('faturas');

  if AuxNode <> nil then
  begin
    ANodes := AuxNode.Childrens.FindAllAnyNs('fat');

    for i := 0 to Length(ANodes) - 1 do
    begin
      NFSe.CondicaoPagamento.Parcelas.New;
      with NFSe.CondicaoPagamento.Parcelas[i] do
      begin
        Parcela        := ObterConteudo(ANodes[i].Childrens.FindAnyNs('nFat'), tcStr);
        DataVencimento := ObterConteudo(ANodes[i].Childrens.FindAnyNs('dVenc'), tcDat);
        Valor          := ObterConteudo(ANodes[i].Childrens.FindAnyNs('vFat'), tcDe2);
      end;
    end;
  end;
end;

procedure TNFSeR_Infisc.LerId(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
  hEmi, aValor: string;
  Ano, Mes, Dia: Word;
  Hora, Minuto: Integer;
begin
  AuxNode := ANode.Childrens.FindAnyNs('Id');

  if AuxNode <> nil then
  begin
    with NFSe do
    begin
      cNFSe := ObterConteudo(AuxNode.Childrens.FindAnyNs('cNFS-e'), tcInt);
      CodigoVerificacao := IntToStr(cNFSe);
      NaturezaOperacao  := StrToNaturezaOperacao(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('natOp'), tcStr));
      SeriePrestacao    := ObterConteudo(AuxNode.Childrens.FindAnyNs('serie'), tcStr);
      Numero            := ObterConteudo(AuxNode.Childrens.FindAnyNs('nNFS-e'), tcStr);
      Competencia       := ObterConteudo(AuxNode.Childrens.FindAnyNs('dEmi'), tcDat);
      refNF             := ObterConteudo(AuxNode.Childrens.FindAnyNs('refNF'), tcStr);

      InfID.ID := OnlyNumber(CodigoVerificacao);

      hEmi   := ObterConteudo(AuxNode.Childrens.FindAnyNs('hEmi'), tcStr);
      Hora   := strToIntDef(Copy(hEmi, 1 , 2), 0);
      Minuto := strToIntDef(copy(hEmi, 4 , 2), 0);

      Ano := YearOf(Competencia);
      Mes := MonthOf(Competencia);
      Dia := DayOf(Competencia);

      DataEmissao := EncodeDateTime( ano, mes, dia, hora, minuto, 0, 0);

      aValor := ObterConteudo(AuxNode.Childrens.FindAnyNs('anulada'), tcStr);

      SituacaoNfse := StrToEnumerado(Ok, aValor, ['N','S'], [snNormal, snCancelado]);

      Servico.CodigoMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('cMunFG'), tcStr);

      Servico.MunicipioIncidencia := StrToIntDef(Servico.CodigoMunicipio, 0);
      // versão 1.1
      ModeloNFSe := ObterConteudo(AuxNode.Childrens.FindAnyNs('mod'), tcStr);

      aValor := ObterConteudo(AuxNode.Childrens.FindAnyNs('cancelada'), tcStr);

      SituacaoNfse := snNormal;
      if aValor = 'S' then
        SituacaoNfse := snCancelado;

      MotivoCancelamento := ObterConteudo(AuxNode.Childrens.FindAnyNs('motCanc'), tcStr);

      aValor := ObterConteudo(AuxNode.Childrens.FindAnyNs('ambienteEmi'), tcStr);

      if aValor = '1' then
        Producao := snSim
      else
        Producao := snNao;
    end;
  end;
end;

procedure TNFSeR_Infisc.LerInformacoesAdic(const ANode: TACBrXmlNode);
var
  ANodes: TACBrXmlNodeArray;
  i: Integer;
begin
  NFSe.OutrasInformacoes := '';

  ANodes := ANode.Childrens.FindAllAnyNs('infAdic');

  for i := 0 to Length(ANodes) - 1 do
  begin
    NFSe.OutrasInformacoes := NFSe.OutrasInformacoes + ANodes[i].Content;
//                  ObterConteudo(ANodes[i].Childrens.FindAnyNs('infAdic'), tcStr);
  end;
end;

procedure TNFSeR_Infisc.LerISS(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('ISS');

  if AuxNode <> nil then
  begin
    with NFSe.Servico.Valores do
    begin
      BaseCalculo    := ObterConteudo(AuxNode.Childrens.FindAnyNs('vBCISS'), tcDe2);
      ValorIss       := ObterConteudo(AuxNode.Childrens.FindAnyNs('vISS'), tcDe2);
      ValorIssRetido := ObterConteudo(AuxNode.Childrens.FindAnyNs('vSTISS'), tcDe2);

      if ValorIssRetido > 0 then
      begin
        IssRetido := stRetencao;
        NFSe.Servico.MunicipioIncidencia := StrToIntDef(NFSe.Tomador.Endereco.CodigoMunicipio, 0);
      end;
    end;
  end;
end;

procedure TNFSeR_Infisc.LerISSST(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('ISSST');

  if AuxNode <> nil then
  begin
    // ISS Substituição Tributária - Falta Implementar
  end;
end;

procedure TNFSeR_Infisc.LerObservacoes(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('Observacoes');

  if AuxNode <> nil then
    NFSe.OutrasInformacoes := ObterConteudo(AuxNode.Childrens.FindAnyNs('xInf'), tcStr);
end;

procedure TNFSeR_Infisc.LerReembolso(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('reemb');

  if AuxNode <> nil then
  begin
    // Reembolso - Falta Implementar
  end;
end;

procedure TNFSeR_Infisc.LerRetencao(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('Ret');

  if AuxNode <> nil then
  begin
    with NFSe.Servico.Valores do
    begin
      ValorIr     := ObterConteudo(AuxNode.Childrens.FindAnyNs('vRetIR'), tcDe2);
      ValorPis    := ObterConteudo(AuxNode.Childrens.FindAnyNs('vRetPISPASEP'), tcDe2);
      ValorCofins := ObterConteudo(AuxNode.Childrens.FindAnyNs('vRetCOFINS'), tcDe2);
      ValorCsll   := ObterConteudo(AuxNode.Childrens.FindAnyNs('vRetCSLL'), tcDe2);
      ValorInss   := ObterConteudo(AuxNode.Childrens.FindAnyNs('vRetINSS'), tcDe2);
    end;
  end;
end;

procedure TNFSeR_Infisc.LerServicos(const ANode: TACBrXmlNode);
var
  ANodes: TACBrXmlNodeArray;
  i: integer;
  AuxNode: TACBrXmlNode;
  AuxNodeItem: TACBrXmlNode;
begin
  NFSe.Servico.MunicipioIncidencia := 0;

  ANodes := ANode.Childrens.FindAllAnyNs('det');

  for i := 0 to Length(ANodes) - 1 do
  begin
    AuxNode := ANodes[i].Childrens.FindAnyNs('serv');

    if AuxNode <> nil then
    begin
      NFSe.Servico.ItemServico.New;
      with NFSe.Servico.ItemServico[i] do
      begin
        CodServ       := ObterConteudo(AuxNode.Childrens.FindAnyNs('cServ'), tcStr);
        Descricao     := ObterConteudo(AuxNode.Childrens.FindAnyNs('xServ'), tcStr);
        Descricao     := CodServ + ' - ' + Descricao;
        Quantidade    := ObterConteudo(AuxNode.Childrens.FindAnyNs('qTrib'), tcDe4);
        ValorUnitario := ObterConteudo(AuxNode.Childrens.FindAnyNs('vUnit'), tcDe3);
        ValorTotal    := ObterConteudo(AuxNode.Childrens.FindAnyNs('vServ'), tcDe2);

        DescontoIncondicionado := ObterConteudo(AuxNode.Childrens.FindAnyNs('vDesc'), tcDe2);

        Aliquota    := ObterConteudo(AuxNode.Childrens.FindAnyNs('pISS'), tcDe2);
        ValorISS    := ObterConteudo(AuxNode.Childrens.FindAnyNs('vISS'), tcDe2);
        BaseCalculo := ObterConteudo(AuxNode.Childrens.FindAnyNs('vBCISS'), tcDe2);
        ValorReducao := ObterConteudo(AuxNode.Childrens.FindAnyNs('vRed'), tcDe2);

        ValorIRRF := ObterConteudo(AuxNode.Childrens.FindAnyNs('vRetIR'), tcDe2);

        AliqRetIRRF := ObterConteudo(AuxNode.Childrens.FindAnyNs('pRetIR'), tcDe2);
        ValorBCRetIRRF := ObterConteudo(AuxNode.Childrens.FindAnyNs('vBCRetIR'), tcDe2);

        ValorPIS := ObterConteudo(AuxNode.Childrens.FindAnyNs('vRetLei10833-PIS-PASEP'), tcDe2);

        if ValorPIS = 0 then
          ValorPIS := ObterConteudo(AuxNode.Childrens.FindAnyNs('vRetPISPASEP'), tcDe2);

        AliqRetPIS := ObterConteudo(AuxNode.Childrens.FindAnyNs('pRetPISPASEP'), tcDe2);
        ValorBCPIS := ObterConteudo(AuxNode.Childrens.FindAnyNs('vBCPISPASEP'), tcDe2);

        ValorCOFINS := ObterConteudo(AuxNode.Childrens.FindAnyNs('vRetLei10833-COFINS'), tcDe2);

        if ValorCOFINS = 0 then
          ValorCOFINS := ObterConteudo(AuxNode.Childrens.FindAnyNs('vRetCOFINS'), tcDe2);

        AliqRetCOFINS := ObterConteudo(AuxNode.Childrens.FindAnyNs('pRetCOFINS'), tcDe2);
        ValorBCCOFINS := ObterConteudo(AuxNode.Childrens.FindAnyNs('vBCCOFINS'), tcDe2);

        ValorCSLL := ObterConteudo(AuxNode.Childrens.FindAnyNs('vRetLei10833-CSLL'), tcDe2);

        if ValorCSLL = 0 then
          ValorCSLL := ObterConteudo(AuxNode.Childrens.FindAnyNs('vRetCSLL'), tcDe2);

        AliqRetCSLL := ObterConteudo(AuxNode.Childrens.FindAnyNs('pRetCSLL'), tcDe2);
        ValorBCCSLL := ObterConteudo(AuxNode.Childrens.FindAnyNs('vBCCSLL'), tcDe2);

        ValorINSS := ObterConteudo(AuxNode.Childrens.FindAnyNs('vRetINSS'), tcDe2);
        AliqRetINSS := ObterConteudo(AuxNode.Childrens.FindAnyNs('pRetINSS'), tcDe2);
        ValorBCINSS := ObterConteudo(AuxNode.Childrens.FindAnyNs('vBCINSS'), tcDe2);

        // versão 1.1
        CodLCServ := ObterConteudo(AuxNode.Childrens.FindAnyNs('cLCServ'), tcStr);
        ValorTotal := ObterConteudo(AuxNode.Childrens.FindAnyNs('vServ'), tcDe3);

        AuxNodeItem := ANodes[i].Childrens.FindAnyNs('ISSST');

        if AuxNodeItem <> nil then
        begin
          AliqISSST := ObterConteudo(AuxNodeItem.Childrens.FindAnyNs('pISSST'), tcDe2);
          ValorISSST := ObterConteudo(AuxNodeItem.Childrens.FindAnyNs('vISSST'), tcDe2);
          BaseCalculo := ObterConteudo(AuxNodeItem.Childrens.FindAnyNs('vBCST'), tcDe2);
        end;
      end;

      // versão 1.1
      if NFSe.Servico.MunicipioIncidencia = 0 then
        NFSe.Servico.MunicipioIncidencia := ObterConteudo(AuxNode.Childrens.FindAnyNs('localTributacao'), tcStr);
    end;
  end;
end;

procedure TNFSeR_Infisc.LerTomador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('TomS');

  if AuxNode <> nil then
  begin
    with NFSe.Tomador do
    begin
      RazaoSocial := ObterConteudo(AuxNode.Childrens.FindAnyNs('xNome'), tcStr);

      with IdentificacaoTomador do
      begin
        CpfCnpj            := ObterCNPJCPF(AuxNode);
        InscricaoMunicipal := ObterConteudo(AuxNode.Childrens.FindAnyNs('IM'), tcStr);
        InscricaoEstadual  := ObterConteudo(AuxNode.Childrens.FindAnyNs('IE'), tcStr);
      end;
    end;

    LerEnderecoTomador(AuxNode);

    with NFSe.Tomador.Contato do
    begin
      Telefone := ObterConteudo(AuxNode.Childrens.FindAnyNs('fone'), tcStr);
      Email    := ObterConteudo(AuxNode.Childrens.FindAnyNs('xEmail'), tcStr);
    end;
  end;
end;

procedure TNFSeR_Infisc.LerTotal(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('total');

  if AuxNode <> nil then
  begin
    with NFSe.Servico.Valores do
    begin
      ValorServicos          := ObterConteudo(AuxNode.Childrens.FindAnyNs('vServ'), tcDe2);
      DescontoIncondicionado := ObterConteudo(AuxNode.Childrens.FindAnyNs('vDesc'), tcDe2);
      ValorLiquidoNfse       := ObterConteudo(AuxNode.Childrens.FindAnyNs('vtLiq'), tcDe2);

      // versão 1.1
      ValorDespesasNaoTributaveis := ObterConteudo(AuxNode.Childrens.FindAnyNs('vtDespesas'), tcDe2);
    end;

    LerFatura(AuxNode);
    LerISS(AuxNode);
    LerRetencao(AuxNode);
  end;
end;

procedure TNFSeR_Infisc.LerTransportadora(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
begin
  AuxNode := ANode.Childrens.FindAnyNs('transportadora');

  if AuxNode <> nil then
  begin
    with NFSe.Transportadora do
    begin
      xNomeTrans      := ObterConteudo(AuxNode.Childrens.FindAnyNs('xNomeTrans'), tcStr);
      xCpfCnpjTrans   := ObterConteudo(AuxNode.Childrens.FindAnyNs('xCpfCnpjTrans'), tcStr);
      xInscEstTrans   := ObterConteudo(AuxNode.Childrens.FindAnyNs('xInscEstTrans'), tcStr);
      xPlacaTrans     := ObterConteudo(AuxNode.Childrens.FindAnyNs('xPlacaTrans'), tcStr);
      xEndTrans       := ObterConteudo(AuxNode.Childrens.FindAnyNs('xEndTrans'), tcStr);
      cMunTrans       := ObterConteudo(AuxNode.Childrens.FindAnyNs('cMunTrans'), tcStr);
      xMunTrans       := ObterConteudo(AuxNode.Childrens.FindAnyNs('xMunTrans'), tcStr);
      xUFTrans        := ObterConteudo(AuxNode.Childrens.FindAnyNs('xUfTrans'), tcStr);
      cPaisTrans      := ObterConteudo(AuxNode.Childrens.FindAnyNs('cPaisTrans'), tcStr);
      xPaisTrans      := ObterConteudo(AuxNode.Childrens.FindAnyNs('xPaisTrans'), tcStr);
      vTipoFreteTrans := StrToTipoFrete(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('vTipoFreteTrans'), tcStr));
    end;
  end;
end;

function TNFSeR_Infisc.LerXml: Boolean;
var
  XmlNode: TACBrXmlNode;
begin
  if EstaVazio(Arquivo) then
    raise Exception.Create('Arquivo xml não carregado.');

  Arquivo := NormatizarXml(Arquivo);

  if FDocument = nil then
    FDocument := TACBrXmlDocument.Create();

  Document.Clear();
  Document.LoadFromXml(Arquivo);

  if (Pos('NFS-e', Arquivo) > 0) then
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

  FreeAndNil(FDocument);
end;

function TNFSeR_Infisc.LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
var
  AuxNode: TACBrXmlNode;
begin
  Result := True;

  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('NFS-e');

  if AuxNode = nil then
    AuxNode := ANode.Childrens.FindAnyNs('infNFSe');

  if AuxNode = nil then Exit;

  LerId(AuxNode);
  LerEmitente(AuxNode);
  LerTomador(AuxNode);
  LerServicos(AuxNode);
  LerTotal(AuxNode);
  LerCobranca(AuxNode);
  LerObservacoes(AuxNode);
  LerReembolso(AuxNode);
  LerISSST(AuxNode);

  // versão 1.1
  LerDespesas(AuxNode);
  LerTransportadora(AuxNode);
  LerFaturas(AuxNode);
  LerInformacoesAdic(AuxNode);

  LerCampoLink;
end;

function TNFSeR_Infisc.LerXmlRps(const ANode: TACBrXmlNode): Boolean;
begin
  Result := LerXmlNfse(ANode);
end;

end.
