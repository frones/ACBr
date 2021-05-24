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
{$IFDEF FPC}
  LResources, Controls, Graphics, Dialogs,
{$ENDIF}
  SysUtils, Classes, StrUtils, DateUtils,
  ACBrUtil,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXConversao, ACBrNFSeXLerXml,
  ACBrNFSeXLerXml_ABRASFv2;

type
  { Provedor com layout próprio }
  { TNFSeR_Infisc }

  TNFSeR_Infisc = class(TNFSeRClass)
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

  { TNFSeR_Infiscv2 }

  TNFSeR_Infiscv2 = class(TNFSeR_ABRASFv2)
  protected

  public

  end;

implementation

//==============================================================================
// Essa unit tem por finalidade exclusiva ler o XML do provedor:
//     Infisc
//==============================================================================

{ TNFSeR_Infisc }

procedure TNFSeR_Infisc.LerCobranca(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.Find('cobr');

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
  ANodes := ANode.Childrens.FindAll('despesas');

  for i := 0 to Length(ANodes) - 1 do
  begin
    NFSe.Despesa.New;
    with NFSe.Despesa.Items[i] do
    begin
      nItemDesp := ProcessarConteudo(ANodes[i].Childrens.Find('nItemDesp'), tcStr);
      xDesp     := ProcessarConteudo(ANodes[i].Childrens.Find('xDesp'), tcStr);
      dDesp     := ProcessarConteudo(ANodes[i].Childrens.Find('dDesp'), tcDat);
      vDesp     := ProcessarConteudo(ANodes[i].Childrens.Find('vDesp'), tcDe2);
    end;
  end;
end;

procedure TNFSeR_Infisc.LerEmitente(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  aValor: string;
begin
  AuxNode := ANode.Childrens.Find('emit');

  if AuxNode = nil then
    AuxNode := ANode.Childrens.Find('prest');

  if AuxNode <> nil then
  begin
    with NFSe.Prestador do
    begin
      RazaoSocial  := ProcessarConteudo(AuxNode.Childrens.Find('xNome'), tcStr);
      NomeFantasia := ProcessarConteudo(AuxNode.Childrens.Find('xFant'), tcStr);

      with IdentificacaoPrestador do
      begin
        Cnpj               := ProcessarConteudo(AuxNode.Childrens.Find('CNPJ'), tcStr);
        InscricaoMunicipal := ProcessarConteudo(AuxNode.Childrens.Find('IM'), tcStr);

        // versão 1.1
        InscricaoEstadual := ProcessarConteudo(AuxNode.Childrens.Find('IE'), tcStr);
      end;
    end;

    LerEnderecoEmitente(AuxNode);

    // versao 1.1
    aValor := ProcessarConteudo(AuxNode.Childrens.Find('regimeTrib'), tcStr);

    case StrToIntDef(aValor, 1) of
      1: NFSe.RegimeEspecialTributacao := retSimplesNacional;
      2: NFSe.RegimeEspecialTributacao := retMicroempresarioEmpresaPP;
    else
      NFSe.RegimeEspecialTributacao := retLucroReal;
    end;

    with NFSe.Prestador.Contato do
    begin
      Telefone := ProcessarConteudo(AuxNode.Childrens.Find('fone'), tcStr);
      Email    := ProcessarConteudo(AuxNode.Childrens.Find('xEmail'), tcStr);
    end;
  end;
end;

procedure TNFSeR_Infisc.LerEnderecoEmitente(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.Find('end');

  if AuxNode <> nil then
  begin
    with NFSe.Prestador.Endereco do
    begin
      Endereco        := ProcessarConteudo(AuxNode.Childrens.Find('xLgr'), tcStr);
      Numero          := ProcessarConteudo(AuxNode.Childrens.Find('nro'), tcStr);
      Complemento     := ProcessarConteudo(AuxNode.Childrens.Find('xCpl'), tcStr);
      Bairro          := ProcessarConteudo(AuxNode.Childrens.Find('xBairro'), tcStr);
      CodigoMunicipio := ProcessarConteudo(AuxNode.Childrens.Find('cMun'), tcStr);
      xMunicipio      := CodCidadeToCidade(StrToIntDef(CodigoMunicipio, 0));
      UF              := ProcessarConteudo(AuxNode.Childrens.Find('UF'), tcStr);
      CEP             := ProcessarConteudo(AuxNode.Childrens.Find('CEP'), tcStr);

      // versão 1.1
      CodigoPais := ProcessarConteudo(AuxNode.Childrens.Find('cPais'), tcInt);
      xPais      := ProcessarConteudo(AuxNode.Childrens.Find('xPais'), tcStr);
    end;

    with NFSe.Prestador.Contato do
    begin
      Telefone := ProcessarConteudo(AuxNode.Childrens.Find('fone'), tcStr);
      Email    := ProcessarConteudo(AuxNode.Childrens.Find('xEmail'), tcStr);
    end;
  end;
end;

procedure TNFSeR_Infisc.LerEnderecoTomador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.Find('ender');

  if AuxNode <> nil then
  begin
    with NFSe.Tomador.Endereco do
    begin
      Endereco        := ProcessarConteudo(AuxNode.Childrens.Find('xLgr'), tcStr);
      Numero          := ProcessarConteudo(AuxNode.Childrens.Find('nro'), tcStr);
      Complemento     := ProcessarConteudo(AuxNode.Childrens.Find('xCpl'), tcStr);
      Bairro          := ProcessarConteudo(AuxNode.Childrens.Find('xBairro'), tcStr);
      CodigoMunicipio := ProcessarConteudo(AuxNode.Childrens.Find('cMun'), tcStr);
      xMunicipio      := CodCidadeToCidade(StrToIntDef(CodigoMunicipio, 0));
      UF              := ProcessarConteudo(AuxNode.Childrens.Find('UF'), tcStr);
      CEP             := ProcessarConteudo(AuxNode.Childrens.Find('CEP'), tcStr);

      // versão 1.1
      CodigoPais := ProcessarConteudo(AuxNode.Childrens.Find('cPais'), tcInt);
      xPais      := ProcessarConteudo(AuxNode.Childrens.Find('xPais'), tcStr);
    end;

    with NFSe.Tomador.Contato do
    begin
      Telefone := ProcessarConteudo(AuxNode.Childrens.Find('fone'), tcStr);
      Email    := ProcessarConteudo(AuxNode.Childrens.Find('xEmail'), tcStr);
    end;
  end;
end;

procedure TNFSeR_Infisc.LerFatura(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.Find('fat');

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
  AuxNode := ANode.Childrens.Find('faturas');

  if AuxNode <> nil then
  begin
    ANodes := AuxNode.Childrens.FindAll('fat');

    for i := 0 to Length(ANodes) - 1 do
    begin
      NFSe.CondicaoPagamento.Parcelas.New;
      with NFSe.CondicaoPagamento.Parcelas[i] do
      begin
        Parcela        := ProcessarConteudo(ANodes[i].Childrens.Find('nFat'), tcStr);
        DataVencimento := ProcessarConteudo(ANodes[i].Childrens.Find('dVenc'), tcDat);
        Valor          := ProcessarConteudo(ANodes[i].Childrens.Find('vFat'), tcDe2);
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
  AuxNode := ANode.Childrens.Find('Id');

  if AuxNode <> nil then
  begin
    with NFSe do
    begin
      CodigoVerificacao := ProcessarConteudo(AuxNode.Childrens.Find('cNFS-e'), tcStr);
      NaturezaOperacao  := StrToNaturezaOperacao(Ok, ProcessarConteudo(AuxNode.Childrens.Find('natOp'), tcStr));
      SeriePrestacao    := ProcessarConteudo(AuxNode.Childrens.Find('serie'), tcStr);
      Numero            := ProcessarConteudo(AuxNode.Childrens.Find('nNFS-e'), tcStr);
      Competencia       := ProcessarConteudo(AuxNode.Childrens.Find('dEmi'), tcDat);
      refNF             := ProcessarConteudo(AuxNode.Childrens.Find('refNF'), tcStr);

      InfID.ID := OnlyNumber(CodigoVerificacao);

      hEmi   := ProcessarConteudo(AuxNode.Childrens.Find('hEmi'), tcStr);
      Hora   := strToInt(Copy(hEmi, 1 , 2));
      Minuto := strToInt(copy(hEmi, 4 , 2));

      Ano := YearOf(Competencia);
      Mes := MonthOf(Competencia);
      Dia := DayOf(Competencia);

      DataEmissao := EncodeDateTime( ano, mes, dia, hora, minuto, 0, 0);

      aValor := ProcessarConteudo(AuxNode.Childrens.Find('anulada'), tcStr);

      Status := StrToEnumerado(Ok, aValor, ['N','S'], [srNormal, srCancelado]);

      Servico.CodigoMunicipio := ProcessarConteudo(AuxNode.Childrens.Find('cMunFG'), tcStr);

      Servico.MunicipioIncidencia := StrToIntDef(Servico.CodigoMunicipio, 0);
      // versão 1.1
      ModeloNFSe := ProcessarConteudo(AuxNode.Childrens.Find('mod'), tcStr);

      aValor := ProcessarConteudo(AuxNode.Childrens.Find('cancelada'), tcStr);
      Cancelada := StrToSimNaoInFisc(Ok, aValor);

      MotivoCancelamento := ProcessarConteudo(AuxNode.Childrens.Find('motCanc'), tcStr);

      aValor := ProcessarConteudo(AuxNode.Childrens.Find('ambienteEmi'), tcStr);
      Producao := StrToSimNaoInFisc(Ok, aValor);
    end;
  end;
end;

procedure TNFSeR_Infisc.LerInformacoesAdic(const ANode: TACBrXmlNode);
var
  ANodes: TACBrXmlNodeArray;
  i: Integer;
begin
  NFSe.OutrasInformacoes := '';

  ANodes := ANode.Childrens.FindAll('infAdic');

  for i := 0 to Length(ANodes) - 1 do
  begin
    NFSe.OutrasInformacoes := NFSe.OutrasInformacoes +
                  ProcessarConteudo(ANodes[i].Childrens.Find('infAdic'), tcStr);
  end;
end;

procedure TNFSeR_Infisc.LerISS(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.Find('ISS');

  if AuxNode <> nil then
  begin
    with NFSe.Servico.Valores do
    begin
      BaseCalculo    := ProcessarConteudo(AuxNode.Childrens.Find('vBCISS'), tcDe2);
      ValorIss       := ProcessarConteudo(AuxNode.Childrens.Find('vISS'), tcDe2);
      ValorIssRetido := ProcessarConteudo(AuxNode.Childrens.Find('vSTISS'), tcDe2);

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
  AuxNode := ANode.Childrens.Find('ISSST');

  if AuxNode <> nil then
  begin
    // ISS Substituição Tributária - Falta Implementar
  end;
end;

procedure TNFSeR_Infisc.LerObservacoes(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.Find('Observacoes');

  if AuxNode <> nil then
    NFSe.OutrasInformacoes := ProcessarConteudo(AuxNode.Childrens.Find('xInf'), tcStr);
end;

procedure TNFSeR_Infisc.LerReembolso(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.Find('reemb');

  if AuxNode <> nil then
  begin
    // Reembolso - Falta Implementar
  end;
end;

procedure TNFSeR_Infisc.LerRetencao(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.Find('Ret');

  if AuxNode <> nil then
  begin
    with NFSe.Servico.Valores do
    begin
      ValorIr     := ProcessarConteudo(AuxNode.Childrens.Find('vRetIR'), tcDe2);
      ValorPis    := ProcessarConteudo(AuxNode.Childrens.Find('vRetPISPASEP'), tcDe2);
      ValorCofins := ProcessarConteudo(AuxNode.Childrens.Find('vRetCOFINS'), tcDe2);
      ValorCsll   := ProcessarConteudo(AuxNode.Childrens.Find('vRetCSLL'), tcDe2);
      ValorInss   := ProcessarConteudo(AuxNode.Childrens.Find('vRetINSS'), tcDe2);
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

  ANodes := ANode.Childrens.FindAll('det');

  for i := 0 to Length(ANodes) - 1 do
  begin
    AuxNode := ANodes[i].Childrens.Find('serv');

    if AuxNode <> nil then
    begin
      NFSe.Servico.ItemServico.New;
      with NFSe.Servico.ItemServico[i] do
      begin
        CodServ       := ProcessarConteudo(AuxNode.Childrens.Find('cServ'), tcStr);
        Descricao     := ProcessarConteudo(AuxNode.Childrens.Find('xServ'), tcStr);
        Descricao     := CodServ + ' - ' + Descricao;
        Quantidade    := ProcessarConteudo(AuxNode.Childrens.Find('qTrib'), tcDe4);
        ValorUnitario := ProcessarConteudo(AuxNode.Childrens.Find('vUnit'), tcDe3);
        ValorTotal    := ProcessarConteudo(AuxNode.Childrens.Find('vServ'), tcDe2);

        DescontoIncondicionado := ProcessarConteudo(AuxNode.Childrens.Find('vDesc'), tcDe2);

        Aliquota    := ProcessarConteudo(AuxNode.Childrens.Find('pISS'), tcDe2);
        ValorIss    := ProcessarConteudo(AuxNode.Childrens.Find('vISS'), tcDe2);
        BaseCalculo := ProcessarConteudo(AuxNode.Childrens.Find('vBCISS'), tcDe2);

        ValorIr  := ProcessarConteudo(AuxNode.Childrens.Find('vRetIRF'), tcDe2);
        ValorPis := ProcessarConteudo(AuxNode.Childrens.Find('vRetLei10833-PIS-PASEP'), tcDe2);

        if ValorPis = 0 then
          ValorPis := ProcessarConteudo(AuxNode.Childrens.Find('vRetPISPASEP'), tcDe2);

        ValorCofins := ProcessarConteudo(AuxNode.Childrens.Find('vRetLei10833-COFINS'), tcDe2);

        if ValorCofins = 0 then
          ValorCofins := ProcessarConteudo(AuxNode.Childrens.Find('vRetCOFINS'), tcDe2);

        ValorCsll := ProcessarConteudo(AuxNode.Childrens.Find('vRetLei10833-CSLL'), tcDe2);

        if ValorCsll = 0 then
          ValorCsll := ProcessarConteudo(AuxNode.Childrens.Find('vRetCSLL'), tcDe2);

        ValorInss := ProcessarConteudo(AuxNode.Childrens.Find('vRetINSS'), tcDe2);

        // versão 1.1
        CodLCServ := ProcessarConteudo(AuxNode.Childrens.Find('cLCServ'), tcStr);
        ValorTotal := ProcessarConteudo(AuxNode.Childrens.Find('vServ'), tcDe3);

        AuxNodeItem := AuxNode.Childrens.Find('ISSST');

        if AuxNodeItem <> nil then
        begin
          AliquotaISSST := ProcessarConteudo(AuxNode.Childrens.Find('pISSST'), tcDe2);
          ValorISSST    := ProcessarConteudo(AuxNode.Childrens.Find('vISSST'), tcDe2);
        end;
      end;

      // versão 1.1
      if NFSe.Servico.MunicipioIncidencia = 0 then
        NFSe.Servico.MunicipioIncidencia := ProcessarConteudo(AuxNode.Childrens.Find('localTributacao'), tcStr);
    end;
  end;
end;

procedure TNFSeR_Infisc.LerTomador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.Find('TomS');

  if AuxNode <> nil then
  begin
    with NFSe.Tomador do
    begin
      RazaoSocial := ProcessarConteudo(AuxNode.Childrens.Find('xNome'), tcStr);

      with IdentificacaoTomador do
      begin
        CpfCnpj            := ProcessarCNPJCPF(AuxNode);
        InscricaoMunicipal := ProcessarConteudo(AuxNode.Childrens.Find('IM'), tcStr);
        InscricaoEstadual  := ProcessarConteudo(AuxNode.Childrens.Find('IE'), tcStr);
      end;
    end;

    LerEnderecoTomador(AuxNode);

    with NFSe.Tomador.Contato do
    begin
      Telefone := ProcessarConteudo(AuxNode.Childrens.Find('fone'), tcStr);
      Email    := ProcessarConteudo(AuxNode.Childrens.Find('xEmail'), tcStr);
    end;
  end;
end;

procedure TNFSeR_Infisc.LerTotal(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.Find('total');

  if AuxNode <> nil then
  begin
    with NFSe.Servico.Valores do
    begin
      ValorServicos          := ProcessarConteudo(AuxNode.Childrens.Find('vServ'), tcDe2);
      DescontoIncondicionado := ProcessarConteudo(AuxNode.Childrens.Find('vDesc'), tcDe2);
      ValorLiquidoNfse       := ProcessarConteudo(AuxNode.Childrens.Find('vtLiq'), tcDe2);

      // versão 1.1
      ValorDespesasNaoTributaveis := ProcessarConteudo(AuxNode.Childrens.Find('vtDespesas'), tcDe2);
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
  AuxNode := ANode.Childrens.Find('transportadora');

  if AuxNode <> nil then
  begin
    with NFSe.Transportadora do
    begin
      xNomeTrans      := ProcessarConteudo(AuxNode.Childrens.Find('xNomeTrans'), tcStr);
      xCpfCnpjTrans   := ProcessarConteudo(AuxNode.Childrens.Find('xCpfCnpjTrans'), tcStr);
      xInscEstTrans   := ProcessarConteudo(AuxNode.Childrens.Find('xInscEstTrans'), tcStr);
      xPlacaTrans     := ProcessarConteudo(AuxNode.Childrens.Find('xPlacaTrans'), tcStr);
      xEndTrans       := ProcessarConteudo(AuxNode.Childrens.Find('xEndTrans'), tcStr);
      cMunTrans       := ProcessarConteudo(AuxNode.Childrens.Find('cMunTrans'), tcStr);
      xMunTrans       := ProcessarConteudo(AuxNode.Childrens.Find('xMunTrans'), tcStr);
      xUFTrans        := ProcessarConteudo(AuxNode.Childrens.Find('xUfTrans'), tcStr);
      cPaisTrans      := ProcessarConteudo(AuxNode.Childrens.Find('cPaisTrans'), tcStr);
      vTipoFreteTrans := StrToTipoFrete(Ok, ProcessarConteudo(AuxNode.Childrens.Find('vTipoFreteTrans'), tcStr));
    end;
  end;
end;

function TNFSeR_Infisc.LerXml: Boolean;
var
  XmlNode: TACBrXmlNode;
begin
  if EstaVazio(Arquivo) then
    raise Exception.Create('Arquivo xml não carregado.');

  Document.Clear();
  Document.LoadFromXml(Arquivo);

  XmlNode := Document.Root;

  if XmlNode = nil then
    raise Exception.Create('Arquivo xml vazio.');

  if tpXML = txmlNFSe then
    Result := LerXmlNfse(XmlNode)
  else
    Result := LerXmlRps(XmlNode);
end;

function TNFSeR_Infisc.LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
var
  AuxNode: TACBrXmlNode;
begin
  Result := True;

  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.Find('NFS-e');

  if AuxNode = nil then
    AuxNode := ANode.Childrens.Find('infNFSe');

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
end;

function TNFSeR_Infisc.LerXmlRps(const ANode: TACBrXmlNode): Boolean;
begin
  Result := LerXmlNfse(ANode);
end;

end.
