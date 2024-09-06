{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
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

unit ACBrMDFe.EnvEvento;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IfEnd}
  ACBrDFeConsts,
  pcnConversao,
  pmdfeConsts,
  pcnSignature,
  ACBrMDFe.EventoClass,
  ACBrBase,
  ACBrXmlBase,
  ACBrXmlWriter,
  ACBrXmlDocument;

type
  TInfEventoCollectionItem = class(TObject)
  private
    FInfEvento: TInfEvento;
    Fsignature: Tsignature;
    FRetInfEvento: TRetInfEvento;
    FXML: string;
  public
    constructor Create;
    destructor Destroy; override;

    property InfEvento: TInfEvento       read FInfEvento    write FInfEvento;
    property signature: Tsignature       read Fsignature    write Fsignature;
    property RetInfEvento: TRetInfEvento read FRetInfEvento write FRetInfEvento;
    property XML: string                 read FXML          write FXML;
  end;

  TInfEventoCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TInfEventoCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfEventoCollectionItem);
  public
    function Add: TInfEventoCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New.'{$EndIf};
    function New: TInfEventoCollectionItem;
    property Items[Index: Integer]: TInfEventoCollectionItem read GetItem write SetItem; default;
  end;

  { TEventoMDFe }

  TEventoMDFe = class(TACBrXmlWriter)
  private
    FidLote: Int64;
    FEvento: TInfEventoCollection;
    FVersao: string;
    FXmlEnvio: string;

    procedure SetEvento(const Value: TInfEventoCollection);

    function GetOpcoes: TACBrXmlWriterOptions;
    procedure SetOpcoes(const Value: TACBrXmlWriterOptions);
  protected
    function CreateOptions: TACBrXmlWriterOptions; override;

    function Gerar_InfEvento(Idx: Integer): TACBrXmlNode;
    function Gerar_DetEvento(Idx: Integer): TACBrXmlNode;
    function Gerar_Evento_Cancelamento(Idx: Integer): TACBrXmlNode;
    function Gerar_Evento_Encerramento(Idx: Integer): TACBrXmlNode;
    function Gerar_Evento_InclusaoCondutor(Idx: Integer): TACBrXmlNode;
    function Gerar_Condutor(const EventoItem: TInfEventoCollectionItem): TACBrXmlNode;

    function Gerar_Evento_InclusaoDFe(Idx: Integer): TACBrXmlNode;
    function Gerar_InfDoc(Idx: Integer): TACBrXmlNodeArray;

    function Gerar_Evento_PagamentoOperacao(Idx: Integer): TACBrXmlNode;
    function Gerar_InfViagens(const EventoItem: TInfEventoCollectionItem): TACBrXmlNode;
    function Gerar_InfPag(Idx: Integer): TACBrXmlNodeArray;
    function Gerar_Comp(Idx1, Idx2: Integer): TACBrXmlNodeArray;
    function Gerar_InfPrazo(Idx1, Idx2: Integer): TACBrXmlNodeArray;
    function Gerar_InfBanc(const EventoItem: TInfEventoCollectionItem; Idx: Integer): TACBrXmlNode;

    function Gerar_Evento_AlteracaoPagamento(Idx: Integer): TACBrXmlNode;
    function Gerar_Evento_ConfirmaServico(Idx: Integer): TACBrXmlNode;
  public
    constructor Create;
    destructor Destroy; override;

    function GerarXml: Boolean; Override;

    function LerXML(const CaminhoArquivo: string): Boolean;
    function LerXMLFromString(const AXML: string): Boolean;
    function ObterNomeArquivo(tpEvento: TpcnTpEvento): string;
    function LerFromIni(const AIniString: string): Boolean;

    property idLote: Int64                read FidLote  write FidLote;
    property Evento: TInfEventoCollection read FEvento  write SetEvento;
    property Versao: string               read FVersao  write FVersao;

    property Opcoes: TACBrXmlWriterOptions read GetOpcoes write SetOpcoes;

    property XmlEnvio: string read FXmlEnvio write FXmlEnvio;
  end;

implementation

uses
  IniFiles,
  ACBrDFeUtil,
  ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.DateTime, ACBrUtil.FilesIO,
  ACBrMDFe.RetEnvEvento,
  pmdfeMDFe,
  pmdfeConversaoMDFe;

{ TEventoMDFe }

constructor TEventoMDFe.Create;
begin
  inherited Create;

  FEvento := TInfEventoCollection.Create();
end;

function TEventoMDFe.CreateOptions: TACBrXmlWriterOptions;
begin
  Result := TACBrXmlWriterOptions.Create();
end;

destructor TEventoMDFe.Destroy;
begin
  FEvento.Free;

  inherited;
end;

function TEventoMDFe.ObterNomeArquivo(tpEvento: TpcnTpEvento): string;
var
  sChave: string;
begin
  sChave := Evento.Items[0].InfEvento.chMDFe;

  case tpEvento of
    teCancelamento:
      Result := sChave + '-can-eve.xml';

    teEncerramento:
      Result := sChave + '-ped-eve.xml';

    teInclusaoCondutor,
    teInclusaoDFe:
      Result := sChave + '-inc-eve.xml';

    tePagamentoOperacao:
      Result := sChave + '-pag-eve.xml';

    teAlteracaoPagtoServMDFe:
      Result := sChave + '-alt-eve.xml';

    teConfirmaServMDFe:
      Result := sChave + '-ser-eve.xml';
  else
    raise EventoException.Create('Obter nome do arquivo de Evento não Implementado!');
  end;
end;

function TEventoMDFe.GerarXML: Boolean;
var
  EventoNode: TACBrXmlNode;
  Ok: Boolean;
begin
//  VersaoDF := StrToVersaoCTe(Ok, Versao);
  ListaDeAlertas.Clear;

  FDocument.Clear();

  EventoNode := CreateElement('eventoMDFe');
  EventoNode.SetNamespace('http://www.portalfiscal.inf.br/mdfe');
  EventoNode.SetAttribute('versao', Versao);

  FDocument.Root := EventoNode;

  EventoNode.AppendChild(Gerar_InfEvento(0));

  // Incluir a assinatura no XML
  if Evento[0].signature.URI <> '' then
    EventoNode.AppendChild(GerarSignature(Evento[0].signature));

  Result := True;
  XmlEnvio := ChangeLineBreak(Document.Xml, '');
end;

function TEventoMDFe.Gerar_InfEvento(Idx: Integer): TACBrXmlNode;
var
  sDoc: string;
  Serie: Integer;
begin
  Result := CreateElement('infEvento');
  Result.SetAttribute('Id', Evento[Idx].InfEvento.id);

  Result.AppendChild(AddNode(tcInt, 'HP08', 'cOrgao', 1, 2, 1,
                                                Evento[Idx].FInfEvento.cOrgao));

  Result.AppendChild(AddNode(tcStr, 'HP09', 'tpAmb', 1, 1, 1,
                           TpAmbToStr(Evento[Idx].InfEvento.tpAmb), DSC_TPAMB));

  sDoc := OnlyNumber(Evento[Idx].InfEvento.CNPJCPF);

  if EstaVazio(sDoc) then
    sDoc := ExtrairCNPJCPFChaveAcesso(Evento[Idx].InfEvento.chMDFe);

  // Verifica a Série do Documento, caso esteja no intervalo de 910-969
  // o emitente é pessoa fisica, logo na chave temos um CPF.
  Serie := ExtrairSerieChaveAcesso(Evento[Idx].InfEvento.chMDFe);

  if (Length(sDoc) = 14) and (Serie >= 910) and (Serie <= 969) then
    sDoc := Copy(sDoc, 4, 11);

  if Length(sDoc) = 14 then
  begin
    Result.AppendChild(AddNode(tcStr, 'HP10', 'CNPJ', 14, 14, 1,
                                                              sDoc , DSC_CNPJ));

    if not ValidarCNPJ(sDoc) then
      wAlerta('HP10', 'CNPJ', DSC_CNPJ, ERR_MSG_INVALIDO);
  end
  else
  begin
    Result.AppendChild(AddNode(tcStr, 'HP11', 'CPF', 11, 11, 1,
                                                               sDoc , DSC_CPF));

    if not ValidarCPF(sDoc) then
      wAlerta('HP11', 'CPF', DSC_CPF, ERR_MSG_INVALIDO);
  end;

  Result.AppendChild(AddNode(tcStr, 'HP12', 'chMDFe', 44, 44, 1,
                                      Evento[Idx].FInfEvento.chMDFe, DSC_CHAVE));

  if not ValidarChave(Evento[Idx].InfEvento.chMDFe) then
    wAlerta('HP12', 'chMDFe', '', 'Chave de MDFe inválida');

  if Versao = '3.00' then
    Result.AppendChild(AddNode(tcStr, 'HP13', 'dhEvento', 1, 50, 1,
      FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', Evento[Idx].InfEvento.dhEvento)+
      GetUTC(CodigoUFparaUF(Evento[Idx].InfEvento.cOrgao),
      Evento[Idx].InfEvento.dhEvento)))
  else
    Result.AppendChild(AddNode(tcStr, 'HP13', 'dhEvento', 1, 25, 1,
      FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', Evento[Idx].InfEvento.dhEvento)));

  Result.AppendChild(AddNode(tcInt, 'HP14', 'tpEvento', 6, 6, 1,
                                            Evento[Idx].FInfEvento.TipoEvento));

  Result.AppendChild(AddNode(tcInt, 'HP15', 'nSeqEvento', 1, 2, 1,
                                            Evento[Idx].FInfEvento.nSeqEvento));

  Result.AppendChild(Gerar_DetEvento(0));
end;

function TEventoMDFe.Gerar_DetEvento(Idx: Integer): TACBrXmlNode;
begin
  Result := CreateElement('detEvento');
  Result.SetAttribute('versaoEvento', Versao);

  case Evento[Idx].InfEvento.tpEvento of
    teCancelamento: Result.AppendChild(Gerar_Evento_Cancelamento(Idx));

    teEncerramento: Result.AppendChild(Gerar_Evento_Encerramento(Idx));

    teInclusaoCondutor: Result.AppendChild(Gerar_Evento_InclusaoCondutor(Idx));

    teInclusaoDFe: Result.AppendChild(Gerar_Evento_InclusaoDFe(Idx));

    tePagamentoOperacao: Result.AppendChild(Gerar_Evento_PagamentoOperacao(Idx));

    teAlteracaoPagtoServMDFe: Result.AppendChild(Gerar_Evento_AlteracaoPagamento(Idx));

    teConfirmaServMDFe: Result.AppendChild(Gerar_Evento_ConfirmaServico(Idx));
  end;
end;

function TEventoMDFe.Gerar_Evento_Cancelamento(Idx: Integer): TACBrXmlNode;
begin
  Result := CreateElement('evCancMDFe');

  Result.AppendChild(AddNode(tcStr, 'EP02', 'descEvento', 4, 60, 1,
                                            Evento[Idx].FInfEvento.DescEvento));

  Result.AppendChild(AddNode(tcStr, 'EP03', 'nProt', 15, 15, 1,
                                       Evento[Idx].FInfEvento.detEvento.nProt));

  Result.AppendChild(AddNode(tcStr, 'EP04', 'xJust', 15, 255, 1,
                                       Evento[Idx].FInfEvento.detEvento.xJust));
end;

function TEventoMDFe.Gerar_Evento_Encerramento(Idx: Integer): TACBrXmlNode;
begin
  Result := CreateElement('evEncMDFe');

  Result.AppendChild(AddNode(tcStr, 'EP02', 'descEvento', 4, 60, 1,
                                            Evento[Idx].FInfEvento.DescEvento));

  Result.AppendChild(AddNode(tcStr, 'EP03', 'nProt', 15, 15, 1,
                                       Evento[Idx].FInfEvento.detEvento.nProt));

  Result.AppendChild(AddNode(tcDat, 'EP04', 'dtEnc', 10, 10, 1,
                                       Evento[Idx].FInfEvento.detEvento.dtEnc));

  Result.AppendChild(AddNode(tcInt, 'EP05', 'cUF', 2, 2, 1,
                                         Evento[Idx].FInfEvento.detEvento.cUF));

  Result.AppendChild(AddNode(tcInt, 'EP06', 'cMun', 7, 7, 1,
                                        Evento[Idx].FInfEvento.detEvento.cMun));

  if Evento.Items[0].InfEvento.detEvento.indEncPorTerceiro = tiSim  then
    Result.AppendChild(AddNode(tcStr, 'EP07', 'indEncPorTerceiro', 1, 1, 1, '1'));
end;

function TEventoMDFe.Gerar_Evento_InclusaoCondutor(Idx: Integer): TACBrXmlNode;
begin
  Result := CreateElement('evIncCondutorMDFe');

  Result.AppendChild(AddNode(tcStr, 'EP02', 'descEvento', 4, 60, 1,
                                            Evento[Idx].FInfEvento.DescEvento));

  Result.AppendChild(Gerar_Condutor(Evento[Idx]));
end;

function TEventoMDFe.Gerar_Condutor(
  const EventoItem: TInfEventoCollectionItem): TACBrXmlNode;
begin
  Result := CreateElement('condutor');

  Result.AppendChild(AddNode(tcStr, 'EP04', 'xNome', 1, 60, 1,
                                         EventoItem.InfEvento.detEvento.xNome));

  Result.AppendChild(AddNode(tcStr, 'EP05', 'CPF', 11, 11, 1,
                                           EventoItem.InfEvento.detEvento.CPF));
end;

function TEventoMDFe.Gerar_Evento_InclusaoDFe(Idx: Integer): TACBrXmlNode;
var
  nodeArray: TACBrXmlNodeArray;
  i: Integer;
begin
  Result := CreateElement('evIncDFeMDFe');

  Result.AppendChild(AddNode(tcStr, 'EP02', 'descEvento', 4, 60, 1,
                                            Evento[Idx].FInfEvento.DescEvento));

  Result.AppendChild(AddNode(tcStr, 'EP03', 'nProt', 15, 15, 1,
                                       Evento[Idx].FInfEvento.detEvento.nProt));

  Result.AppendChild(AddNode(tcInt, 'HP04', 'cMunCarrega', 7, 7, 1,
                                 Evento[Idx].FInfEvento.detEvento.cMunCarrega));

  Result.AppendChild(AddNode(tcStr, 'HP05', 'xMunCarrega', 2, 60, 1,
                                 Evento[Idx].FInfEvento.detEvento.xMunCarrega));

  nodeArray := Gerar_InfDoc(Idx);
  if nodeArray <> nil then
  begin
    for i := 0 to Length(nodeArray) - 1 do
    begin
      Result.AppendChild(nodeArray[i]);
    end;
  end;
end;

function TEventoMDFe.Gerar_InfDoc(Idx: Integer): TACBrXmlNodeArray;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, Evento[Idx].FInfEvento.detEvento.infDoc.Count);

  for i := 0 to Evento[Idx].FInfEvento.detEvento.infDoc.Count - 1 do
  begin
    Result[i] := CreateElement('infDoc');

    Result[i].AppendChild(AddNode(tcInt, 'HP07', 'cMunDescarga', 7, 7, 1,
                       Evento[Idx].InfEvento.detEvento.infDoc[i].cMunDescarga));

    Result[i].AppendChild(AddNode(tcStr, 'HP08', 'xMunDescarga', 2, 60, 1,
                       Evento[Idx].InfEvento.detEvento.infDoc[i].xMunDescarga));

    Result[i].AppendChild(AddNode(tcStr, 'HP09', 'chNFe', 44, 44, 1,
                              Evento[Idx].InfEvento.detEvento.infDoc[i].chNFe));
  end;

  if Evento[Idx].FInfEvento.detEvento.infDoc.Count > 990 then
    wAlerta('#1', 'infDoc', '', ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TEventoMDFe.Gerar_Evento_PagamentoOperacao(Idx: Integer): TACBrXmlNode;
var
  nodeArray: TACBrXmlNodeArray;
  i: Integer;
begin
  Result := CreateElement('evPagtoOperMDFe');

  Result.AppendChild(AddNode(tcStr, 'EP02', 'descEvento', 4, 60, 1,
                                            Evento[Idx].FInfEvento.DescEvento));

  Result.AppendChild(AddNode(tcStr, 'EP03', 'nProt', 15, 15, 1,
                                       Evento[Idx].FInfEvento.detEvento.nProt));

  Result.AppendChild(Gerar_InfViagens(Evento[Idx]));

  nodeArray := Gerar_InfPag(Idx);
  if nodeArray <> nil then
  begin
    for i := 0 to Length(nodeArray) - 1 do
    begin
      Result.AppendChild(nodeArray[i]);
    end;
  end;
end;

function TEventoMDFe.Gerar_Evento_ConfirmaServico(Idx: Integer): TACBrXmlNode;
begin
  Result := CreateElement('evConfirmaServMDFe');

  Result.AppendChild(AddNode(tcStr, 'EP02', 'descEvento', 4, 60, 1,
                                            Evento[Idx].FInfEvento.DescEvento));

  Result.AppendChild(AddNode(tcStr, 'EP03', 'nProt', 15, 15, 1,
                                       Evento[Idx].FInfEvento.detEvento.nProt));
end;

function TEventoMDFe.Gerar_InfViagens(
  const EventoItem: TInfEventoCollectionItem): TACBrXmlNode;
begin
  Result := CreateElement('infViagens');

  Result.AppendChild(AddNode(tcInt, 'EP04', 'qtdViagens', 5, 5, 1,
                         EventoItem.InfEvento.detEvento.infViagens.qtdViagens));

  Result.AppendChild(AddNode(tcInt, 'EP04', 'nroViagem', 5, 5, 1,
                          EventoItem.InfEvento.detEvento.infViagens.nroViagem));
end;

function TEventoMDFe.Gerar_InfPag(Idx: Integer): TACBrXmlNodeArray;
var
  i, j: integer;
  Item: TinfPagCollection;
  nodeArray: TACBrXmlNodeArray;
begin
  Result := nil;

  Item := Evento[Idx].FInfEvento.detEvento.infPag;
  SetLength(Result, Item.Count);

  for i := 0 to Item.Count - 1 do
  begin
    Result[i] := CreateElement('infPag');

    Result[i].AppendChild(AddNode(tcStr, 'HP07', 'xNome', 2, 60, 0,
                                                     Item[i].xNome, DSC_XNOME));

    if Item[i].idEstrangeiro <> '' then
      Result[i].AppendChild(AddNode(tcStr, 'HP07', 'idEstrangeiro', 2, 20, 0,
                                      Item[i].idEstrangeiro, DSC_IDESTRANGEIRO))
    else
      Result[i].AppendChild(AddNodeCNPJCPF('EP11', 'EP12', Item[i].CNPJCPF));

    nodeArray := Gerar_Comp(Idx, i);
    if nodeArray <> nil then
    begin
      for j := 0 to Length(nodeArray) - 1 do
      begin
        Result[i].AppendChild(nodeArray[j]);
      end;
    end;

    Result[i].AppendChild(AddNode(tcDe2, 'HP08', 'vContrato', 1, 15, 1,
                                             Item[i].vContrato, DSC_VCONTRATO));

    Result[i].AppendChild(AddNode(tcStr, 'HP09', 'indPag', 1, 1, 1,
                                     TIndPagToStr(Item[i].indPag), DSC_INDPAG));

    Result[i].AppendChild(AddNode(tcDe2, 'HP08', 'vAdiant', 1, 15, 1,
                                                 Item[i].vAdiant, DSC_VADIANT));

    if Item[i].indAntecipaAdiant = tiSim then
      Result[i].AppendChild(AddNode(tcStr, 'HP08', 'indAntecipaAdiant', 1, 1, 1, '1'));

    if Item[i].indPag = ipPrazo then
    begin
      nodeArray := Gerar_InfPrazo(Idx, i);
      if nodeArray <> nil then
      begin
        for j := 0 to Length(nodeArray) - 1 do
        begin
          Result[i].AppendChild(nodeArray[j]);
        end;
      end;
    end;

    Result[i].AppendChild(AddNode(tcStr, 'HP08', 'tpAntecip', 1, 1, 0,
                             tpAntecipToStr(Item[i].tpAntecip), DSC_TPANTECIP));

    Result[i].AppendChild(Gerar_InfBanc(Evento[Idx], i));
  end;

  if Item.Count > 990 then
    wAlerta('#1', 'infPag', '', ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TEventoMDFe.Gerar_Comp(Idx1, Idx2: Integer): TACBrXmlNodeArray;
var
  i: integer;
  Item: TCompCollection;
begin
  Result := nil;

  Item :=  Evento[Idx1].FInfEvento.detEvento.infPag[Idx2].Comp;
  SetLength(Result, Item.Count);

  for i := 0 to Item.Count - 1 do
  begin
    Result[i] := CreateElement('Comp');

    Result[i].AppendChild(AddNode(tcStr, 'HP07', 'tpComp', 2, 2, 1,
                                       TCompToStr(Item[i].tpComp), DSC_TPCOMP));

    Result[i].AppendChild(AddNode(tcDe2, 'HP08', 'vComp', 1, 15, 1,
                                                     Item[i].vComp, DSC_VCOMP));

    Result[i].AppendChild(AddNode(tcStr, 'HP09', 'xComp', 2, 60, 0,
                                                     Item[i].xComp, DSC_XCOMP));
  end;

  if Item.Count > 990 then
    wAlerta('#1', 'Comp', '', ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TEventoMDFe.Gerar_InfPrazo(Idx1, Idx2: Integer): TACBrXmlNodeArray;
var
  i: integer;
  Item: TInfPrazoCollection;
begin
  Result := nil;

  Item :=  Evento[Idx1].FInfEvento.detEvento.infPag[Idx2].infPrazo;
  SetLength(Result, Item.Count);

  for i := 0 to Item.Count - 1 do
  begin
    Result[i] := CreateElement('infPrazo');

    Result[i].AppendChild(AddNode(tcStr, 'HP07', 'nParcela', 3, 3, 1,
                           FormatFloat('000', Item[i].nParcela), DSC_NPARCELA));

    Result[i].AppendChild(AddNode(tcDat, 'HP08', 'dVenc', 10, 10, 1,
                                                     Item[i].dVenc, DSC_DVENC));

    Result[i].AppendChild(AddNode(tcDe2, 'HP09', 'vParcela', 1, 15, 1,
                                               Item[i].vParcela, DSC_VPARCELA));
  end;

  if Item.Count > 990 then
    wAlerta('#1', 'infPrazo', '', ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TEventoMDFe.Gerar_InfBanc(
  const EventoItem: TInfEventoCollectionItem; Idx: Integer): TACBrXmlNode;
var
  Item: TinfBanc;
begin
  Item := EventoItem.InfEvento.detEvento.infPag[Idx].infBanc;
  Result := CreateElement('infBanc');

  if Item.PIX <> '' then
    Result.AppendChild(AddNode(tcStr, 'HP19', 'PIX', 2, 60, 1, Item.PIX))
  else
  begin
    if Item.CNPJIPEF <> '' then
    Result.AppendChild(AddNode(tcStr, 'HP19', 'CNPJIPEF', 14, 14, 1,
                                                   Item.CNPJIPEF, DSC_CNPJIPEF))
    else
    begin
      Result.AppendChild(AddNode(tcStr, 'HP19', 'codBanco', 3, 5, 1,
                                                  Item.codBanco, DSC_CODBANCO));

      Result.AppendChild(AddNode(tcStr, 'HP19', 'codAgencia', 1, 10, 1,
                                              Item.codAgencia, DSC_CODAGENCIA));
    end;
  end;
end;

function TEventoMDFe.Gerar_Evento_AlteracaoPagamento(
  Idx: Integer): TACBrXmlNode;
var
  nodeArray: TACBrXmlNodeArray;
  i: Integer;
begin
  Result := CreateElement('evAlteracaoPagtoServMDFe');

  Result.AppendChild(AddNode(tcStr, 'EP02', 'descEvento', 4, 60, 1,
                                            Evento[Idx].FInfEvento.DescEvento));

  Result.AppendChild(AddNode(tcStr, 'EP03', 'nProt', 15, 15, 1,
                                       Evento[Idx].FInfEvento.detEvento.nProt));

  nodeArray := Gerar_InfPag(Idx);
  if nodeArray <> nil then
  begin
    for i := 0 to Length(nodeArray) - 1 do
    begin
      Result.AppendChild(nodeArray[i]);
    end;
  end;
end;

function TEventoMDFe.GetOpcoes: TACBrXmlWriterOptions;
begin
  Result := TACBrXmlWriterOptions(FOpcoes);
end;

procedure TEventoMDFe.SetEvento(const Value: TInfEventoCollection);
begin
  FEvento.Assign(Value);
end;

procedure TEventoMDFe.SetOpcoes(const Value: TACBrXmlWriterOptions);
begin
  FOpcoes := Value;
end;

function TEventoMDFe.LerXML(const CaminhoArquivo: string): Boolean;
var
  ArqEvento: TStringList;
begin
  ArqEvento := TStringList.Create;

  try
    ArqEvento.LoadFromFile(CaminhoArquivo);
    Result := LerXMLFromString(ArqEvento.Text);
  finally
    ArqEvento.Free;
  end;
end;

function TEventoMDFe.LerXMLFromString(const AXML: string): Boolean;
var
  RetEventoMDFe: TRetEventoMDFe;
  i, j: Integer;
begin
  RetEventoMDFe := TRetEventoMDFe.Create;

  try
    RetEventoMDFe.XmlRetorno := AXML;
    Result := RetEventoMDFe.LerXml;

    with FEvento.New do
    begin
      XML                  := AXML;
      infEvento.Id         := RetEventoMDFe.InfEvento.Id;
      InfEvento.cOrgao     := RetEventoMDFe.InfEvento.cOrgao;
      infEvento.tpAmb      := RetEventoMDFe.InfEvento.tpAmb;
      infEvento.CNPJCPF    := RetEventoMDFe.InfEvento.CNPJCPF;
      infEvento.chMDFe     := RetEventoMDFe.InfEvento.chMDFe;
      infEvento.dhEvento   := RetEventoMDFe.InfEvento.dhEvento;
      infEvento.tpEvento   := RetEventoMDFe.InfEvento.tpEvento;
      infEvento.nSeqEvento := RetEventoMDFe.InfEvento.nSeqEvento;

      infEvento.VersaoEvento         := RetEventoMDFe.InfEvento.VersaoEvento;
      infEvento.detEvento.descEvento := RetEventoMDFe.InfEvento.detEvento.descEvento;
      infEvento.detEvento.nProt      := RetEventoMDFe.InfEvento.detEvento.nProt;
      infEvento.detEvento.dtEnc      := RetEventoMDFe.InfEvento.detEvento.dtEnc;
      infEvento.detEvento.cUF        := RetEventoMDFe.InfEvento.detEvento.cUF;
      infEvento.detEvento.cMun       := RetEventoMDFe.InfEvento.detEvento.cMun;
      infEvento.detEvento.xJust      := RetEventoMDFe.InfEvento.detEvento.xJust;
      infEvento.detEvento.xNome      := RetEventoMDFe.InfEvento.detEvento.xNome;
      infEvento.detEvento.CPF        := RetEventoMDFe.InfEvento.detEvento.CPF;

      infEvento.detEvento.indEncPorTerceiro := RetEventoMDFe.InfEvento.detEvento.indEncPorTerceiro;

      infEvento.detEvento.cMunCarrega := RetEventoMDFe.InfEvento.detEvento.cMunCarrega;
      infEvento.detEvento.xMunCarrega := RetEventoMDFe.InfEvento.detEvento.xMunCarrega;

      InfEvento.detEvento.infViagens.qtdViagens := RetEventoMDFe.InfEvento.detEvento.infViagens.qtdViagens;
      InfEvento.detEvento.infViagens.nroViagem := RetEventoMDFe.InfEvento.detEvento.infViagens.nroViagem;

      signature.URI             := RetEventoMDFe.signature.URI;
      signature.DigestValue     := RetEventoMDFe.signature.DigestValue;
      signature.SignatureValue  := RetEventoMDFe.signature.SignatureValue;
      signature.X509Certificate := RetEventoMDFe.signature.X509Certificate;

      if RetEventoMDFe.retEvento.Count > 0 then
      begin
        FRetInfEvento.Id          := RetEventoMDFe.retEvento.Items[0].RetInfEvento.Id;
        FRetInfEvento.tpAmb       := RetEventoMDFe.retEvento.Items[0].RetInfEvento.tpAmb;
        FRetInfEvento.verAplic    := RetEventoMDFe.retEvento.Items[0].RetInfEvento.verAplic;
        FRetInfEvento.cOrgao      := RetEventoMDFe.retEvento.Items[0].RetInfEvento.cOrgao;
        FRetInfEvento.cStat       := RetEventoMDFe.retEvento.Items[0].RetInfEvento.cStat;
        FRetInfEvento.xMotivo     := RetEventoMDFe.retEvento.Items[0].RetInfEvento.xMotivo;
        FRetInfEvento.chMDFe      := RetEventoMDFe.retEvento.Items[0].RetInfEvento.chMDFe;
        FRetInfEvento.tpEvento    := RetEventoMDFe.retEvento.Items[0].RetInfEvento.tpEvento;
        FRetInfEvento.xEvento     := RetEventoMDFe.retEvento.Items[0].RetInfEvento.xEvento;
        FRetInfEvento.nSeqEvento  := RetEventoMDFe.retEvento.Items[0].RetInfEvento.nSeqEvento;
        FRetInfEvento.CNPJDest    := RetEventoMDFe.retEvento.Items[0].RetInfEvento.CNPJDest;
        FRetInfEvento.emailDest   := RetEventoMDFe.retEvento.Items[0].RetInfEvento.emailDest;
        FRetInfEvento.dhRegEvento := RetEventoMDFe.retEvento.Items[0].RetInfEvento.dhRegEvento;
        FRetInfEvento.nProt       := RetEventoMDFe.retEvento.Items[0].RetInfEvento.nProt;
        FRetInfEvento.XML         := RetEventoMDFe.retEvento.Items[0].RetInfEvento.XML;
      end;

      for i := 0 to RetEventoMDFe.InfEvento.detEvento.infDoc.Count -1 do
      begin
        infEvento.detEvento.infDoc.New;

        infEvento.detEvento.infDoc[i].cMunDescarga := RetEventoMDFe.InfEvento.detEvento.infDoc[i].cMunDescarga;
        infEvento.detEvento.infDoc[i].xMunDescarga := RetEventoMDFe.InfEvento.detEvento.infDoc[i].xMunDescarga;
        infEvento.detEvento.infDoc[i].chNFe        := RetEventoMDFe.InfEvento.detEvento.infDoc[i].chNFe;
      end;

      for i := 0 to RetEventoMDFe.InfEvento.detEvento.infPag.Count - 1 do
      begin
        infEvento.detEvento.infPag.New;

        InfEvento.detEvento.infPag[i].xNome         := RetEventoMDFe.InfEvento.detEvento.infPag[i].xNome;
        InfEvento.detEvento.infPag[i].idEstrangeiro := RetEventoMDFe.InfEvento.detEvento.infPag[i].idEstrangeiro;
        InfEvento.detEvento.infPag[i].CNPJCPF       := RetEventoMDFe.InfEvento.detEvento.infPag[i].CNPJCPF;

        for j := 0 to RetEventoMDFe.InfEvento.detEvento.infPag[i].Comp.Count - 1 do
        begin
          InfEvento.detEvento.infPag[i].Comp.New;

          InfEvento.detEvento.infPag[i].Comp[j].tpComp := RetEventoMDFe.InfEvento.detEvento.infPag[i].Comp[j].tpComp;
          InfEvento.detEvento.infPag[i].Comp[j].vComp  := RetEventoMDFe.InfEvento.detEvento.infPag[i].Comp[j].vComp;
          InfEvento.detEvento.infPag[i].Comp[j].xComp  := RetEventoMDFe.InfEvento.detEvento.infPag[i].Comp[j].xComp;
        end;

        InfEvento.detEvento.infPag[i].vContrato := RetEventoMDFe.InfEvento.detEvento.infPag[i].vContrato;
        InfEvento.detEvento.infPag[i].indPag    := RetEventoMDFe.InfEvento.detEvento.infPag[i].indPag;
        InfEvento.detEvento.infPag[i].vAdiant   := RetEventoMDFe.InfEvento.detEvento.infPag[i].vAdiant;

        InfEvento.detEvento.infPag[i].indAntecipaAdiant := RetEventoMDFe.InfEvento.detEvento.infPag[i].indAntecipaAdiant;
        InfEvento.detEvento.infPag[i].tpAntecip := RetEventoMDFe.InfEvento.detEvento.infPag[i].tpAntecip;

        if InfEvento.detEvento.infPag[i].indPag = ipPrazo then
        begin
          for j := 0 to RetEventoMDFe.InfEvento.detEvento.infPag[i].infPrazo.Count - 1 do
          begin
            InfEvento.detEvento.infPag[i].infPrazo.New;

            InfEvento.detEvento.infPag[i].infPrazo[j].nParcela := RetEventoMDFe.InfEvento.detEvento.infPag[i].infPrazo[j].nParcela;
            InfEvento.detEvento.infPag[i].infPrazo[j].dVenc    := RetEventoMDFe.InfEvento.detEvento.infPag[i].infPrazo[j].dVenc;
            InfEvento.detEvento.infPag[i].infPrazo[j].vParcela := RetEventoMDFe.InfEvento.detEvento.infPag[i].infPrazo[j].vParcela;
          end;
        end;

        InfEvento.detEvento.infPag[i].infBanc.PIX        := RetEventoMDFe.InfEvento.detEvento.infPag[i].infBanc.PIX;
        InfEvento.detEvento.infPag[i].infBanc.CNPJIPEF   := RetEventoMDFe.InfEvento.detEvento.infPag[i].infBanc.CNPJIPEF;
        InfEvento.detEvento.infPag[i].infBanc.codBanco   := RetEventoMDFe.InfEvento.detEvento.infPag[i].infBanc.codBanco;
        InfEvento.detEvento.infPag[i].infBanc.codAgencia := RetEventoMDFe.InfEvento.detEvento.infPag[i].infBanc.codAgencia;
      end;
    end;
  finally
    RetEventoMDFe.Free;
  end;
end;

function TEventoMDFe.LerFromIni(const AIniString: string): Boolean;
var
  I, J, K: Integer;
  sSecao, sFim: String;
  INIRec: TMemIniFile;
  Ok: Boolean;
  ItemInfDoc: TInfDocCollectionItem;
  ItemInfPag: TinfPagCollectionItem;
  ItemComp: TCompCollectionItem;
  ItemInfPrazo: TInfPrazoCollectionItem;
begin
  Self.Evento.Clear;

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);

    idLote := INIRec.ReadInteger('EVENTO', 'idLote', 0);

    I := 1;
    while true do
    begin
      sSecao := 'EVENTO'+IntToStrZero(I,3);
      sFim   := INIRec.ReadString(sSecao, 'chMDFe', 'FIM');

      if (sFim = 'FIM') or (Length(sFim) <= 0) then
        break;

      with Self.Evento.New do
      begin
        infEvento.chMDFe     := INIRec.ReadString(sSecao, 'chMDFe', '');
        infEvento.cOrgao     := INIRec.ReadInteger(sSecao, 'cOrgao', 0);
        infEvento.CNPJCPF    := INIRec.ReadString(sSecao, 'CNPJCPF', '');
        infEvento.dhEvento   := StringToDateTime(INIRec.ReadString(sSecao, 'dhEvento', ''));
        infEvento.tpEvento   := StrToTpEventoMDFe(Ok, INIRec.ReadString(sSecao, 'tpEvento', ''));
        infEvento.nSeqEvento := INIRec.ReadInteger(sSecao, 'nSeqEvento', 1);

        // Usado no detalhamento do evento
        infEvento.detEvento.xJust := INIRec.ReadString(sSecao, 'xJust', '');
        infEvento.detEvento.nProt := INIRec.ReadString(sSecao, 'nProt', '');
        InfEvento.detEvento.dtEnc := StringToDateTime(INIRec.ReadString(sSecao, 'dtEnc', ''));
        InfEvento.detEvento.cUF   := INIRec.ReadInteger(sSecao, 'cUF', 0);
        InfEvento.detEvento.cMun  := INIRec.ReadInteger(sSecao, 'cMun', 0);
        infEvento.detEvento.xNome := INIRec.ReadString(sSecao, 'xNome', '');
        infEvento.detEvento.CPF   := INIRec.ReadString(sSecao, 'CPF', '');

        infEvento.detEvento.indEncPorTerceiro := StrToTIndicador(Ok, INIRec.ReadString(sSecao, 'indEncPorTerceiro', '0'));

        infEvento.detEvento.cMunCarrega := INIRec.ReadInteger(sSecao, 'cMunCarrega', 0);
        infEvento.detEvento.xMunCarrega := INIRec.ReadString(sSecao, 'xMunCarrega', '');

        Self.Evento.Items[I-1].InfEvento.detEvento.infDoc.Clear;

        J := 1;
        while true do
        begin
          // J varia de 0000 até 2000
          sSecao := 'infDoc' + IntToStrZero(J, 4);
          sFim   := INIRec.ReadString(sSecao, 'chNFe', 'FIM');
          if (sFim = 'FIM') or (Length(sFim) <= 0) then
            break;

          ItemInfDoc := Self.Evento.Items[I-1].InfEvento.detEvento.infDoc.New;

          ItemInfDoc.cMunDescarga := INIRec.ReadInteger(sSecao, 'cMunDescarga', 0);
          ItemInfDoc.xMunDescarga := INIRec.ReadString(sSecao, 'xMunDescarga', '');
          ItemInfDoc.chNFe        := sFim;

          Inc(J);
        end;

        sSecao := 'infViagens';

        if INIRec.SectionExists(sSecao) then
        begin
          Self.Evento.Items[I-1].InfEvento.detEvento.infViagens.qtdViagens := INIRec.ReadInteger(sSecao, 'qtdViagens', 0);
          Self.Evento.Items[I-1].InfEvento.detEvento.infViagens.nroViagem  := INIRec.ReadInteger(sSecao, 'nroViagem', 0);
        end;

        sSecao := 'infPag001';

        if INIRec.SectionExists(sSecao) then
        begin
          Self.Evento.Items[I-1].InfEvento.detEvento.infPag.Clear;

          J := 1;
          while true do
          begin
            sSecao := 'infPag' + IntToStrZero(J, 3);
            sFim   := INIRec.ReadString(sSecao, 'CNPJCPF', INIRec.ReadString(sSecao, 'idEstrangeiro', 'FIM'));

            if sFim = 'FIM' then
              break;

            ItemInfPag := Self.Evento.Items[I-1].InfEvento.detEvento.infPag.New;

            ItemInfPag.xNome         := INIRec.ReadString(sSecao, 'xNome', '');
            ItemInfPag.idEstrangeiro := INIRec.ReadString(sSecao, 'idEstrangeiro', '');

            if ItemInfPag.idEstrangeiro = '' then
              ItemInfPag.CNPJCPF := INIRec.ReadString(sSecao, 'CNPJCPF', '');

            ItemInfPag.vContrato := StringToFloatDef(INIRec.ReadString(sSecao, 'vContrato', ''), 0 );
            ItemInfPag.indPag    := StrToTIndPag(ok, INIRec.ReadString(sSecao, 'indPag', '0'));
            ItemInfPag.vAdiant   := StringToFloatDef(INIRec.ReadString(sSecao, 'vAdiant', ''), 0 );

            ItemInfPag.indAntecipaAdiant := StrToTIndicador(ok, INIRec.ReadString(sSecao, 'indAntecipaAdiant', '0'));
            ItemInfPag.tpAntecip := StrTotpAntecip(ok, INIRec.ReadString(sSecao, 'tpAntecip', ''));

            K := 1;
            while true do
            begin
              sSecao := 'Comp' + IntToStrZero(J, 3) + IntToStrZero(K, 3);
              sFim   := INIRec.ReadString(sSecao, 'vComp', 'FIM');

              if sFim = 'FIM' then
                break;

              ItemComp := ItemInfPag.Comp.New;

              ItemComp.tpComp := StrToTComp(ok, INIRec.ReadString(sSecao, 'tpComp', '01'));
              ItemComp.vComp  := StringToFloatDef(INIRec.ReadString(sSecao, 'vComp', ''), 0 );
              ItemComp.xComp  := INIRec.ReadString(sSecao, 'xComp', '');

              Inc(K);
            end;

            if ItemInfPag.indPag = ipPrazo then
            begin
              K := 1;
              while true do
              begin
                sSecao := 'infPrazo' + IntToStrZero(J, 3) + IntToStrZero(K, 3);
                sFim   := INIRec.ReadString(sSecao, 'vParcela', 'FIM');

                if sFim = 'FIM' then
                  break;

                ItemInfPrazo := ItemInfPag.infPrazo.New;

                ItemInfPrazo.nParcela := INIRec.ReadInteger(sSecao, 'nParcela', 1);
                ItemInfPrazo.dVenc    := StringToDateTime(INIRec.ReadString(sSecao, 'dVenc', '0'));
                ItemInfPrazo.vParcela := StringToFloatDef(INIRec.ReadString(sSecao, 'vParcela', ''), 0 );

                Inc(K);
              end;
            end;

            sSecao := 'infBanc' + IntToStrZero(J, 3);

            if INIRec.SectionExists(sSecao) then
            begin
              ItemInfPag.infBanc.PIX := INIRec.ReadString(sSecao, 'PIX', '');

              if ItemInfPag.infBanc.PIX = '' then
              begin
                ItemInfPag.infBanc.CNPJIPEF := INIRec.ReadString(sSecao, 'CNPJIPEF', '');

                if ItemInfPag.infBanc.CNPJIPEF = '' then
                begin
                  ItemInfPag.infBanc.codBanco   := INIRec.ReadString(sSecao, 'codBanco', '');
                  ItemInfPag.infBanc.codAgencia := INIRec.ReadString(sSecao, 'codAgencia', '');
                end;
              end;
            end;

            Inc(J);
          end;
        end;
      end;

      Inc(I);
    end;
  finally
    INIRec.Free;
  end;

  Result := True;
end;

{ TInfEventoCollection }

function TInfEventoCollection.Add: TInfEventoCollectionItem;
begin
  Result := Self.New;
end;

function TInfEventoCollection.GetItem(
  Index: Integer): TInfEventoCollectionItem;
begin
  Result := TInfEventoCollectionItem(inherited Items[Index]);
end;

procedure TInfEventoCollection.SetItem(Index: Integer;
  Value: TInfEventoCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TInfEventoCollection.New: TInfEventoCollectionItem;
begin
  Result := TInfEventoCollectionItem.Create;
  Self.Add(Result);
end;

{ TInfEventoCollectionItem }

constructor TInfEventoCollectionItem.Create;
begin
  inherited Create;

  FInfEvento := TInfEvento.Create;
  Fsignature := Tsignature.Create;
  FRetInfEvento := TRetInfEvento.Create;
end;

destructor TInfEventoCollectionItem.Destroy;
begin
  FInfEvento.Free;
  fsignature.Free;
  FRetInfEvento.Free;

  inherited;
end;

end.

