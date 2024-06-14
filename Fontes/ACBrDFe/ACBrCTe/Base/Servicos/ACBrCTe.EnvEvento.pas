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

unit ACBrCTe.EnvEvento;

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
  pcteConsts,
  pcteConversaoCTe,
  pcnSignature,
  ACBrCTe.EventoClass,
  ACBrBase,
  ACBrXmlBase,
  ACBrXmlWriter,
  ACBrXmlDocument;

type
  EventoCTeException = class(Exception);

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

  { TEventoCTe }

  TEventoCTe = class(TACBrXmlWriter)
  private
    FidLote: Int64;
    FEvento: TInfEventoCollection;
    FVersao: string;
    FXmlEnvio: string;
    FVersaoDF: TVersaoCTe;

    procedure SetEvento(const Value: TInfEventoCollection);

    function GetOpcoes: TACBrXmlWriterOptions;
    procedure SetOpcoes(const Value: TACBrXmlWriterOptions);
  protected
    function CreateOptions: TACBrXmlWriterOptions; override;

    function Gerar_InfEvento(Idx: Integer): TACBrXmlNode;
    function Gerar_Evento: TACBrXmlNodeArray;
    function Gerar_DetEvento(Idx: Integer): TACBrXmlNode;
    function Gerar_Evento_CCe(Idx: Integer): TACBrXmlNode;
    function Gerar_InfCorrecao(Idx: Integer): TACBrXmlNodeArray;
    function Gerar_Evento_Cancelamento(Idx: Integer): TACBrXmlNode;
    function Gerar_Evento_EPEC(Idx: Integer): TACBrXmlNode;
    function Gerar_EPECTomador(const EventoItem: TInfEventoCollectionItem): TACBrXmlNode;
    function Gerar_Evento_MultiModal(Idx: Integer): TACBrXmlNode;
    function Gerar_Evento_PrestacaoDesacordo(Idx: Integer): TACBrXmlNode;
    function Gerar_Evento_CancPrestacaoDesacordo(Idx: Integer): TACBrXmlNode;
    function Gerar_Evento_GTV(Idx: Integer): TACBrXmlNode;
    function Gerar_InfGTV(Idx: Integer): TACBrXmlNodeArray;
    function Gerar_InfEspecie(IdxEv, IdxEs: Integer): TACBrXmlNodeArray;
    function Gerar_Remetente(IdxEv, IdxEs: Integer): TACBrXmlNode;
    function Gerar_Destinatario(IdxEv, IdxEs: Integer): TACBrXmlNode;

    function Gerar_Evento_ComprEntrega(Idx: Integer): TACBrXmlNode;
    function Gerar_InfEntrega(Idx: Integer): TACBrXmlNodeArray;
    function Gerar_Evento_CancComprEntrega(Idx: Integer): TACBrXmlNode;
    function Gerar_Evento_InsucessoEntrega(Idx: Integer): TACBrXmlNode;
    function Gerar_Evento_CancInsucessoEntrega(Idx: Integer): TACBrXmlNode;
  public
    constructor Create;
    destructor Destroy; override;

    function GerarXml: Boolean; Override;

    function LerXML(const CaminhoArquivo: string): Boolean;
    function LerXMLFromString(const AXML: string): Boolean;
    function ObterNomeArquivo(tpEvento: TpcnTpEvento): string;
    function LerFromIni(const AIniString: string; CCe: Boolean = True): Boolean;

    property idLote: Int64                read FidLote   write FidLote;
    property Evento: TInfEventoCollection read FEvento   write SetEvento;
    property Versao: string               read FVersao   write FVersao;
    property XmlEnvio: string             read FXmlEnvio write FXmlEnvio;
    property VersaoDF: TVersaoCTe         read FVersaoDF write FVersaoDF;
  end;

implementation

uses
  IniFiles,
  ACBrDFeUtil,
  ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.DateTime, ACBrUtil.FilesIO,
  ACBrCTe.RetEnvEvento;

{ TEventoCTe }

constructor TEventoCTe.Create;
begin
  inherited Create;

  FEvento := TInfEventoCollection.Create();
end;

function TEventoCTe.CreateOptions: TACBrXmlWriterOptions;
begin
  Result := TACBrXmlWriterOptions.Create();
end;

destructor TEventoCTe.Destroy;
begin
  FEvento.Free;

  inherited;
end;

function TEventoCTe.ObterNomeArquivo(tpEvento: TpcnTpEvento): string;
begin
  case tpEvento of
    teCCe: Result := IntToStr(Self.idLote) + '-cce.xml';

    teCancelamento,
    teCancSubst: Result := IntToStr(Self.idLote) + '-can-eve.xml';

    teManifDestCiencia,
    teManifDestConfirmacao,
    teManifDestDesconhecimento,
    teManifDestOperNaoRealizada: Result := IntToStr(Self.idLote) + '-man-des.xml';

    teEPEC: Result := Evento[0].InfEvento.chCTe + '-ped-epec.xml';

    tePedProrrog1,
    tePedProrrog2: Result := Evento[0].InfEvento.chCTe + '-ped-prorr.xml';

    teCanPedProrrog1,
    teCanPedProrrog2: Result := Evento[0].InfEvento.chCTe + '-can-prorr.xml';

    teComprEntregaCTe: Result := Evento[0].InfEvento.chCTe + '-comp-entr.xml';

    teCancComprEntregaCTe: Result := Evento[0].InfEvento.chCTe + '-can-entr.xml';
  else
    raise EventoException.Create('Obter nome do arquivo de Evento não Implementado!');
  end;
end;

function TEventoCTe.GerarXML: Boolean;
var
  EventoNode: TACBrXmlNode;
  nodeArray: TACBrXmlNodeArray;
  i: Integer;
begin
  ListaDeAlertas.Clear;

  FDocument.Clear();

  EventoNode := CreateElement('envEvento');
  EventoNode.SetNamespace('http://www.portalfiscal.inf.br/cte');
  EventoNode.SetAttribute('versao', Versao);

  FDocument.Root := EventoNode;

  EventoNode.AppendChild(AddNode(tcInt64, '#1', 'idLote', 1, 15, 1,
                                                          FidLote, DSC_IDLOTE));

  nodeArray := Gerar_Evento;
  if nodeArray <> nil then
  begin
    for i := 0 to Length(nodeArray) - 1 do
    begin
      EventoNode.AppendChild(nodeArray[i]);
    end;
  end;

  Result := True;
  XmlEnvio := ChangeLineBreak(Document.Xml, '');
end;

function TEventoCTe.Gerar_Evento: TACBrXmlNodeArray;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, Evento.Count);

  for i := 0 to Evento.Count - 1 do
  begin
    Evento[i].InfEvento.id := 'ID' + Evento[i].InfEvento.TipoEvento +
                               OnlyNumber(Evento[i].InfEvento.chCTe) +
                               Format('%.2d', [Evento[i].InfEvento.nSeqEvento]);

    if Length(Evento[i].InfEvento.id) < 54 then
      wAlerta('HP07', 'ID', '', 'ID de Evento inválido');

    Result[i] := CreateElement('evento');
    Result[i].SetNamespace('http://www.portalfiscal.inf.br/cte');
    Result[i].SetAttribute('versao', Versao);

    Result[i].AppendChild(Gerar_InfEvento(i));

    // Incluir a assinatura no XML
    if Evento[i].signature.URI <> '' then
      Result[i].AppendChild(GerarSignature(Evento[i].signature));
  end;

  if Evento.Count > 20 then
    wAlerta('#1', 'evento', '', ERR_MSG_MAIOR_MAXIMO + '20');
end;

function TEventoCTe.Gerar_Evento_CCe(Idx: Integer): TACBrXmlNode;
var
  nodeArray: TACBrXmlNodeArray;
  i: Integer;
begin
  Result := CreateElement('evCCeCTe');

  Result.AppendChild(AddNode(tcStr, 'HP19', 'descEvento', 4, 60, 1,
                                            Evento[Idx].FInfEvento.DescEvento));

  nodeArray := Gerar_InfCorrecao(Idx);
  if nodeArray <> nil then
  begin
    for i := 0 to Length(nodeArray) - 1 do
    begin
      Result.AppendChild(nodeArray[i]);
    end;
  end;

  Result.AppendChild(AddNode(tcStr, 'HP20a', 'xCondUso', 1, 5000, 1,
                                    Evento[Idx].FInfEvento.detEvento.xCondUso));
end;

function TEventoCTe.Gerar_InfCorrecao(Idx: Integer): TACBrXmlNodeArray;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, Evento[Idx].FInfEvento.detEvento.infCorrecao.Count);

  for i := 0 to Evento[Idx].FInfEvento.detEvento.infCorrecao.Count - 1 do
  begin
    Result[i] := CreateElement('infCorrecao');

    Result[i].AppendChild(AddNode(tcStr, 'EP04', 'grupoAlterado', 1, 20, 1,
                Evento[Idx].FInfEvento.detEvento.infCorrecao[i].grupoAlterado));

    Result[i].AppendChild(AddNode(tcStr, 'EP05', 'campoAlterado', 1, 20, 1,
                Evento[Idx].FInfEvento.detEvento.infCorrecao[i].campoAlterado));

    Result[i].AppendChild(AddNode(tcStr, 'EP06', 'valorAlterado', 1, 500, 1,
                Evento[Idx].FInfEvento.detEvento.infCorrecao[i].valorAlterado));

    Result[i].AppendChild(AddNode(tcInt, 'EP07', 'nroItemAlterado', 2, 2, 0,
              Evento[Idx].FInfEvento.detEvento.infCorrecao[i].nroItemAlterado));
  end;

  if Evento[Idx].FInfEvento.detEvento.infCorrecao.Count > 990 then
    wAlerta('#1', 'infCorrecao', '', ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TEventoCTe.Gerar_Evento_Cancelamento(Idx: Integer): TACBrXmlNode;
begin
  Result := CreateElement('evCancCTe');

  Result.AppendChild(AddNode(tcStr, 'EP02', 'descEvento', 4, 60, 1,
                                            Evento[Idx].FInfEvento.DescEvento));

  Result.AppendChild(AddNode(tcStr, 'EP03', 'nProt', 15, 15, 1,
                                       Evento[Idx].FInfEvento.detEvento.nProt));

  Result.AppendChild(AddNode(tcStr, 'EP04', 'xJust', 15, 255, 1,
                                       Evento[Idx].FInfEvento.detEvento.xJust));

end;

function TEventoCTe.Gerar_Evento_EPEC(Idx: Integer): TACBrXmlNode;
begin
  Result := CreateElement('evEPECCTe');

  Result.AppendChild(AddNode(tcStr, 'EP02', 'descEvento', 4, 60, 1,
                                            Evento[Idx].FInfEvento.DescEvento));

  Result.AppendChild(AddNode(tcStr, 'EP04', 'xJust', 15, 255, 1,
                                       Evento[Idx].FInfEvento.detEvento.xJust));

  Result.AppendChild(AddNode(tcDe2, 'EP05', 'vICMS', 1, 15, 1,
                            Evento[Idx].FInfEvento.detEvento.vICMS, DSC_VICMS));

  Result.AppendChild(AddNode(tcDe2, 'EP05', 'vICMSST', 1, 15, 1,
                          Evento[Idx].FInfEvento.detEvento.vICMSST, DSC_VICMS));

  Result.AppendChild(AddNode(tcDe2, 'EP06', 'vTPrest', 1, 15, 1,
                        Evento[Idx].FInfEvento.detEvento.vTPrest, DSC_VTPREST));

  Result.AppendChild(AddNode(tcDe2, 'EP07', 'vCarga', 1, 15, 1,
                          Evento[Idx].FInfEvento.detEvento.vCarga, DSC_VTMERC));

  Result.AppendChild(Gerar_EPECTomador(Evento[Idx]));

  Result.AppendChild(AddNode(tcStr, 'EP14', 'modal', 2, 2, 1,
              TpModalToStr(Evento[Idx].FInfEvento.detEvento.modal), DSC_MODAL));

  Result.AppendChild(AddNode(tcStr, 'EP15', 'UFIni', 2, 2, 1,
                               Evento[Idx].FInfEvento.detEvento.UFIni, DSC_UF));

  if not ValidarUF(Evento[Idx].InfEvento.detEvento.UFIni) then
    wAlerta('EP15', 'UFIni', DSC_UF, ERR_MSG_INVALIDO);

  Result.AppendChild(AddNode(tcStr, 'EP16', 'UFFim', 2, 2, 1,
                               Evento[Idx].FInfEvento.detEvento.UFFim, DSC_UF));

  if not ValidarUF(Evento[Idx].InfEvento.detEvento.UFFim) then
    wAlerta('EP16', 'UFFim', DSC_UF, ERR_MSG_INVALIDO);

  if VersaoDF >= ve300 then
  begin
    // Segundo o Manual página 104 devemos informar o valor "0" para tpCTe
    Result.AppendChild(AddNode(tcStr, 'EP17', 'tpCTe', 1, 1, 1, '0', DSC_TPCTE));

    Result.AppendChild(AddNode(tcStr, 'EP18', 'dhEmi', 25, 25, 1,
         DateTimeTodh(Evento[Idx].InfEvento.detEvento.dhEmi) +
                      GetUTC(Evento[Idx].InfEvento.detEvento.UF,
                      Evento[Idx].InfEvento.detEvento.dhEmi), DSC_DEMI));
  end;
end;

function TEventoCTe.Gerar_EPECTomador(const EventoItem: TInfEventoCollectionItem): TACBrXmlNode;
begin
  if VersaoDF >= ve300 then
    Result := CreateElement('toma4')
  else
    Result := CreateElement('toma04');

  Result.AppendChild(AddNode(tcStr, 'EP09', 'toma', 1, 1, 1,
                TpTomadorToStr(EventoItem.InfEvento.detEvento.toma), DSC_TOMA));

  Result.AppendChild(AddNode(tcStr, 'EP10', 'UF', 2, 2, 1,
                                    EventoItem.InfEvento.detEvento.UF, DSC_UF));

  if not ValidarUF(EventoItem.InfEvento.detEvento.UF) then
    wAlerta('EP10', 'UF', DSC_UF, ERR_MSG_INVALIDO);

  Result.AppendChild(AddNodeCNPJCPF('EP11', 'EP12',
                                       EventoItem.InfEvento.detEvento.CNPJCPF));

  if EventoItem.InfEvento.detEvento.IE <> '' then
  begin
    if Trim(EventoItem.InfEvento.detEvento.IE) = 'ISENTO' then
      Result.AppendChild(AddNode(tcStr, 'EP13', 'IE', 0, 14, 0,
                                     EventoItem.InfEvento.detEvento.IE, DSC_IE))
    else
      Result.AppendChild(AddNode(tcStr, 'EP13', 'IE', 0, 14, 0,
                        OnlyNumber(EventoItem.InfEvento.detEvento.IE), DSC_IE));

    if not ValidarIE(EventoItem.InfEvento.detEvento.IE, EventoItem.InfEvento.detEvento.UF) then
      wAlerta('EP13', 'IE', DSC_IE, ERR_MSG_INVALIDO);
  end;
end;

function TEventoCTe.Gerar_Evento_MultiModal(Idx: Integer): TACBrXmlNode;
begin
  Result := CreateElement('evRegMultimodal');

  Result.AppendChild(AddNode(tcStr, 'HP19', 'descEvento', 4, 60, 1,
                                            Evento[Idx].FInfEvento.DescEvento));

  Result.AppendChild(AddNode(tcStr, 'EP03', 'xRegistro', 15, 1000, 1,
                                   Evento[Idx].FInfEvento.detEvento.xRegistro));

  Result.AppendChild(AddNode(tcStr, 'EP04', 'nDoc', 1, 43, 0,
                                        Evento[Idx].FInfEvento.detEvento.nDoc));
end;

function TEventoCTe.Gerar_Evento_PrestacaoDesacordo(Idx: Integer): TACBrXmlNode;
begin
  Result := CreateElement('evPrestDesacordo');

  Result.AppendChild(AddNode(tcStr, 'EP02', 'descEvento', 4, 60, 1,
                                            Evento[Idx].FInfEvento.DescEvento));

  Result.AppendChild(AddNode(tcStr, 'EP03', 'indDesacordoOper', 1, 1, 1, '1'));

  Result.AppendChild(AddNode(tcStr, 'EP04', 'xObs', 15, 255, 1,
                                        Evento[Idx].FInfEvento.detEvento.xObs));
end;

function TEventoCTe.Gerar_Evento_CancPrestacaoDesacordo(
  Idx: Integer): TACBrXmlNode;
begin
  Result := CreateElement('evCancPrestDesacordo');

  Result.AppendChild(AddNode(tcStr, 'EP02', 'descEvento', 4, 60, 1,
                                            Evento[Idx].FInfEvento.DescEvento));

  Result.AppendChild(AddNode(tcStr, 'EP03', 'nProtEvPrestDes', 1, 15, 1,
                                       Evento[Idx].FInfEvento.detEvento.nProt));
end;

function TEventoCTe.Gerar_Evento_GTV(Idx: Integer): TACBrXmlNode;
var
  nodeArray: TACBrXmlNodeArray;
  i: Integer;
begin
  Result := CreateElement('evGTV');

  Result.AppendChild(AddNode(tcStr, 'EP02', 'descEvento', 4, 60, 1,
                                            Evento[Idx].FInfEvento.DescEvento));

  nodeArray := Gerar_InfGTV(Idx);
  if nodeArray <> nil then
  begin
    for i := 0 to Length(nodeArray) - 1 do
    begin
      Result.AppendChild(nodeArray[i]);
    end;
  end;
end;

function TEventoCTe.Gerar_InfGTV(Idx: Integer): TACBrXmlNodeArray;
var
  nodeArray: TACBrXmlNodeArray;
  i, j: Integer;
begin
  Result := nil;
  SetLength(Result, Evento[Idx].FInfEvento.detEvento.infGTV.Count);

  for i := 0 to Evento[Idx].FInfEvento.detEvento.infGTV.Count - 1 do
  begin
    Result[i] := CreateElement('infGTV');

    Result[i].AppendChild(AddNode(tcStr, 'EP04', 'nDoc', 20, 20, 1,
                               Evento[Idx].InfEvento.detEvento.infGTV[i].nDoc));

    Result[i].AppendChild(AddNode(tcStr, 'EP05', 'id', 20, 20, 1,
                                 Evento[Idx].InfEvento.detEvento.infGTV[i].id));

    Result[i].AppendChild(AddNode(tcStr, 'EP06', 'serie', 3, 3, 0,
                              Evento[Idx].InfEvento.detEvento.infGTV[i].serie));

    Result[i].AppendChild(AddNode(tcStr, 'EP07', 'subserie', 3, 3, 0,
                           Evento[Idx].InfEvento.detEvento.infGTV[i].subserie));

    Result[i].AppendChild(AddNode(tcDat, 'EP08', 'dEmi', 10, 10, 1,
                               Evento[Idx].InfEvento.detEvento.infGTV[i].dEmi));

    Result[i].AppendChild(AddNode(tcInt, 'EP09', 'nDV', 1, 1, 1,
                                Evento[Idx].InfEvento.detEvento.infGTV[i].nDV));

    Result[i].AppendChild(AddNode(tcDe4, 'EP10', 'qCarga', 1, 15, 1,
                             Evento[Idx].InfEvento.detEvento.infGTV[i].qCarga));

    nodeArray := Gerar_InfEspecie(Idx, i);
    if nodeArray <> nil then
    begin
      for j := 0 to Length(nodeArray) - 1 do
      begin
        Result[i].AppendChild(nodeArray[j]);
      end;
    end;

    Result[i].AppendChild(Gerar_Remetente(Idx, i));

    Result[i].AppendChild(Gerar_Destinatario(Idx, i));

    Result[i].AppendChild(AddNode(tcStr, 'EP26', 'placa', 7, 7, 0,
                              Evento[Idx].InfEvento.detEvento.infGTV[i].placa));

    Result[i].AppendChild(AddNode(tcStr, 'EP27', 'UF', 2, 2, 0,
                                 Evento[Idx].InfEvento.detEvento.infGTV[i].UF));

    Result[i].AppendChild(AddNode(tcStr, 'EP28', 'RNTRC', 6, 8, 0,
                              Evento[Idx].InfEvento.detEvento.infGTV[i].RNTRC));
  end;

  if Evento[Idx].FInfEvento.detEvento.infGTV.Count > 990 then
    wAlerta('#1', 'infGTV', '', ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TEventoCTe.Gerar_InfEspecie(IdxEv, IdxEs: Integer): TACBrXmlNodeArray;
var
  nodeArray: TACBrXmlNodeArray;
  i: Integer;
begin
  Result := nil;
  SetLength(Result, Evento[IdxEv].FInfEvento.detEvento.infGTV[IdxEs].infEspecie.Count);

  for i := 0 to Evento[IdxEv].FInfEvento.detEvento.infGTV[IdxEs].infEspecie.Count - 1 do
  begin
    Result[i] := CreateElement('infEspecie');

    Result[i].AppendChild(AddNode(tcStr, 'EP12', 'tpEspecie', 1, 1, 1,
      TEspecieToStr(Evento[IdxEv].FInfEvento.detEvento.infGTV[IdxEs].infEspecie[i].tpEspecie)));

    Result[i].AppendChild(AddNode(tcDe2, 'EP13', 'vEspecie', 1, 15, 0,
      Evento[IdxEv].FInfEvento.detEvento.infGTV[IdxEs].infEspecie[i].vEspecie));
  end;
end;

function TEventoCTe.Gerar_Remetente(IdxEv, IdxEs: Integer): TACBrXmlNode;
begin
  Result := CreateElement('rem');

  Result.AppendChild(AddNodeCNPJCPF('EP15', 'EP16',
            Evento[IdxEv].InfEvento.detEvento.infGTV.Items[IdxEs].rem.CNPJCPF));

  if Evento[IdxEv].InfEvento.detEvento.infGTV.Items[IdxEs].rem.IE <> '' then
  begin
    if Trim(Evento[IdxEv].InfEvento.detEvento.infGTV.Items[IdxEs].rem.IE) = 'ISENTO' then
      Result.AppendChild(AddNode(tcStr, 'EP17', 'IE', 0, 14, 0,
          Evento[IdxEv].InfEvento.detEvento.infGTV.Items[IdxEs].rem.IE, DSC_IE))
    else
      Result.AppendChild(AddNode(tcStr, 'EP17', 'IE', 0, 14, 0,
        OnlyNumber(Evento[IdxEv].InfEvento.detEvento.infGTV.Items[IdxEs].rem.IE), DSC_IE));

    if not ValidarIE(Evento[IdxEv].InfEvento.detEvento.infGTV.Items[IdxEs].rem.IE,
           Evento[IdxEv].InfEvento.detEvento.infGTV.Items[IdxEs].rem.UF) then
      wAlerta('EP17', 'IE', DSC_IE, ERR_MSG_INVALIDO);
  end;

  Result.AppendChild(AddNode(tcStr, 'EP18', 'UF', 2, 2, 1,
                 Evento[IdxEv].InfEvento.detEvento.infGTV.Items[IdxEs].rem.UF));

  Result.AppendChild(AddNode(tcStr, 'EP19', 'xNome', 2, 60, 1,
              Evento[IdxEv].InfEvento.detEvento.infGTV.Items[IdxEs].rem.xNome));
end;

function TEventoCTe.Gerar_Destinatario(IdxEv, IdxEs: Integer): TACBrXmlNode;
begin
  Result := CreateElement('dest');

  Result.AppendChild(AddNodeCNPJCPF('EP21', 'EP22',
            Evento[IdxEv].InfEvento.detEvento.infGTV.Items[IdxEs].dest.CNPJCPF));

  if Evento[IdxEv].InfEvento.detEvento.infGTV.Items[IdxEs].dest.IE <> '' then
  begin
    if Trim(Evento[IdxEv].InfEvento.detEvento.infGTV.Items[IdxEs].dest.IE) = 'ISENTO' then
      Result.AppendChild(AddNode(tcStr, 'EP23', 'IE', 0, 14, 0,
          Evento[IdxEv].InfEvento.detEvento.infGTV.Items[IdxEs].dest.IE, DSC_IE))
    else
      Result.AppendChild(AddNode(tcStr, 'EP23', 'IE', 0, 14, 0,
        OnlyNumber(Evento[IdxEv].InfEvento.detEvento.infGTV.Items[IdxEs].dest.IE), DSC_IE));

    if not ValidarIE(Evento[IdxEv].InfEvento.detEvento.infGTV.Items[IdxEs].dest.IE,
           Evento[IdxEv].InfEvento.detEvento.infGTV.Items[IdxEs].dest.UF) then
      wAlerta('EP23', 'IE', DSC_IE, ERR_MSG_INVALIDO);
  end;

  Result.AppendChild(AddNode(tcStr, 'EP24', 'UF', 2, 2, 1,
                 Evento[IdxEv].InfEvento.detEvento.infGTV.Items[IdxEs].dest.UF));

  Result.AppendChild(AddNode(tcStr, 'EP25', 'xNome', 2, 60, 1,
              Evento[IdxEv].InfEvento.detEvento.infGTV.Items[IdxEs].dest.xNome));
end;

function TEventoCTe.Gerar_Evento_ComprEntrega(Idx: Integer): TACBrXmlNode;
var
  nodeArray: TACBrXmlNodeArray;
  i: Integer;
begin
  Result := CreateElement('evCECTe');

  Result.AppendChild(AddNode(tcStr, 'EP02', 'descEvento', 4, 60, 1,
                                            Evento[Idx].FInfEvento.DescEvento));

  Result.AppendChild(AddNode(tcStr, 'EP03', 'nProt', 15, 15, 1,
                                       Evento[Idx].FInfEvento.detEvento.nProt));

  Result.AppendChild(AddNode(tcStr, 'EP05', 'dhEntrega', 25, 25, 1,
    DateTimeTodh(Evento[Idx].InfEvento.detEvento.dhEntrega) +
                 GetUTC(Evento[Idx].InfEvento.detEvento.UF,
                 Evento[Idx].InfEvento.detEvento.dhEntrega), DSC_DEMI));

  Result.AppendChild(AddNode(tcStr, 'EP04', 'nDoc', 2, 20, 1,
                                        Evento[Idx].FInfEvento.detEvento.nDoc));

  Result.AppendChild(AddNode(tcStr, 'EP06', 'xNome', 2, 60, 1,
                                       Evento[Idx].FInfEvento.detEvento.xNome));

  Result.AppendChild(AddNode(tcDe6, 'EP07', 'latitude', 1, 10, 0,
                                    Evento[Idx].FInfEvento.detEvento.latitude));

  Result.AppendChild(AddNode(tcDe6, 'EP08', 'longitude', 1, 11, 0,
                                   Evento[Idx].FInfEvento.detEvento.longitude));

  Result.AppendChild(AddNode(tcStr, 'EP09', 'hashEntrega', 28, 28, 1,
                                 Evento[Idx].FInfEvento.detEvento.hashEntrega));

  Result.AppendChild(AddNode(tcStr, 'EP10', 'dhHashEntrega', 25, 25, 1,
    DateTimeTodh(Evento[Idx].InfEvento.detEvento.dhHashEntrega) +
                 GetUTC(Evento[Idx].InfEvento.detEvento.UF,
                 Evento[Idx].InfEvento.detEvento.dhHashEntrega), DSC_DEMI));

  nodeArray := Gerar_InfEntrega(Idx);
  if nodeArray <> nil then
  begin
    for i := 0 to Length(nodeArray) - 1 do
    begin
      Result.AppendChild(nodeArray[i]);
    end;
  end;
end;

function TEventoCTe.Gerar_InfEntrega(Idx: Integer): TACBrXmlNodeArray;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, Evento[Idx].FInfEvento.detEvento.infEntrega.Count);

  for i := 0 to Evento[Idx].FInfEvento.detEvento.infEntrega.Count - 1 do
  begin
    Result[i] := CreateElement('infEntrega');

    Result[i].AppendChild(AddNode(tcStr, 'EP12', 'chNFe', 44, 44, 1,
                Evento[Idx].FInfEvento.detEvento.infEntrega[i].chNFe));
  end;

  if Evento[Idx].FInfEvento.detEvento.infEntrega.Count > 2000 then
    wAlerta('#1', 'infEntrega', '', ERR_MSG_MAIOR_MAXIMO + '2000');
end;

function TEventoCTe.Gerar_Evento_CancComprEntrega(Idx: Integer): TACBrXmlNode;
begin
  Result := CreateElement('evCancCECTe');

  Result.AppendChild(AddNode(tcStr, 'IP02', 'descEvento', 4, 60, 1,
                                            Evento[Idx].FInfEvento.DescEvento));

  Result.AppendChild(AddNode(tcStr, 'IP03', 'nProt', 15, 15, 1,
                                       Evento[Idx].FInfEvento.detEvento.nProt));

  Result.AppendChild(AddNode(tcStr, 'IP04', 'nProtCE', 15, 15, 1,
                                     Evento[Idx].FInfEvento.detEvento.nProtCE));
end;

function TEventoCTe.Gerar_Evento_InsucessoEntrega(Idx: Integer): TACBrXmlNode;
var
  nodeArray: TACBrXmlNodeArray;
  i: Integer;
begin
  Result := CreateElement('evIECTe');

  Result.AppendChild(AddNode(tcStr, 'EP02', 'descEvento', 4, 60, 1,
                                            Evento[Idx].FInfEvento.DescEvento));

  Result.AppendChild(AddNode(tcStr, 'EP03', 'nProt', 15, 15, 1,
                                 Evento[Idx].FInfEvento.detEvento.nProt));

  Result.AppendChild(AddNode(tcStr, 'EP05', 'dhTentativaEntrega', 55, 25, 1,
    DateTimeTodh(Evento[Idx].InfEvento.detEvento.dhTentativaEntrega) +
                 GetUTC(Evento[Idx].InfEvento.detEvento.UF,
                Evento[Idx].InfEvento.detEvento.dhTentativaEntrega), DSC_DEMI));

  Result.AppendChild(AddNode(tcInt, 'EP04', 'nTentativa', 3, 3, 0,
                                  Evento[Idx].FInfEvento.detEvento.nTentativa));

  Result.AppendChild(AddNode(tcStr, 'EP06', 'tpMotivo', 1, 1, 1,
                      tpMotivoToStr(Evento[Idx].InfEvento.detEvento.tpMotivo)));

  if Evento[Idx].InfEvento.detEvento.tpMotivo = tmOutro then
    Result.AppendChild(AddNode(tcStr, 'EP07', 'xJustMotivo', 25, 250, 0,
                                 Evento[Idx].FInfEvento.detEvento.xJustMotivo));

  Result.AppendChild(AddNode(tcDe6, 'EP08', 'latitude', 1, 10, 0,
                                    Evento[Idx].FInfEvento.detEvento.latitude));

  Result.AppendChild(AddNode(tcDe6, 'EP09', 'longitude', 1, 11, 0,
                                   Evento[Idx].FInfEvento.detEvento.longitude));

  Result.AppendChild(AddNode(tcStr, 'EP10', 'hashTentativaEntrega', 28, 28, 1,
                        Evento[Idx].FInfEvento.detEvento.hashTentativaEntrega));

  Result.AppendChild(AddNode(tcStr, 'EP11', 'dhHashTentativaEntrega', 25, 25, 0,
    DateTimeTodh(Evento[Idx].InfEvento.detEvento.dhHashTentativaEntrega) +
                      GetUTC(Evento[Idx].InfEvento.detEvento.UF,
                      Evento[Idx].InfEvento.detEvento.dhHashTentativaEntrega)));

   nodeArray := Gerar_InfEntrega(Idx);
  if nodeArray <> nil then
  begin
    for i := 0 to Length(nodeArray) - 1 do
    begin
      Result.AppendChild(nodeArray[i]);
    end;
  end;
end;

function TEventoCTe.Gerar_Evento_CancInsucessoEntrega(
  Idx: Integer): TACBrXmlNode;
begin
  Result := CreateElement('evCancIECTe');

  Result.AppendChild(AddNode(tcStr, 'IP02', 'descEvento', 4, 60, 1,
                                            Evento[Idx].FInfEvento.DescEvento));

  Result.AppendChild(AddNode(tcStr, 'IP03', 'nProt', 15, 15, 1,
                                       Evento[Idx].FInfEvento.detEvento.nProt));

  Result.AppendChild(AddNode(tcStr, 'IP04', 'nProtIE', 15, 15, 1,
                                     Evento[Idx].FInfEvento.detEvento.nProtIE));
end;

function TEventoCTe.Gerar_InfEvento(Idx: Integer): TACBrXmlNode;
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

  sDoc := OnlyNumber(Evento[Idx].InfEvento.CNPJ);

  if EstaVazio(sDoc) then
    sDoc := ExtrairCNPJCPFChaveAcesso(Evento[Idx].InfEvento.chCTe);

  // Verifica a Série do Documento, caso esteja no intervalo de 910-969
  // o emitente é pessoa fisica, logo na chave temos um CPF.
  Serie := ExtrairSerieChaveAcesso(Evento[Idx].InfEvento.chCTe);

  if (Length(sDoc) = 14) and (Serie >= 910) and (Serie <= 969) and
     not (Evento[Idx].InfEvento.tpEvento in [teManifDestConfirmacao..teManifDestOperNaoRealizada]) then
  begin
    sDoc := Copy(sDoc, 4, 11);
  end;

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

  Result.AppendChild(AddNode(tcStr, 'HP12', 'chCTe', 44, 44, 1,
                                      Evento[Idx].FInfEvento.chCTe, DSC_CHAVE));

  if not ValidarChave(Evento[Idx].InfEvento.chCTe) then
    wAlerta('HP12', 'chCTe', '', 'Chave de CTe inválida');

  Result.AppendChild(AddNode(tcStr, 'HP13', 'dhEvento', 1, 50, 1,
    FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', Evento[Idx].InfEvento.dhEvento)+
    GetUTC(CodigoUFparaUF(Evento[Idx].InfEvento.cOrgao),
    Evento[Idx].InfEvento.dhEvento)));

  Result.AppendChild(AddNode(tcInt, 'HP14', 'tpEvento', 6, 6, 1,
                                            Evento[Idx].FInfEvento.TipoEvento));

  Result.AppendChild(AddNode(tcInt, 'HP15', 'nSeqEvento', 1, 2, 1,
                                            Evento[Idx].FInfEvento.nSeqEvento));

  Result.AppendChild(AddNode(tcStr, 'HP16', 'verEvento', 1, 4, 1, Versao));

  Result.AppendChild(Gerar_DetEvento(Idx));
end;

function TEventoCTe.Gerar_DetEvento(Idx: Integer): TACBrXmlNode;
begin
  Result := CreateElement('detEvento');
  Result.SetAttribute('versao', Versao);

  case Evento[Idx].InfEvento.tpEvento of
    teCCe: Result.AppendChild(Gerar_Evento_CCe(Idx));

    teCancelamento: Result.AppendChild(Gerar_Evento_Cancelamento(Idx));

    teEPEC: Result.AppendChild(Gerar_Evento_EPEC(Idx));

    teMultiModal: Result.AppendChild(Gerar_Evento_MultiModal(Idx));

    tePrestDesacordo: Result.AppendChild(Gerar_Evento_PrestacaoDesacordo(Idx));

    teCancPrestDesacordo: Result.AppendChild(Gerar_Evento_CancPrestacaoDesacordo(Idx));

    teGTV: Result.AppendChild(Gerar_Evento_GTV(Idx));

    teComprEntregaCTe: Result.AppendChild(Gerar_Evento_ComprEntrega(Idx));

    teCancComprEntrega: Result.AppendChild(Gerar_Evento_CancComprEntrega(Idx));

    teInsucessoEntregaCTe: Result.AppendChild(Gerar_Evento_InsucessoEntrega(Idx));

    teCancInsucessoEntregaCTe: Result.AppendChild(Gerar_Evento_CancInsucessoEntrega(Idx));
  end;
end;

function TEventoCTe.GetOpcoes: TACBrXmlWriterOptions;
begin
  Result := TACBrXmlWriterOptions(FOpcoes);
end;

procedure TEventoCTe.SetEvento(const Value: TInfEventoCollection);
begin
  FEvento.Assign(Value);
end;

procedure TEventoCTe.SetOpcoes(const Value: TACBrXmlWriterOptions);
begin
  FOpcoes := Value;
end;

function TEventoCTe.LerXML(const CaminhoArquivo: string): Boolean;
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

function TEventoCTe.LerXMLFromString(const AXML: string): Boolean;
var
  RetEventoCTe: TRetEventoCTe;
  i, j: Integer;
begin
  RetEventoCTe := TRetEventoCTe.Create;

  try
    RetEventoCTe.XmlRetorno := AXML;
    Result := RetEventoCTe.LerXml;

    with FEvento.New do
    begin
      XML := AXML;

      infEvento.Id := RetEventoCTe.InfEvento.Id;
      InfEvento.cOrgao := RetEventoCTe.InfEvento.cOrgao;
      infEvento.tpAmb := RetEventoCTe.InfEvento.tpAmb;
      infEvento.CNPJ := RetEventoCTe.InfEvento.CNPJ;
      infEvento.chCTe := RetEventoCTe.InfEvento.chCTe;
      infEvento.dhEvento := RetEventoCTe.InfEvento.dhEvento;
      infEvento.tpEvento := RetEventoCTe.InfEvento.tpEvento;
      infEvento.nSeqEvento := RetEventoCTe.InfEvento.nSeqEvento;

      infEvento.VersaoEvento := RetEventoCTe.InfEvento.VersaoEvento;
      infEvento.detEvento.descEvento := RetEventoCTe.InfEvento.detEvento.descEvento;
      infEvento.detEvento.nProt := RetEventoCTe.InfEvento.detEvento.nProt;
      infEvento.detEvento.xJust := RetEventoCTe.InfEvento.DetEvento.xJust;
      infEvento.detEvento.vICMS := RetEventoCTe.InfEvento.DetEvento.vICMS;
      infEvento.detEvento.vICMSST := RetEventoCTe.InfEvento.DetEvento.vICMSST;
      infEvento.detEvento.vTPrest := RetEventoCTe.InfEvento.DetEvento.vTPrest;
      infEvento.detEvento.vCarga := RetEventoCTe.InfEvento.DetEvento.vCarga;
      infEvento.detEvento.toma := RetEventoCTe.InfEvento.DetEvento.toma;
      infEvento.detEvento.UF := RetEventoCTe.InfEvento.DetEvento.UF;
      infEvento.detEvento.CNPJCPF := RetEventoCTe.InfEvento.DetEvento.CNPJCPF;
      infEvento.detEvento.IE := RetEventoCTe.InfEvento.DetEvento.IE;
      infEvento.detEvento.modal := RetEventoCTe.InfEvento.DetEvento.modal;
      infEvento.detEvento.UFIni := RetEventoCTe.InfEvento.DetEvento.UFIni;
      infEvento.detEvento.UFFim := RetEventoCTe.InfEvento.DetEvento.UFFim;
      infEvento.detEvento.xCondUso := RetEventoCTe.InfEvento.DetEvento.xCondUso;
      infEvento.detEvento.xOBS := RetEventoCTe.InfEvento.detEvento.xOBS;
      infEvento.detEvento.dhEntrega := RetEventoCTe.InfEvento.detEvento.dhEntrega;
      infEvento.detEvento.nDoc := RetEventoCTe.InfEvento.detEvento.nDoc;
      infEvento.detEvento.xNome := RetEventoCTe.InfEvento.detEvento.xNome;
      infEvento.detEvento.latitude := RetEventoCTe.InfEvento.detEvento.latitude;
      infEvento.detEvento.longitude := RetEventoCTe.InfEvento.detEvento.longitude;

      infEvento.detEvento.hashEntrega := RetEventoCTe.InfEvento.detEvento.hashEntrega;
      infEvento.detEvento.dhHashEntrega := RetEventoCTe.InfEvento.detEvento.dhHashEntrega;

      infEvento.detEvento.nProtCE := RetEventoCTe.InfEvento.detEvento.nProtCE;

      infEvento.detEvento.dhTentativaEntrega := RetEventoCTe.InfEvento.detEvento.dhTentativaEntrega;
      infEvento.detEvento.nTentativa := RetEventoCTe.InfEvento.detEvento.nTentativa;
      infEvento.detEvento.tpMotivo := RetEventoCTe.InfEvento.detEvento.tpMotivo;
      infEvento.detEvento.xJustMotivo := RetEventoCTe.InfEvento.detEvento.xJustMotivo;
      infEvento.detEvento.hashTentativaEntrega := RetEventoCTe.InfEvento.detEvento.hashTentativaEntrega;
      infEvento.detEvento.dhHashTentativaEntrega := RetEventoCTe.InfEvento.detEvento.dhHashTentativaEntrega;

      for i := 0 to RetEventoCTe.InfEvento.detEvento.infCorrecao.Count -1 do
      begin
        infEvento.detEvento.infCorrecao.New;
        infEvento.detEvento.infCorrecao[i].grupoAlterado := RetEventoCTe.InfEvento.detEvento.infCorrecao[i].grupoAlterado;
        infEvento.detEvento.infCorrecao[i].campoAlterado := RetEventoCTe.InfEvento.detEvento.infCorrecao[i].campoAlterado;
        infEvento.detEvento.infCorrecao[i].valorAlterado := RetEventoCTe.InfEvento.detEvento.infCorrecao[i].valorAlterado;
        infEvento.detEvento.infCorrecao[i].nroItemAlterado := RetEventoCTe.InfEvento.detEvento.infCorrecao[i].nroItemAlterado;
      end;

      for i := 0 to RetEventoCTe.InfEvento.detEvento.infGTV.Count -1 do
      begin
        infEvento.detEvento.infGTV.New;
        infEvento.detEvento.infGTV[i].nDoc := RetEventoCTe.InfEvento.detEvento.infGTV[i].nDoc;
        infEvento.detEvento.infGTV[i].id := RetEventoCTe.InfEvento.detEvento.infGTV[i].id;
        infEvento.detEvento.infGTV[i].serie := RetEventoCTe.InfEvento.detEvento.infGTV[i].serie;
        infEvento.detEvento.infGTV[i].subserie := RetEventoCTe.InfEvento.detEvento.infGTV[i].subserie;
        infEvento.detEvento.infGTV[i].dEmi := RetEventoCTe.InfEvento.detEvento.infGTV[i].dEmi;
        infEvento.detEvento.infGTV[i].nDV := RetEventoCTe.InfEvento.detEvento.infGTV[i].nDV;
        infEvento.detEvento.infGTV[i].qCarga := RetEventoCTe.InfEvento.detEvento.infGTV[i].qCarga;

        for j := 0 to RetEventoCTe.InfEvento.detEvento.infGTV[i].infEspecie.Count -1 do
        begin
          infEvento.detEvento.infGTV[i].infEspecie.New;
          infEvento.detEvento.infGTV[i].infEspecie[j].tpEspecie := RetEventoCTe.InfEvento.detEvento.infGTV[i].infEspecie[j].tpEspecie;
          infEvento.detEvento.infGTV[i].infEspecie[j].vEspecie := RetEventoCTe.InfEvento.detEvento.infGTV[i].infEspecie[j].vEspecie;
        end;

        infEvento.detEvento.infGTV[i].rem.CNPJCPF := RetEventoCTe.InfEvento.detEvento.infGTV[i].rem.CNPJCPF;
        infEvento.detEvento.infGTV[i].rem.IE := RetEventoCTe.InfEvento.detEvento.infGTV[i].rem.IE;
        infEvento.detEvento.infGTV[i].rem.UF := RetEventoCTe.InfEvento.detEvento.infGTV[i].rem.UF;
        infEvento.detEvento.infGTV[i].rem.xNome := RetEventoCTe.InfEvento.detEvento.infGTV[i].rem.xNome;

        infEvento.detEvento.infGTV[i].dest.CNPJCPF := RetEventoCTe.InfEvento.detEvento.infGTV[i].dest.CNPJCPF;
        infEvento.detEvento.infGTV[i].dest.IE := RetEventoCTe.InfEvento.detEvento.infGTV[i].dest.IE;
        infEvento.detEvento.infGTV[i].dest.UF := RetEventoCTe.InfEvento.detEvento.infGTV[i].dest.UF;
        infEvento.detEvento.infGTV[i].dest.xNome := RetEventoCTe.InfEvento.detEvento.infGTV[i].dest.xNome;

        infEvento.detEvento.infGTV[i].placa := RetEventoCTe.InfEvento.detEvento.infGTV[i].placa;
        infEvento.detEvento.infGTV[i].UF := RetEventoCTe.InfEvento.detEvento.infGTV[i].UF;
        infEvento.detEvento.infGTV[i].RNTRC := RetEventoCTe.InfEvento.detEvento.infGTV[i].RNTRC;
      end;

      if RetEventoCTe.retEvento.Count > 0 then
      begin
        FRetInfEvento.Id := RetEventoCTe.retEvento.Items[0].RetInfEvento.Id;
        FRetInfEvento.tpAmb := RetEventoCTe.retEvento.Items[0].RetInfEvento.tpAmb;
        FRetInfEvento.verAplic := RetEventoCTe.retEvento.Items[0].RetInfEvento.verAplic;
        FRetInfEvento.cOrgao := RetEventoCTe.retEvento.Items[0].RetInfEvento.cOrgao;
        FRetInfEvento.cStat := RetEventoCTe.retEvento.Items[0].RetInfEvento.cStat;
        FRetInfEvento.xMotivo := RetEventoCTe.retEvento.Items[0].RetInfEvento.xMotivo;
        FRetInfEvento.chCTe := RetEventoCTe.retEvento.Items[0].RetInfEvento.chCTe;
        FRetInfEvento.tpEvento := RetEventoCTe.retEvento.Items[0].RetInfEvento.tpEvento;
        FRetInfEvento.xEvento := RetEventoCTe.retEvento.Items[0].RetInfEvento.xEvento;
        FRetInfEvento.nSeqEvento := RetEventoCTe.retEvento.Items[0].RetInfEvento.nSeqEvento;
        FRetInfEvento.CNPJDest := RetEventoCTe.retEvento.Items[0].RetInfEvento.CNPJDest;
        FRetInfEvento.emailDest := RetEventoCTe.retEvento.Items[0].RetInfEvento.emailDest;
        FRetInfEvento.dhRegEvento := RetEventoCTe.retEvento.Items[0].RetInfEvento.dhRegEvento;
        FRetInfEvento.nProt := RetEventoCTe.retEvento.Items[0].RetInfEvento.nProt;
        FRetInfEvento.XML := RetEventoCTe.retEvento.Items[0].RetInfEvento.XML;
      end;

      for i := 0 to RetEventoCTe.InfEvento.detEvento.infEntrega.Count -1 do
      begin
        infEvento.detEvento.infEntrega.New;
        infEvento.detEvento.infEntrega[i].chNFe := RetEventoCTe.InfEvento.detEvento.infEntrega[i].chNFe;
      end;

      signature.URI := RetEventoCTe.signature.URI;
      signature.DigestValue := RetEventoCTe.signature.DigestValue;
      signature.SignatureValue := RetEventoCTe.signature.SignatureValue;
      signature.X509Certificate := RetEventoCTe.signature.X509Certificate;
    end;
  finally
    RetEventoCTe.Free;
  end;
end;

function TEventoCTe.LerFromIni(const AIniString: string; CCe: Boolean): Boolean;
var
  I, J, K: Integer;
  sSecao, sFim: string;
  INIRec: TMemIniFile;
  ok: Boolean;
begin
{$IFNDEF COMPILER23_UP}
  Result := False;
{$ENDIF}
  Self.Evento.Clear;

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);
    idLote := INIRec.ReadInteger( 'EVENTO', 'idLote', 0);

    I := 1;
    while true do
    begin
      sSecao := 'EVENTO' + IntToStrZero(I, 3);
      sFim := INIRec.ReadString(sSecao, 'chCTe', 'FIM');
      if (sFim = 'FIM') or (Length(sFim) <= 0) then
        break ;

      with Self.Evento.New do
      begin
        infEvento.chCTe := INIRec.ReadString(sSecao, 'chCTe', '');
        infEvento.cOrgao := INIRec.ReadInteger(sSecao, 'cOrgao', 0);
        infEvento.CNPJ := INIRec.ReadString(sSecao, 'CNPJ', '');
        infEvento.dhEvento := StringToDateTime(INIRec.ReadString(sSecao, 'dhEvento', ''));
        infEvento.tpEvento := StrToTpEventoCTe(ok, INIRec.ReadString(sSecao, 'tpEvento', ''));
        infEvento.nSeqEvento := INIRec.ReadInteger(sSecao, 'nSeqEvento', 1);
        infEvento.detEvento.xCondUso := '';
        infEvento.detEvento.xJust := INIRec.ReadString(sSecao, 'xJust', '');
        infEvento.detEvento.nProt := INIRec.ReadString(sSecao, 'nProt', '');
        infEvento.detEvento.nProtCE := INIRec.ReadString(sSecao, 'nProtCE', '');
        infEvento.detEvento.nProtIE := INIRec.ReadString(sSecao, 'nProtIE', '');

        case InfEvento.tpEvento of
          teEPEC:
            begin
              infEvento.detEvento.vICMS := StringToFloatDef(INIRec.ReadString(sSecao, 'vICMS', ''), 0);
              infEvento.detEvento.vICMSST := StringToFloatDef(INIRec.ReadString(sSecao, 'vICMSST', ''), 0);
              infEvento.detEvento.vTPrest := StringToFloatDef(INIRec.ReadString(sSecao, 'vTPrest', ''), 0);
              infEvento.detEvento.vCarga := StringToFloatDef(INIRec.ReadString(sSecao, 'vCarga', ''), 0);
              InfEvento.detEvento.modal := StrToTpModal(ok, INIRec.ReadString(sSecao, 'modal', '01'));
              infEvento.detEvento.UFIni := INIRec.ReadString(sSecao, 'UFIni', '');
              infEvento.detEvento.UFFim := INIRec.ReadString(sSecao, 'UFFim', '');
              infEvento.detEvento.dhEmi := StringToDateTime(INIRec.ReadString(sSecao, 'dhEmi', ''));

              infEvento.detEvento.toma := StrToTpTomador(ok, INIRec.ReadString('TOMADOR', 'toma', '1'));
              infEvento.detEvento.UF := INIRec.ReadString('TOMADOR', 'UF', '');
              infEvento.detEvento.CNPJCPF := INIRec.ReadString('TOMADOR', 'CNPJCPF', '');
              infEvento.detEvento.IE := INIRec.ReadString('TOMADOR', 'IE', '');
            end;

          teCCe:
            begin
              Self.Evento.Items[I-1].InfEvento.detEvento.infCorrecao.Clear;

              J := 1;
              while true do
              begin
                sSecao := 'DETEVENTO' + IntToStrZero(J, 3);
                sFim := INIRec.ReadString(sSecao, 'grupoAlterado', 'FIM');

                if (sFim = 'FIM') or (Length(sFim) <= 0) then
                  break;

                with Self.Evento.Items[I-1].InfEvento.detEvento.infCorrecao.New do
                begin
                  grupoAlterado := INIRec.ReadString(sSecao, 'grupoAlterado', '');
                  campoAlterado := INIRec.ReadString(sSecao, 'campoAlterado', '');
                  valorAlterado := INIRec.ReadString(sSecao, 'valorAlterado', '');
                  nroItemAlterado := INIRec.ReadInteger(sSecao, 'nroItemAlterado', 0);
                end;
                Inc(J);
              end;
            end;

          teMultiModal:
            begin
              infEvento.detEvento.xRegistro := INIRec.ReadString(sSecao, 'xRegistro', '');
              infEvento.detEvento.nDoc := INIRec.ReadString(sSecao, 'nDoc', '');
            end;

          tePrestDesacordo:
            begin
              infEvento.detEvento.xOBS := INIRec.ReadString(sSecao, 'xObs', '');
            end;

          teCancPrestDesacordo:
            begin
              infEvento.detEvento.nProt := INIRec.ReadString(sSecao, 'nProt', '');
            end;

          teGTV:
            begin
              Self.Evento.Items[I-1].InfEvento.detEvento.infGTV.Clear;

              J := 1;
              while true do
              begin
                sSecao := 'infGTV' + IntToStrZero(J, 3);
                sFim := INIRec.ReadString(sSecao, 'nDoc', 'FIM');

                if (sFim = 'FIM') or (Length(sFim) <= 0) then
                  break;

                with Self.Evento.Items[I-1].InfEvento.detEvento.infGTV.New do
                begin
                  nDoc := sFim;
                  id := INIRec.ReadString(sSecao, 'id', '');
                  serie := INIRec.ReadString(sSecao, 'serie', '');
                  subserie := INIRec.ReadString(sSecao, 'subserie', '');
                  dEmi := StringToDateTime(INIRec.ReadString(sSecao, 'dEmi', ''));
                  nDV := INIRec.ReadInteger(sSecao, 'nDV', 0);
                  qCarga := StringToFloatDef(INIRec.ReadString(sSecao, 'qCarga', ''), 0);
                  placa := INIRec.ReadString(sSecao, 'placa', '');
                  UF := INIRec.ReadString(sSecao, 'UF', '');
                  RNTRC := INIRec.ReadString(sSecao, 'RNTRC', '');
                end;

                Self.Evento.Items[I-1].InfEvento.detEvento.infGTV.Items[J].infEspecie.Clear;

                K := 1;
                while true do
                begin
                  sSecao := 'infEspecie' + IntToStrZero(J, 3) + IntToStrZero(K, 3);
                  sFim := INIRec.ReadString(sSecao, 'tpEspecie', 'FIM');

                  if (sFim = 'FIM') or (Length(sFim) <= 0) then
                    break;

                  with Self.Evento.Items[I-1].InfEvento.detEvento.infGTV.Items[J].infEspecie.New do
                  begin
                    tpEspecie := StrToTEspecie(Ok, sFim);
                    vEspecie := StringToFloatDef(INIRec.ReadString(sSecao, 'vEspecie', ''), 0);
                  end;
                  Inc(K);
                end;

                sSecao := 'rem' + IntToStrZero(J, 3);
                sFim := INIRec.ReadString(sSecao, 'CNPJCPF', 'FIM');

                if (sFim = 'FIM') or (Length(sFim) <= 0) then
                  break;

                with Self.Evento.Items[I-1].InfEvento.detEvento.infGTV.Items[J].rem do
                begin
                  CNPJCPF := sFim;
                  IE := INIRec.ReadString(sSecao, 'IE', '');
                  UF := INIRec.ReadString(sSecao, 'UF', '');
                  xNome := INIRec.ReadString(sSecao, 'xNome', '');
                end;

                sSecao := 'dest' + IntToStrZero(J, 3);
                sFim := INIRec.ReadString(sSecao, 'CNPJCPF', 'FIM');

                if (sFim = 'FIM') or (Length(sFim) <= 0) then
                  break;

                with Self.Evento.Items[I-1].InfEvento.detEvento.infGTV.Items[J].dest do
                begin
                  CNPJCPF := sFim;
                  IE := INIRec.ReadString(sSecao, 'IE', '');
                  UF := INIRec.ReadString(sSecao, 'UF', '');
                  xNome := INIRec.ReadString(sSecao, 'xNome', '');
                end;

                Inc(J);
              end;
            end;

          teComprEntrega:
            begin
              infEvento.detEvento.nProt := INIRec.ReadString(sSecao, 'nProt', '');
              infEvento.detEvento.UF := INIRec.ReadString(sSecao, 'UF', '');
              infEvento.detEvento.dhEntrega := StringToDateTime(INIRec.ReadString(sSecao, 'dhEntrega', ''));
              infEvento.detEvento.nDoc := INIRec.ReadString(sSecao, 'nDoc', '');
              infEvento.detEvento.xNome := INIRec.ReadString(sSecao, 'xNome', '');
              infEvento.detEvento.latitude := StringToFloatDef(INIRec.ReadString(sSecao, 'latitude', ''), 0);
              infEvento.detEvento.longitude := StringToFloatDef(INIRec.ReadString(sSecao, 'longitude', ''), 0);

              infEvento.detEvento.hashEntrega := INIRec.ReadString(sSecao, 'hashEntrega', '');
              infEvento.detEvento.dhHashEntrega := StringToDateTime(INIRec.ReadString(sSecao, 'dhHashEntrega', ''));

              Self.Evento.Items[I-1].InfEvento.detEvento.infEntrega.Clear;

              J := 1;
              while true do
              begin
                // J varia de 0000 até 2000
                sSecao := 'infEntrega' + IntToStrZero(J, 4);
                sFim := INIRec.ReadString(sSecao, 'chNFe', 'FIM');

                if (sFim = 'FIM') or (Length(sFim) <= 0) then
                  break;

                with Self.Evento.Items[I-1].InfEvento.detEvento.infEntrega.New do
                  chNFe := sFim;

                Inc(J);
              end;
            end;

          teInsucessoEntregaCTe:
            begin
              infEvento.detEvento.nProt := INIRec.ReadString(sSecao, 'nProt', '');
              infEvento.detEvento.dhTentativaEntrega := StringToDateTime(INIRec.ReadString(sSecao, 'dhTentativaEntrega', ''));
              infEvento.detEvento.nTentativa := INIRec.ReadInteger(sSecao, 'nTentativa', 0);
              infEvento.detEvento.tpMotivo := StrTotpMotivo(ok, INIRec.ReadString(sSecao, 'tpMotivo', '1'));
              infEvento.detEvento.xJustMotivo := INIRec.ReadString(sSecao, 'xJustMotivo', '');
              infEvento.detEvento.latitude := StringToFloatDef(INIRec.ReadString(sSecao, 'latitude', ''), 0);
              infEvento.detEvento.longitude := StringToFloatDef(INIRec.ReadString(sSecao, 'longitude', ''), 0);

              infEvento.detEvento.hashTentativaEntrega := INIRec.ReadString(sSecao, 'hashTentativaEntrega', '');
              infEvento.detEvento.dhHashTentativaEntrega := StringToDateTime(INIRec.ReadString(sSecao, 'dhHashTentativaEntrega', ''));

              Self.Evento.Items[I-1].InfEvento.detEvento.infEntrega.Clear;

              J := 1;
              while true do
              begin
                // J varia de 0000 até 2000
                sSecao := 'infEntrega' + IntToStrZero(J, 4);
                sFim := INIRec.ReadString(sSecao, 'chNFe', 'FIM');

                if (sFim = 'FIM') or (Length(sFim) <= 0) then
                  break;

                with Self.Evento.Items[I-1].InfEvento.detEvento.infEntrega.New do
                  chNFe := sFim;

                Inc(J);
              end;
            end;
        end;
      end;

      Inc(I);
    end;

    Result := True;
  finally
    INIRec.Free;
  end;
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
  FRetInfEvento := TRetInfEvento.Create;
  Fsignature := Tsignature.Create;
end;

destructor TInfEventoCollectionItem.Destroy;
begin
  FInfEvento.Free;
  FRetInfEvento.Free;
  Fsignature.Free;

  inherited;
end;

end.

