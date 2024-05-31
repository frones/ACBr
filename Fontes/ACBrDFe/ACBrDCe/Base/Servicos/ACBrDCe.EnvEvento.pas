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

unit ACBrDCe.EnvEvento;

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
  pcnSignature,
  ACBrDCe.Consts,
  ACBrDCe.EventoClass,
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

  { TEventoDCe }

  TEventoDCe = class(TACBrXmlWriter)
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
    function Gerar_Evento: TACBrXmlNodeArray;
    function Gerar_Evento_Cancelamento(Idx: Integer): TACBrXmlNode;

  public
    constructor Create;
    destructor Destroy; override;

    function GerarXml: Boolean; Override;

    function LerXML(const CaminhoArquivo: string): Boolean;
    function LerXMLFromString(const AXML: string): Boolean;
    function ObterNomeArquivo(tpEvento: TpcnTpEvento): string;
    function LerFromIni(const AIniString: string; CCe: Boolean = True): Boolean;

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
  ACBrDCe.RetEnvEvento,
  ACBrDCe.Conversao;

{ TEventoDCe }

constructor TEventoDCe.Create;
begin
  inherited Create;

  FEvento  := TInfEventoCollection.Create();
end;

function TEventoDCe.CreateOptions: TACBrXmlWriterOptions;
begin
  Result := TACBrXmlWriterOptions.Create();
end;

destructor TEventoDCe.Destroy;
begin
  FEvento.Free;

  inherited;
end;

function TEventoDCe.ObterNomeArquivo(tpEvento: TpcnTpEvento): string;
begin
  case tpEvento of
    teCancelamento: Result := IntToStr(Self.idLote) + '-can-eve.xml';
  else
    raise EventoException.Create('Obter nome do arquivo de Evento não Implementado!');
  end;
end;

function TEventoDCe.GerarXML: Boolean;
var
  EventoNode: TACBrXmlNode;
  nodeArray: TACBrXmlNodeArray;
  i: Integer;
begin
  ListaDeAlertas.Clear;

  FDocument.Clear();

  EventoNode := CreateElement('envEvento');
  EventoNode.SetNamespace('http://www.portalfiscal.inf.br/dce');
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

function TEventoDCe.Gerar_Evento: TACBrXmlNodeArray;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, Evento.Count);

  for i := 0 to Evento.Count - 1 do
  begin
    Evento[i].InfEvento.id := 'ID' + Evento[i].InfEvento.TipoEvento +
                               OnlyNumber(Evento[i].InfEvento.chDCe) +
                               Format('%.2d', [Evento[i].InfEvento.nSeqEvento]);

    if Length(Evento[i].InfEvento.id) < 54 then
      wAlerta('HP07', 'ID', '', 'ID de Evento inválido');

    Result[i] := CreateElement('evento');
    Result[i].SetNamespace('http://www.portalfiscal.inf.br/dce');
    Result[i].SetAttribute('versao', Versao);

    Result[i].AppendChild(Gerar_InfEvento(i));

    // Incluir a assinatura no XML
    if Evento[i].signature.URI <> '' then
      Result[i].AppendChild(GerarSignature(Evento[i].signature));
  end;

  if Evento.Count > 20 then
    wAlerta('#1', 'evento', '', ERR_MSG_MAIOR_MAXIMO + '20');
end;

function TEventoDCe.Gerar_Evento_Cancelamento(Idx: Integer): TACBrXmlNode;
begin
//italo
end;

function TEventoDCe.Gerar_InfEvento(Idx: Integer): TACBrXmlNode;
var
  sDoc: string;
  Serie: Integer;
begin
  Result := CreateElement('infEvento');
  Result.SetAttribute('Id', Evento[Idx].InfEvento.id);

  Result.AppendChild(AddNode(tcInt, 'HP08', 'cOrgao', 1, 2, 1,
                                                Evento[Idx].FInfEvento.cOrgao));

//italo  Result.AppendChild(AddNode(tcStr, 'HP09', 'tpAmb', 1, 1, 1,
//                           TpAmbToStr(Evento[Idx].InfEvento.tpAmb), DSC_TPAMB));

  sDoc := OnlyNumber(Evento[Idx].InfEvento.CNPJCPF);

  if EstaVazio(sDoc) then
    sDoc := ExtrairCNPJCPFChaveAcesso(Evento[Idx].InfEvento.chDCe);

  // Verifica a Série do Documento, caso esteja no intervalo de 910-969
  // o emitente é pessoa fisica, logo na chave temos um CPF.
  Serie := ExtrairSerieChaveAcesso(Evento[Idx].InfEvento.chDCe);

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

  Result.AppendChild(AddNode(tcStr, 'HP12', 'chDCe', 44, 44, 1,
                                      Evento[Idx].FInfEvento.chDCe, DSC_CHAVE));

  if not ValidarChave(Evento[Idx].InfEvento.chDCe) then
    wAlerta('HP12', 'chDCe', '', 'Chave de DCe inválida');

  Result.AppendChild(AddNode(tcStr, 'HP13', 'dhEvento', 1, 50, 1,
    FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', Evento[Idx].InfEvento.dhEvento)+
    GetUTC(CodigoUFparaUF(Evento[Idx].InfEvento.cOrgao),
    Evento[Idx].InfEvento.dhEvento)));

  Result.AppendChild(AddNode(tcInt, 'HP14', 'tpEvento', 6, 6, 1,
                                            Evento[Idx].FInfEvento.TipoEvento));

  Result.AppendChild(AddNode(tcInt, 'HP15', 'nSeqEvento', 1, 2, 1,
                                            Evento[Idx].FInfEvento.nSeqEvento));

  Result.AppendChild(AddNode(tcStr, 'HP16', 'verEvento', 1, 4, 1, Versao));

  if Evento[Idx].InfEvento.tpEvento = teAtorInteressadoNFe then
    FOpcoes.RetirarAcentos := False;  // Não funciona sem acentos

  case Evento[Idx].InfEvento.tpEvento of
    teCancelamento: Result.AppendChild(Gerar_Evento_Cancelamento(Idx));
  end;
end;

function TEventoDCe.GetOpcoes: TACBrXmlWriterOptions;
begin
  Result := TACBrXmlWriterOptions(FOpcoes);
end;

procedure TEventoDCe.SetEvento(const Value: TInfEventoCollection);
begin
  FEvento.Assign(Value);
end;

procedure TEventoDCe.SetOpcoes(const Value: TACBrXmlWriterOptions);
begin
  FOpcoes := Value;
end;

function TEventoDCe.LerXML(const CaminhoArquivo: string): Boolean;
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

function TEventoDCe.LerXMLFromString(const AXML: string): Boolean;
var
  RetEventoDCe: TRetEventoDCe;
  i: Integer;
begin
  RetEventoDCe := TRetEventoDCe.Create;

  try
    RetEventoDCe.XmlRetorno := AXML;
    Result := RetEventoDCe.LerXml;

    with FEvento.New do
    begin
      XML                    := AXML;
      {
      infEvento.ID           := RetEventoDCe.InfEvento.id;
      infEvento.cOrgao       := RetEventoDCe.InfEvento.cOrgao;
      infEvento.tpAmb        := RetEventoDCe.InfEvento.tpAmb;
      infEvento.CNPJ         := RetEventoDCe.InfEvento.CNPJ;
      infEvento.chNFe        := RetEventoDCe.InfEvento.chNFe;
      infEvento.dhEvento     := RetEventoDCe.InfEvento.dhEvento;
      infEvento.tpEvento     := RetEventoDCe.InfEvento.tpEvento;
      infEvento.nSeqEvento   := RetEventoDCe.InfEvento.nSeqEvento;
      infEvento.VersaoEvento := RetEventoDCe.InfEvento.VersaoEvento;

      infEvento.DetEvento.descEvento := RetEventoDCe.InfEvento.DetEvento.descEvento;
      infEvento.DetEvento.xCorrecao  := RetEventoDCe.InfEvento.DetEvento.xCorrecao;
      infEvento.DetEvento.xCondUso   := RetEventoDCe.InfEvento.DetEvento.xCondUso;
      infEvento.DetEvento.nProt      := RetEventoDCe.InfEvento.DetEvento.nProt;
      infEvento.DetEvento.xJust      := RetEventoDCe.InfEvento.DetEvento.xJust;
      infEvento.DetEvento.chNFeRef   := RetEventoDCe.InfEvento.DetEvento.chNFeRef;

      infEvento.detEvento.cOrgaoAutor := RetEventoDCe.InfEvento.detEvento.cOrgaoAutor;
      infEvento.detEvento.tpAutor     := RetEventoDCe.InfEvento.detEvento.tpAutor;
      infEvento.detEvento.verAplic    := RetEventoDCe.InfEvento.detEvento.verAplic;
      infEvento.detEvento.dhEmi       := RetEventoDCe.InfEvento.detEvento.dhEmi;
      infEvento.detEvento.tpNF        := RetEventoDCe.InfEvento.detEvento.tpNF;
      infEvento.detEvento.IE          := RetEventoDCe.InfEvento.detEvento.IE;
}
      signature.URI             := RetEventoDCe.signature.URI;
      signature.DigestValue     := RetEventoDCe.signature.DigestValue;
      signature.SignatureValue  := RetEventoDCe.signature.SignatureValue;
      signature.X509Certificate := RetEventoDCe.signature.X509Certificate;
      {
      if RetEventoDCe.retEvento.Count > 0 then
      begin
        RetInfEvento.Id := RetEventoDCe.retEvento[0].RetInfEvento.Id;
        RetInfEvento.tpAmb := RetEventoDCe.retEvento[0].RetInfEvento.tpAmb;
        RetInfEvento.verAplic := RetEventoDCe.retEvento[0].RetInfEvento.verAplic;
        RetInfEvento.cOrgao := RetEventoDCe.retEvento[0].RetInfEvento.cOrgao;
        RetInfEvento.cStat := RetEventoDCe.retEvento[0].RetInfEvento.cStat;
        RetInfEvento.xMotivo := RetEventoDCe.retEvento[0].RetInfEvento.xMotivo;
        RetInfEvento.chNFe := RetEventoDCe.retEvento[0].RetInfEvento.chNFe;
        RetInfEvento.tpEvento := RetEventoDCe.retEvento[0].RetInfEvento.tpEvento;
        RetInfEvento.xEvento := RetEventoDCe.retEvento[0].RetInfEvento.xEvento;
        RetInfEvento.nSeqEvento := RetEventoDCe.retEvento[0].RetInfEvento.nSeqEvento;
        RetInfEvento.cOrgaoAutor := RetEventoDCe.retEvento[0].RetInfEvento.cOrgaoAutor;
        RetInfEvento.CNPJDest := RetEventoDCe.retEvento[0].RetInfEvento.CNPJDest;
        RetInfEvento.emailDest := RetEventoDCe.retEvento[0].RetInfEvento.emailDest;
        RetInfEvento.dhRegEvento := RetEventoDCe.retEvento[0].RetInfEvento.dhRegEvento;
        RetInfEvento.nProt := RetEventoDCe.retEvento[0].RetInfEvento.nProt;
        RetInfEvento.XML := RetEventoDCe.retEvento[0].RetInfEvento.XML;
      end;
      }
    end;
  finally
    RetEventoDCe.Free;
  end;
end;

function TEventoDCe.LerFromIni(const AIniString: string; CCe: Boolean): Boolean;
var
  I, J: Integer;
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
      sFim := INIRec.ReadString(sSecao, 'chNFe', 'FIM');
      if (sFim = 'FIM') or (Length(sFim) <= 0) then
        break ;

      with Self.Evento.New do
      begin
        infEvento.cOrgao := INIRec.ReadInteger(sSecao, 'cOrgao', 0);
        {
        infEvento.CNPJ   := INIRec.ReadString(sSecao, 'CNPJ', '');
        infEvento.chNFe  := sFim;
        infEvento.dhEvento := StringToDateTime(INIRec.ReadString(sSecao, 'dhEvento', ''));
        infEvento.tpEvento := StrToTpEventoNFe(ok,INIRec.ReadString(sSecao, 'tpEvento', ''));

        infEvento.nSeqEvento := INIRec.ReadInteger(sSecao, 'nSeqEvento', 1);
        infEvento.versaoEvento := INIRec.ReadString(sSecao, 'versaoEvento', '1.00');
        infEvento.detEvento.cOrgaoAutor := INIRec.ReadInteger(sSecao, 'cOrgaoAutor', 0);
        infEvento.detEvento.verAplic := INIRec.ReadString(sSecao, 'verAplic', '1.0');

        infEvento.detEvento.xCorrecao := INIRec.ReadString(sSecao, 'xCorrecao', '');
        infEvento.detEvento.xCondUso := INIRec.ReadString(sSecao, 'xCondUso', '');
        infEvento.detEvento.nProt := INIRec.ReadString(sSecao, 'nProt', '');
        infEvento.detEvento.xJust := INIRec.ReadString(sSecao, 'xJust', '');
        infEvento.detEvento.chNFeRef := INIRec.ReadString(sSecao, 'chNFeRef', '');
        infEvento.detEvento.nProtEvento := INIRec.ReadString(sSecao, 'nProtEvento', '');
        }
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

