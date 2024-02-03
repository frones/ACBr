{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Leivio Ramos de Fontenele                       }
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

unit pcnReinfR2060;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IFEND}
  ACBrBase,
  pcnConversao, pcnGerador,
  ACBrUtil.Base, ACBrUtil.FilesIO,
  ACBrDFeConsts,
  pcnCommonReinf, pcnConversaoReinf, pcnGeradorReinf;

type
  TR2060Collection = class;
  TR2060CollectionItem = class;
  TevtCPRB = class;

  {Classes específicas deste evento}
  TinfoCPRB = class;
  TideEstab = class;
  TtipoCodCollection = class;
  TtipoCodCollectionItem = class;
  TtipoAjusteCollection = class;
  TtipoAjusteCollectionItem = class;
  TinfoProcCollection = class;
  TinfoProcCollectionItem = class;

  TR2060Collection = class(TReinfCollection)
  private
    function GetItem(Index: Integer): TR2060CollectionItem;
    procedure SetItem(Index: Integer; Value: TR2060CollectionItem);
  public
    function Add: TR2060CollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TR2060CollectionItem;

    property Items[Index: Integer]: TR2060CollectionItem read GetItem write SetItem; default;
  end;

  TR2060CollectionItem = class(TObject)
  private
    FTipoEvento: TTipoEvento;
    FevtCPRB: TevtCPRB;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    property TipoEvento: TTipoEvento read FTipoEvento;
    property evtCPRB: TevtCPRB read FevtCPRB write FevtCPRB;
  end;

  TevtCPRB = class(TReinfEvento) //Classe do elemento principal do XML do evento!
  private
    FIdeEvento: TIdeEvento2;
    FideContri: TideContri;
    FinfoCPRB: TinfoCPRB;

    {Geradores específicos desta classe}
    procedure GerarideEstab;
    procedure GerartipoCod(Lista: TtipoCodCollection);
    procedure GerartipoAjuste(Lista: TtipoAjusteCollection);
    procedure GerarinfoProc(Lista: TinfoProcCollection);
  public
    constructor Create(AACBrReinf: TObject); override;
    destructor  Destroy; override;

    function GerarXML: Boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property ideEvento: TIdeEvento2 read FIdeEvento write FIdeEvento;
    property ideContri: TideContri read FideContri write FideContri;
    property infoCPRB: TinfoCPRB read FinfoCPRB write FinfoCPRB;
  end;

  { TinfoCPRB }
  TinfoCPRB = class(TObject)
  private
    FideEstab: TideEstab;
  public
    constructor Create;
    destructor Destroy; override;

    property ideEstab: TideEstab read FideEstab write FideEstab;
  end;

  { TideEstab }
  TideEstab = class(TObject)
  private
    FtpInscEstab: TtpInsc;
    FnrInscEstab: string;
    FvlrRecBrutaTotal: double;
    FvlrCPApurTotal: double;
    FvlrCPRBSuspTotal: double;
    FtipoCod: TtipoCodCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property tpInscEstab: TtpInsc read FtpInscEstab write FtpInscEstab default tiCNPJ;
    property nrInscEstab: string read FnrInscEstab write FnrInscEstab;
    property vlrRecBrutaTotal: double read FvlrRecBrutaTotal write FvlrRecBrutaTotal;
    property vlrCPApurTotal: double read FvlrCPApurTotal write FvlrCPApurTotal;
    property vlrCPRBSuspTotal: double read FvlrCPRBSuspTotal write FvlrCPRBSuspTotal;
    property tipoCod: TtipoCodCollection read FtipoCod write FtipoCod;
  end;

  TtipoCodCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TtipoCodCollectionItem;
    procedure SetItem(Index: Integer; Value: TtipoCodCollectionItem);
  public
    function Add: TtipoCodCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TtipoCodCollectionItem;

    property Items[Index: Integer]: TtipoCodCollectionItem read GetItem write SetItem; default;
  end;

  TtipoCodCollectionItem = class(TObject)
  private
    FcodAtivEcon: string;
    FvlrRecBrutaAtiv: double;
    FvlrExcRecBruta: double;
    FvlrAdicRecBruta: double;
    FvlrBcCPRB: double;
    FvlrCPRBapur: double;
    FtipoAjuste: TtipoAjusteCollection;
    FinfoProc: TinfoProcCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property codAtivEcon: string read FcodAtivEcon write FcodAtivEcon;
    property vlrRecBrutaAtiv: double read FvlrRecBrutaAtiv write FvlrRecBrutaAtiv;
    property vlrExcRecBruta: double read FvlrExcRecBruta write FvlrExcRecBruta;
    property vlrAdicRecBruta: double read FvlrAdicRecBruta write FvlrAdicRecBruta;
    property vlrBcCPRB: double read FvlrBcCPRB write FvlrBcCPRB;
    property vlrCPRBapur: double read FvlrCPRBapur write FvlrCPRBapur;
    property tipoAjuste: TtipoAjusteCollection read FtipoAjuste write FtipoAjuste;
    property infoProc: TinfoProcCollection read FinfoProc write FinfoProc;
  end;

  TtipoAjusteCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TtipoAjusteCollectionItem;
    procedure SetItem(Index: Integer; Value: TtipoAjusteCollectionItem);
  public
    function Add: TtipoAjusteCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TtipoAjusteCollectionItem;

    property Items[Index: Integer]: TtipoAjusteCollectionItem read GetItem write SetItem; default;
  end;

  TtipoAjusteCollectionItem = class(TObject)
  private
    FtpAjuste: TtpAjuste;
    FcodAjuste: TcodAjuste;
    FvlrAjuste: double;
    FdescAjuste: string;
    FdtAjuste: string;
  public
    property tpAjuste: TtpAjuste read FtpAjuste write FtpAjuste;
    property codAjuste: TcodAjuste read FcodAjuste write FcodAjuste;
    property vlrAjuste: double read FvlrAjuste write FvlrAjuste;
    property descAjuste: string read FdescAjuste write FdescAjuste;
    property dtAjuste: string read FdtAjuste write FdtAjuste;
  end;

  TinfoProcCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TinfoProcCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfoProcCollectionItem);
  public
    function Add: TinfoProcCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TinfoProcCollectionItem;

    property Items[Index: Integer]: TinfoProcCollectionItem read GetItem write SetItem; default;
  end;

  TinfoProcCollectionItem = class(TObject)
  private
    FtpProc: TtpProc;
    FnrProc: String;
    FcodSusp: String;
    FvlrCPRBSusp: double;
  public
    property tpProc: TtpProc read FtpProc write FtpProc;
    property nrProc: String read FnrProc write FnrProc;
    property codSusp: String read FcodSusp write FcodSusp;
    property vlrCPRBSusp: double read FvlrCPRBSusp write FvlrCPRBSusp;
  end;

implementation

uses
  IniFiles,
  ACBrReinf, ACBrDFeUtil;

{ TR2060Collection }

function TR2060Collection.Add: TR2060CollectionItem;
begin
  Result := Self.New;
end;

function TR2060Collection.GetItem(Index: Integer): TR2060CollectionItem;
begin
  Result := TR2060CollectionItem(inherited Items[Index]);
end;

function TR2060Collection.New: TR2060CollectionItem;
begin
  Result := TR2060CollectionItem.Create(FACBrReinf);
  Self.Add(Result);
end;

procedure TR2060Collection.SetItem(Index: Integer; Value: TR2060CollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TR2060CollectionItem }

constructor TR2060CollectionItem.Create(AOwner: TComponent);
begin
  inherited Create;

  FTipoEvento := teR2060;
  FevtCPRB    := TevtCPRB.Create(AOwner);
end;

destructor TR2060CollectionItem.Destroy;
begin
  inherited;

  FevtCPRB.Free;
end;

{ TevtCPRB }

constructor TevtCPRB.Create(AACBrReinf: TObject);
begin
  inherited Create(AACBrReinf);

  FideContri := TideContri.Create;
  FIdeEvento := TIdeEvento2.Create;
  FinfoCPRB  := TinfoCPRB.Create;
end;

destructor TevtCPRB.Destroy;
begin
  FideContri.Free;
  FIdeEvento.Free;
  FinfoCPRB.Free;

  inherited;
end;

{ TinfoCPRB }

constructor TinfoCPRB.Create;
begin
  FideEstab := TideEstab.Create;
end;

destructor TinfoCPRB.Destroy;
begin
  FideEstab.Free;

  inherited;
end;

{ TideEstab }

constructor TideEstab.Create;
begin
  FtipoCod := TtipoCodCollection.Create;
end;

destructor TideEstab.Destroy;
begin
  FtipoCod.Free;

  inherited;
end;

{ TtipoCodCollection }

function TtipoCodCollection.Add: TtipoCodCollectionItem;
begin
  Result := Self.New;
end;

function TtipoCodCollection.GetItem(
  Index: Integer): TtipoCodCollectionItem;
begin
  Result := TtipoCodCollectionItem(inherited Items[Index]);
end;

function TtipoCodCollection.New: TtipoCodCollectionItem;
begin
  Result := TtipoCodCollectionItem.Create;
  Self.Add(Result);
end;

procedure TtipoCodCollection.SetItem(Index: Integer;
  Value: TtipoCodCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TtipoCodCollectionItem }

constructor TtipoCodCollectionItem.Create;
begin
  FtipoAjuste := TtipoAjusteCollection.Create;
  FinfoProc   := TinfoProcCollection.Create;
end;

destructor TtipoCodCollectionItem.Destroy;
begin
  FtipoAjuste.Free;
  FinfoProc.Free;

  inherited;
end;

{ TtipoAjusteCollection }

function TtipoAjusteCollection.Add: TtipoAjusteCollectionItem;
begin
  Result := Self.New;
end;

function TtipoAjusteCollection.GetItem(
  Index: Integer): TtipoAjusteCollectionItem;
begin
  Result := TtipoAjusteCollectionItem(inherited Items[Index]);
end;

function TtipoAjusteCollection.New: TtipoAjusteCollectionItem;
begin
  Result := TtipoAjusteCollectionItem.Create;
  Self.Add(Result);
end;

procedure TtipoAjusteCollection.SetItem(Index: Integer;
  Value: TtipoAjusteCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TinfoProcCollection }

function TinfoProcCollection.Add: TinfoProcCollectionItem;
begin
  Result := Self.New;
end;

function TinfoProcCollection.GetItem(
  Index: Integer): TinfoProcCollectionItem;
begin
  Result := TinfoProcCollectionItem(inherited Items[Index]);
end;

function TinfoProcCollection.New: TinfoProcCollectionItem;
begin
  Result := TinfoProcCollectionItem.Create;
  Self.Add(Result);
end;

procedure TinfoProcCollection.SetItem(Index: Integer;
  Value: TinfoProcCollectionItem);
begin
  inherited Items[Index] := Value;
end;

procedure TevtCPRB.GerarideEstab;
begin
  Gerador.wGrupo('ideEstab');

  Gerador.wCampo(tcStr, '', 'tpInscEstab',      1,  1, 1, TpInscricaoToStr(Self.FinfoCPRB.ideEstab.tpInscEstab));
  Gerador.wCampo(tcStr, '', 'nrInscEstab',      1, 14, 1, Self.FinfoCPRB.ideEstab.nrInscEstab);
  Gerador.wCampo(tcDe2, '', 'vlrRecBrutaTotal', 1, 14, 1, Self.FinfoCPRB.ideEstab.vlrRecBrutaTotal);
  Gerador.wCampo(tcDe2, '', 'vlrCPApurTotal',   1, 14, 1, Self.FinfoCPRB.ideEstab.vlrCPApurTotal);
  Gerador.wCampo(tcDe2, '', 'vlrCPRBSuspTotal', 1, 14, 0, Self.FinfoCPRB.ideEstab.vlrCPRBSuspTotal);

  GerartipoCod(Self.FinfoCPRB.ideEstab.tipoCod);

  Gerador.wGrupo('/ideEstab');
end;

procedure TevtCPRB.GerartipoCod(Lista: TtipoCodCollection);
var
  item: TtipoCodCollectionItem;
  i: Integer;
begin
  for i := 0 to Lista.Count - 1 do
  begin
    Item := Lista.Items[i];

    Gerador.wGrupo('tipoCod');

    Gerador.wCampo(tcStr, '', 'codAtivEcon',     1,  8, 1, item.codAtivEcon);
    Gerador.wCampo(tcDe2, '', 'vlrRecBrutaAtiv', 1, 14, 1, item.vlrRecBrutaAtiv);
    Gerador.wCampo(tcDe2, '', 'vlrExcRecBruta',  1, 14, 1, item.vlrExcRecBruta);
    Gerador.wCampo(tcDe2, '', 'vlrAdicRecBruta', 1, 14, 1, item.vlrAdicRecBruta);
    Gerador.wCampo(tcDe2, '', 'vlrBcCPRB',       1, 14, 1, item.vlrBcCPRB);
    Gerador.wCampo(tcDe2, '', 'vlrCPRBapur',     1, 14, 0, item.vlrCPRBapur);

    GerartipoAjuste(item.tipoAjuste);
    GerarinfoProc(item.infoProc);

    Gerador.wGrupo('/tipoCod');
  end;

  if Lista.Count > 999 then
    Gerador.wAlerta('', 'tipoCod', 'Lista de Tipos de Códigos de Atividade', ERR_MSG_MAIOR_MAXIMO + '999');
end;

procedure TevtCPRB.GerartipoAjuste(Lista: TtipoAjusteCollection);
var
  item: TtipoAjusteCollectionItem;
  i: Integer;
begin
  for i := 0 to Lista.Count - 1 do
  begin
    Item := Lista.Items[i];

    Gerador.wGrupo('tipoAjuste');

    Gerador.wCampo(tcStr, '', 'tpAjuste',   1,  1, 1, tpAjusteToStr(item.tpAjuste));
    Gerador.wCampo(tcStr, '', 'codAjuste',  1,  2, 1, codAjusteToStr(item.codAjuste));
    Gerador.wCampo(tcDe2, '', 'vlrAjuste',  1, 14, 1, item.vlrAjuste);
    Gerador.wCampo(tcStr, '', 'descAjuste', 1, 20, 1, item.descAjuste);
    Gerador.wCampo(tcStr, '', 'dtAjuste',   7,  7, 1, item.dtAjuste);

    Gerador.wGrupo('/tipoAjuste');
  end;

  if Lista.Count > 999 then
    Gerador.wAlerta('', 'tipoAjuste', 'Lista de Tipos de Ajustes', ERR_MSG_MAIOR_MAXIMO + '999');
end;

procedure TevtCPRB.GerarinfoProc(Lista: TinfoProcCollection);
var
  item: TinfoProcCollectionItem;
  i: Integer;
begin
  for i := 0 to Lista.Count - 1 do
  begin
    Item := Lista.Items[i];

    Gerador.wGrupo('infoProc');

    Gerador.wCampo(tcStr, '', 'tpProc',      1,  1, 1, TpProcToStr( item.tpProc ));
    Gerador.wCampo(tcStr, '', 'nrProc',      1, 21, 1, item.nrProc);
    Gerador.wCampo(tcStr, '', 'codSusp',     1, 14, 0, item.codSusp);
    Gerador.wCampo(tcDe2, '', 'vlrCPRBSusp', 1, 14, 1, item.vlrCPRBSusp);

    Gerador.wGrupo('/infoProc');
  end;

  if Lista.Count > 50 then
    Gerador.wAlerta('', 'infoProc', 'Lista de Informações de Processos', ERR_MSG_MAIOR_MAXIMO + '50');
end;

function TevtCPRB.GerarXML: Boolean;
begin
  try
    Self.VersaoDF := TACBrReinf(FACBrReinf).Configuracoes.Geral.VersaoDF;

    Self.Id := GerarChaveReinf(now, self.ideContri.NrInsc, self.Sequencial, self.ideContri.TpInsc);

    GerarCabecalho('evtInfoCPRB');
    Gerador.wGrupo('evtCPRB id="' + Self.Id + '"');

    GerarIdeEvento2(Self.IdeEvento);
    GerarideContri(Self.ideContri);

    Gerador.wGrupo('infoCPRB');

    GerarideEstab;

    Gerador.wGrupo('/infoCPRB');
    Gerador.wGrupo('/evtCPRB');

    GerarRodape;

    FXML := Gerador.ArquivoFormatoXML;
//    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtCPRB');

//    Validar(schevtInfoCPRB);
  except on e:exception do
    raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '');
end;

function TevtCPRB.LerArqIni(const AIniString: String): Boolean;
var
  INIRec: TMemIniFile;
  Ok: Boolean;
  sSecao, sFim: String;
  I, J: Integer;
begin
  Result := True;

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);

    with Self do
    begin
      sSecao := 'evtCPRB';
      Id         := INIRec.ReadString(sSecao, 'Id', '');
      Sequencial := INIRec.ReadInteger(sSecao, 'Sequencial', 0);

      sSecao := 'ideEvento';
      ideEvento.indRetif := StrToIndRetificacao(Ok, INIRec.ReadString(sSecao, 'indRetif', '1'));
      ideEvento.NrRecibo := INIRec.ReadString(sSecao, 'nrRecibo', EmptyStr);
      ideEvento.perApur  := INIRec.ReadString(sSecao, 'perApur', EmptyStr);
      ideEvento.ProcEmi  := StrToProcEmiReinf(Ok, INIRec.ReadString(sSecao, 'procEmi', '1'));
      ideEvento.VerProc  := INIRec.ReadString(sSecao, 'verProc', EmptyStr);

      sSecao := 'ideContri';
      ideContri.OrgaoPublico := (TACBrReinf(FACBrReinf).Configuracoes.Geral.TipoContribuinte = tcOrgaoPublico);
      ideContri.TpInsc       := StrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
      ideContri.NrInsc       := INIRec.ReadString(sSecao, 'nrInsc', EmptyStr);

      sSecao := 'ideEstab';
      infoCPRB.ideEstab.tpInscEstab       := StrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInscEstab', '1'));
      infoCPRB.ideEstab.nrInscEstab       := INIRec.ReadString(sSecao, 'nrInscEstab', EmptyStr);
      infoCPRB.ideEstab.vlrRecBrutaTotal  := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrRecBrutaTotal', ''), 0);
      infoCPRB.ideEstab.vlrCPApurTotal    := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrCPApurTotal', ''), 0);
      infoCPRB.ideEstab.vlrCPRBSuspTotal  := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrCPRBSuspTotal', ''), 0);

      with infoCPRB.ideEstab do
      begin
        I := 1;
        while true do
        begin
          // de 001 até 999
          sSecao := 'tipoCod' + IntToStrZero(I, 3);
          sFim   := INIRec.ReadString(sSecao, 'codAtivEcon', 'FIM');

          if (sFim = 'FIM') or (Length(sFim) <= 0) then
            break;

          with tipoCod.New do
          begin
            codAtivEcon := sFim;
            vlrRecBrutaAtiv := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrRecBrutaAtiv', ''), 0);
            vlrExcRecBruta  := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrExcRecBruta', ''), 0);
            vlrAdicRecBruta := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrAdicRecBruta', ''), 0);
            vlrBcCPRB       := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrBcCPRB', ''), 0);
            vlrCPRBapur     := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrCPRBapur', ''), 0);

            J := 1;
            while true do
            begin
              // de 001 até 999
              sSecao := 'tipoAjuste' + IntToStrZero(I, 3) + IntToStrZero(J, 3);
              sFim   := INIRec.ReadString(sSecao, 'tpAjuste', 'FIM');

              if (sFim = 'FIM') or (Length(sFim) <= 0) then
                break;

              with tipoAjuste.New do
              begin
                tpAjuste   := StrTotpAjuste(Ok, sFim);
                codAjuste  := StrTocodAjuste(Ok, INIRec.ReadString(sSecao, 'codAjuste', ''));
                vlrAjuste  := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrAjuste', ''), 0);
                descAjuste := INIRec.ReadString(sSecao, 'descAjuste', '');
                dtAjuste   := INIRec.ReadString(sSecao, 'dtAjuste', '');
              end;

              Inc(J);
            end;

            J := 1;
            while true do
            begin
              // de 00 até 50
              sSecao := 'infoProc' + IntToStrZero(I, 3) + IntToStrZero(J, 2);
              sFim   := INIRec.ReadString(sSecao, 'tpProc', 'FIM');

              if (sFim = 'FIM') or (Length(sFim) <= 0) then
                break;

              with infoProc.New do
              begin
                tpProc      := StrToTpProc(Ok, sFim);
                nrProc      := INIRec.ReadString(sSecao, 'nrProc', '');
                codSusp     := INIRec.ReadString(sSecao, 'codSusp', '');
                vlrCPRBSusp := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrCPRBSusp', ''), 0);
              end;

              Inc(J);
            end;
          end;

          Inc(I);
        end;
      end;
    end;

    GerarXML;
    XML := FXML;
  finally
    INIRec.Free;
  end;
end;

end.
