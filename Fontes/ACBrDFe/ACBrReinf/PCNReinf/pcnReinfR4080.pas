{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Renato Rubinho                                  }
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

unit pcnReinfR4080;

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
  ACBrUtil.Base, ACBrUtil.FilesIO, ACBrUtil.DateTime,
  pcnConsts,
  pcnCommonReinf, pcnConversaoReinf, pcnGeradorReinf;

type
  {Classes específicas deste evento}

  TR4080Collection = class;
  TR4080CollectionItem = class;
  TevtRetRec = class;
  TideEstab = class;
  TideFont = class;
  TideRendCollection = class;
  TideRendCollectionItem = class;
  TinfoRecCollection = class;
  TinfoRecCollectionItem = class;
  TinfoProcRetCollection = class;
  TinfoProcRetCollectionItem = class;

  { TR4080Collection }
  TR4080Collection = class(TReinfCollection)
  private
    function GetItem(Index: Integer): TR4080CollectionItem;
    procedure SetItem(Index: Integer; Value: TR4080CollectionItem);
  public
    function Add: TR4080CollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TR4080CollectionItem;

    property Items[Index: Integer]: TR4080CollectionItem read GetItem write SetItem; default;
  end;

  { TR4080CollectionItem }
  TR4080CollectionItem = class(TObject)
  private
    FTipoEvento: TTipoEvento;
    FevtRetRec: TevtRetRec;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    property TipoEvento: TTipoEvento read FTipoEvento;
    property evtRetRec: TevtRetRec read FevtRetRec write FevtRetRec;
  end;

  { TevtRetRec }
  TevtRetRec = class(TReinfEvento) //Classe do elemento principal do XML do evento!
  private
    FIdeEvento: TIdeEvento2;
    FideContri: TideContri;
    FideEstab: TideEstab;

    {Geradores específicos desta classe}
    procedure GerarideEstab;
    procedure GerarideFont;
    procedure GerarideRend(Lista: TideRendCollection);
    procedure GerarinfoRec(Lista: TinfoRecCollection);
    procedure GerarinfoProcRet(Lista: TinfoProcRetCollection);
  public
    constructor Create(AACBrReinf: TObject); override;
    destructor  Destroy; override;

    function GerarXML: Boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property ideEvento: TIdeEvento2 read FIdeEvento write FIdeEvento;
    property ideContri: TideContri read FideContri write FideContri;
    property ideEstab: TideEstab read FideEstab write FideEstab;
  end;

  { TideEstab }
  TideEstab = class(TObject)
  private
    FtpInscEstab: TtpInsc;
    FnrInscEstab: string;

    FideFont: TideFont;
  public
    constructor Create;
    destructor Destroy; override;

    property tpInscEstab: TtpInsc read FtpInscEstab write FtpInscEstab default tiCNPJ;
    property nrInscEstab: string read FnrInscEstab write FnrInscEstab;
    property ideFont: TideFont read FideFont write FideFont;
  end;

  { TideFont }
  TideFont = class(TObject)
  private
    FcnpjFont: string;
    FideRend: TideRendCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property cnpjFont: string read FcnpjFont write FcnpjFont;
    property ideRend: TideRendCollection read FideRend write FideRend;
  end;

  { TideRendCollection }
  TideRendCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TideRendCollectionItem;
    procedure SetItem(Index: Integer; Value: TideRendCollectionItem);
  public
    function Add: TideRendCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TideRendCollectionItem;

    property Items[Index: Integer]: TideRendCollectionItem read GetItem write SetItem; default;
  end;

  { TideRendCollectionItem }
  TideRendCollectionItem = class(TObject)
  private
    FnatRend: string;
    Fobserv: string;
    FinfoRec: TinfoRecCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property natRend: string read FnatRend write FnatRend;
    property observ: string read Fobserv write Fobserv;
    property infoRec: TinfoRecCollection read FinfoRec write FinfoRec;
  end;

  { TinfoRecCollection }
  TinfoRecCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TinfoRecCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfoRecCollectionItem);
  public
    function Add: TinfoRecCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TinfoRecCollectionItem;

    property Items[Index: Integer]: TinfoRecCollectionItem read GetItem write SetItem; default;
  end;

  { TinfoRecCollectionItem }
  TinfoRecCollectionItem = class(TObject)
  private
    FdtFG: TDateTime;
    FvlrBruto: double;
    FvlrBaseIR: double;
    FvlrIR: double;
    Fobserv: string;
    FinfoProcRet: TinfoProcRetCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property dtFG: TDateTime read FdtFG write FdtFG;
    property vlrBruto: double read FvlrBruto write FvlrBruto;
    property vlrBaseIR: double read FvlrBaseIR write FvlrBaseIR;
    property vlrIR: double read FvlrIR write FvlrIR;
    property observ: string read Fobserv write Fobserv;
    property infoProcRet: TinfoProcRetCollection read FinfoProcRet write FinfoProcRet;
  end;

  { TinfoProcRetCollection }
  TinfoProcRetCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TinfoProcRetCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfoProcRetCollectionItem);
  public
    function Add: TinfoProcRetCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TinfoProcRetCollectionItem;

    property Items[Index: Integer]: TinfoProcRetCollectionItem read GetItem write SetItem; default;
  end;

  { TinfoProcRetCollectionItem }
  TinfoProcRetCollectionItem = class(TObject)
  private
    FtpProcRet: TtpProc;
    FnrProcRet: string;
    FcodSusp: string;
    FvlrBaseSuspIR: double;
    FvlrNIR: double;
    FvlrDepIR: double;
  public
    property tpProcRet: TtpProc read FtpProcRet write FtpProcRet;
    property nrProcRet: string read FnrProcRet write FnrProcRet;
    property codSusp: string read FcodSusp write FcodSusp;
    property vlrBaseSuspIR: double read FvlrBaseSuspIR write FvlrBaseSuspIR;
    property vlrNIR: double read FvlrNIR write FvlrNIR;
    property vlrDepIR: double read FvlrDepIR write FvlrDepIR;
  end;
  
implementation

uses
  IniFiles,
  ACBrReinf, ACBrDFeUtil;

{ TR4080Collection }

function TR4080Collection.Add: TR4080CollectionItem;
begin
  Result := Self.New;
end;

function TR4080Collection.GetItem(Index: Integer): TR4080CollectionItem;
begin
  Result := TR4080CollectionItem(inherited Items[Index]);
end;

function TR4080Collection.New: TR4080CollectionItem;
begin
  Result := TR4080CollectionItem.Create(FACBrReinf);
  Self.Add(Result);
end;

procedure TR4080Collection.SetItem(Index: Integer; Value: TR4080CollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TR4080CollectionItem }

constructor TR4080CollectionItem.Create(AOwner: TComponent);
begin
  inherited Create;

  FTipoEvento := teR4080;
  FevtRetRec := TevtRetRec.Create(AOwner);
end;

destructor TR4080CollectionItem.Destroy;
begin
  inherited;

  FevtRetRec.Free;
end;

{ TevtRetRec }

constructor TevtRetRec.Create(AACBrReinf: TObject);
begin
  inherited Create(AACBrReinf);

  FideContri       := TideContri.Create;
  FIdeEvento       := TIdeEvento2.Create;
  FideEstab        := TideEstab.Create;
end;

destructor TevtRetRec.Destroy;
begin
  FideContri.Free;
  FIdeEvento.Free;
  FideEstab.Free;

  inherited;
end;

{ TideEstab }

constructor TideEstab.Create;
begin
  FideFont := TideFont.Create;
end;

destructor TideEstab.Destroy;
begin
  FideFont.Free;

  inherited;
end;

{ TideFont }

constructor TideFont.Create;
begin
  FideRend := TideRendCollection.Create;
end;

destructor TideFont.Destroy;
begin
  FideRend.Free;

  inherited;
end;

procedure TevtRetRec.GerarideEstab;
begin
  Gerador.wGrupo('ideEstab');

  Gerador.wCampo(tcStr, '', 'tpInscEstab', 1,  1, 1, TpInscricaoToStr(Self.ideEstab.tpInscEstab));
  Gerador.wCampo(tcStr, '', 'nrInscEstab', 1, 14, 1, Self.ideEstab.nrInscEstab);

  GerarideFont;

  Gerador.wGrupo('/ideEstab');
end;

procedure TevtRetRec.GerarideFont;
begin
  Gerador.wGrupo('ideFont');

  with Self.ideEstab do
  begin
    Gerador.wCampo(tcStr, '', 'cnpjFont', 14, 14, 1, ideFont.cnpjFont);

    GerarideRend(ideFont.ideRend);
  end;

  Gerador.wGrupo('/ideFont');
end;

procedure TevtRetRec.GerarideRend(Lista: TideRendCollection);
var
  item: TideRendCollectionItem;
  i: Integer;
begin
  for i := 0 to Lista.Count - 1 do
  begin
    Item := Lista.Items[i];

    Gerador.wGrupo('ideRend');

    Gerador.wCampo(tcStr, '', 'natRend',  5,   5, 1, item.natRend);
    Gerador.wCampo(tcStr, '', 'observ',   1, 200, 0, item.observ);

    GerarinfoRec(item.infoRec);

    Gerador.wGrupo('/ideRend');
  end;

  if Lista.Count > 100 then
    Gerador.wAlerta('', 'ideRend', 'Identificação do rendimento', ERR_MSG_MAIOR_MAXIMO + '100');
end;

procedure TevtRetRec.GerarinfoRec(Lista: TinfoRecCollection);
var
  item: TinfoRecCollectionItem;
  i: Integer;
begin
  for i := 0 to Lista.Count - 1 do
  begin
    Item := Lista.Items[i];

    Gerador.wGrupo('infoRec');
    Gerador.wCampo(tcDat, '', 'dtFG',      10,  10, 1, item.dtFG);
    Gerador.wCampo(tcDe2, '', 'vlrBruto',   1,  14, 1, item.vlrBruto);
    Gerador.wCampo(tcDe2, '', 'vlrBaseIR',  1,  14, 1, item.vlrBaseIR);
    Gerador.wCampo(tcDe2, '', 'vlrIR',      1,  14, 1, item.vlrIR);
    if TACBrReinf(FACBrReinf).Configuracoes.Geral.VersaoDF >= v2_01_02 then
      Gerador.wCampo(tcStr, '', 'observ',1, 200, 0, item.observ);

    GerarinfoProcRet(item.infoProcRet);
    Gerador.wGrupo('/infoRec');
  end;

  if Lista.Count > 999 then
    Gerador.wAlerta('', 'infoRec',
                    'Informações relativas ao recebimento do rendimento',
                    ERR_MSG_MAIOR_MAXIMO + '999');
end;

procedure TevtRetRec.GerarinfoProcRet(Lista: TinfoProcRetCollection);
var
  item: TinfoProcRetCollectionItem;
  i: Integer;
begin
  for i := 0 to Lista.Count - 1 do
  begin
    Item := Lista.Items[i];

    Gerador.wGrupo('infoProcRet');
    Gerador.wCampo(tcStr, '', 'tpProcRet',      1,    1, 1, TpProcToStr(item.tpProcRet));
    Gerador.wCampo(tcStr, '', 'nrProcRet',      1,   21, 1, item.nrProcRet);
    Gerador.wCampo(tcStr, '', 'codSusp',        1,   14, 0, item.codSusp);
    Gerador.wCampo(tcDe2, '', 'vlrBaseSuspIR',  1,   14, 0, item.vlrBaseSuspIR);
    Gerador.wCampo(tcDe2, '', 'vlrNIR',         1,   14, 0, item.vlrNIR);
    Gerador.wCampo(tcDe2, '', 'vlrDepIR',       1,   14, 0, item.vlrDepIR);
    Gerador.wGrupo('/infoProcRet');
  end;

  if Lista.Count > 50 then
    Gerador.wAlerta('', 'infoProcRet',
                    'Informações de processos relacionados a não retenção de tributos ou a depósitos judiciais',
                    ERR_MSG_MAIOR_MAXIMO + '50');
end;

function TevtRetRec.GerarXML: Boolean;
begin
  try
    Self.VersaoDF := TACBrReinf(FACBrReinf).Configuracoes.Geral.VersaoDF;

    Self.Id := GerarChaveReinf(now, self.ideContri.NrInsc, self.Sequencial, self.ideContri.TpInsc);

    GerarCabecalho('evt4080RetencaoRecebimento');
    Gerador.wGrupo('evtRetRec id="' + Self.Id + '"');

    GerarIdeEvento2(Self.IdeEvento);
    GerarideContri(Self.ideContri);

    GerarideEstab;

    Gerador.wGrupo('/evtRetRec');

    GerarRodape;

    FXML := Gerador.ArquivoFormatoXML;
  except on e:exception do
    raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '');
end;

function TevtRetRec.LerArqIni(const AIniString: String): Boolean;
var
  INIRec: TMemIniFile;
  Ok: Boolean;
  sSecao, sFim: String;
  I, I2, I3: Integer;
begin
  Result := True;

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);

    with Self do
    begin
      sSecao := 'evtRetRec';
      Id         := INIRec.ReadString(sSecao, 'Id', '');
      Sequencial := INIRec.ReadInteger(sSecao, 'Sequencial', 0);

      sSecao := 'ideEvento';
      ideEvento.indRetif := StrToIndRetificacao(Ok, INIRec.ReadString(sSecao, 'indRetif', '1'));
      ideEvento.NrRecibo := INIRec.ReadString(sSecao, 'nrRecibo', EmptyStr);
      ideEvento.perApur  := INIRec.ReadString(sSecao, 'perApur', EmptyStr);
      ideEvento.ProcEmi  := StrToProcEmiReinf(Ok, INIRec.ReadString(sSecao, 'procEmi', '1'));
      ideEvento.VerProc  := INIRec.ReadString(sSecao, 'verProc', EmptyStr);

      sSecao := 'ideContri';
      ideContri.TpInsc := StrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
      ideContri.NrInsc := INIRec.ReadString(sSecao, 'nrInsc', EmptyStr);

      sSecao := 'ideEstab';
      ideEstab.tpInscEstab := StrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInscEstab', '1'));
      ideEstab.nrInscEstab := INIRec.ReadString(sSecao, 'nrInscEstab', EmptyStr);

      with ideEstab do
      begin
        sSecao := 'ideFont';
        ideFont.cnpjFont := INIRec.ReadString(sSecao, 'cnpjFont', EmptyStr);

        I := 1;
        while true do
        begin
          // de 01 até 100
          sSecao := 'ideRend' + IntToStrZero(I, 3);
          sFim   := INIRec.ReadString(sSecao, 'natRend', 'FIM');

          if (sFim = 'FIM') or (Length(sFim) <= 0) then
            break;

          with ideFont.ideRend.New do
          begin
            natRend := sFim;
            observ  := INIRec.ReadString(sSecao, 'observ', '');

            I2 := 1;
            while true do
            begin
              // de 1 até 999
              sSecao := 'infoRec' + IntToStrZero(I, 3) + IntToStrZero(I2, 3);
              sFim   := INIRec.ReadString(sSecao, 'dtFG', 'FIM');

              if (sFim = 'FIM') or (Length(sFim) <= 0) then
                break;

              with infoRec.New do
              begin
                dtFG      := StringToDateTime(sFim);
                vlrBruto  := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrBruto', ''), 0);
                vlrBaseIR := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrBaseIR', ''), 0);
                vlrIR     := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrIR', ''), 0);
                if TACBrReinf(FACBrReinf).Configuracoes.Geral.VersaoDF >= v2_01_02 then
                  observ  := INIRec.ReadString(sSecao, 'observ', '');

                I3 := 1;
                while true do
                begin
                  // de 1 até 50
                  sSecao := 'infoProcRet' + IntToStrZero(I, 3) +
                                            IntToStrZero(I2, 3) +
                                            IntToStrZero(I3, 3);

                  sFim := INIRec.ReadString(sSecao, 'tpProcRet', 'FIM');

                  if (sFim = 'FIM') or (Length(sFim) <= 0) then
                    break;

                  with infoProcRet.New do
                  begin
                    tpProcRet     := StrToTpProc(Ok, sFim);
                    nrProcRet     := INIRec.ReadString(sSecao, 'nrProcRet', '');
                    codSusp       := INIRec.ReadString(sSecao, 'codSusp', '');
                    vlrBaseSuspIR := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrBaseSuspIR', ''), 0);
                    vlrNIR        := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrNIR', ''), 0);
                    vlrDepIR      := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrDepIR', ''), 0);
                  end;

                  Inc(I3);
                end;
              end;

              Inc(I2);
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

{ TideRendCollection }

function TideRendCollection.Add: TideRendCollectionItem;
begin
  Result := Self.New;
end;

function TideRendCollection.GetItem(
  Index: Integer): TideRendCollectionItem;
begin
  Result := TideRendCollectionItem(inherited Items[Index]);
end;

function TideRendCollection.New: TideRendCollectionItem;
begin
  Result := TideRendCollectionItem.Create;
  Self.Add(Result);
end;

procedure TideRendCollection.SetItem(Index: Integer;
  Value: TideRendCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TideRendCollectionItem }

constructor TideRendCollectionItem.Create;
begin
  FinfoRec := TinfoRecCollection.Create;
end;

destructor TideRendCollectionItem.Destroy;
begin
  FinfoRec.Free;

  inherited;
end;

{ TinfoRecCollection }

function TinfoRecCollection.Add: TinfoRecCollectionItem;
begin
  Result := Self.New;
end;

function TinfoRecCollection.GetItem(
  Index: Integer): TinfoRecCollectionItem;
begin
  Result := TinfoRecCollectionItem(inherited Items[Index]);
end;

function TinfoRecCollection.New: TinfoRecCollectionItem;
begin
  Result := TinfoRecCollectionItem.Create;
  Self.Add(Result);
end;

procedure TinfoRecCollection.SetItem(Index: Integer;
  Value: TinfoRecCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TinfoRecCollectionItem }

constructor TinfoRecCollectionItem.Create;
begin
  FinfoProcRet := TinfoProcRetCollection.Create;
end;

destructor TinfoRecCollectionItem.Destroy;
begin
  FinfoProcRet.Free;

  inherited;
end;

{ TinfoProcRetCollection }

function TinfoProcRetCollection.Add: TinfoProcRetCollectionItem;
begin
  Result := Self.New;
end;

function TinfoProcRetCollection.GetItem(
  Index: Integer): TinfoProcRetCollectionItem;
begin
  Result := TinfoProcRetCollectionItem(inherited Items[Index]);
end;

function TinfoProcRetCollection.New: TinfoProcRetCollectionItem;
begin
  Result := TinfoProcRetCollectionItem.Create;
  Self.Add(Result);
end;

procedure TinfoProcRetCollection.SetItem(Index: Integer;
  Value: TinfoProcRetCollectionItem);
begin
  inherited Items[Index] := Value;
end;

end.


