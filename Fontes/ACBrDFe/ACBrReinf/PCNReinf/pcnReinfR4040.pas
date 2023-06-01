{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Renato Tanchela Rubinho                         }
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

unit pcnReinfR4040;

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

  TR4040Collection = class;
  TR4040CollectionItem = class;
  TevtBenefNId = class;
  TideEstab = class;
  TideNatCollection = class;
  TideNatCollectionItem = class;
  TinfoPgtoCollection = class;
  TinfoPgtoCollectionItem = class;
  TinfoProcRetCollection = class;
  TinfoProcRetCollectionItem = class;

  { TR4040Collection }
  TR4040Collection = class(TReinfCollection)
  private
    function GetItem(Index: Integer): TR4040CollectionItem;
    procedure SetItem(Index: Integer; Value: TR4040CollectionItem);
  public
    function Add: TR4040CollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TR4040CollectionItem;

    property Items[Index: Integer]: TR4040CollectionItem read GetItem write SetItem; default;
  end;

  { TR4040CollectionItem }
  TR4040CollectionItem = class(TObject)
  private
    FTipoEvento: TTipoEvento;
    FevtBenefNId: TevtBenefNId;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    property TipoEvento: TTipoEvento read FTipoEvento;
    property evtBenefNId: TevtBenefNId read FevtBenefNId write FevtBenefNId;
  end;

  { TevtBenefNId }
  TevtBenefNId = class(TReinfEvento) //Classe do elemento principal do XML do evento!
  private
    FIdeEvento: TIdeEvento2;
    FideContri: TideContri;
    FinfoComplContri: TinfoComplContri;
    FideEstab: TideEstab;

    {Geradores específicos desta classe}
    procedure GerarideEstab;
    procedure GerarideNat(Lista: TideNatCollection);
    procedure GerarinfoPgto(Lista: TinfoPgtoCollection);
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
    FideEvtAdic: string;
    FideNat: TideNatCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property tpInscEstab: TtpInsc read FtpInscEstab write FtpInscEstab default tiCNPJ;
    property nrInscEstab: string read FnrInscEstab write FnrInscEstab;
    property ideEvtAdic: string read FideEvtAdic write FideEvtAdic;
    property ideNat: TideNatCollection read FideNat write FideNat;
  end;

  { TideNatCollection }
  TideNatCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TideNatCollectionItem;
    procedure SetItem(Index: Integer; Value: TideNatCollectionItem);
  public
    function Add: TideNatCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TideNatCollectionItem;

    property Items[Index: Integer]: TideNatCollectionItem read GetItem write SetItem; default;
  end;

  { TideNatCollectionItem }
  TideNatCollectionItem = class(TObject)
  private
    FnatRend: string;
    FinfoPgto: TinfoPgtoCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property natRend: string read FnatRend write FnatRend;
    property infoPgto: TinfoPgtoCollection read FinfoPgto write FinfoPgto;
  end;

  { TinfoPgtoCollection }
  TinfoPgtoCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TinfoPgtoCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfoPgtoCollectionItem);
  public
    function Add: TinfoPgtoCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TinfoPgtoCollectionItem;

    property Items[Index: Integer]: TinfoPgtoCollectionItem read GetItem write SetItem; default;
  end;

  { TinfoPgtoCollectionItem }
  TinfoPgtoCollectionItem = class(TObject)
  private
    FdtFG: TDateTime;
    FvlrLiq: double;
    FvlrBaseIR: double;
    FvlrIR: double;
    FdtEscrCont: TDateTime;
    Fdescr: string;
    FinfoProcRet: TinfoProcRetCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property dtFG: TDateTime read FdtFG write FdtFG;
    property vlrLiq: double read FvlrLiq write FvlrLiq;
    property vlrBaseIR: double read FvlrBaseIR write FvlrBaseIR;
    property vlrIR: double read FvlrIR write FvlrIR;
    property dtEscrCont: TDateTime read FdtEscrCont write FdtEscrCont;
    property descr: string read Fdescr write Fdescr;
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

{ TR4040Collection }

function TR4040Collection.Add: TR4040CollectionItem;
begin
  Result := Self.New;
end;

function TR4040Collection.GetItem(Index: Integer): TR4040CollectionItem;
begin
  Result := TR4040CollectionItem(inherited Items[Index]);
end;

function TR4040Collection.New: TR4040CollectionItem;
begin
  Result := TR4040CollectionItem.Create(FACBrReinf);
  Self.Add(Result);
end;

procedure TR4040Collection.SetItem(Index: Integer; Value: TR4040CollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TR4040CollectionItem }

constructor TR4040CollectionItem.Create(AOwner: TComponent);
begin
  inherited Create;

  FTipoEvento := teR4040;
  FevtBenefNId := TevtBenefNId.Create(AOwner);
end;

destructor TR4040CollectionItem.Destroy;
begin
  inherited;

  FevtBenefNId.Free;
end;

{ TevtBenefNId }

constructor TevtBenefNId.Create(AACBrReinf: TObject);
begin
  inherited Create(AACBrReinf);

  FideContri       := TideContri.Create;
  FIdeEvento       := TIdeEvento2.Create;
  FinfoComplContri := TinfoComplContri.Create;
  FideEstab        := TideEstab.Create;
end;

destructor TevtBenefNId.Destroy;
begin
  FideContri.Free;
  FIdeEvento.Free;
  FinfoComplContri.Free;
  FideEstab.Free;

  inherited;
end;

{ TideEstab }

constructor TideEstab.Create;
begin
  FideNat := TideNatCollection.Create;
end;

destructor TideEstab.Destroy;
begin
  FideNat.Free;

  inherited;
end;

{ TideNatCollectionItem }

constructor TideNatCollectionItem.Create;
begin
  FinfoPgto := TinfoPgtoCollection.Create;
end;

destructor TideNatCollectionItem.Destroy;
begin
  FinfoPgto.Free;

  inherited;
end;

procedure TevtBenefNId.GerarideEstab;
begin
  Gerador.wGrupo('ideEstab');

  Gerador.wCampo(tcStr, '', 'tpInscEstab', 1,  1, 1, TpInscricaoToStr(Self.ideEstab.tpInscEstab));
  Gerador.wCampo(tcStr, '', 'nrInscEstab', 1, 14, 1, Self.ideEstab.nrInscEstab);

  if TACBrReinf(FACBrReinf).Configuracoes.Geral.VersaoDF >= v2_01_02 then
    Gerador.wCampo(tcStr, '', 'ideEvtAdic', 1, 8, 0, Self.ideEstab.ideEvtAdic);

  GerarideNat(Self.ideEstab.ideNat);

  Gerador.wGrupo('/ideEstab');
end;

procedure TevtBenefNId.GerarideNat(Lista: TideNatCollection);
var
  item: TideNatCollectionItem;
  i: Integer;
begin
  for i := 0 to Lista.Count - 1 do
  begin
    Item := Lista.Items[i];

    Gerador.wGrupo('ideNat');
    Gerador.wCampo(tcStr, '', 'natRend',  5,   5, 1, Item.natRend);

    GerarinfoPgto(Item.infoPgto);

    Gerador.wGrupo('/ideNat');
  end;

  if Lista.Count > 2 then
    Gerador.wAlerta('', 'ideNat',
                    'Informações de pagamentos a beneficiários não indentificados',
                    ERR_MSG_MAIOR_MAXIMO + '31');
end;

procedure TevtBenefNId.GerarinfoPgto(Lista: TinfoPgtoCollection);
var
  item: TinfoPgtoCollectionItem;
  i: Integer;
begin
  for i := 0 to Lista.Count - 1 do
  begin
    Item := Lista.Items[i];

    Gerador.wGrupo('infoPgto');
    Gerador.wCampo(tcDat, '', 'dtFG',       10,  10,  1, item.dtFG);
    Gerador.wCampo(tcDe2, '', 'vlrLiq',      1,  14,  0, item.vlrLiq);
    Gerador.wCampo(tcDe2, '', 'vlrBaseIR',   1,  14,  1, item.vlrBaseIR);
    Gerador.wCampo(tcDe2, '', 'vlrIR',       1,  14,  0, item.vlrIR);

    if TACBrReinf(FACBrReinf).Configuracoes.Geral.VersaoDF >= v2_01_02 then
      Gerador.wCampo(tcDat, '', 'dtEscrCont', 1,  10, 0, item.dtEscrCont);

    Gerador.wCampo(tcStr, '', 'descr',       1, 200,  1, item.descr);

    GerarinfoProcRet(item.infoProcRet);

    Gerador.wGrupo('/infoPgto');
  end;

  if Lista.Count > 31 then
    Gerador.wAlerta('', 'infoPgto',
                    'Informações de pagamentos a beneficiários não indentificados',
                    ERR_MSG_MAIOR_MAXIMO + '31');
end;

procedure TevtBenefNId.GerarinfoProcRet(Lista: TinfoProcRetCollection);
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

function TevtBenefNId.GerarXML: Boolean;
begin
  try
    Self.VersaoDF := TACBrReinf(FACBrReinf).Configuracoes.Geral.VersaoDF;

    Self.Id := GerarChaveReinf(now, self.ideContri.NrInsc, self.Sequencial, self.ideContri.TpInsc);

    GerarCabecalho('evt4040PagtoBenefNaoIdentificado');
    Gerador.wGrupo('evtBenefNId id="' + Self.Id + '"');

    GerarIdeEvento2(Self.IdeEvento);
    GerarideContri(Self.ideContri);

    GerarideEstab;

    Gerador.wGrupo('/evtBenefNId');

    GerarRodape;

    FXML := Gerador.ArquivoFormatoXML;
  except on e:exception do
    raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '');
end;

function TevtBenefNId.LerArqIni(const AIniString: String): Boolean;
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
      sSecao := 'evtBenefNId';
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
      if TACBrReinf(FACBrReinf).Configuracoes.Geral.VersaoDF >= v2_01_02 then
        ideEstab.ideEvtAdic := INIRec.ReadString(sSecao, 'ideEvtAdic', EmptyStr);

      with ideEstab do
      begin
        I := 1;
        while true do
        begin
          // de 01 até 2
          sSecao := 'ideNat' + IntToStrZero(I, 3);
          sFim   := INIRec.ReadString(sSecao, 'natRend', 'FIM');

          if (sFim = 'FIM') or (Length(sFim) <= 0) then
            break;

          with ideNat.New do
          begin
            natRend := sFim;

            I2 := 1;
            while true do
            begin
              // de 1 até 31
              sSecao := 'infoPgto' + IntToStrZero(I, 3) + IntToStrZero(I2, 3);
              sFim   := INIRec.ReadString(sSecao, 'dtFG', 'FIM');

              if (sFim = 'FIM') or (Length(sFim) <= 0) then
                break;

              with infoPgto.New do
              begin
                dtFG      := StringToDateTime(sFim);
                vlrLiq    := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrLiq', ''), 0);
                vlrBaseIR := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrBaseIR', ''), 0);
                vlrIR     := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrIR', ''), 0);

                if TACBrReinf(FACBrReinf).Configuracoes.Geral.VersaoDF >= v2_01_02 then
                  dtEscrCont := StringToDateTime(INIRec.ReadString(sSecao, 'dtEscrCont', ''));

                descr     := INIRec.ReadString(sSecao, 'descr', '');

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

{ TideNatCollection }

function TideNatCollection.Add: TideNatCollectionItem;
begin
  Result := Self.New;
end;

function TideNatCollection.GetItem(Index: Integer): TideNatCollectionItem;
begin
  Result := TideNatCollectionItem(inherited Items[Index]);
end;

function TideNatCollection.New: TideNatCollectionItem;
begin
  Result := TideNatCollectionItem.Create;
  Self.Add(Result);
end;

procedure TideNatCollection.SetItem(Index: Integer;
  Value: TideNatCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TinfoPgtoCollection }

function TinfoPgtoCollection.Add: TinfoPgtoCollectionItem;
begin
  Result := Self.New;
end;

function TinfoPgtoCollection.GetItem(
  Index: Integer): TinfoPgtoCollectionItem;
begin
  Result := TinfoPgtoCollectionItem(inherited Items[Index]);
end;

function TinfoPgtoCollection.New: TinfoPgtoCollectionItem;
begin
  Result := TinfoPgtoCollectionItem.Create;
  Self.Add(Result);
end;

procedure TinfoPgtoCollection.SetItem(Index: Integer;
  Value: TinfoPgtoCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TinfoPgtoCollectionItem }

constructor TinfoPgtoCollectionItem.Create;
begin
  FinfoProcRet := TinfoProcRetCollection.Create;
end;

destructor TinfoPgtoCollectionItem.Destroy;
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

