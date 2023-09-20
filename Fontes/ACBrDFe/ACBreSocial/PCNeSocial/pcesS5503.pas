{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
{                              Jean Carlo Cantu                                }
{                              Tiago Ravache                                   }
{                              Guilherme Costa                                 }
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

unit pcesS5503;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IfEnd}
  ACBrBase,
  pcnConversao, pcnLeitor,
  ACBrUtil.Base,
  ACBrUtil.FilesIO,
  ACBrUtil.DateTime,
  pcesCommon, pcesConversaoeSocial;

type
  TevtFGTSProcTrab = class;
  TideProc = class;
  TinfoTrabFGTSCollection = class;
  TinfoTrabFGTSCollectionItem = class;
  TinfoFGTSProcTrab = class;
  TideEstab = class;
  TbasePerRefCollection = class;
  TbasePerRefCollectionItem = class;

  TS5503 = class(TInterfacedObject, IEventoeSocial)
  private
    FTipoEvento: TTipoEvento;
    FevtFGTSProcTrab: TevtFGTSProcTrab;

    function GetXml : string;
    procedure SetXml(const Value: string);
    function GetTipoEvento : TTipoEvento;
  public
    constructor Create;
    destructor Destroy; override;

    function GetEvento : TObject;
    property Xml: String read GetXml write SetXml;
    property TipoEvento: TTipoEvento read GetTipoEvento;
    property evtFGTSProcTrab: TevtFGTSProcTrab read FevtFGTSProcTrab write FevtFGTSProcTrab;
  end;

  TevtFGTSProcTrab = class(TObject)
  private
    FLeitor: TLeitor;
    FId: String;
    FXML: String;
    FVersaoDF: TVersaoeSocial;
    FideEvento: TideEvento5;
    FideEmpregador: TideEmpregador;
    FideProc: TideProc;
    FideTrabalhador: TideTrabalhador;
    FinfoTrabFGTS: TinfoTrabFGTSCollection;
  public
    constructor Create;
    destructor  Destroy; override;

    function LerXML: boolean;
    function SalvarINI: boolean;

    property ideEvento      : TideEvento5 read FideEvento write FideEvento;
    property ideEmpregador  : TideEmpregador read FideEmpregador write FideEmpregador;
    property ideProc        : TideProc read FideProc write FideProc;
    property IdeTrabalhador : TideTrabalhador read FideTrabalhador write FideTrabalhador;
    property infoTrabFGTS   : TinfoTrabFGTSCollection read FinfoTrabFGTS write FinfoTrabFGTS;
    property Leitor         : TLeitor read FLeitor write FLeitor;
    property Id             : String read FId;
    property XML            : String read FXML;
    property VersaoDF       : TVersaoeSocial read FVersaoDF write FVersaoDF;
  end;

  TideProc = class(TObject)
  private
    Forigem: tpOrigemProc;
    FnrProcTrab: string;
  public
    property origem: tpOrigemProc read Forigem;
    property nrProcTrab: string read FnrProcTrab;
  end;

  TInfoTrabFGTSCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TInfoTrabFGTSCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfoTrabFGTSCollectionItem);
  public
    function New: TInfoTrabFGTSCollectionItem;
    property Items[Index: Integer]: TInfoTrabFGTSCollectionItem read GetItem write SetItem;
  end;

  TInfoTrabFGTSCollectionItem = class(TObject)
  private
    Fmatricula: String;
    FcodCateg: Integer;
    FcategOrig: Integer;
    FinfoFGTSProcTrab: TinfoFGTSProcTrab;
  public
    constructor Create;
    destructor Destroy; override;

    property matricula       : string  read Fmatricula;
    property codCateg        : Integer read FcodCateg;
    property categOrig       : Integer read FcategOrig;
    property infoFGTSProcTrab: TinfoFGTSProcTrab read FinfoFGTSProcTrab;
  end;

  TinfoFGTSProcTrab = class(TObject)
  private
    FtotalFGTS: double;
    FideEstab: TideEstab;
  public
    constructor Create;
    destructor Destroy; override;

    property totalFGTS: double read FtotalFGTS;
    property ideEstab: TideEstab read FideEstab;
  end;

  TideEstab = class(TObject)
  private
    FtpInsc: tpTpInsc;
    FnrInsc: string;
    FbasePerRef: TbasePerRefCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property tpInsc: tpTpInsc read FtpInsc;
    property nrInsc: string read FnrInsc;
    property basePerRef: TbasePerRefCollection read FbasePerRef;
  end;

  TbasePerRefCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TbasePerRefCollectionItem;
    procedure SetItem(Index: Integer; Value: TbasePerRefCollectionItem);
  public
    function New: TbasePerRefCollectionItem;
    property Items[Index: Integer]: TbasePerRefCollectionItem read GetItem write SetItem;
  end;

  TbasePerRefCollectionItem = class(TObject)
  private
    FperRef: string;
    FcodCateg: integer;
    FtpValorProcTrab: integer;
    FremFGTSProcTrab: double;
    FdpsFGTSProcTrab: double;
    FremFGTSSefip: double;
    FdpsFGTSSefip: double;
    FremFGTSDecAnt: double;
    FdpsFGTSDecAnt: double;
  public
    property perRef: string read FperRef;
    property codCateg: integer read FcodCateg;
    property tpValorProcTrab: integer read FtpValorProcTrab;
    property remFGTSProcTrab: double read FremFGTSProcTrab;
    property dpsFGTSProcTrab: double read FdpsFGTSProcTrab;
    property remFGTSSefip: double read FremFGTSSefip;
    property dpsFGTSSefip: double read FdpsFGTSSefip;
    property remFGTSDecAnt: double read FremFGTSDecAnt;
    property dpsFGTSDecAnt: double read FdpsFGTSDecAnt;
  end;

implementation

uses
  IniFiles;

{ TS5503 }

constructor TS5503.Create;
begin
  FTipoEvento := teS5503;
  FevtFGTSProcTrab := TevtFGTSProcTrab.Create;
end;

destructor TS5503.Destroy;
begin
  FevtFGTSProcTrab.Free;

  inherited;
end;

function TS5503.GetEvento : TObject;
begin
  Result := self;
end;

function TS5503.GetXml : string;
begin
  Result := FevtFGTSProcTrab.XML;
end;

procedure TS5503.SetXml(const Value: string);
begin
  if Value = FevtFGTSProcTrab.XML then Exit;

  FevtFGTSProcTrab.FXML := Value;
  FevtFGTSProcTrab.Leitor.Arquivo := Value;
  FevtFGTSProcTrab.LerXML;

end;

function TS5503.GetTipoEvento : TTipoEvento;
begin
  Result := FTipoEvento;
end;

{ TInfoTrabFGTSCollection }

function TInfoTrabFGTSCollection.GetItem(Index: Integer): TInfoTrabFGTSCollectionItem;
begin
  Result := TInfoTrabFGTSCollectionItem(inherited Items[Index]);
end;

procedure TInfoTrabFGTSCollection.SetItem(Index: Integer; Value: TInfoTrabFGTSCollectionItem);
begin
  inherited Items[Index] := Value;
end;
function TInfoTrabFGTSCollection.New: TInfoTrabFGTSCollectionItem;
begin
  Result := TInfoTrabFGTSCollectionItem.Create;
  Self.Add(Result);
end;

{ TInfoTrabFGTSCollectionItem }

constructor TInfoTrabFGTSCollectionItem.Create;
begin
  inherited Create;

  FinfoFGTSProcTrab := TinfoFGTSProcTrab.Create;
end;

destructor TInfoTrabFGTSCollectionItem.Destroy;
begin
  FinfoFGTSProcTrab.Free;

  inherited;
end;

{ TinfoFGTSProcTrab }

constructor TinfoFGTSProcTrab.Create;
begin
  inherited Create;

  FideEstab := TideEstab.Create;
end;

destructor TinfoFGTSProcTrab.Destroy;
begin
  FideEstab.Free;

  inherited;
end;

{ TideEstab }

constructor TideEstab.Create;
begin
  inherited Create;

  FbasePerRef := TbasePerRefCollection.Create;
end;

destructor TideEstab.Destroy;
begin
  FbasePerRef.Free;

  inherited;
end;

{ TbaseRefCollection }

function TbasePerRefCollection.GetItem(Index: Integer): TbasePerRefCollectionItem;
begin
  Result := TbasePerRefCollectionItem(inherited Items[Index]);
end;

procedure TbasePerRefCollection.SetItem(Index: Integer; Value: TbasePerRefCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TbasePerRefCollection.New: TbasePerRefCollectionItem;
begin
  Result := TbasePerRefCollectionItem.Create;
  Self.Add(Result);
end;

{ TevtFGTSProcTrab }

constructor TevtFGTSProcTrab.Create;
begin
  inherited Create;
  FLeitor         := TLeitor.Create;
  FideEvento      := TideEvento5.Create;
  FideEmpregador  := TideEmpregador.Create;
  FideProc        := TideProc.Create;
  FideTrabalhador := TideTrabalhador.Create;
  FinfoTrabFGTS   := TinfoTrabFGTSCollection.Create;
end;

destructor TevtFGTSProcTrab.Destroy;
begin
  FLeitor.Free;

  FideEvento.Free;
  FideEmpregador.Free;
  FideTrabalhador.Free;
  FideProc.Free;
  FinfoTrabFGTS.Free;

  inherited;
end;

function TevtFGTSProcTrab.LerXML: boolean;
var
  ok: Boolean;
  i, j: Integer;
  s: string;
begin
  try
    FXML := Leitor.Arquivo;

    // Capturar a versão do evento
    s := Copy(FXML, Pos('/evt/evtFGTSProcTrab/', FXML)+18, 16);
    s := Copy(s, 1, Pos('"', s)-1);
    Self.VersaoDF := StrToVersaoeSocialSchemas(s);

    if leitor.rExtrai(1, 'evtFGTSProcTrab') <> '' then
    begin
      FId := Leitor.rAtributo('Id=');

      if leitor.rExtrai(2, 'ideEvento') <> '' then
      begin
        ideEvento.nrRecArqBase := leitor.rCampo(tcStr, 'nrRecArqBase');
        ideEvento.perApur      := leitor.rCampo(tcStr, 'perApur');
      end; { ideEvento }

      if leitor.rExtrai(2, 'ideEmpregador') <> '' then
      begin
        ideEmpregador.tpInsc := eSStrToTpInscricao(ok, leitor.rCampo(tcStr, 'tpInsc'));
        ideEmpregador.nrInsc := leitor.rCampo(tcStr, 'nrInsc');
      end; { ideEmpregador }

      if leitor.rExtrai(2, 'ideProc') <> '' then
      begin
        ideProc.Forigem     := eSStrToTpOrigemProc(ok, leitor.rCampo(tcStr, 'origem'));
        ideProc.FnrProcTrab := leitor.rCampo(tcStr, 'nrProcTrab');
      end; { ideProc }

      if leitor.rExtrai(2, 'ideTrabalhador') <> '' then
        ideTrabalhador.cpfTrab := leitor.rCampo(tcStr, 'cpfTrab');

      i := 0;
      while leitor.rExtrai(2, 'infoTrabFGTS', '', i + 1) <> '' do
      begin
        with infoTrabFGTS do
        begin
          Items[i].Fmatricula := leitor.rCampo(tcStr, 'matricula');
          Items[i].FcodCateg  := leitor.rCampo(tcInt, 'codCateg');
          Items[i].FcategOrig := leitor.rCampo(tcInt, 'categOrig');

          if leitor.rExtrai(3, 'infoFGTSProcTrab') <> '' then
          begin
            with Items[i].infoFGTSProcTrab do
            begin
              FtotalFGTS := leitor.rCampo(tcDe2, 'totalFGTS');

              if leitor.rExtrai(4, 'ideEstab') <> '' then
              begin
                with ideEstab do
                begin
                  FtpInsc := eSStrToTpInscricao(ok, leitor.rCampo(tcStr, 'tpInsc'));
                  FnrInsc := leitor.rCampo(tcStr, 'nrInsc');

                  j := 0;
                  while Leitor.rExtrai(5, 'basePerRef', '', j + 1) <> '' do
                  begin
                    with basePerRef do
                    begin
                      New;
                      Items[j].FperRef          := leitor.rCampo(tcStr, 'perRef');
                      Items[j].FcodCateg        := leitor.rCampo(tcInt, 'codCateg');
                      Items[j].FtpValorProcTrab := leitor.rCampo(tcInt, 'tpValorProcTrab');
                      Items[j].FremFGTSProcTrab := leitor.rCampo(tcDe2, 'remFGTSProcTrab');
                      Items[j].FdpsFGTSProcTrab := leitor.rCampo(tcDe2, 'dpsFGTSProcTrab');
                      Items[j].FremFGTSSefip    := leitor.rCampo(tcDe2, 'remFGTSSefip');
                      Items[j].FdpsFGTSSefip    := leitor.rCampo(tcDe2, 'dpsFGTSSefip');
                      Items[j].FremFGTSDecAnt   := leitor.rCampo(tcDe2, 'remFGTSDecAnt');
                      Items[j].FdpsFGTSDecAnt   := leitor.rCampo(tcDe2, 'dpsFGTSDecAnt');
                    end;

                    inc(j);
                  end; { basePerRef }
                end;
              end; { ideEstab }
            end;
          end; { infoFGTSProcTrab }
        end;

        inc(i);
      end; { infoTrabFGTS }

      Result := True;
    end; { evtFGTSProcTrab }
  except
    on e: Exception do  begin
      Result := False;
    end;
  end;
end;

function TevtFGTSProcTrab.SalvarINI: boolean;
begin
  Result := False;
end;

end.
