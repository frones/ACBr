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

unit pcnReinfR2040;

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
  TR2040Collection = class;
  TR2040CollectionItem = class;
  TevtAssocDespRep = class;

  {Classes específicas deste evento}
  TideEstab = class;
  TrecursosRepCollection = class;
  TrecursosRepCollectionItem = class;
  TinfoRecursoCollection = class;
  TinfoRecursoCollectionItem = class;
  TinfoProcCollection = class;
  TinfoProcCollectionItem = class;

  TR2040Collection = class(TReinfCollection)
  private
    function GetItem(Index: Integer): TR2040CollectionItem;
    procedure SetItem(Index: Integer; Value: TR2040CollectionItem);
  public
    function Add: TR2040CollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TR2040CollectionItem;

    property Items[Index: Integer]: TR2040CollectionItem read GetItem write SetItem; default;
  end;

  TR2040CollectionItem = class(TObject)
  private
    FTipoEvento: TTipoEvento;
    FevtAssocDespRep: TevtAssocDespRep;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    property TipoEvento: TTipoEvento read FTipoEvento;
    property evtAssocDespRep: TevtAssocDespRep read FevtAssocDespRep write FevtAssocDespRep;
  end;

  TevtAssocDespRep = class(TReinfEvento) //Classe do elemento principal do XML do evento!
  private
    FIdeEvento: TIdeEvento2;
    FideContri: TideContri;
    FideEstab: TideEstab;

    {Geradores específicos desta classe}
    procedure GerarideEstab;
    procedure GerarrecursosRep(Lista: TrecursosRepCollection);
    procedure GerarinfoRecurso(Lista: TinfoRecursoCollection);
    procedure GerarinfoProc(Lista: TinfoProcCollection);
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
    FrecursosRep: TrecursosRepCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property tpInscEstab: TtpInsc read FtpInscEstab write FtpInscEstab default tiCNPJ;
    property nrInscEstab: string read FnrInscEstab write FnrInscEstab;
    property recursosRep: TrecursosRepCollection read FrecursosRep write FrecursosRep;
  end;

  TrecursosRepCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TrecursosRepCollectionItem;
    procedure SetItem(Index: Integer; Value: TrecursosRepCollectionItem);
  public
    function Add: TrecursosRepCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TrecursosRepCollectionItem;

    property Items[Index: Integer]: TrecursosRepCollectionItem read GetItem write SetItem; default;
  end;

  TrecursosRepCollectionItem = class(TObject)
  private
    FcnpjAssocDesp: string;
    FvlrTotalRep: Double;
    FvlrTotalRet: Double;
    FvlrTotalNRet: Double;
    FinfoRecurso: TinfoRecursoCollection;
    FinfoProc: TinfoProcCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property cnpjAssocDesp: string read FcnpjAssocDesp write FcnpjAssocDesp;
    property vlrTotalRep: Double read FvlrTotalRep write FvlrTotalRep;
    property vlrTotalRet: Double read FvlrTotalRet write FvlrTotalRet;
    property vlrTotalNRet: Double read FvlrTotalNRet write FvlrTotalNRet;
    property infoRecurso: TinfoRecursoCollection read FinfoRecurso write FinfoRecurso;
    property infoProc: TinfoProcCollection read FinfoProc write FinfoProc;
  end;

  TinfoRecursoCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TinfoRecursoCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfoRecursoCollectionItem);
  public
    function Add: TinfoRecursoCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TinfoRecursoCollectionItem;

    property Items[Index: Integer]: TinfoRecursoCollectionItem read GetItem write SetItem; default;
  end;

  TinfoRecursoCollectionItem = class(TObject)
  private
    FvlrRetApur: double;
    FvlrBruto: double;
    FdescRecurso: String;
    FtpRepasse: TtpRepasse;
  public
    property tpRepasse: TtpRepasse read FtpRepasse write FtpRepasse;
    property descRecurso: String read FdescRecurso write FdescRecurso;
    property vlrBruto: double read FvlrBruto write FvlrBruto;
    property vlrRetApur: double read FvlrRetApur write FvlrRetApur;
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
    FcodSusp: String;
    FtpProc: TtpProc;
    FnrProc: String;
    FvlrNRet: double;
  public
    property tpProc: TtpProc read FtpProc write FtpProc;
    property nrProc: String read FnrProc write FnrProc;
    property codSusp: String read FcodSusp write FcodSusp;
    property vlrNRet: double read FvlrNRet write FvlrNRet;
  end;

implementation

uses
  IniFiles,
  ACBrReinf, ACBrDFeUtil;

{ TR2040Collection }

function TR2040Collection.Add: TR2040CollectionItem;
begin
  Result := Self.New;
end;

function TR2040Collection.GetItem(Index: Integer): TR2040CollectionItem;
begin
  Result := TR2040CollectionItem(inherited Items[Index]);
end;

function TR2040Collection.New: TR2040CollectionItem;
begin
  Result := TR2040CollectionItem.Create(FACBrReinf);
  Self.Add(Result);
end;

procedure TR2040Collection.SetItem(Index: Integer; Value: TR2040CollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TR2040CollectionItem }

constructor TR2040CollectionItem.Create(AOwner: TComponent);
begin
  inherited Create;

  FTipoEvento      := teR2040;
  FevtAssocDespRep := TevtAssocDespRep.Create(AOwner);
end;

destructor TR2040CollectionItem.Destroy;
begin
  inherited;

  FevtAssocDespRep.Free;
end;

{ TevtAssocDespRep }

constructor TevtAssocDespRep.Create(AACBrReinf: TObject);
begin
  inherited Create(AACBrReinf);

  FideContri := TideContri.Create;
  FIdeEvento := TIdeEvento2.Create;
  FideEstab  := TideEstab.Create;
end;

destructor TevtAssocDespRep.Destroy;
begin
  FideContri.Free;
  FIdeEvento.Free;
  FideEstab.Free;

  inherited;
end;

{ TideEstab }

constructor TideEstab.Create;
begin
  FrecursosRep := TrecursosRepCollection.Create;
end;

destructor TideEstab.Destroy;
begin
  FrecursosRep.Free;

  inherited;
end;

{ TrecursosRepCollection }

function TrecursosRepCollection.Add: TrecursosRepCollectionItem;
begin
  Result := Self.New;
end;

function TrecursosRepCollection.GetItem(
  Index: Integer): TrecursosRepCollectionItem;
begin
  Result := TrecursosRepCollectionItem(inherited Items[Index]);
end;

function TrecursosRepCollection.New: TrecursosRepCollectionItem;
begin
  Result := TrecursosRepCollectionItem.Create;
  Self.Add(Result);
end;

procedure TrecursosRepCollection.SetItem(Index: Integer;
  Value: TrecursosRepCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TrecursosRepCollectionItem }

constructor TrecursosRepCollectionItem.Create;
begin
  FinfoRecurso := TinfoRecursoCollection.Create;
  FinfoProc    := TinfoProcCollection.Create;
end;

destructor TrecursosRepCollectionItem.Destroy;
begin
  FinfoRecurso.Free;
  FinfoProc.Free;

  inherited;
end;

{ TinfoRecursoCollection }

function TinfoRecursoCollection.Add: TinfoRecursoCollectionItem;
begin
  Result := Self.New;
end;

function TinfoRecursoCollection.GetItem(
  Index: Integer): TinfoRecursoCollectionItem;
begin
  Result := TinfoRecursoCollectionItem(inherited Items[Index]);
end;

function TinfoRecursoCollection.New: TinfoRecursoCollectionItem;
begin
  Result := TinfoRecursoCollectionItem.Create;
  Self.Add(Result);
end;

procedure TinfoRecursoCollection.SetItem(Index: Integer;
  Value: TinfoRecursoCollectionItem);
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

procedure TevtAssocDespRep.GerarideEstab;
begin
  Gerador.wGrupo('ideEstab');

  Gerador.wCampo(tcStr, '', 'tpInscEstab', 1,  1, 1, TpInscricaoToStr(Self.ideEstab.tpInscEstab));
  Gerador.wCampo(tcStr, '', 'nrInscEstab', 1, 14, 1, Self.ideEstab.nrInscEstab);

  GerarrecursosRep(Self.ideEstab.recursosRep);

  Gerador.wGrupo('/ideEstab');
end;

procedure TevtAssocDespRep.GerarrecursosRep(Lista: TrecursosRepCollection);
var
  item: TrecursosRepCollectionItem;
  i: Integer;
begin
  for i := 0 to Lista.Count - 1 do
  begin
    Item := Lista.Items[i];

    Gerador.wGrupo('recursosRep');

    Gerador.wCampo(tcStr, '', 'cnpjAssocDesp', 14, 14, 1, item.cnpjAssocDesp);
    Gerador.wCampo(tcDe2, '', 'vlrTotalRep',    1, 14, 1, item.vlrTotalRep);
    Gerador.wCampo(tcDe2, '', 'vlrTotalRet',    1, 14, 1, item.vlrTotalRet);
    Gerador.wCampo(tcDe2, '', 'vlrTotalNRet',   1, 14, 0, item.vlrTotalNRet);

    GerarinfoRecurso(item.infoRecurso);
    GerarinfoProc(item.infoProc);

    Gerador.wGrupo('/recursosRep');
  end;

  if Lista.Count > 999 then
    Gerador.wAlerta('', 'recursosRep', 'Lista de Recursos Repassados', ERR_MSG_MAIOR_MAXIMO + '999');
end;

procedure TevtAssocDespRep.GerarinfoRecurso(Lista: TinfoRecursoCollection);
var
  item: TinfoRecursoCollectionItem;
  i: Integer;
begin
  for i := 0 to Lista.Count - 1 do
  begin
    Item := Lista.Items[i];

    Gerador.wGrupo('infoRecurso');

    Gerador.wCampo(tcStr, '', 'tpRepasse',   1,  1, 1, tpRepasseToStr( item.tpRepasse ));
    Gerador.wCampo(tcStr, '', 'descRecurso', 1, 20, 1, item.descRecurso);
    Gerador.wCampo(tcDe2, '', 'vlrBruto',    1, 14, 1, item.vlrBruto);
    Gerador.wCampo(tcDe2, '', 'vlrRetApur',  1, 14, 1, item.vlrRetApur);

    Gerador.wGrupo('/infoRecurso');
  end;

  if Lista.Count > 999 then
    Gerador.wAlerta('', 'infoRecurso', 'Lista de Detalhamento de Recursos', ERR_MSG_MAIOR_MAXIMO + '999');
end;

procedure TevtAssocDespRep.GerarinfoProc(Lista: TinfoProcCollection);
var
  item: TinfoProcCollectionItem;
  i: Integer;
begin
  for i := 0 to Lista.Count - 1 do
  begin
    Item := Lista.Items[i];

    Gerador.wGrupo('infoProc');

    Gerador.wCampo(tcStr, '', 'tpProc',  1,  1, 1, TpProcToStr( item.tpProc ));
    Gerador.wCampo(tcStr, '', 'nrProc',  1, 21, 1, item.nrProc);
    Gerador.wCampo(tcStr, '', 'codSusp', 1, 14, 0, item.codSusp);
    Gerador.wCampo(tcDe2, '', 'vlrNRet', 1, 14, 1, item.vlrNRet);

    Gerador.wGrupo('/infoProc');
  end;

  if Lista.Count > 50 then
    Gerador.wAlerta('', 'infoProc', 'Lista de Informações de Processos', ERR_MSG_MAIOR_MAXIMO + '50');
end;

function TevtAssocDespRep.GerarXML: Boolean;
begin
  try
    Self.VersaoDF := TACBrReinf(FACBrReinf).Configuracoes.Geral.VersaoDF;

    Self.Id := GerarChaveReinf(now, self.ideContri.NrInsc, self.Sequencial, self.ideContri.TpInsc);

    GerarCabecalho('evtRecursoRepassadoAssociacao');
    Gerador.wGrupo('evtAssocDespRep id="' + Self.Id + '"');

    GerarIdeEvento2(Self.IdeEvento);

    Gerador.wGrupo('ideContri');

    GerarideContri(Self.ideContri, False);
    GerarideEstab;

    Gerador.wGrupo('/ideContri');
    Gerador.wGrupo('/evtAssocDespRep');

    GerarRodape;

    FXML := Gerador.ArquivoFormatoXML;
//    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtAssocDespRep');

//    Validar(schevtRecursoRepassadoAssociacao);
  except on e:exception do
    raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '');
end;

function TevtAssocDespRep.LerArqIni(const AIniString: String): Boolean;
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
      sSecao := 'evtAssocDespRep';
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
      ideEstab.tpInscEstab := StrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInscEstab', '1'));
      ideEstab.nrInscEstab := INIRec.ReadString(sSecao, 'nrInscEstab', EmptyStr);

      with ideEstab do
      begin
        I := 1;
        while true do
        begin
          // de 001 até 999
          sSecao := 'recursosRep' + IntToStrZero(I, 3);
          sFim   := INIRec.ReadString(sSecao, 'cnpjAssocDesp', 'FIM');

          if (sFim = 'FIM') or (Length(sFim) <= 0) then
            break;

          with recursosRep.New do
          begin
            cnpjAssocDesp := sFim;
            vlrTotalRep   := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrTotalRep', ''), 0);
            vlrTotalRet   := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrTotalRet', ''), 0);
            vlrTotalNRet  := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrTotalNRet', ''), 0);

            J := 1;
            while true do
            begin
              // de 001 até 999
              sSecao := 'infoRecurso' + IntToStrZero(I, 3) + IntToStrZero(J, 3);
              sFim   := INIRec.ReadString(sSecao, 'tpRepasse', 'FIM');

              if (sFim = 'FIM') or (Length(sFim) <= 0) then
                break;

              with infoRecurso.New do
              begin
                tpRepasse   := StrTotpRepasse(Ok, sFim);
                descRecurso := INIRec.ReadString(sSecao, 'descRecurso', '');
                vlrBruto    := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrBruto', ''), 0);
                vlrRetApur  := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrRetApur', ''), 0);
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
                tpProc  := StrToTpProc(Ok, sFim);
                nrProc  := INIRec.ReadString(sSecao, 'nrProc', '');
                codSusp := INIRec.ReadString(sSecao, 'codSusp', '');
                vlrNRet := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrNRet', ''), 0);
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
