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

unit pcnReinfR2030;

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
  pcnConsts,
  pcnCommonReinf, pcnConversaoReinf, pcnGeradorReinf;

type
  TR2030Collection = class;
  TR2030CollectionItem = class;
  TevtAssocDespRec = class;

  {Classes específicas deste evento}
  TideEstab = class;
  TrecursosRecCollection = class;
  TrecursosRecCollectionItem = class;
  TinfoRecursoCollection = class;
  TinfoRecursoCollectionItem = class;
  TinfoProcCollection = class;
  TinfoProcCollectionItem = class;

  TR2030Collection = class(TReinfCollection)
  private
    function GetItem(Index: Integer): TR2030CollectionItem;
    procedure SetItem(Index: Integer; Value: TR2030CollectionItem);
  public
    function Add: TR2030CollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TR2030CollectionItem;

    property Items[Index: Integer]: TR2030CollectionItem read GetItem write SetItem; default;
  end;

  TR2030CollectionItem = class(TObject)
  private
    FTipoEvento: TTipoEvento;
    FevtAssocDespRec: TevtAssocDespRec;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    property TipoEvento: TTipoEvento read FTipoEvento;
    property evtAssocDespRec: TevtAssocDespRec read FevtAssocDespRec write FevtAssocDespRec;
  end;

  TevtAssocDespRec = class(TReinfEvento) //Classe do elemento principal do XML do evento!
  private
    FIdeEvento: TIdeEvento2;
    FideContri: TideContri;
    FideEstab: TideEstab;

    {Geradores específicos desta classe}
    procedure GerarideEstab;
    procedure GerarrecursosRec(Lista: TrecursosRecCollection);
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
    FrecursosRec: TrecursosRecCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property tpInscEstab: TtpInsc read FtpInscEstab write FtpInscEstab default tiCNPJ;
    property nrInscEstab: string read FnrInscEstab write FnrInscEstab;
    property recursosRec: TrecursosRecCollection read FrecursosRec write FrecursosRec;
  end;

  TrecursosRecCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TrecursosRecCollectionItem;
    procedure SetItem(Index: Integer; Value: TrecursosRecCollectionItem);
  public
    function Add: TrecursosRecCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TrecursosRecCollectionItem;

    property Items[Index: Integer]: TrecursosRecCollectionItem read GetItem write SetItem; default;
  end;

  TrecursosRecCollectionItem = class(TObject)
  private
    FcnpjOrigRecurso: string;
    FrecEmprExt: string;
    FnmEmprExt: string;
    FvlrTotalRec: Double;
    FvlrTotalRet: Double;
    FvlrTotalNRet: Double;
    FinfoRecurso: TinfoRecursoCollection;
    FinfoProc: TinfoProcCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property cnpjOrigRecurso: string read FcnpjOrigRecurso write FcnpjOrigRecurso;
    property recEmprExt: string read FrecEmprExt write FrecEmprExt;
    property nmEmprExt: string read FnmEmprExt write FnmEmprExt;
    property vlrTotalRec: Double read FvlrTotalRec write FvlrTotalRec;
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

{ TR2030Collection }

function TR2030Collection.Add: TR2030CollectionItem;
begin
  Result := Self.New;
end;

function TR2030Collection.GetItem(Index: Integer): TR2030CollectionItem;
begin
  Result := TR2030CollectionItem(inherited Items[Index]);
end;

function TR2030Collection.New: TR2030CollectionItem;
begin
  Result := TR2030CollectionItem.Create(FACBrReinf);
  Self.Add(Result);
end;

procedure TR2030Collection.SetItem(Index: Integer; Value: TR2030CollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TR2030CollectionItem }

constructor TR2030CollectionItem.Create(AOwner: TComponent);
begin
  inherited Create;

  FTipoEvento      := teR2030;
  FevtAssocDespRec := TevtAssocDespRec.Create(AOwner);
end;

destructor TR2030CollectionItem.Destroy;
begin
  inherited;

  FevtAssocDespRec.Free;
end;

{ TevtAssocDespRec }

constructor TevtAssocDespRec.Create(AACBrReinf: TObject);
begin
  inherited Create(AACBrReinf);

  FideContri := TideContri.Create;
  FIdeEvento := TIdeEvento2.Create;
  FideEstab  := TideEstab.Create;
end;

destructor TevtAssocDespRec.Destroy;
begin
  FideContri.Free;
  FIdeEvento.Free;
  FideEstab.Free;

  inherited;
end;

{ TideEstab }

constructor TideEstab.Create;
begin
  FrecursosRec := TrecursosRecCollection.Create;
end;

destructor TideEstab.Destroy;
begin
  FrecursosRec.Free;

  inherited;
end;

{ TrecursosRecCollection }

function TrecursosRecCollection.Add: TrecursosRecCollectionItem;
begin
  Result := Self.New;
end;

function TrecursosRecCollection.GetItem(
  Index: Integer): TrecursosRecCollectionItem;
begin
  Result := TrecursosRecCollectionItem(inherited Items[Index]);
end;

function TrecursosRecCollection.New: TrecursosRecCollectionItem;
begin
  Result := TrecursosRecCollectionItem.Create;
  Self.Add(Result);
end;

procedure TrecursosRecCollection.SetItem(Index: Integer;
  Value: TrecursosRecCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TrecursosRecCollectionItem }

constructor TrecursosRecCollectionItem.Create;
begin
  FinfoRecurso := TinfoRecursoCollection.Create;
  FinfoProc    := TinfoProcCollection.Create;
end;

destructor TrecursosRecCollectionItem.Destroy;
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

procedure TevtAssocDespRec.GerarideEstab;
begin
  Gerador.wGrupo('ideEstab');

  Gerador.wCampo(tcStr, '', 'tpInscEstab', 1,  1, 1, TpInscricaoToStr(Self.ideEstab.tpInscEstab));
  Gerador.wCampo(tcStr, '', 'nrInscEstab', 1, 14, 1, Self.ideEstab.nrInscEstab);

  GerarrecursosRec(Self.ideEstab.recursosRec);

  Gerador.wGrupo('/ideEstab');
end;

procedure TevtAssocDespRec.GerarrecursosRec(Lista: TrecursosRecCollection);
var
  item: TrecursosRecCollectionItem;
  i: Integer;
begin
  for i := 0 to Lista.Count - 1 do
  begin
    Item := Lista.Items[i];

    Gerador.wGrupo('recursosRec');

    Gerador.wCampo(tcStr, '', 'cnpjOrigRecurso', 14, 14, 0, item.cnpjOrigRecurso);
    if TACBrReinf(FACBrReinf).Configuracoes.Geral.VersaoDF >= v2_01_02 then
    begin
      Gerador.wCampo(tcStr, '', 'recEmprExt',       1,  1, 0, item.recEmprExt);
      Gerador.wCampo(tcStr, '', 'nmEmprExt',        1, 70, 0, item.nmEmprExt);
    end;
    Gerador.wCampo(tcDe2, '', 'vlrTotalRec',      1, 14, 1, item.vlrTotalRec);
    Gerador.wCampo(tcDe2, '', 'vlrTotalRet',      1, 14, 1, item.vlrTotalRet);
    Gerador.wCampo(tcDe2, '', 'vlrTotalNRet',     1, 14, 0, item.vlrTotalNRet);

    GerarinfoRecurso(item.infoRecurso);
    GerarinfoProc(item.infoProc);

    Gerador.wGrupo('/recursosRec');
  end;

  if Lista.Count > 999 then
    Gerador.wAlerta('', 'recursosRec', 'Lista de Recursos Recebidos', ERR_MSG_MAIOR_MAXIMO + '999');
end;

procedure TevtAssocDespRec.GerarinfoRecurso(Lista: TinfoRecursoCollection);
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

procedure TevtAssocDespRec.GerarinfoProc(Lista: TinfoProcCollection);
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

function TevtAssocDespRec.GerarXML: Boolean;
begin
  try
    Self.VersaoDF := TACBrReinf(FACBrReinf).Configuracoes.Geral.VersaoDF;

    Self.Id := GerarChaveReinf(now, self.ideContri.NrInsc, self.Sequencial, self.ideContri.TpInsc);

    GerarCabecalho('evtRecursoRecebidoAssociacao');
    Gerador.wGrupo('evtAssocDespRec id="' + Self.Id + '"');

    GerarIdeEvento2(Self.IdeEvento);

    Gerador.wGrupo('ideContri');

    GerarideContri(Self.ideContri, False);
    GerarideEstab;

    Gerador.wGrupo('/ideContri');
    Gerador.wGrupo('/evtAssocDespRec');

    GerarRodape;

    FXML := Gerador.ArquivoFormatoXML;
//    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtAssocDespRec');

//    Validar(schevtRecursoRecebidoAssociacao);
  except on e:exception do
    raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '');
end;

function TevtAssocDespRec.LerArqIni(const AIniString: String): Boolean;
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
      sSecao := 'evtAssocDespRec';
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
          sSecao := 'recursosRec' + IntToStrZero(I, 3);
          sFim   := INIRec.ReadString(sSecao, 'cnpjOrigRecurso', 'FIM');

          if (sFim = 'FIM') or (Length(sFim) <= 0) then
          begin
            if TACBrReinf(FACBrReinf).Configuracoes.Geral.VersaoDF >= v2_01_02 then
              sFim := INIRec.ReadString(sSecao, 'recEmprExt', 'FIM');

            if (sFim = 'FIM') or (Length(sFim) <= 0) then
              break;
          end;

          with recursosRec.New do
          begin
            cnpjOrigRecurso := INIRec.ReadString(sSecao, 'cnpjOrigRecurso', '');
            if TACBrReinf(FACBrReinf).Configuracoes.Geral.VersaoDF >= v2_01_02 then
            begin
              recEmprExt := INIRec.ReadString(sSecao, 'recEmprExt', '');
              nmEmprExt  := INIRec.ReadString(sSecao, 'nmEmprExt', '');
            end;
            vlrTotalRec     := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrTotalRec', ''), 0);
            vlrTotalRet     := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrTotalRet', ''), 0);
            vlrTotalNRet    := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrTotalNRet', ''), 0);

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
