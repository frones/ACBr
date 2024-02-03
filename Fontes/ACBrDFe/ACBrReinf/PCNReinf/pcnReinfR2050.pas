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

unit pcnReinfR2050;

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
  TR2050Collection = class;
  TR2050CollectionItem = class;
  TevtComProd = class;

  {Classes específicas deste evento}
  TinfoComProd = class;
  TideEstab = class;
  TtipoComCollection = class;
  TtipoComCollectionItem = class;
  TinfoProcCollection = class;
  TinfoProcCollectionItem = class;

  TR2050Collection = class(TReinfCollection)
  private
    function GetItem(Index: Integer): TR2050CollectionItem;
    procedure SetItem(Index: Integer; Value: TR2050CollectionItem);
  public
    function Add: TR2050CollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TR2050CollectionItem;

    property Items[Index: Integer]: TR2050CollectionItem read GetItem write SetItem; default;
  end;

  TR2050CollectionItem = class(TObject)
  private
    FTipoEvento: TTipoEvento;
    FevtComProd: TevtComProd;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    property TipoEvento: TTipoEvento read FTipoEvento;
    property evtComProd: TevtComProd read FevtComProd write FevtComProd;
  end;

  TevtComProd = class(TReinfEvento) //Classe do elemento principal do XML do evento!
  private
    FIdeEvento: TIdeEvento2;
    FideContri: TideContri;
    FinfoComProd: TinfoComProd;

    {Geradores específicos desta classe}
    procedure GerarideEstab;
    procedure GerartipoCom(Lista: TtipoComCollection);
    procedure GerarinfoProc(Lista: TinfoProcCollection);
  public
    constructor Create(AACBrReinf: TObject); override;
    destructor  Destroy; override;

    function GerarXML: Boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property ideEvento: TIdeEvento2 read FIdeEvento write FIdeEvento;
    property ideContri: TideContri read FideContri write FideContri;
    property infoComProd: TinfoComProd read FinfoComProd write FinfoComProd;
  end;

  { TinfoComProd }
  TinfoComProd = class(TObject)
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
    FvlrCPApur: double;
    FvlrRatApur: double;
    FvlrSenarApur: double;
    FvlrCPSuspTotal: double;
    FvlrRatSuspTotal: double;
    FvlrSenarSuspTotal: double;
    FtipoCom: TtipoComCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property tpInscEstab: TtpInsc read FtpInscEstab write FtpInscEstab default tiCNPJ;
    property nrInscEstab: string read FnrInscEstab write FnrInscEstab;
    property vlrRecBrutaTotal: double read FvlrRecBrutaTotal write FvlrRecBrutaTotal;
    property vlrCPApur: double read FvlrCPApur write FvlrCPApur;
    property vlrRatApur: double read FvlrRatApur write FvlrRatApur;
    property vlrSenarApur: double read FvlrSenarApur write FvlrSenarApur;
    property vlrCPSuspTotal: double read FvlrCPSuspTotal write FvlrCPSuspTotal;
    property vlrRatSuspTotal: double read FvlrRatSuspTotal write FvlrRatSuspTotal;
    property vlrSenarSuspTotal: double read FvlrSenarSuspTotal write FvlrSenarSuspTotal;
    property tipoCom: TtipoComCollection read FtipoCom write FtipoCom;
  end;

  TtipoComCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TtipoComCollectionItem;
    procedure SetItem(Index: Integer; Value: TtipoComCollectionItem);
  public
    function Add: TtipoComCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TtipoComCollectionItem;

    property Items[Index: Integer]: TtipoComCollectionItem read GetItem write SetItem; default;
  end;

  TtipoComCollectionItem = class(TObject)
  private
    FindCom: TindCom;
    FvlrRecBruta: double;
    FinfoProc: TinfoProcCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property indCom: TindCom read FindCom write FindCom;
    property vlrRecBruta: double read FvlrRecBruta write FvlrRecBruta;
    property infoProc: TinfoProcCollection read FinfoProc write FinfoProc;
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
    FvlrSenarSusp: double;
    FtpProc: TtpProc;
    FnrProc: String;
    FvlrRatSusp: double;
    FvlrCPSusp: double;
  public
    property tpProc: TtpProc read FtpProc write FtpProc;
    property nrProc: String read FnrProc write FnrProc;
    property codSusp: String read FcodSusp write FcodSusp;
    property vlrCPSusp: double read FvlrCPSusp write FvlrCPSusp;
    property vlrRatSusp: double read FvlrRatSusp write FvlrRatSusp;
    property vlrSenarSusp: double read FvlrSenarSusp write FvlrSenarSusp;
  end;

implementation

uses
  IniFiles,
  ACBrReinf, ACBrDFeUtil;

{ TR2050Collection }

function TR2050Collection.Add: TR2050CollectionItem;
begin
  Result := Self.New;
end;

function TR2050Collection.GetItem(Index: Integer): TR2050CollectionItem;
begin
  Result := TR2050CollectionItem(inherited Items[Index]);
end;

function TR2050Collection.New: TR2050CollectionItem;
begin
  Result := TR2050CollectionItem.Create(FACBrReinf);
  Self.Add(Result);
end;

procedure TR2050Collection.SetItem(Index: Integer; Value: TR2050CollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TR2050CollectionItem }

constructor TR2050CollectionItem.Create(AOwner: TComponent);
begin
  inherited Create;

  FTipoEvento := teR2050;
  FevtComProd := TevtComProd.Create(AOwner);
end;

destructor TR2050CollectionItem.Destroy;
begin
  inherited;

  FevtComProd.Free;
end;

{ TevtComProd }

constructor TevtComProd.Create(AACBrReinf: TObject);
begin
  inherited Create(AACBrReinf);

  FideContri   := TideContri.Create;
  FIdeEvento   := TIdeEvento2.Create;
  FinfoComProd := TinfoComProd.Create;
end;

destructor TevtComProd.Destroy;
begin
  FideContri.Free;
  FIdeEvento.Free;
  FinfoComProd.Free;

  inherited;
end;

{ TinfoComProd }

constructor TinfoComProd.Create;
begin
  FideEstab := TideEstab.Create;
end;

destructor TinfoComProd.Destroy;
begin
  FideEstab.Free;

  inherited;
end;

{ TideEstab }

constructor TideEstab.Create;
begin
  FtipoCom := TtipoComCollection.Create;
end;

destructor TideEstab.Destroy;
begin
  FtipoCom.Free;

  inherited;
end;

{ TtipoComCollection }

function TtipoComCollection.Add: TtipoComCollectionItem;
begin
  Result := Self.New;
end;

function TtipoComCollection.GetItem(
  Index: Integer): TtipoComCollectionItem;
begin
  Result := TtipoComCollectionItem(inherited Items[Index]);
end;

function TtipoComCollection.New: TtipoComCollectionItem;
begin
  Result := TtipoComCollectionItem.Create;
  Self.Add(Result);
end;

procedure TtipoComCollection.SetItem(Index: Integer;
  Value: TtipoComCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TtipoComCollectionItem }

constructor TtipoComCollectionItem.Create;
begin
  FinfoProc := TinfoProcCollection.Create;
end;

destructor TtipoComCollectionItem.Destroy;
begin
  FinfoProc.Free;

  inherited;
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

procedure TevtComProd.GerarideEstab;
begin
  Gerador.wGrupo('ideEstab');

  Gerador.wCampo(tcStr, '', 'tpInscEstab',       1,  1, 1, TpInscricaoToStr(Self.FinfoComProd.ideEstab.tpInscEstab));
  Gerador.wCampo(tcStr, '', 'nrInscEstab',       1, 14, 1, Self.FinfoComProd.ideEstab.nrInscEstab);
  Gerador.wCampo(tcDe2, '', 'vlrRecBrutaTotal',  1, 14, 1, Self.FinfoComProd.ideEstab.vlrRecBrutaTotal);
  Gerador.wCampo(tcDe2, '', 'vlrCPApur',         1, 14, 1, Self.FinfoComProd.ideEstab.vlrCPApur);
  Gerador.wCampo(tcDe2, '', 'vlrRatApur',        1, 14, 1, Self.FinfoComProd.ideEstab.vlrRatApur);
  Gerador.wCampo(tcDe2, '', 'vlrSenarApur',      1, 14, 1, Self.FinfoComProd.ideEstab.vlrSenarApur);
  Gerador.wCampo(tcDe2, '', 'vlrCPSuspTotal',    1, 14, 0, Self.FinfoComProd.ideEstab.vlrCPSuspTotal);
  Gerador.wCampo(tcDe2, '', 'vlrRatSuspTotal',   1, 14, 0, Self.FinfoComProd.ideEstab.vlrRatSuspTotal);
  Gerador.wCampo(tcDe2, '', 'vlrSenarSuspTotal', 1, 14, 0, Self.FinfoComProd.ideEstab.vlrSenarSuspTotal);

  GerartipoCom(Self.FinfoComProd.ideEstab.tipoCom);

  Gerador.wGrupo('/ideEstab');
end;

procedure TevtComProd.GerartipoCom(Lista: TtipoComCollection);
var
  item: TtipoComCollectionItem;
  i: Integer;
begin
  for i := 0 to Lista.Count - 1 do
  begin
    Item := Lista.Items[i];

    Gerador.wGrupo('tipoCom');

    Gerador.wCampo(tcStr, '', 'indCom',      1,  1, 1, indComToStr( item.indCom ));
    Gerador.wCampo(tcDe2, '', 'vlrRecBruta', 1, 14, 1, item.vlrRecBruta);

    GerarinfoProc(item.infoProc);

    Gerador.wGrupo('/tipoCom');
  end;

  if Lista.Count > 4 then
    Gerador.wAlerta('', 'tipoCom', 'Lista de Tipos de Comercialização', ERR_MSG_MAIOR_MAXIMO + '4');
end;

procedure TevtComProd.GerarinfoProc(Lista: TinfoProcCollection);
var
  item: TinfoProcCollectionItem;
  i: Integer;
begin
  for i := 0 to Lista.Count - 1 do
  begin
    Item := Lista.Items[i];

    Gerador.wGrupo('infoProc');

    Gerador.wCampo(tcStr, '', 'tpProc',       1,  1, 1, TpProcToStr( item.tpProc ));
    Gerador.wCampo(tcStr, '', 'nrProc',       1, 21, 1, item.nrProc);
    Gerador.wCampo(tcStr, '', 'codSusp',      1, 14, 0, item.codSusp);
    Gerador.wCampo(tcDe2, '', 'vlrCPSusp',    1, 14, 0, item.vlrCPSusp);
    Gerador.wCampo(tcDe2, '', 'vlrRatSusp',   1, 14, 0, item.vlrRatSusp);
    Gerador.wCampo(tcDe2, '', 'vlrSenarSusp', 1, 14, 0, item.vlrSenarSusp);

    Gerador.wGrupo('/infoProc');
  end;

  if Lista.Count > 50 then
    Gerador.wAlerta('', 'infoProc', 'Lista de Informações de Processos', ERR_MSG_MAIOR_MAXIMO + '50');
end;

function TevtComProd.GerarXML: Boolean;
begin
  try
    Self.VersaoDF := TACBrReinf(FACBrReinf).Configuracoes.Geral.VersaoDF;

    Self.Id := GerarChaveReinf(now, self.ideContri.NrInsc, self.Sequencial, self.ideContri.TpInsc);

    GerarCabecalho('evtInfoProdRural');
    Gerador.wGrupo('evtComProd id="' + Self.Id + '"');

    GerarIdeEvento2(Self.IdeEvento);
    GerarideContri(Self.ideContri);

    Gerador.wGrupo('infoComProd');

    GerarideEstab;

    Gerador.wGrupo('/infoComProd');
    Gerador.wGrupo('/evtComProd');

    GerarRodape;

    FXML := Gerador.ArquivoFormatoXML;
//    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtComProd');

//    Validar(schevtInfoProdRural);
  except on e:exception do
    raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '');
end;

function TevtComProd.LerArqIni(const AIniString: String): Boolean;
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
      sSecao := 'evtComProd';
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
      infoComProd.ideEstab.tpInscEstab       := StrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInscEstab', '1'));
      infoComProd.ideEstab.nrInscEstab       := INIRec.ReadString(sSecao, 'nrInscEstab', EmptyStr);
      infoComProd.ideEstab.vlrRecBrutaTotal  := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrRecBrutaTotal', ''), 0);
      infoComProd.ideEstab.vlrCPApur         := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrCPApur', ''), 0);
      infoComProd.ideEstab.vlrRatApur        := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrRatApur', ''), 0);
      infoComProd.ideEstab.vlrSenarApur      := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrSenarApur', ''), 0);
      infoComProd.ideEstab.vlrCPSuspTotal    := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrCPSuspTotal', ''), 0);
      infoComProd.ideEstab.vlrRatSuspTotal   := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrRatSuspTotal', ''), 0);
      infoComProd.ideEstab.vlrSenarSuspTotal := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrSenarSuspTotal', ''), 0);

      with infoComProd.ideEstab do
      begin
        I := 1;
        while true do
        begin
          // de 1 até 3
          sSecao := 'tipoCom' + IntToStrZero(I, 1);
          sFim   := INIRec.ReadString(sSecao, 'indCom', 'FIM');

          if (sFim = 'FIM') or (Length(sFim) <= 0) then
            break;

          with tipoCom.New do
          begin
            indCom      := StrToindCom(Ok, sFim);
            vlrRecBruta := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrRecBruta', ''), 0);

            J := 1;
            while true do
            begin
              // de 01 até 50
              sSecao := 'infoProc' + IntToStrZero(I, 1) + IntToStrZero(J, 2);
              sFim   := INIRec.ReadString(sSecao, 'tpProc', 'FIM');

              if (sFim = 'FIM') or (Length(sFim) <= 0) then
                break;

              with infoProc.New do
              begin
                tpProc       := StrToTpProc(Ok, sFim);
                nrProc       := INIRec.ReadString(sSecao, 'nrProc', '');
                codSusp      := INIRec.ReadString(sSecao, 'codSusp', '');
                vlrCPSusp    := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrCPSusp', ''), 0);
                vlrRatSusp   := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrRatSusp', ''), 0);
                vlrSenarSusp := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrSenarSusp', ''), 0);
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
