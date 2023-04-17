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

unit pcnReinfR4099;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IFEND}
  ACBrBase,
  pcnConversao, pcnGerador, ACBrUtil.FilesIO,
  pcnCommonReinf, pcnConversaoReinf, pcnGeradorReinf;

type
  {Classes específicas deste evento}
  TR4099Collection = class;
  TR4099CollectionItem = class;
  TevtFech = class;
  TideRespInf = class;
  TinfoFech = class;

  { TR4099Collection }
  TR4099Collection = class(TReinfCollection)
  private
    function GetItem(Index: Integer): TR4099CollectionItem;
    procedure SetItem(Index: Integer; Value: TR4099CollectionItem);
  public
    function Add: TR4099CollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TR4099CollectionItem;

    property Items[Index: Integer]: TR4099CollectionItem read GetItem write SetItem; default;
  end;

  { TR4099CollectionItem }
  TR4099CollectionItem = class(TObject)
  private
    FTipoEvento: TTipoEvento;
    FevtFech: TevtFech;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    property TipoEvento: TTipoEvento read FTipoEvento;
    property evtFech: TevtFech read FevtFech write FevtFech;
  end;

  { TevtFech }
  TevtFech = class(TReinfEvento) //Classe do elemento principal do XML do evento!
  private
    FIdeEvento: TIdeEvento2;
    FideContri: TideContri;
    FideRespInf: TideRespInf;
    FinfoFech: TinfoFech;

    {Geradores específicos desta classe}
    procedure GerarideRespInf;
    procedure GerarinfoFech;
  public
    constructor Create(AACBrReinf: TObject); override;
    destructor  Destroy; override;

    function GerarXML: Boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property ideEvento: TIdeEvento2 read FIdeEvento write FIdeEvento;
    property ideContri: TideContri read FideContri write FideContri;
    property ideRespInf: TideRespInf read FideRespInf write FideRespInf;
    property infoFech: TinfoFech read FinfoFech write FinfoFech;
  end;

  { TideRespInf }
  TideRespInf = class(TObject)
  private
    FnmResp: string;
    FcpfResp: string;
    Ftelefone: string;
    Femail: string;
  public
    property nmResp: string read FnmResp write FnmResp;
    property cpfResp: string read FcpfResp write FcpfResp;
    property telefone: string read Ftelefone write Ftelefone;
    property email: string read Femail write Femail;
  end;

  { TinfoFech }
  TinfoFech = class(TObject)
  private
    FfechRet: TtpFechRet;
  public
    property fechRet: TtpFechRet read FfechRet write FfechRet;
  end;

implementation

uses
  IniFiles,
  ACBrReinf, ACBrDFeUtil;

{ TR4099Collection }

function TR4099Collection.Add: TR4099CollectionItem;
begin
  Result := Self.New;
end;

function TR4099Collection.GetItem(Index: Integer): TR4099CollectionItem;
begin
  Result := TR4099CollectionItem(inherited Items[Index]);
end;

function TR4099Collection.New: TR4099CollectionItem;
begin
  Result := TR4099CollectionItem.Create(FACBrReinf);
  Self.Add(Result);
end;

procedure TR4099Collection.SetItem(Index: Integer; Value: TR4099CollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TR4099CollectionItem }

constructor TR4099CollectionItem.Create(AOwner: TComponent);
begin
  inherited Create;

  FTipoEvento    := teR4099;
  FevtFech := TevtFech.Create(AOwner);
end;

destructor TR4099CollectionItem.Destroy;
begin
  inherited;

  FevtFech.Free;
end;

{ TevtFech }

constructor TevtFech.Create(AACBrReinf: TObject);
begin
  inherited Create(AACBrReinf);

  FideContri  := TideContri.create;
  FIdeEvento  := TIdeEvento2.create;
  FideRespInf := TideRespInf.Create;
  FinfoFech   := TinfoFech.Create;
end;

destructor TevtFech.Destroy;
begin
  FideContri.Free;
  FIdeEvento.Free;
  FideRespInf.Free;
  FinfoFech.Free;

  inherited;
end;

procedure TevtFech.GerarideRespInf;
begin
  if (FideRespInf.nmResp <> EmptyStr) and (FideRespInf.cpfResp <> EmptyStr) then
  begin
    Gerador.wGrupo('ideRespInf');

    Gerador.wCampo(tcStr, '', 'nmResp',    1, 70, 1, FideRespInf.nmResp);
    Gerador.wCampo(tcStr, '', 'cpfResp',  11, 11, 1, FideRespInf.cpfResp);
    Gerador.wCampo(tcStr, '', 'telefone',  1, 13, 0, FideRespInf.telefone);
    Gerador.wCampo(tcStr, '', 'email',     1, 60, 0, FideRespInf.email);

    Gerador.wGrupo('/ideRespInf');
  end;
end;

procedure TevtFech.GerarinfoFech;
begin
  Gerador.wGrupo('infoFech');
  Gerador.wCampo(tcStr, '', 'fechRet', 1, 1, 1, tpFechRetToStr(FinfoFech.fechRet));
  Gerador.wGrupo('/infoFech');
end;

function TevtFech.GerarXML: Boolean;
begin
  try
    Self.VersaoDF := TACBrReinf(FACBrReinf).Configuracoes.Geral.VersaoDF;

    Self.Id := GerarChaveReinf(now, self.ideContri.NrInsc, self.Sequencial, self.ideContri.TpInsc);

    GerarCabecalho('evt4099FechamentoDirf');
    Gerador.wGrupo('evtFech id="' + Self.Id + '"');

    GerarIdeEvento2(Self.IdeEvento, True, False);
    GerarideContri(Self.ideContri);

    GerarideRespInf;
    GerarinfoFech;

    Gerador.wGrupo('/evtFech');

    GerarRodape;

    FXML := Gerador.ArquivoFormatoXML;
  except on e:exception do
    raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '');
end;

function TevtFech.LerArqIni(const AIniString: String): Boolean;
var
  INIRec: TMemIniFile;
  Ok: Boolean;
  sSecao: String;
begin
  Self.VersaoDF := TACBrReinf(FACBrReinf).Configuracoes.Geral.VersaoDF;

  Result := True;

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);

    with Self do
    begin
      sSecao := 'evtFech';
      Id         := INIRec.ReadString(sSecao, 'Id', '');
      Sequencial := INIRec.ReadInteger(sSecao, 'Sequencial', 0);

      sSecao := 'ideEvento';
      ideEvento.perApur := INIRec.ReadString(sSecao, 'perApur', EmptyStr);
      ideEvento.ProcEmi := StrToProcEmiReinf(Ok, INIRec.ReadString(sSecao, 'procEmi', '1'));
      ideEvento.VerProc := INIRec.ReadString(sSecao, 'verProc', EmptyStr);

      sSecao := 'ideContri';
      ideContri.TpInsc := StrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
      ideContri.NrInsc := INIRec.ReadString(sSecao, 'nrInsc', EmptyStr);

      sSecao := 'ideRespInf';
      if INIRec.ReadString(sSecao, 'nmResp', EmptyStr) <> '' then
      begin
        ideRespInf.nmResp   := INIRec.ReadString(sSecao, 'nmResp', EmptyStr);
        ideRespInf.cpfResp  := INIRec.ReadString(sSecao, 'cpfResp', EmptyStr);
        ideRespInf.telefone := INIRec.ReadString(sSecao, 'telefone', EmptyStr);
        ideRespInf.email    := INIRec.ReadString(sSecao, 'email', EmptyStr);
      end;

      sSecao := 'infoFech';
      infoFech.fechRet := StrTotpFechRet(Ok, INIRec.ReadString(sSecao, 'fechRet', '0'));
    end;

    GerarXML;
    XML := FXML;
  finally
    INIRec.Free;
  end;
end;

end.
