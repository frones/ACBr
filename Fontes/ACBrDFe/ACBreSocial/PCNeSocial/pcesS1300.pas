{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
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

{******************************************************************************
|* Historico
|*
|* 27/10/2015: Jean Carlo Cantu, Tiago Ravache
|*  - Doação do componente para o Projeto ACBr
|* 28/08/2017: Leivio Fontenele - leivio@yahoo.com.br
|*  - Implementação comunicação, envelope, status e retorno do componente com webservice.
******************************************************************************}

{$I ACBr.inc}

unit pcesS1300;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IfEnd}
  ACBrBase,
  ACBrDFeConsts,
  pcnConversao, pcnGerador,
  pcesCommon, pcesConversaoeSocial, pcesGerador;

type
  TS1300CollectionItem = class;
  TEvtContrSindPatr = class;
  TContribSindColecao = class;

  TS1300Collection = class(TeSocialCollection)
  private
    function GetItem(Index: Integer): TS1300CollectionItem;
    procedure SetItem(Index: Integer; Value: TS1300CollectionItem);
  public
    function Add: TS1300CollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TS1300CollectionItem;
    property Items[Index: Integer]: TS1300CollectionItem read GetItem write SetItem; default;
  end;

  TS1300CollectionItem = class(TObject)
  private
    FTipoEvento: TTipoEvento;
    FEvtContrSindPatr: TEvtContrSindPatr;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    property TipoEvento: TTipoEvento read FTipoEvento;
    property EvtContrSindPatr: TEvtContrSindPatr read FEvtContrSindPatr write FEvtContrSindPatr;
  end;

  TEvtContrSindPatr = class(TESocialEvento)
  private
    FIdeEvento: TIdeEvento3;
    FIdeEmpregador: TIdeEmpregador;
    FContribSind: TContribSindColecao;

    {Geradores específicos da classe}
    procedure GerarContribSind();
  public
    constructor Create(AACBreSocial: TObject); override;
    destructor  Destroy; override;

    function GerarXML: boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property IdeEvento: TIdeEvento3 read FIdeEvento write FIdeEvento;
    property IdeEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property ContribSind: TContribSindColecao read FContribSind write FContribSind;
  end;

  TContribSindItem = class(TObject)
  private
    FcnpjSindic: string;
    FvlrContribSind: Double;
  public
    property cnpjSindic: string read FcnpjSindic write FcnpjSindic;
    property vlrContribSind: Double read FvlrContribSind write FvlrContribSind;
  end;

  TContribSindColecao = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TContribSindItem;
    procedure SetItem(Index: Integer; const Value: TContribSindItem);
  public
    function Add: TContribSindItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TContribSindItem;
    property Items[Index: Integer]: TContribSindItem read GetItem write SetItem;
  end;

implementation

uses
  IniFiles,
  ACBrUtil.Base,
  ACBrUtil.FilesIO,
  ACBreSocial;

{ TS1300Collection }

function TS1300Collection.Add: TS1300CollectionItem;
begin
  Result := Self.New;
end;

function TS1300Collection.GetItem(Index: Integer): TS1300CollectionItem;
begin
  Result := TS1300CollectionItem(inherited Items[Index]);
end;

procedure TS1300Collection.SetItem(Index: Integer;
  Value: TS1300CollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TS1300Collection.New: TS1300CollectionItem;
begin
  Result := TS1300CollectionItem.Create(FACBreSocial);
  Self.Add(Result);
end;

{TS1300CollectionItem}
constructor TS1300CollectionItem.Create(AOwner: TComponent);
begin
  inherited Create;
  FTipoEvento       := teS1300;
  FEvtContrSindPatr := TEvtContrSindPatr.Create(AOwner);
end;

destructor TS1300CollectionItem.Destroy;
begin
  FEvtContrSindPatr.Free;

  inherited;
end;

{ TEvtSolicTotal }
constructor TEvtContrSindPatr.Create(AACBreSocial: TObject);
begin
  inherited Create(AACBreSocial);

  FIdeEvento     := TIdeEvento3.Create;
  FIdeEmpregador := TIdeEmpregador.Create;
  FContribSind   := TContribSindColecao.Create;
end;

destructor TEvtContrSindPatr.destroy;
begin
  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FContribSind.Free;

  inherited;
end;

procedure TEvtContrSindPatr.GerarContribSind;
var
  i: Integer;
begin
  for i := 0 to ContribSind.Count - 1 do
  begin
    Gerador.wGrupo('contribSind');

    Gerador.wCampo(tcStr, '', 'cnpjSindic',     14, 14, 1, ContribSind.Items[i].cnpjSindic);
    Gerador.wCampo(tcDe2, '', 'vlrContribSind',  1, 14, 1, ContribSind.Items[i].vlrContribSind);

    Gerador.wGrupo('/contribSind');
  end;

  if ContribSind.Count > 999 then
    Gerador.wAlerta('', 'contribSind', 'Lista de Contribuição Sindical', ERR_MSG_MAIOR_MAXIMO + '999');
end;

function TEvtContrSindPatr.GerarXML: boolean;
begin
  try
    inherited GerarXML;
    Self.VersaoDF := TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF;
     
    Self.Id := GerarChaveEsocial(now, self.ideEmpregador.NrInsc, self.Sequencial);

    GerarCabecalho('evtContrSindPatr');
    Gerador.wGrupo('evtContrSindPatr Id="' + Self.Id + '"');

    GerarIdeEvento3(self.IdeEvento);
    GerarIdeEmpregador(self.IdeEmpregador);
    GerarContribSind;

    Gerador.wGrupo('/evtContrSindPatr');

    GerarRodape;

    FXML := Gerador.ArquivoFormatoXML;
//    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtContrSindPatr');

//    Validar(schevtContrSindPatr);
  except on e:exception do
    raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '')
end;

function TContribSindColecao.Add: TContribSindItem;
begin
  Result := Self.New;
end;

function TContribSindColecao.GetItem(Index: Integer): TContribSindItem;
begin
  Result := TContribSindItem(inherited Items[Index]);
end;

procedure TContribSindColecao.SetItem(Index: Integer; const Value: TContribSindItem);
begin
  inherited Items[Index] := Value;
end;

function TEvtContrSindPatr.LerArqIni(const AIniString: String): Boolean;
var
  INIRec: TMemIniFile;
  Ok: Boolean;
  sSecao, sFim: String;
  I: Integer;
begin
  Result := True;

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);

    with Self do
    begin
      sSecao := 'evtContrSindPatr';
      Id         := INIRec.ReadString(sSecao, 'Id', '');
      Sequencial := INIRec.ReadInteger(sSecao, 'Sequencial', 0);

      sSecao := 'ideEvento';
      ideEvento.indRetif    := eSStrToIndRetificacao(Ok, INIRec.ReadString(sSecao, 'indRetif', '1'));
      ideEvento.NrRecibo    := INIRec.ReadString(sSecao, 'nrRecibo', EmptyStr);
      ideEvento.IndApuracao := eSStrToIndApuracao(Ok, INIRec.ReadString(sSecao, 'indApuracao', '1'));
      ideEvento.perApur     := INIRec.ReadString(sSecao, 'perApur', EmptyStr);
      ideEvento.ProcEmi     := eSStrToProcEmi(Ok, INIRec.ReadString(sSecao, 'procEmi', '1'));
      ideEvento.VerProc     := INIRec.ReadString(sSecao, 'verProc', EmptyStr);

      sSecao := 'ideEmpregador';
      ideEmpregador.OrgaoPublico := (TACBreSocial(FACBreSocial).Configuracoes.Geral.TipoEmpregador = teOrgaoPublico);
      ideEmpregador.TpInsc       := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
      ideEmpregador.NrInsc       := INIRec.ReadString(sSecao, 'nrInsc', EmptyStr);

      I := 1;
      while true do
      begin
        // de 001 até 999
        sSecao := 'contribSind' + IntToStrZero(I, 3);
        sFim   := INIRec.ReadString(sSecao, 'cnpjSindic', 'FIM');

        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break;

        with contribSind.New do
        begin
          cnpjSindic     := sFim;
          vlrContribSind := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrContribSind', ''), 0);
        end;

        Inc(I);
      end;
    end;

    GerarXML;
    XML := FXML;
  finally
    INIRec.Free;
  end;
end;

function TContribSindColecao.New: TContribSindItem;
begin
  Result := TContribSindItem.Create;
  Self.Add(Result);
end;

end.
