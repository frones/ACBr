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

unit pcesS2231;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$ELSE}
   Contnrs,
  {$IFEND}
  ACBrBase, pcnConversao, ACBrUtil.Strings,
  pcesCommon, pcesConversaoeSocial, pcesGerador;

type

  TS2231CollectionItem = class;
  TEvtCessao = class;
  TInfoCessao = class;
  TIniCessao = class;
  TFimCessao = class;

  TS2231Collection = class(TeSocialCollection)
  private
    function GetItem(Index: Integer): TS2231CollectionItem;
    procedure SetItem(Index: Integer; Value: TS2231CollectionItem);
  public
    function Add: TS2231CollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TS2231CollectionItem;
    property Items[Index: Integer]: TS2231CollectionItem read GetItem write SetItem; default;
  end;

  TS2231CollectionItem = class(TObject)
  private
    FTipoEvento : TTipoEvento;
    FEvtCessao: TEvtCessao;
  public
    constructor Create(AOwner: TComponent);
    destructor  Destroy; override;
    
    property TipoEvento: TTipoEvento read FTipoEvento;
    property EvtCessao: TEvtCessao read FEvtCessao write FEvtCessao;
  end;

  TEvtCessao = class(TeSocialEvento)
  private
    FIdeEvento: TIdeEvento2;
    FIdeEmpregador: TIdeEmpregador;
    FIdeVinculo: TIdeVinculo;
    FInfoCessao: TInfoCessao;

    procedure GerarInfoCessao(pInfoCessao: TInfoCessao);
    procedure GerarIniCessao(pIniCessao: TIniCessao);
    procedure GerarFimCessao(pFimCessao: TFimCessao);
  public
    constructor Create(AACBreSocial: TObject); override;
    destructor Destroy; override;

    function GerarXML: boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property IdeEvento: TIdeEvento2 read FIdeEvento write FIdeEvento;
    property IdeEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property IdeVinculo: TIdeVinculo read FIdeVinculo write FIdeVinculo;
    property InfoCessao: TInfoCessao read FInfoCessao write FInfoCessao;
  end;

  TIniCessao = class
  private
    FDtIniCessao: TDateTime;
    FCnpjCess: string;
    FRespRemun: tpSimNao;
  public
    property dtIniCessao: TDateTime read FDtIniCessao write FDtIniCessao;
    property cnpjCess: string read FCnpjCess write FCnpjCess;
    property respRemun: tpSimNao read FRespRemun write FRespRemun;
  end;

  TFimCessao = class
  private
    FDtTermCessao: TDateTime;
  public
    property dtTermCessao: TDateTime read FDtTermCessao write FDtTermCessao;
  end;

  TInfoCessao = class(TObject)
  private
    FIniCessao: TIniCessao;
    FFimCessao: TFimCessao;
    
    function getIniCessao(): TIniCessao;
    function getFimCessao(): TFimCessao;
  public
    constructor Create;
    destructor  Destroy; override;
    
    function iniCessaoInst(): Boolean;
    function fimCessaoInst(): Boolean;
    
    property iniCessao: TIniCessao read getIniCessao write FIniCessao;
    property fimCessao: TFimCessao read getFimCessao write FFimCessao;
  end;

implementation

uses
  IniFiles,
  ACBrUtil.FilesIO,
  ACBrUtil.DateTime,
  ACBreSocial;

{ TS2231Collection }

function TS2231Collection.Add: TS2231CollectionItem;
begin
  Result := Self.New;
end;

function TS2231Collection.GetItem(Index: Integer): TS2231CollectionItem;
begin
   Result := TS2231CollectionItem(inherited Items[Index]);
end;

procedure TS2231Collection.SetItem(Index: Integer; Value: TS2231CollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TS2231Collection.New: TS2231CollectionItem;
begin
  Result := TS2231CollectionItem.Create(FACBreSocial);
  Self.Add(Result);
end;

{ TS2231CollectionItem }

constructor TS2231CollectionItem.Create(AOwner: TComponent);
begin
  inherited Create;
  FTipoEvento := teS2231;
  FEvtCessao  := TEvtCessao.Create(AOwner);
end;

destructor TS2231CollectionItem.Destroy;
begin
  FEvtCessao.Free;

  inherited;
end;

{ TInfoCessao }

constructor TInfoCessao.Create;
begin
  inherited Create;

  FIniCessao := nil;
  FFimCessao := nil;
end;

destructor TInfoCessao.Destroy;
begin
  if iniCessaoInst() then
    FreeAndNil(FIniCessao);
  if fimCessaoInst() then
    FreeAndNil(FFimCessao);

  inherited;
end;

function TInfoCessao.getIniCessao: TIniCessao;
begin
  if not(Assigned(FIniCessao)) then
    FIniCessao := TIniCessao.Create;
  Result := FIniCessao;
end;

function TInfoCessao.getFimCessao: TFimCessao;
begin
  if not(Assigned(FFimCessao)) then
    FFimCessao := TFimCessao.Create;
  Result := FFimCessao;
end;

function TInfoCessao.iniCessaoInst: Boolean;
begin
  Result := Assigned(FIniCessao);
end;

function TInfoCessao.fimCessaoInst: Boolean;
begin
  Result := Assigned(FFimCessao);
end;

{ TEvtCessao }

constructor TEvtCessao.Create(AACBreSocial: TObject);
begin
  inherited Create(AACBreSocial);

  FIdeEvento     := TIdeEvento2.Create;
  FIdeEmpregador := TIdeEmpregador.Create;
  FIdeVinculo    := TIdeVinculo.Create;
  FInfoCessao    := TInfoCessao.Create;
end;

destructor TEvtCessao.Destroy;
begin
  FreeAndNil(FIdeEvento);
  FreeAndNil(FIdeEmpregador);
  FreeAndNil(FIdeVinculo);
  FreeAndNil(FInfoCessao);

  inherited;
end;

procedure TEvtCessao.GerarInfoCessao(pInfoCessao: TInfoCessao);
begin
  Gerador.wGrupo('infoCessao');

  if pInfoCessao.iniCessaoInst() then
    GerarIniCessao(pInfoCessao.iniCessao);
    
  if pInfoCessao.fimCessaoInst() then
    GerarFimCessao(pInfoCessao.fimCessao);
  
  Gerador.wGrupo('/infoCessao');
end;

procedure TEvtCessao.GerarIniCessao(pIniCessao: TIniCessao);
begin
  if pIniCessao.dtIniCessao > 0 then
  begin
    Gerador.wGrupo('iniCessao');

    Gerador.wCampo(tcDat, '', 'dtIniCessao', 10, 10, 1, pIniCessao.dtIniCessao);
    Gerador.wCampo(tcStr, '', 'cnpjCess',    14, 14, 1, pIniCessao.cnpjCess);
    Gerador.wCampo(tcStr, '', 'respRemun',    1,  1, 1, eSSimNaoToStr(pIniCessao.respRemun));

    Gerador.wGrupo('/iniCessao');
  end;
end;

procedure TEvtCessao.GerarFimCessao(pFimCessao: TFimCessao);
begin
  if pFimCessao.dtTermCessao > 0 then
  begin
    Gerador.wGrupo('fimCessao');

    Gerador.wCampo(tcDat, '', 'dtTermCessao', 10, 10, 1, pFimCessao.dtTermCessao);

    Gerador.wGrupo('/fimCessao');
  end;
end;

function TEvtCessao.GerarXML: boolean;
begin
  try
    inherited GerarXML;
    Self.VersaoDF := TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF;
     
    Self.Id := GerarChaveEsocial(now, self.ideEmpregador.NrInsc, self.Sequencial);

    GerarCabecalho('evtCessao');

    Gerador.wGrupo('evtCessao Id="' + Self.Id + '"');

    GerarIdeEvento2(self.IdeEvento);
    GerarIdeEmpregador(self.IdeEmpregador);
    GerarIdeVinculo(self.IdeVinculo, False, True);
    GerarInfoCessao(self.InfoCessao);

    Gerador.wGrupo('/evtCessao');

    GerarRodape;

    FXML := Gerador.ArquivoFormatoXML;
//    XML := Assinar(Gerador.ArquivoFormatoXML, 'EvtCessao');

//    Validar(schEvtCessao);
  except on e:exception do
    raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '')
end;

function TEvtCessao.LerArqIni(const AIniString: String): Boolean;
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
      sSecao := 'evtCessao';
      Id         := INIRec.ReadString(sSecao, 'Id', '');
      Sequencial := INIRec.ReadInteger(sSecao, 'Sequencial', 0);

      sSecao := 'ideEvento';
      ideEvento.indRetif    := eSStrToIndRetificacao(Ok, INIRec.ReadString(sSecao, 'indRetif', '1'));
      ideEvento.NrRecibo    := INIRec.ReadString(sSecao, 'nrRecibo', EmptyStr);
      ideEvento.ProcEmi     := eSStrToProcEmi(Ok, INIRec.ReadString(sSecao, 'procEmi', '1'));
      ideEvento.VerProc     := INIRec.ReadString(sSecao, 'verProc', EmptyStr);

      sSecao := 'ideEmpregador';
      ideEmpregador.TpInsc       := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
      ideEmpregador.NrInsc       := INIRec.ReadString(sSecao, 'nrInsc', EmptyStr);

      sSecao := 'ideVinculo';
      ideVinculo.CpfTrab   := INIRec.ReadString(sSecao, 'cpfTrab', EmptyStr);
      ideVinculo.Matricula := INIRec.ReadString(sSecao, 'matricula', EmptyStr);

      sSecao := 'iniCessao';
      if INIRec.ReadString(sSecao, 'dtIniCessao', '') <> '' then
      begin
        InfoCessao.iniCessao.dtIniCessao := StringToDateTime(INIRec.ReadString(sSecao, 'dtIniCessao', '0'));
        InfoCessao.iniCessao.cnpjCess    := INIRec.ReadString(sSecao, 'cnpjCess', '00');
        InfoCessao.iniCessao.respRemun   := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'respRemun', 'S'));
      end;

      sSecao := 'fimCessao';
      InfoCessao.fimCessao.dtTermCessao := StringToDateTime(INIRec.ReadString(sSecao, 'dtTermCessao', '0'));

    end;

    GerarXML;
    XML := FXML;
  finally
    INIRec.Free;
  end;

end;

end.
