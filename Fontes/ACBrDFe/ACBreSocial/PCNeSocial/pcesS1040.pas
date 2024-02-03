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
{                              Leivio Fontenele                                }
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

unit pcesS1040;

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
  ACBrBase, pcnConversao,
  pcesCommon, pcesConversaoeSocial, pcesGerador;

type
  TS1040CollectionItem = class;
  TEvtTabFuncao = class;
  TInfoFuncao = class;

  TS1040Collection = class(TeSocialCollection)
  private
    function GetItem(Index: Integer): TS1040CollectionItem;
    procedure SetItem(Index: Integer; Value: TS1040CollectionItem);
  public
    function Add: TS1040CollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TS1040CollectionItem;
    property Items[Index: Integer]: TS1040CollectionItem read GetItem write SetItem; default;
  end;

  TS1040CollectionItem = class(TObject)
  private
    FTipoEvento: TTipoEvento;
    FEvtTabFuncao: TEvtTabFuncao;
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;
    property TipoEvento: TTipoEvento read FTipoEvento;
    property EvtTabFuncao: TEvtTabFuncao read FEvtTabFuncao write FEvtTabFuncao;
  end;

  TEvtTabFuncao = class(TeSocialEvento)
  private
    FModoLancamento: TModoLancamento;
    fIdeEvento: TIdeEvento;
    fIdeEmpregador: TIdeEmpregador;
    fInfoFuncao: TInfoFuncao;
    FACBreSocial: TObject;

    {Geradores específicos da classe}
    procedure GerarDadosFuncao;
    procedure GerarIdeFuncao;
  public
    constructor Create(AACBreSocial: TObject); override;
    destructor  Destroy; override;

    function GerarXML: boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property ModoLancamento: TModoLancamento read FModoLancamento write FModoLancamento;
    property IdeEvento: TIdeEvento read fIdeEvento write fIdeEvento;
    property IdeEmpregador: TIdeEmpregador read fIdeEmpregador write fIdeEmpregador;
    property InfoFuncao: TInfoFuncao read fInfoFuncao write fInfoFuncao;
  end;

  TIdeFuncao = class(TObject)
  private
    FCodFuncao: string;
    FIniValid: string;
    FFimValid: string;
  public
    property codFuncao: string read FCodFuncao write FCodFuncao;
    property iniValid: string read FIniValid write FIniValid;
    property fimValid: string read FFimValid write FFimValid;
  end;

  TDadosFuncao = class(TObject)
  private
    FDscFuncao: string;
    FCodCBO: string;
  public
    property dscFuncao: string read FDscFuncao write FDscFuncao;
    property codCBO: string read FCodCBO write FCodCBO;
  end;

  TInfoFuncao = class(TObject)
  private
    fIdeFuncao: TIdeFuncao;
    fDadosFuncao: TDadosFuncao;
    fNovaValidade: TIdePeriodo;

    function getDadosFuncao: TDadosFuncao;
    function getNovaValidade: TIdePeriodo;
  public
    constructor Create;
    destructor Destroy; override;

    function dadosFuncaoInst(): Boolean;
    function novaValidadeInst(): Boolean;

    property IdeFuncao: TIdeFuncao read fIdeFuncao write fIdeFuncao;
    property DadosFuncao: TDadosFuncao read getDadosFuncao write fDadosFuncao;
    property NovaValidade: TIdePeriodo read getNovaValidade write fNovaValidade;
  end;

implementation

uses
  IniFiles,
  ACBrUtil.FilesIO,
  ACBreSocial;

{ TS1040Collection }

function TS1040Collection.Add: TS1040CollectionItem;
begin
  Result := Self.New;
end;

function TS1040Collection.GetItem(Index: Integer): TS1040CollectionItem;
begin
  Result := TS1040CollectionItem(inherited Items[Index]);
end;

procedure TS1040Collection.SetItem(Index: Integer;
  Value: TS1040CollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TS1040Collection.New: TS1040CollectionItem;
begin
  Result := TS1040CollectionItem.Create(FACBreSocial);
  Self.Add(Result);
end;

{ TS1040CollectionItem }

constructor TS1040CollectionItem.Create(AOwner: TComponent);
begin
  inherited Create;
  FTipoEvento   := teS1040;
  FEvtTabFuncao := TEvtTabFuncao.Create(AOwner);
end;

destructor TS1040CollectionItem.Destroy;
begin
  FEvtTabFuncao.Free;

  inherited;
end;

{ TInfoFuncao }

constructor TInfoFuncao.Create;
begin
  inherited Create;
  fIdeFuncao    := TIdeFuncao.Create;
  fDadosFuncao  := nil;
  fNovaValidade := nil;
end;

function TInfoFuncao.dadosFuncaoInst: Boolean;
begin
  Result := Assigned(fDadosFuncao);
end;

destructor TInfoFuncao.destroy;
begin
  fIdeFuncao.Free;
  FreeAndNil(fDadosFuncao);
  FreeAndNil(fNovaValidade);

  inherited;
end;

function TInfoFuncao.getDadosFuncao: TDadosFuncao;
begin
  if Not(Assigned(fDadosFuncao)) then
    fDadosFuncao := TDadosFuncao.Create;
  Result := fDadosFuncao;
end;

function TInfoFuncao.getNovaValidade: TIdePeriodo;
begin
  if Not(Assigned(FNovaValidade)) then
    FNovaValidade := TIdePeriodo.Create;
  Result := FNovaValidade;
end;

function TInfoFuncao.novaValidadeInst: Boolean;
begin
  Result := Assigned(FNovaValidade);
end;

{ TEvtTabFuncao }

constructor TEvtTabFuncao.Create(AACBreSocial: TObject);
begin
  inherited Create(AACBreSocial);

  FACBreSocial := AACBreSocial;
  fIdeEvento := TIdeEvento.Create;
  fIdeEmpregador := TIdeEmpregador.Create;
  fInfoFuncao := TInfoFuncao.Create;
end;

destructor TEvtTabFuncao.destroy;
begin
  fIdeEvento.Free;
  fIdeEmpregador.Free;
  fInfoFuncao.Free;

  inherited;
end;

procedure TEvtTabFuncao.GerardadosFuncao;
begin
  Gerador.wGrupo('dadosFuncao');

  Gerador.wCampo(tcStr, '', 'dscFuncao', 1, 100, 1, self.InfoFuncao.DadosFuncao.dscFuncao);
  Gerador.wCampo(tcStr, '', 'codCBO',    1,   6, 1, self.InfoFuncao.DadosFuncao.codCBO);

  Gerador.wGrupo('/dadosFuncao');
end;

procedure TEvtTabFuncao.GerarIdeFuncao;
begin
  Gerador.wGrupo('ideFuncao');

  Gerador.wCampo(tcStr, '', 'codFuncao', 1, 30, 1, self.InfoFuncao.IdeFuncao.codFuncao);
  Gerador.wCampo(tcStr, '', 'iniValid',  7,  7, 1, self.InfoFuncao.IdeFuncao.iniValid);
  Gerador.wCampo(tcStr, '', 'fimValid',  7,  7, 0, self.InfoFuncao.IdeFuncao.fimValid);

  Gerador.wGrupo('/ideFuncao');
end;

function TEvtTabFuncao.GerarXML: boolean;
begin
  try
    inherited GerarXML;
    Self.VersaoDF := TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF;
     
    Self.Id := GerarChaveEsocial(now, self.ideEmpregador.NrInsc, self.Sequencial);

    GerarCabecalho('evtTabFuncao');
    Gerador.wGrupo('evtTabFuncao Id="' + Self.Id + '"');

    GerarIdeEvento(self.IdeEvento);
    GerarIdeEmpregador(self.IdeEmpregador);

    Gerador.wGrupo('infoFuncao');

    GerarModoAbertura(Self.ModoLancamento);
    GerarIdeFuncao;

    if Self.ModoLancamento <> mlExclusao then
    begin
      GerarDadosFuncao;

      if Self.ModoLancamento = mlAlteracao then
        if (InfoFuncao.novaValidadeInst()) then
          GerarIdePeriodo(self.InfoFuncao.NovaValidade, 'novaValidade');
    end;

    GerarModoFechamento(Self.ModoLancamento);

    Gerador.wGrupo('/infoFuncao');
    Gerador.wGrupo('/evtTabFuncao');

    GerarRodape;

    FXML := Gerador.ArquivoFormatoXML;
//    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtTabFuncao');

//    Validar(schevtTabFuncao);
  except on e:exception do
    raise Exception.Create(Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '')
end;

function TEvtTabFuncao.LerArqIni(const AIniString: String): Boolean;
var
  INIRec: TMemIniFile;
  Ok: Boolean;
  sSecao: String;
begin
  Result := True;

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);

    with Self do
    begin
      sSecao := 'evtTabFuncao';
      Id             := INIRec.ReadString(sSecao, 'Id', '');
      Sequencial     := INIRec.ReadInteger(sSecao, 'Sequencial', 0);
      ModoLancamento := eSStrToModoLancamento(Ok, INIRec.ReadString(sSecao, 'ModoLancamento', 'inclusao'));

      sSecao := 'ideEvento';
      ideEvento.ProcEmi := eSStrToProcEmi(Ok, INIRec.ReadString(sSecao, 'procEmi', '1'));
      ideEvento.VerProc := INIRec.ReadString(sSecao, 'verProc', EmptyStr);

      sSecao := 'ideEmpregador';
      ideEmpregador.OrgaoPublico := (TACBreSocial(FACBreSocial).Configuracoes.Geral.TipoEmpregador = teOrgaoPublico);
      ideEmpregador.TpInsc       := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
      ideEmpregador.NrInsc       := INIRec.ReadString(sSecao, 'nrInsc', EmptyStr);

      sSecao := 'ideFuncao';
      infoFuncao.ideFuncao.codFuncao := INIRec.ReadString(sSecao, 'codFuncao', EmptyStr);
      infoFuncao.ideFuncao.IniValid  := INIRec.ReadString(sSecao, 'iniValid', EmptyStr);
      infoFuncao.ideFuncao.FimValid  := INIRec.ReadString(sSecao, 'fimValid', EmptyStr);

      if (ModoLancamento <> mlExclusao) then
      begin
        sSecao := 'dadosFuncao';
        infoFuncao.dadosFuncao.dscFuncao := INIRec.ReadString(sSecao, 'dscFuncao', EmptyStr);
        infoFuncao.dadosFuncao.codCBO    := INIRec.ReadString(sSecao, 'codCBO', '1');

        if ModoLancamento = mlAlteracao then
        begin
          sSecao := 'novaValidade';
          infoFuncao.novaValidade.IniValid := INIRec.ReadString(sSecao, 'iniValid', EmptyStr);
          infoFuncao.novaValidade.FimValid := INIRec.ReadString(sSecao, 'fimValid', EmptyStr);
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

