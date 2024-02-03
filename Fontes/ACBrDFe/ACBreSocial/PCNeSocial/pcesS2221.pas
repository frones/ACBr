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

unit pcesS2221;

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
  ACBrBase, pcnConversao, pcnGerador,
  pcesCommon, pcesConversaoeSocial, pcesGerador;

type
  TS2221CollectionItem = class;
  TEvtToxic = class;

  TS2221Collection = class(TeSocialCollection)
  private
    function GetItem(Index: Integer): TS2221CollectionItem;
    procedure SetItem(Index: Integer; Value: TS2221CollectionItem);
  public
    function Add: TS2221CollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TS2221CollectionItem;
    property Items[Index: Integer]: TS2221CollectionItem read GetItem write SetItem; default;
  end;

  TS2221CollectionItem = class(TObject)
  private
    FTipoEvento: TTipoEvento;
    FEvtToxic: TEvtToxic;
  public
    constructor Create(AOwner: TComponent);
    destructor  Destroy; override;
    property TipoEvento: TTipoEvento read FTipoEvento;
    property EvtToxic: TEvtToxic read FEvtToxic write FEvtToxic;
  end;

  TToxicologico = class(TObject)
  private
    FdtExame: TDateTime;
    FnmMed: String;
    FufCRM: String;
    FcodSeqExame: String;
    FcnpjLab: String;
    FindRecusa: tpSimNao;
    FnrCRM: String;
  public
     property dtExame: TDateTime read FdtExame write FdtExame;
     property cnpjLab: String read FcnpjLab write FcnpjLab;
     property codSeqExame: String read FcodSeqExame write FcodSeqExame;
     property nmMed: String read FnmMed write FnmMed;
     property nrCRM: String read FnrCRM write FnrCRM;
     property ufCRM: String read FufCRM write FufCRM;
     property indRecusa: tpSimNao read FindRecusa write FindRecusa;
  end;

  TEvtToxic = class(TeSocialEvento)
  private
    FIdeEvento: TIdeEvento2;
    FIdeEmpregador: TIdeEmpregador;
    FIdeVinculo: TIdeVinculo;
    FToxicologico: TToxicologico;

    { Geradores da classe }
    procedure GerarToxicologico(objToxicologico: TToxicologico);
  public
    constructor Create(AACBreSocial: TObject); override;
    destructor  Destroy; override;

    function GerarXML: boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property IdeEvento: TIdeEvento2 read FIdeEvento write FIdeEvento;
    property IdeEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property IdeVinculo: TIdeVinculo read FIdeVinculo write FIdeVinculo;
    property toxicologico: TToxicologico read FToxicologico write FToxicologico;
  end;

implementation

uses
  IniFiles,
  ACBrUtil.FilesIO,
  ACBrUtil.DateTime,
  ACBreSocial;

{ TS2221CollectionItem }

constructor TS2221CollectionItem.Create(AOwner: TComponent);
begin
  inherited Create;
  FTipoEvento := teS2221;
  FEvtToxic   := TEvtToxic.Create(AOwner);
end;

destructor TS2221CollectionItem.Destroy;
begin
  FEvtToxic.Free;

  inherited;
end;

{ TS2221Collection }

function TS2221Collection.Add: TS2221CollectionItem;
begin
  Result := Self.New;
end;

function TS2221Collection.GetItem(Index: Integer): TS2221CollectionItem;
begin
  Result := TS2221CollectionItem(inherited Items[Index]);
end;

procedure TS2221Collection.SetItem(Index: Integer; Value: TS2221CollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TS2221Collection.New: TS2221CollectionItem;
begin
  Result := TS2221CollectionItem.Create(FACBreSocial);
  Self.Add(Result);
end;

{ TEvtToxic }

constructor TEvtToxic.Create(AACBreSocial: TObject);
begin
  inherited Create(AACBreSocial);

  FIdeEvento     := TIdeEvento2.Create;
  FIdeEmpregador := TIdeEmpregador.Create;
  FIdeVinculo    := TIdeVinculo.Create;
  FToxicologico  := TToxicologico.Create;
end;

destructor TEvtToxic.Destroy;
begin
  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FIdeVinculo.Free;
  FToxicologico.Free;

  inherited;
end;

procedure TEvtToxic.GerarToxicologico(objToxicologico: TToxicologico);
begin
  Gerador.wGrupo('toxicologico');

  Gerador.wCampo(tcDat, '', 'dtExame',    10, 10, 1, objToxicologico.dtExame);
  Gerador.wCampo(tcStr, '', 'cnpjLab',     1, 14, 0, objToxicologico.cnpjLab);
  Gerador.wCampo(tcStr, '', 'codSeqExame', 1, 11, 0, objToxicologico.codSeqExame);
  Gerador.wCampo(tcStr, '', 'nmMed',       1, 70, 0, objToxicologico.nmMed);
  Gerador.wCampo(tcStr, '', 'nrCRM',       1,  8, 0, objToxicologico.nrCRM);
  Gerador.wCampo(tcStr, '', 'ufCRM',       1,  2, 0, objToxicologico.ufCRM);
  Gerador.wCampo(tcStr, '', 'indRecusa',   1,  1, 1, eSSimNaoToStr(objToxicologico.indRecusa));

  Gerador.wGrupo('/toxicologico');
end;

function TEvtToxic.GerarXML: boolean;
begin
  try
    inherited GerarXML;
    Self.VersaoDF := TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF;

    Self.Id := GerarChaveEsocial(now, Self.ideEmpregador.NrInsc, Self.Sequencial);

    GerarCabecalho('evtToxic');
    Gerador.wGrupo('evtToxic Id="' + Self.Id + '"');

    GerarIdeEvento2(Self.IdeEvento);
    GerarIdeEmpregador(Self.IdeEmpregador);
    GerarIdeVinculo(Self.IdeVinculo);
    GerarToxicologico(Self.Toxicologico);

    Gerador.wGrupo('/evtToxic');

    GerarRodape;

    FXML := Gerador.ArquivoFormatoXML;
//    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtToxic');

//    Validar(schEvtToxic);
  except on e:exception do
    raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '')
end;

function TEvtToxic.LerArqIni(const AIniString: String): Boolean;
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
      sSecao := 'evtToxic';
      Id         := INIRec.ReadString(sSecao, 'Id', '');
      Sequencial := INIRec.ReadInteger(sSecao, 'Sequencial', 0);

      sSecao := 'ideEvento';
      ideEvento.indRetif    := eSStrToIndRetificacao(Ok, INIRec.ReadString(sSecao, 'indRetif', '1'));
      ideEvento.NrRecibo    := INIRec.ReadString(sSecao, 'nrRecibo', EmptyStr);
      ideEvento.ProcEmi     := eSStrToProcEmi(Ok, INIRec.ReadString(sSecao, 'procEmi', '1'));
      ideEvento.VerProc     := INIRec.ReadString(sSecao, 'verProc', EmptyStr);

      sSecao := 'ideEmpregador';
      ideEmpregador.OrgaoPublico := (TACBreSocial(FACBreSocial).Configuracoes.Geral.TipoEmpregador = teOrgaoPublico);
      ideEmpregador.TpInsc       := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
      ideEmpregador.NrInsc       := INIRec.ReadString(sSecao, 'nrInsc', EmptyStr);

      sSecao := 'ideVinculo';
      ideVinculo.CpfTrab   := INIRec.ReadString(sSecao, 'cpfTrab', EmptyStr);
      ideVinculo.NisTrab   := INIRec.ReadString(sSecao, 'nisTrab', EmptyStr);
      ideVinculo.Matricula := INIRec.ReadString(sSecao, 'matricula', EmptyStr);
      IdeVinculo.codCateg  := INIRec.ReadInteger(sSecao, 'codCateg', 0);

      sSecao := 'toxicologico';
      toxicologico.dtExame     := StringToDateTime(INIRec.ReadString(sSecao, 'dtExame', '0'));
      toxicologico.cnpjLab     := INIRec.ReadString(sSecao, 'cnpjLab', '');
      toxicologico.codSeqExame := INIRec.ReadString(sSecao, 'codSeqExame', '');
      toxicologico.nmMed       := INIRec.ReadString(sSecao, 'nmMed', '');
      toxicologico.nrCRM       := INIRec.ReadString(sSecao, 'nrCRM', '');
      toxicologico.ufCRM       := INIRec.ReadString(sSecao, 'ufCRM', '');
      toxicologico.indRecusa   := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'indRecusa', 'N'));
    end;

    GerarXML;
    XML := FXML;
  finally
    INIRec.Free;
  end;
end;

end.
