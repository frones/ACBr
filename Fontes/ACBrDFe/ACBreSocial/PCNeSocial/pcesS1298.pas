{******************************************************************************}
{ Projeto: Componente ACBreSocial                                              }
{  Biblioteca multiplataforma de componentes Delphi para envio dos eventos do  }
{ eSocial - http://www.esocial.gov.br/                                         }
{                                                                              }
{ Direitos Autorais Reservados (c) 2008 Wemerson Souto                         }
{                                       Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }
{                                                                              }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 27/10/2015: Jean Carlo Cantu, Tiago Ravache
|*  - Doação do componente para o Projeto ACBr
|* 01/03/2016: Guilherme Costa
|*  - Passado o namespace para geração do cabeçalho
******************************************************************************}
{$I ACBr.inc}

unit pcesS1298;

interface

uses
  SysUtils, Classes,
  pcnConversao, ACBrUtil,
  pcesCommon, pcesConversaoeSocial, pcesGerador;

type
  TS1298Collection = class;
  TS1298CollectionItem = class;
  TEvtReabreEvPer = class;

  TS1298Collection = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TS1298CollectionItem;
    procedure SetItem(Index: Integer; Value: TS1298CollectionItem);
  public
    function Add: TS1298CollectionItem;
    property Items[Index: Integer]: TS1298CollectionItem read GetItem write SetItem; default;
  end;

  TS1298CollectionItem = class(TCollectionItem)
  private
    FTipoEvento: TTipoEvento;
    FEvtReabreEvPer: TEvtReabreEvPer;

    procedure setEvtReabreEvPer(const Value: TEvtReabreEvPer);
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;
  published
    property TipoEvento: TTipoEvento read FTipoEvento;
    property EvtReabreEvPer: TEvtReabreEvPer read FEvtReabreEvPer write setEvtReabreEvPer;
  end;

  TEvtReabreEvPer = class(TESocialEvento)
  private
    FIdeEvento: TIdeEvento4;
    FIdeEmpregador: TIdeEmpregador;
    FACBreSocial: TObject;

    {Geradores específicos da classe}
  public
    constructor Create(AACBreSocial: TObject);overload;
    destructor  Destroy; override;

    function GerarXML: boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property IdeEvento: TIdeEvento4 read FIdeEvento write FIdeEvento;
    property IdeEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
  end;

implementation

uses
  IniFiles,
  ACBreSocial;

{ TS1298Collection }

function TS1298Collection.Add: TS1298CollectionItem;
begin
  Result := TS1298CollectionItem(inherited Add);
  Result.Create(TComponent(Self.Owner));
end;

function TS1298Collection.GetItem(Index: Integer): TS1298CollectionItem;
begin
  Result := TS1298CollectionItem(inherited GetItem(Index));
end;

procedure TS1298Collection.SetItem(Index: Integer; Value: TS1298CollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{TS1298CollectionItem}
constructor TS1298CollectionItem.Create(AOwner: TComponent);
begin
  FTipoEvento := teS1298;
  FEvtReabreEvPer := TEvtReabreEvPer.Create(AOwner);
end;

destructor TS1298CollectionItem.Destroy;
begin
  FEvtReabreEvPer.Free;

  inherited;
end;

procedure TS1298CollectionItem.setEvtReabreEvPer(const Value: TEvtReabreEvPer);
begin
  FEvtReabreEvPer.Assign(Value);
end;

{ TEvtSolicTotal }
constructor TEvtReabreEvPer.Create(AACBreSocial: TObject);
begin
  inherited;

  FACBreSocial := AACBreSocial;
  FIdeEvento := TIdeEvento4.Create;
  FIdeEmpregador := TIdeEmpregador.Create;
end;

destructor TEvtReabreEvPer.destroy;
begin
  FIdeEvento.Free;
  FIdeEmpregador.Free;

  inherited;
end;

function TEvtReabreEvPer.GerarXML: boolean;
begin
  try
    Self.VersaoDF := TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF;
     
    Self.Id := GerarChaveEsocial(now, self.ideEmpregador.NrInsc, self.Sequencial);

    GerarCabecalho('evtReabreEvPer');
    Gerador.wGrupo('evtReabreEvPer Id="' + Self.Id + '"');

    GerarIdeEvento4(self.IdeEvento);
    GerarIdeEmpregador(self.IdeEmpregador);

    Gerador.wGrupo('/evtReabreEvPer');

    GerarRodape;

    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtReabreEvPer');

    Validar(schevtReabreEvPer);
  except on e:exception do
    raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '')
end;

function TEvtReabreEvPer.LerArqIni(const AIniString: String): Boolean;
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
      sSecao := 'evtReabreEvPer';
      Id         := INIRec.ReadString(sSecao, 'Id', '');
      Sequencial := INIRec.ReadInteger(sSecao, 'Sequencial', 0);

      sSecao := 'ideEvento';
      ideEvento.IndApuracao := eSStrToIndApuracao(Ok, INIRec.ReadString(sSecao, 'indApuracao', '1'));
      ideEvento.perApur     := INIRec.ReadString(sSecao, 'perApur', EmptyStr);
      ideEvento.TpAmb       := eSStrTotpAmb(Ok, INIRec.ReadString(sSecao, 'tpAmb', '1'));
      ideEvento.ProcEmi     := eSStrToProcEmi(Ok, INIRec.ReadString(sSecao, 'procEmi', '1'));
      ideEvento.VerProc     := INIRec.ReadString(sSecao, 'verProc', EmptyStr);

      sSecao := 'ideEmpregador';
      ideEmpregador.OrgaoPublico := (TACBreSocial(FACBreSocial).Configuracoes.Geral.TipoEmpregador = teOrgaoPublico);
      ideEmpregador.TpInsc       := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
      ideEmpregador.NrInsc       := INIRec.ReadString(sSecao, 'nrInsc', EmptyStr);
    end;

    GerarXML;
  finally
     INIRec.Free;
  end;
end;

end.
