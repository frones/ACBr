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

unit pcesS1295;

interface

uses
  SysUtils, Classes, Contnrs,
  pcnConversao, ACBrUtil,
  pcesCommon, pcesConversaoeSocial, pcesGerador;

type
  TS1295CollectionItem = class;
  TEvtTotConting = class;

  TS1295Collection = class(TeSocialCollection)
  private
    function GetItem(Index: Integer): TS1295CollectionItem;
    procedure SetItem(Index: Integer; Value: TS1295CollectionItem);
  public
    function Add: TS1295CollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TS1295CollectionItem;
    property Items[Index: Integer]: TS1295CollectionItem read GetItem write SetItem; default;
  end;

  TS1295CollectionItem = class(TObject)
  private
    FTipoEvento: TTipoEvento;
    FEvtTotConting: TEvtTotConting;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    property TipoEvento: TTipoEvento read FTipoEvento;
    property evtTotConting: TEvtTotConting read FEvtTotConting write FEvtTotConting;
  end;

  TEvtTotConting = class(TESocialEvento)
  private
    FIdeEvento: TIdeEvento3;
    FIdeEmpregador: TIdeEmpregador;
    FIdeRespInf : TIdeRespInf;

    {Geradores específicos da classe}
  public
    constructor Create(AACBreSocial: TObject); override;
    destructor  Destroy; override;

    function GerarXML: boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property IdeEvento: TIdeEvento3 read FIdeEvento write FIdeEvento;
    property IdeEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property IdeRespInf: TIdeRespInf read FIdeRespInf write FIdeRespInf;
  end;

implementation

uses
  IniFiles,
  ACBreSocial;

{ TS1295Collection }

function TS1295Collection.Add: TS1295CollectionItem;
begin
  Result := Self.New;
end;

function TS1295Collection.GetItem(Index: Integer): TS1295CollectionItem;
begin
  Result := TS1295CollectionItem(inherited GetItem(Index));
end;

procedure TS1295Collection.SetItem(Index: Integer; Value: TS1295CollectionItem);
begin
  inherited SetItem(Index, Value);
end;

function TS1295Collection.New: TS1295CollectionItem;
begin
  Result := TS1295CollectionItem.Create(FACBreSocial);
  Self.Add(Result);
end;

{TS1295CollectionItem}
constructor TS1295CollectionItem.Create(AOwner: TComponent);
begin
  inherited Create;
  FTipoEvento    := teS1295;
  FEvtTotConting := TEvtTotConting.Create(AOwner);
end;

destructor TS1295CollectionItem.Destroy;
begin
  FEvtTotConting.Free;

  inherited;
end;

{ TEvtSolicTotal }
constructor TEvtTotConting.Create(AACBreSocial: TObject);
begin
  inherited Create(AACBreSocial);

  FIdeEvento     := TIdeEvento3.Create;
  FIdeEmpregador := TIdeEmpregador.Create;
  FIdeRespInf    := TIdeRespInf.Create;
end;

destructor TEvtTotConting.Destroy;
begin
  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FIdeRespInf.Free;

  inherited;
end;

function TEvtTotConting.GerarXML: boolean;
begin
  try
    Self.VersaoDF := TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF;
     
    Self.Id := GerarChaveEsocial(now, self.ideEmpregador.NrInsc, self.Sequencial);

    GerarCabecalho('evtTotConting');
    Gerador.wGrupo('evtTotConting Id="' + Self.Id + '"');

    GerarIdeEvento3(self.IdeEvento, False);
    GerarIdeEmpregador(self.IdeEmpregador);
    GerarIdeRespInf(Self.IdeRespInf);

    Gerador.wGrupo('/evtTotConting');

    GerarRodape;

    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtTotConting');

    Validar(schevtTotConting);
  except on e:exception do
    raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '')
end;

function TEvtTotConting.LerArqIni(const AIniString: String): Boolean;
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
      sSecao := 'evtTotConting';
      Id         := INIRec.ReadString(sSecao, 'Id', '');
      Sequencial := INIRec.ReadInteger(sSecao, 'Sequencial', 0);

      sSecao := 'ideEvento';
      ideEvento.IndApuracao := eSStrToIndApuracao(Ok, INIRec.ReadString(sSecao, 'indApuracao', '1'));
      ideEvento.perApur     := INIRec.ReadString(sSecao, 'perApur', EmptyStr);
      ideEvento.ProcEmi     := eSStrToProcEmi(Ok, INIRec.ReadString(sSecao, 'procEmi', '1'));
      ideEvento.VerProc     := INIRec.ReadString(sSecao, 'verProc', EmptyStr);

      sSecao := 'ideEmpregador';
      ideEmpregador.OrgaoPublico := (TACBreSocial(FACBreSocial).Configuracoes.Geral.TipoEmpregador = teOrgaoPublico);
      ideEmpregador.TpInsc       := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
      ideEmpregador.NrInsc       := INIRec.ReadString(sSecao, 'nrInsc', EmptyStr);

      sSecao := 'ideRespInf';
      if INIRec.ReadString(sSecao, 'nmResp', '') <> '' then
      begin
        ideRespinf.nmResp   := INIRec.ReadString(sSecao, 'nmResp', '');
        ideRespinf.cpfResp  := INIRec.ReadString(sSecao, 'cpfResp', '');
        ideRespinf.telefone := INIRec.ReadString(sSecao, 'telefone', '');
        ideRespinf.email    := INIRec.ReadString(sSecao, 'email', '');
      end;
    end;

    GerarXML;
  finally
     INIRec.Free;
  end;
end;

end.
