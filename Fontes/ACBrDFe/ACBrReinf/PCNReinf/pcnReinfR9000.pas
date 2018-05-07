{******************************************************************************}
{ Projeto: Componente ACBrReinf                                                }
{  Biblioteca multiplataforma de componentes Delphi para envio de eventos do   }
{ Reinf                                                                        }

{ Direitos Autorais Reservados (c) 2017 Leivio Ramos de Fontenele              }
{                                                                              }

{ Colaboradores nesse arquivo:                                                 }

{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }


{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }

{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }

{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Leivio Ramos de Fontenele  -  leivio@yahoo.com.br                            }
{******************************************************************************}
{******************************************************************************
|* Historico
|*
|* 04/12/2017: Renato Rubinho
|*  - Implementados registros que faltavam e isoladas as respectivas classes
*******************************************************************************}

{$I ACBr.inc}

unit pcnReinfR9000;

interface

uses
  SysUtils, Classes,
  pcnConversao, pcnGerador, ACBrUtil,
  pcnCommonReinf, pcnConversaoReinf, pcnGeradorReinf;

type
  TR9000Collection = class;
  TR9000CollectionItem = class;
  TevtExclusao = class;

  {Classes específicas deste evento}
  TinfoExclusao = class;

  TR9000Collection = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TR9000CollectionItem;
    procedure SetItem(Index: Integer; Value: TR9000CollectionItem);
  public
    function Add: TR9000CollectionItem;
    property Items[Index: Integer]: TR9000CollectionItem read GetItem write SetItem; default;
  end;

  TR9000CollectionItem = class(TCollectionItem)
  private
    FTipoEvento: TTipoEvento;
    FevtExclusao: TevtExclusao;
    procedure setevtExclusao(const Value: TevtExclusao);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  published
    property TipoEvento: TTipoEvento read FTipoEvento;
    property evtExclusao: TevtExclusao read FevtExclusao write setevtExclusao;
  end;

  TevtExclusao = class(TReinfEvento) //Classe do elemento principal do XML do evento!
  private
    FIdeEvento: TIdeEvento;
    FideContri: TideContri;
    FACBrReinf: TObject;
    FinfoExclusao: TinfoExclusao;

    {Geradores específicos desta classe}
    procedure GerarinfoExclusao;
  public
    constructor Create(AACBrReinf: TObject); overload;
    destructor  Destroy; override;

    function GerarXML: Boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property ideEvento: TIdeEvento read FIdeEvento write FIdeEvento;
    property ideContri: TideContri read FideContri write FideContri;
    property infoExclusao: TinfoExclusao read FinfoExclusao write FinfoExclusao;
  end;

  { TinfoExclusao }
  TinfoExclusao = class(TPersistent)
  private
    FtpEvento: string;
    FnrRecEvt: string;
    FperApu: string;
  public
    property tpEvento: string read FtpEvento write FtpEvento;
    property nrRecEvt: string read FnrRecEvt write FnrRecEvt;
    property perApur: string read FperApu write FperApu;
  end;

implementation

uses
  IniFiles,
  ACBrReinf, ACBrDFeUtil;

{ TR9000Collection }

function TR9000Collection.Add: TR9000CollectionItem;
begin
  Result := TR9000CollectionItem(inherited Add);
end;

function TR9000Collection.GetItem(Index: Integer): TR9000CollectionItem;
begin
  Result := TR9000CollectionItem(inherited GetItem(Index));
end;

procedure TR9000Collection.SetItem(Index: Integer; Value: TR9000CollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TR9000CollectionItem }

procedure TR9000CollectionItem.AfterConstruction;
begin
  inherited;
  FTipoEvento := teR9000;
  FevtExclusao := TevtExclusao.Create(Collection.Owner);
end;

procedure TR9000CollectionItem.BeforeDestruction;
begin
  inherited;
  FevtExclusao.Free;
end;

procedure TR9000CollectionItem.setevtExclusao(const Value: TevtExclusao);
begin
  FevtExclusao.Assign(Value);
end;

{ TevtExclusao }

constructor TevtExclusao.Create(AACBrReinf: TObject);
begin
  inherited;

  FACBrReinf := AACBrReinf;

  FideContri    := TideContri.create;
  FIdeEvento    := TIdeEvento2.create;
  FinfoExclusao := TinfoExclusao.Create;
end;

destructor TevtExclusao.Destroy;
begin
  FideContri.Free;
  FIdeEvento.Free;
  FinfoExclusao.Free;

  inherited;
end;

procedure TevtExclusao.GerarinfoExclusao;
begin
  Gerador.wGrupo('infoExclusao');

  Gerador.wCampo(tcStr, '', 'tpEvento', 6,  6, 1, FinfoExclusao.tpEvento);
  Gerador.wCampo(tcStr, '', 'nrRecEvt', 1, 52, 1, FinfoExclusao.nrRecEvt);
  Gerador.wCampo(tcStr, '', 'perApur',  7, 10, 1, FinfoExclusao.perApur);

  Gerador.wGrupo('/infoExclusao');
end;

function TevtExclusao.GerarXML: Boolean;
begin
  try
    Self.VersaoDF := TACBrReinf(FACBrReinf).Configuracoes.Geral.VersaoDF;

    Self.Id := GerarChaveReinf(now, self.ideContri.NrInsc, self.Sequencial);

    GerarCabecalho('evtExclusao');
    Gerador.wGrupo('evtExclusao id="' + Self.Id + '"');

    GerarIdeEvento(Self.IdeEvento);
    GerarideContri(Self.ideContri);

    GerarinfoExclusao;

    Gerador.wGrupo('/evtExclusao');

    GerarRodape;

    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtExclusao');

    Validar(schevtExclusao);
  except on e:exception do
    raise Exception.Create(e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '');
end;

function TevtExclusao.LerArqIni(const AIniString: String): Boolean;
var
  INIRec: TMemIniFile;
  Ok: Boolean;
  sSecao: String;
begin
  Result := False;

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);

    with Self do
    begin
      sSecao := 'evtExclusao';
      Id         := INIRec.ReadString(sSecao, 'Id', '');
      Sequencial := INIRec.ReadInteger(sSecao, 'Sequencial', 0);

      sSecao := 'ideEvento';
      ideEvento.TpAmb   := StrTotpAmbReinf(Ok, INIRec.ReadString(sSecao, 'tpAmb', '1'));
      ideEvento.ProcEmi := StrToProcEmiReinf(Ok, INIRec.ReadString(sSecao, 'procEmi', '1'));
      ideEvento.VerProc := INIRec.ReadString(sSecao, 'verProc', EmptyStr);

      sSecao := 'ideContri';
      ideContri.OrgaoPublico := (TACBrReinf(FACBrReinf).Configuracoes.Geral.TipoContribuinte = tcOrgaoPublico);
      ideContri.TpInsc       := StrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
      ideContri.NrInsc       := INIRec.ReadString(sSecao, 'nrInsc', EmptyStr);

      sSecao := 'infoExclusao';
      infoExclusao.tpEvento := INIRec.ReadString(sSecao, 'tpEvento', EmptyStr);
      infoExclusao.nrRecEvt := INIRec.ReadString(sSecao, 'nrRecEvt', EmptyStr);
      infoExclusao.perApur  := INIRec.ReadString(sSecao, 'perApur', EmptyStr);
    end;

    GerarXML;

    Result := True;
  finally
     INIRec.Free;
  end;
end;

end.
