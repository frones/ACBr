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

unit pcnReinfR2098;

interface

uses
  SysUtils, Classes, Contnrs,
  pcnConversao, pcnGerador, ACBrUtil,
  pcnCommonReinf, pcnConversaoReinf, pcnGeradorReinf;

type

  TevtReabreEvPer = class(TReinfEvento) //Classe do elemento principal do XML do evento!
  private
    FIdeEvento: TIdeEvento2;
    FideContri: TideContri;

  public
    constructor Create(AACBrReinf: TObject); override;
    destructor  Destroy; override;

    function GerarXML: Boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property ideEvento: TIdeEvento2 read FIdeEvento write FIdeEvento;
    property ideContri: TideContri read FideContri write FideContri;
  end;

  TR2098CollectionItem = class(TObject)
  private
    FTipoEvento: TTipoEvento;
    FevtReabreEvPer: TevtReabreEvPer;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    property TipoEvento: TTipoEvento read FTipoEvento;
    property evtReabreEvPer: TevtReabreEvPer read FevtReabreEvPer write FevtReabreEvPer;
  end;

  TR2098Collection = class(TReinfCollection)
  private
    function GetItem(Index: Integer): TR2098CollectionItem;
    procedure SetItem(Index: Integer; Value: TR2098CollectionItem);
  public
    function Add: TR2098CollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TR2098CollectionItem;

    property Items[Index: Integer]: TR2098CollectionItem read GetItem write SetItem; default;
  end;

implementation

uses
  IniFiles,
  ACBrReinf, ACBrDFeUtil;

{ TR2098Collection }

function TR2098Collection.Add: TR2098CollectionItem;
begin
  Result := Self.New;
end;

function TR2098Collection.GetItem(Index: Integer): TR2098CollectionItem;
begin
  Result := TR2098CollectionItem(inherited GetItem(Index));
end;

function TR2098Collection.New: TR2098CollectionItem;
begin
  Result := TR2098CollectionItem.Create(FACBrReinf);
  Self.Add(Result);
end;

procedure TR2098Collection.SetItem(Index: Integer; Value: TR2098CollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TR2098CollectionItem }

constructor TR2098CollectionItem.Create(AOwner: TComponent);
begin
  inherited Create;

  FTipoEvento     := teR2098;
  FevtReabreEvPer := TevtReabreEvPer.Create(AOwner);
end;

destructor TR2098CollectionItem.Destroy;
begin
  inherited;

  FevtReabreEvPer.Free;
end;

{ TevtReabreEvPer }

constructor TevtReabreEvPer.Create(AACBrReinf: TObject);
begin
  inherited Create(AACBrReinf);

  FideContri := TideContri.create;
  FIdeEvento := TIdeEvento2.create;
end;

destructor TevtReabreEvPer.Destroy;
begin
  FideContri.Free;
  FIdeEvento.Free;

  inherited;
end;

function TevtReabreEvPer.GerarXML: Boolean;
begin
  try
    Self.VersaoDF := TACBrReinf(FACBrReinf).Configuracoes.Geral.VersaoDF;

    Self.Id := GerarChaveReinf(now, self.ideContri.NrInsc, self.Sequencial);

    GerarCabecalho('evtReabreEvPer');
    Gerador.wGrupo('evtReabreEvPer id="' + Self.Id + '"');

    GerarIdeEvento2(Self.IdeEvento, True, False);
    GerarideContri(Self.ideContri);

    Gerador.wGrupo('/evtReabreEvPer');

    GerarRodape;

    FXML := Gerador.ArquivoFormatoXML;
//    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtReabreEvPer');

//    Validar(schevtReabreEvPer);
  except on e:exception do
    raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '');
end;

function TevtReabreEvPer.LerArqIni(const AIniString: String): Boolean;
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
      ideEvento.perApur := INIRec.ReadString(sSecao, 'perApur', EmptyStr);
      ideEvento.ProcEmi := StrToProcEmiReinf(Ok, INIRec.ReadString(sSecao, 'procEmi', '1'));
      ideEvento.VerProc := INIRec.ReadString(sSecao, 'verProc', EmptyStr);

      sSecao := 'ideContri';
      ideContri.OrgaoPublico := (TACBrReinf(FACBrReinf).Configuracoes.Geral.TipoContribuinte = tcOrgaoPublico);
      ideContri.TpInsc       := StrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
      ideContri.NrInsc       := INIRec.ReadString(sSecao, 'nrInsc', EmptyStr);
    end;

    GerarXML;
    XML := FXML;
  finally
    INIRec.Free;
  end;
end;

end.
