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

unit pcnReinfR2099;

interface

uses
  SysUtils, Classes,
  pcnConversao, pcnGerador, ACBrUtil,
  pcnCommonReinf, pcnConversaoReinf, pcnGeradorReinf;

type
  TR2099Collection = class;
  TR2099CollectionItem = class;
  TevtFechaEvPer = class;

  {Classes específicas deste evento}
  TideRespInf = class;
  TinfoFech = class;

  TR2099Collection = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TR2099CollectionItem;
    procedure SetItem(Index: Integer; Value: TR2099CollectionItem);
  public
    function Add: TR2099CollectionItem;
    property Items[Index: Integer]: TR2099CollectionItem read GetItem write SetItem; default;
  end;

  TR2099CollectionItem = class(TCollectionItem)
  private
    FTipoEvento: TTipoEvento;
    FevtFechaEvPer: TevtFechaEvPer;
    procedure setevtFechaEvPer(const Value: TevtFechaEvPer);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  published
    property TipoEvento: TTipoEvento read FTipoEvento;
    property evtFechaEvPer: TevtFechaEvPer read FevtFechaEvPer write setevtFechaEvPer;
  end;

  TevtFechaEvPer = class(TReinfEvento) //Classe do elemento principal do XML do evento!
  private
    FIdeEvento: TIdeEvento2;
    FideContri: TideContri;
    FACBrReinf: TObject;
    FideRespInf: TideRespInf;
    FinfoFech: TinfoFech;

    {Geradores específicos desta classe}
    procedure GerarideRespInf;
    procedure GerarinfoFech;
  public
    constructor Create(AACBrReinf: TObject); overload;
    destructor  Destroy; override;

    function GerarXML: Boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property ideEvento: TIdeEvento2 read FIdeEvento write FIdeEvento;
    property ideContri: TideContri read FideContri write FideContri;
    property ideRespInf: TideRespInf read FideRespInf write FideRespInf;
    property infoFech: TinfoFech read FinfoFech write FinfoFech;
  end;

  { TideRespInf }
  TideRespInf = class(TPersistent)
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
  TinfoFech = class(TPersistent)
  private
    FevtServTm: TtpSimNao;
    FevtServPr: TtpSimNao;
    FevtAssDespRec: TtpSimNao;
    FevtAssDespRep: TtpSimNao;
    FevtComProd: TtpSimNao;
    FevtCPRB: TtpSimNao;
    FevtPgtos: TtpSimNao;
    FcompSemMovto: string;
  public
    property evtServTm: TtpSimNao read FevtServTm write FevtServTm;
    property evtServPr: TtpSimNao read FevtServPr write FevtServPr;
    property evtAssDespRec: TtpSimNao read FevtAssDespRec write FevtAssDespRec;
    property evtAssDespRep: TtpSimNao read FevtAssDespRep write FevtAssDespRep;
    property evtComProd: TtpSimNao read FevtComProd write FevtComProd;
    property evtCPRB: TtpSimNao read FevtCPRB write FevtCPRB;
    property evtPgtos: TtpSimNao read FevtPgtos write FevtPgtos;
    property compSemMovto: string read FcompSemMovto write FcompSemMovto;
  end;

implementation

uses
  IniFiles,
  ACBrReinf, ACBrDFeUtil;

{ TR2099Collection }

function TR2099Collection.Add: TR2099CollectionItem;
begin
  Result := TR2099CollectionItem(inherited Add);
end;

function TR2099Collection.GetItem(Index: Integer): TR2099CollectionItem;
begin
  Result := TR2099CollectionItem(inherited GetItem(Index));
end;

procedure TR2099Collection.SetItem(Index: Integer; Value: TR2099CollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TR2099CollectionItem }

procedure TR2099CollectionItem.AfterConstruction;
begin
  inherited;
  FTipoEvento := teR2099;
  FevtFechaEvPer := TevtFechaEvPer.Create(Collection.Owner);
end;

procedure TR2099CollectionItem.BeforeDestruction;
begin
  inherited;
  FevtFechaEvPer.Free;
end;

procedure TR2099CollectionItem.setevtFechaEvPer(const Value: TevtFechaEvPer);
begin
  FevtFechaEvPer.Assign(Value);
end;

{ TevtFechaEvPer }

constructor TevtFechaEvPer.Create(AACBrReinf: TObject);
begin
  inherited;

  FACBrReinf := AACBrReinf;

  FideContri  := TideContri.create;
  FIdeEvento  := TIdeEvento2.create;
  FideRespInf := TideRespInf.Create;
  FinfoFech   := TinfoFech.Create;
end;

destructor TevtFechaEvPer.Destroy;
begin
  FideContri.Free;
  FIdeEvento.Free;
  FideRespInf.Free;
  FinfoFech.Free;

  inherited;
end;

procedure TevtFechaEvPer.GerarideRespInf;
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

procedure TevtFechaEvPer.GerarinfoFech;
begin
  Gerador.wGrupo('infoFech');

  Gerador.wCampo(tcStr, '', 'evtServTm',     1, 1, 1, SimNaoToStr(FinfoFech.evtServTm));
  Gerador.wCampo(tcStr, '', 'evtServPr',     1, 1, 1, SimNaoToStr(FinfoFech.evtServPr));
  Gerador.wCampo(tcStr, '', 'evtAssDespRec', 1, 1, 1, SimNaoToStr(FinfoFech.evtAssDespRec));
  Gerador.wCampo(tcStr, '', 'evtAssDespRep', 1, 1, 1, SimNaoToStr(FinfoFech.evtAssDespRep));
  Gerador.wCampo(tcStr, '', 'evtComProd',    1, 1, 1, SimNaoToStr(FinfoFech.evtComProd));
  Gerador.wCampo(tcStr, '', 'evtCPRB',       1, 1, 1, SimNaoToStr(FinfoFech.evtCPRB));
  Gerador.wCampo(tcStr, '', 'evtPgtos',      1, 1, 1, SimNaoToStr(FinfoFech.evtPgtos));
  Gerador.wCampo(tcStr, '', 'compSemMovto',  1, 7, 0, FinfoFech.compSemMovto);

  Gerador.wGrupo('/infoFech');
end;

function TevtFechaEvPer.GerarXML: Boolean;
begin
  try
    Self.VersaoDF := TACBrReinf(FACBrReinf).Configuracoes.Geral.VersaoDF;

    Self.Id := GerarChaveReinf(now, self.ideContri.NrInsc, self.Sequencial);

    GerarCabecalho('evtFechamento');
    Gerador.wGrupo('evtFechaEvPer id="' + Self.Id + '"');

    GerarIdeEvento2(Self.IdeEvento, True, False);
    GerarideContri(Self.ideContri);

    GerarideRespInf;
    GerarinfoFech;

    Gerador.wGrupo('/evtFechaEvPer');

    GerarRodape;

    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtFechaEvPer');

    Validar(schevtFechamento);
  except on e:exception do
    raise Exception.Create(e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '');
end;

function TevtFechaEvPer.LerArqIni(const AIniString: String): Boolean;
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
      sSecao := 'evtFechaEvPer';
      Id         := INIRec.ReadString(sSecao, 'Id', '');
      Sequencial := INIRec.ReadInteger(sSecao, 'Sequencial', 0);

      sSecao := 'ideEvento';
      ideEvento.perApur := INIRec.ReadString(sSecao, 'perApur', EmptyStr);
      ideEvento.TpAmb   := StrTotpAmbReinf(Ok, INIRec.ReadString(sSecao, 'tpAmb', '1'));
      ideEvento.ProcEmi := StrToProcEmiReinf(Ok, INIRec.ReadString(sSecao, 'procEmi', '1'));
      ideEvento.VerProc := INIRec.ReadString(sSecao, 'verProc', EmptyStr);

      sSecao := 'ideContri';
      ideContri.OrgaoPublico := (TACBrReinf(FACBrReinf).Configuracoes.Geral.TipoContribuinte = tcOrgaoPublico);
      ideContri.TpInsc       := StrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
      ideContri.NrInsc       := INIRec.ReadString(sSecao, 'nrInsc', EmptyStr);

      sSecao := 'ideRespInf';
      ideRespInf.nmResp   := INIRec.ReadString(sSecao, 'nmResp', EmptyStr);
      ideRespInf.cpfResp  := INIRec.ReadString(sSecao, 'cpfResp', EmptyStr);
      ideRespInf.telefone := INIRec.ReadString(sSecao, 'telefone', EmptyStr);
      ideRespInf.email    := INIRec.ReadString(sSecao, 'email', EmptyStr);

      sSecao := 'infoFech';
      infoFech.evtServTm     := StrToSimNao(Ok, INIRec.ReadString(sSecao, 'evtServTm', 'N'));
      infoFech.evtServPr     := StrToSimNao(Ok, INIRec.ReadString(sSecao, 'evtServPr', 'N'));
      infoFech.evtAssDespRec := StrToSimNao(Ok, INIRec.ReadString(sSecao, 'evtAssDespRec', 'N'));
      infoFech.evtAssDespRep := StrToSimNao(Ok, INIRec.ReadString(sSecao, 'evtAssDespRep', 'N'));
      infoFech.evtComProd    := StrToSimNao(Ok, INIRec.ReadString(sSecao, 'evtComProd', 'N'));
      infoFech.evtCPRB       := StrToSimNao(Ok, INIRec.ReadString(sSecao, 'evtCPRB', 'N'));
      infoFech.evtPgtos      := StrToSimNao(Ok, INIRec.ReadString(sSecao, 'evtPgtos', 'N'));
      infoFech.compSemMovto  := INIRec.ReadString(sSecao, 'compSemMovto', EmptyStr);
    end;

    GerarXML;

    Result := True;
  finally
     INIRec.Free;
  end;
end;

end.
