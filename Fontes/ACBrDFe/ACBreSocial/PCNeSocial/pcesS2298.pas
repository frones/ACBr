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

unit pcesS2298;

interface

uses
  SysUtils, Classes,
  pcnConversao, ACBrUtil,
  pcesCommon, pcesConversaoeSocial, pcesGerador;

type
  TS2298Collection = class;
  TS2298CollectionItem = class;
  TEvtReintegr = class;
  TInfoReintegr = class;

  TS2298Collection = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TS2298CollectionItem;
    procedure SetItem(Index: Integer; Value: TS2298CollectionItem);
  public
    function Add: TS2298CollectionItem;
    property Items[Index: Integer]: TS2298CollectionItem read GetItem write SetItem; default;
  end;

  TS2298CollectionItem = class(TCollectionItem)
  private
    FTipoEvento: TTipoEvento;
    FEvtReintegr: TEvtReintegr;

    procedure setEvtReintegr(const Value: TEvtReintegr);
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor  Destroy; override;
  published
    property TipoEvento: TTipoEvento read FTipoEvento;
    property EvtReintegr: TEvtReintegr read FEvtReintegr write setEvtReintegr;
  end;

  TEvtReintegr = class(TeSocialEvento)
  private
    FIdeEvento: TIdeEvento2;
    FIdeEmpregador: TIdeEmpregador;
    FIdeVinculo: TIdeVinculo;
    FInfoReintegr: TInfoReintegr;
    FACBreSocial: TObject;

    procedure GerarInfoReintegr;
  public
    constructor Create(AACBreSocial: TObject); overload;
    destructor  Destroy; override;

    function GerarXML: boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property IdeEvento: TIdeEvento2 read FIdeEvento write FIdeEvento;
    property IdeEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property IdeVinculo: TIdeVinculo read FIdeVinculo write FIdeVinculo;
    property InfoReintegr: TInfoReintegr read FInfoReintegr write FInfoReintegr;
  end;

  TInfoReintegr = class
  private
     FtpReint: tpTpReint;
     FnrProcJud: string;
     FnrLeiAnistia: tpNrLeiAnistia;
     FdtEfetRetorno: TDateTime;
     FdtEfeito: TDateTime;
     FindPagtoJuizo: tpSimNao;
  public
    property tpReint: tpTpReint read FtpReint write FtpReint;
    property nrProcJud: string read FnrProcJud write FnrProcJud;
    property nrLeiAnistia: tpNrLeiAnistia read FnrLeiAnistia write FnrLeiAnistia;
    property dtEfetRetorno: TDateTime read FdtEfetRetorno write FdtEfetRetorno;
    property dtEfeito: TDateTime read FdtEfeito write FdtEfeito;
    property indPagtoJuizo: tpSimNao read FindPagtoJuizo write FindPagtoJuizo;
  end;

implementation

uses
  IniFiles,
  ACBreSocial, ACBrDFeUtil;

{ TS2298Collection }

function TS2298Collection.Add: TS2298CollectionItem;
begin
  Result := TS2298CollectionItem(inherited Add);
  Result.Create(TComponent(Self.Owner));
end;

function TS2298Collection.GetItem(Index: Integer): TS2298CollectionItem;
begin
  Result := TS2298CollectionItem(inherited GetItem(Index));
end;

procedure TS2298Collection.SetItem(Index: Integer;
  Value: TS2298CollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TS2298CollectionItem }
constructor TS2298CollectionItem.Create(AOwner: TComponent);
begin
  FTipoEvento  := teS2298;
  FEvtReintegr := TEvtReintegr.Create(AOwner);
end;

destructor TS2298CollectionItem.Destroy;
begin
  FEvtReintegr.Free;

  inherited;
end;

procedure TS2298CollectionItem.setEvtReintegr(const Value: TEvtReintegr);
begin
  FEvtReintegr.Assign(Value);
end;

{ TEvtReintegracao }
constructor TEvtReintegr.Create(AACBreSocial: TObject);
begin
  inherited;

  FACBreSocial := AACBreSocial;
  FIdeEvento     := TIdeEvento2.Create;
  FIdeEmpregador := TIdeEmpregador.Create;
  FIdeVinculo    := TIdeVinculo.Create;
  FInfoReintegr  := TInfoReintegr.Create;
end;

destructor TEvtReintegr.destroy;
begin
  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FIdeVinculo.Free;
  FInfoReintegr.Free;

  inherited;
end;

procedure TEvtReintegr.GerarInfoReintegr;
begin
  Gerador.wGrupo('infoReintegr');

  Gerador.wCampo(tcStr, '', 'tpReint', 1, 1, 1, eSTpReintToStr(self.InfoReintegr.tpReint));

  if eSTpReintToStr(self.InfoReintegr.tpReint) = '1' then
    Gerador.wCampo(tcStr, '', 'nrProcJud', 1, 20, 0, self.InfoReintegr.nrProcJud);

  if eSTpReintToStr(self.InfoReintegr.tpReint) = '2' then
    Gerador.wCampo(tcStr, '', 'nrLeiAnistia', 1, 13, 0, eSNrLeiAnistiaToStr(self.InfoReintegr.nrLeiAnistia));

  Gerador.wCampo(tcDat, '', 'dtEfetRetorno', 10, 10, 1, self.InfoReintegr.dtEfetRetorno);
  Gerador.wCampo(tcDat, '', 'dtEfeito',      10, 10, 1, self.InfoReintegr.dtEfeito);
  Gerador.wCampo(tcStr, '', 'indPagtoJuizo',  1,  1, 1, eSSimNaoToStr(self.InfoReintegr.indPagtoJuizo));

  Gerador.wGrupo('/infoReintegr');
end;

function TEvtReintegr.GerarXML: boolean;
begin
  try
    Self.VersaoDF := TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF;
     
    Self.Id := GerarChaveEsocial(now, self.ideEmpregador.NrInsc, self.Sequencial);

    GerarCabecalho('evtReintegr');
    Gerador.wGrupo('evtReintegr Id="' + Self.Id + '"');

    GerarIdeEvento2(self.IdeEvento);
    GerarIdeEmpregador(self.IdeEmpregador);
    GerarIdeVinculo(self.IdeVinculo);
    GerarInfoReintegr;

    Gerador.wGrupo('/evtReintegr');

    GerarRodape;

    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtReintegr');

    Validar(schevtReintegr);
  except on e:exception do
    raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '')
end;

function TEvtReintegr.LerArqIni(const AIniString: String): Boolean;
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
      sSecao := 'evtReintegr';
      Id         := INIRec.ReadString(sSecao, 'Id', '');
      Sequencial := INIRec.ReadInteger(sSecao, 'Sequencial', 0);

      sSecao := 'ideEvento';
      ideEvento.indRetif    := eSStrToIndRetificacao(Ok, INIRec.ReadString(sSecao, 'indRetif', '1'));
      ideEvento.NrRecibo    := INIRec.ReadString(sSecao, 'nrRecibo', EmptyStr);
      ideEvento.TpAmb       := eSStrTotpAmb(Ok, INIRec.ReadString(sSecao, 'tpAmb', '1'));
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

      sSecao := 'infoReintegr';
      infoReintegr.tpReint       := eSStrToTpReint(Ok, INIRec.ReadString(sSecao, 'tpReint', '1'));
      infoReintegr.nrProcJud     := INIRec.ReadString(sSecao, 'nrProcJud', EmptyStr);
      infoReintegr.nrLeiAnistia  := eSStrToNrLeiAnistia(Ok, INIRec.ReadString(sSecao, 'nrLeiAnistia', 'LEI6683_1979'));
      infoReintegr.dtEfetRetorno := StringToDateTime(INIRec.ReadString(sSecao, 'dtEfetRetorno', '0'));
      infoReintegr.dtEfeito      := StringToDateTime(INIRec.ReadString(sSecao, 'dtEfeito', '0'));
      infoReintegr.indPagtoJuizo := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'indPagtoJuizo', 'S'));
    end;

    GerarXML;

    Result := True;
  finally
     INIRec.Free;
  end;
end;

end.
