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

unit pcesS1299;

interface

uses
  SysUtils, Classes,
  pcnConversao, ACBrUtil,
  pcesCommon, pcesConversaoeSocial, pcesGerador;

type
  TS1299Collection = class;
  TS1299CollectionItem = class;
  TEvtFechaEvPer = class;
  TInfoFech= class;

  TS1299Collection = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TS1299CollectionItem;
    procedure SetItem(Index: Integer; Value: TS1299CollectionItem);
  public
    function Add: TS1299CollectionItem;
    property Items[Index: Integer]: TS1299CollectionItem read GetItem write SetItem; default;
  end;

  TS1299CollectionItem = class(TCollectionItem)
  private
    FTipoEvento: TTipoEvento;
    FEvtFechaEvPer: TEvtFechaEvPer;
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;
  published
    property TipoEvento: TTipoEvento read FTipoEvento;
    property EvtFechaEvPer: TEvtFechaEvPer read FEvtFechaEvPer write FEvtFechaEvPer;
  end;

  TEvtFechaEvPer = class(TESocialEvento)
  private
    FIdeEvento: TIdeEvento3;
    FIdeEmpregador: TIdeEmpregador;
    FIdeRespInf : TIdeRespInf;
    FInfoFech: TInfoFech;

    {Geradores específicos da classe}
    procedure GerarInfoFech;
  public
    constructor Create(AACBreSocial: TObject); override;
    destructor  Destroy; override;

    function GerarXML: boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property IdeEvento: TIdeEvento3 read FIdeEvento write FIdeEvento;
    property IdeEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property IdeRespInf: TIdeRespInf read FIdeRespInf write FIdeRespInf;
    property InfoFech: TInfoFech read FInfoFech write FInfoFech;
  end;

  TInfoFech = class
  private
    FevtRemun: TpSimNao;
    FevtPgtos: TpSimNao;
    FevtAqProd: TpSimNao;
    FevtComProd: TpSimNao;
    FevtContratAvNP: TpSimNao;
    FevtInfoComplPer: TpSimNao;
    FcompSemMovto : string;
  public
    constructor create;
    destructor Destroy; override;

    property evtRemun: TpSimNao read FevtRemun write FevtRemun;
    property evtPgtos: TpSimNao read FevtPgtos write FevtPgtos;
    property evtAqProd: TpSimNao read FevtAqProd write FevtAqProd;
    property evtComProd: TpSimNao read FevtComProd write FevtComProd;
    property evtContratAvNP: TpSimNao read FevtContratAvNP write FevtContratAvNP;
    property evtInfoComplPer: TpSimNao read FevtInfoComplPer write FevtInfoComplPer;
    property compSemMovto : string read FcompSemMovto write FcompSemMovto;
  end;

implementation

uses
  IniFiles,
  ACBreSocial;

{ TS1299Collection }

function TS1299Collection.Add: TS1299CollectionItem;
begin
  Result := TS1299CollectionItem(inherited Add);
  Result.Create(TComponent(Self.Owner));
end;

function TS1299Collection.GetItem(Index: Integer): TS1299CollectionItem;
begin
  Result := TS1299CollectionItem(inherited GetItem(Index));
end;

procedure TS1299Collection.SetItem(Index: Integer; Value: TS1299CollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{TS1299CollectionItem}
constructor TS1299CollectionItem.Create(AOwner: TComponent);
begin
  FTipoEvento := teS1299;
  FEvtFechaEvPer := TEvtFechaEvPer.Create(AOwner);
end;

destructor TS1299CollectionItem.Destroy;
begin
  FEvtFechaEvPer.Free;

  inherited;
end;

{ TEvtSolicTotal }
constructor TEvtFechaEvPer.Create(AACBreSocial: TObject);
begin
  inherited Create(AACBreSocial);

  FIdeEvento     := TIdeEvento3.Create;
  FIdeEmpregador := TIdeEmpregador.Create;
  FIdeRespInf    := TIdeRespInf.Create;
  FInfoFech      := TInfoFech.Create;
end;

destructor TEvtFechaEvPer.destroy;
begin
  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FIdeRespInf.Free;
  FInfoFech.Free;

  inherited;
end;

procedure TEvtFechaEvPer.GerarInfoFech;
begin
  Gerador.wGrupo('infoFech');

  Gerador.wCampo(tcStr, '', 'evtRemun',        1, 1, 1, eSSimNaoToStr(self.InfoFech.evtRemun));
  Gerador.wCampo(tcStr, '', 'evtPgtos',        1, 1, 1, eSSimNaoToStr(self.InfoFech.evtPgtos));
  Gerador.wCampo(tcStr, '', 'evtAqProd',       1, 1, 1, eSSimNaoToStr(self.InfoFech.evtAqProd));
  Gerador.wCampo(tcStr, '', 'evtComProd',      1, 1, 1, eSSimNaoToStr(self.InfoFech.evtComProd));
  Gerador.wCampo(tcStr, '', 'evtContratAvNP',  1, 1, 1, eSSimNaoToStr(self.InfoFech.evtContratAvNP));
  Gerador.wCampo(tcStr, '', 'evtInfoComplPer', 1, 1, 1, eSSimNaoToStr(self.InfoFech.evtInfoComplPer));

  if ((eSSimNaoToStr(self.InfoFech.evtRemun)        = 'N') and
      (eSSimNaoToStr(self.InfoFech.evtPgtos)        = 'N') and
      (eSSimNaoToStr(self.InfoFech.evtAqProd)       = 'N') and
      (eSSimNaoToStr(self.InfoFech.evtComProd)      = 'N') and
      (eSSimNaoToStr(self.InfoFech.evtContratAvNP)  = 'N') and
      (eSSimNaoToStr(self.InfoFech.evtInfoComplPer) = 'N')) then
    Gerador.wCampo(tcStr, '', 'compSemMovto', 1, 7, 0, self.InfoFech.compSemMovto);

  Gerador.wGrupo('/infoFech');
end;

function TEvtFechaEvPer.GerarXML: boolean;
begin
  try
    Self.VersaoDF := TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF;
     
    Self.Id := GerarChaveEsocial(now, self.ideEmpregador.NrInsc, self.Sequencial);

    GerarCabecalho('evtFechaEvPer');
    Gerador.wGrupo('evtFechaEvPer Id="' + Self.Id + '"');

    GerarIdeEvento3(self.IdeEvento, False);
    GerarIdeEmpregador(self.IdeEmpregador);
    GerarIdeRespInf(Self.IdeRespInf);
    GerarInfoFech;

    Gerador.wGrupo('/evtFechaEvPer');

    GerarRodape;

    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtFechaEvPer');

    Validar(schevtFechaEvPer);
  except on e:exception do
    raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '')
end;

{ TInfoFech }

constructor TInfoFech.create;
begin
  inherited;
end;

destructor TInfoFech.destroy;
begin
  inherited;
end;

function TEvtFechaEvPer.LerArqIni(const AIniString: String): Boolean;
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
      sSecao := 'evtFechaEvPer';
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

      sSecao := 'ideRespInf';
      if INIRec.ReadString(sSecao, 'nmResp', '') <> '' then
      begin
        ideRespInf.nmResp   := INIRec.ReadString(sSecao, 'nmResp', EmptyStr);
        ideRespInf.cpfResp  := INIRec.ReadString(sSecao, 'cpfResp', EmptyStr);
        ideRespInf.telefone := INIRec.ReadString(sSecao, 'telefone', EmptyStr);
        ideRespInf.email    := INIRec.ReadString(sSecao, 'email', EmptyStr);
      end;

      sSecao := 'infoFech';
      infoFech.evtRemun        := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'evtRemun', 'S'));
      infoFech.evtPgtos        := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'evtPgtos', 'S'));
      infoFech.evtAqProd       := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'evtAqProd', 'S'));
      infoFech.evtComProd      := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'evtComProd', 'S'));
      infoFech.evtContratAvNP  := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'evtContratAvNP', 'S'));
      infoFech.evtInfoComplPer := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'evtInfoComplPer', 'S'));
      infoFech.compSemMovto    := INIRec.ReadString(sSecao, 'compSemMovto', '');
    end;

    GerarXML;
  finally
     INIRec.Free;
  end;
end;

end.
