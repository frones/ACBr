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
|*  - Alterações para validação com o XSD
******************************************************************************}
{$I ACBr.inc}

unit pcesS2220;

interface

uses
  SysUtils, Classes,
  pcnConversao, pcnGerador, ACBrUtil,
  pcesCommon, pcesConversaoeSocial, pcesGerador;

type
  TS2220Collection = class;
  TS2220CollectionItem = class;
  TevtMonit = class;
  TexMedOcup = class;
  TAso = class;
  TExameCollectionItem = class;
  TExameCollection = class;
  TRespMonit = class;
  TMedico = class;
  
  TS2220Collection = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TS2220CollectionItem;
    procedure SetItem(Index: Integer; Value: TS2220CollectionItem);
  public
    function Add: TS2220CollectionItem;
    property Items[Index: Integer]: TS2220CollectionItem read GetItem write SetItem; default;
  end;

  TS2220CollectionItem = class(TCollectionItem)
  private
    FTipoEvento: TTipoEvento;
    FevtMonit: TevtMonit;

  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;
  published
    property TipoEvento: TTipoEvento read FTipoEvento;
    property evtMonit: TevtMonit read FevtMonit write FevtMonit;
  end;

  TevtMonit = class(TeSocialEvento)
  private
    FIdeEvento: TIdeEvento2;
    FIdeEmpregador: TIdeEmpregador;
    FIdeVinculo: TIdeVinculo;
    FexMedOcup: TexMedOcup;

    { Geradores da classe }
    procedure GerarExame;
    procedure GerarMedico;
    procedure GerarExMedOcup;
    procedure GerarAso;
    procedure GerarRespMonit;
  public
    constructor Create(AACBreSocial: TObject); override;
    destructor Destroy; override;

    function GerarXML: boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property IdeEvento: TIdeEvento2 read FIdeEvento write FIdeEvento;
    property IdeEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property IdeVinculo: TIdeVinculo read FIdeVinculo write FIdeVinculo;
    property exMedOcup: TexMedOcup read FexMedOcup write FexMedOcup;
  end;

  TexMedOcup = class(TPersistent)
  private
    FtpExameOcup: tpTpExameOcup;
    FAso: TAso;
    FRespMonit : TRespMonit;
  public
    property tpExameOcup: tpTpExameOcup read FtpExameOcup write FtpExameOcup;
    property Aso : TAso read FAso write FAso;
    property RespMonit : TRespMonit read FRespMonit write FRespMonit;

    constructor create;
    destructor Destroy; override;
  end;

  TAso = class(TPersistent)
  private
    FDtAso: TDateTime;
    FResAso: tpResAso;
    FExame: TExameCollection;
    FMedico: TMedico;
  public
    constructor create;
    destructor Destroy; override;

    property DtAso: TDateTime read FDtAso write FDtAso;
    property ResAso: tpResAso read FResAso write FResAso;
    property Exame: TExameCollection read FExame write FExame;
    property Medico: TMedico read FMedico write FMedico;
  end;

  TExameCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TExameCollectionItem;
    procedure SetItem(Index: Integer; const Value: TExameCollectionItem);
  public
    constructor Create(AOwner: TPersistent);
    function Add: TExameCollectionItem;
    property Items[Index: Integer]: TExameCollectionItem read GetItem write SetItem;
  end;

  TExameCollectionItem = class(TCollectionItem)
  private
    FDtExm: TDateTime;
    FProcRealizado: integer;
    FObsProc: string;
    FOrdExame: tpOrdExame;
    FIndResult: tpIndResult;
  public      
    property DtExm: TDateTime read FDtExm write FDtExm;
    property ProcRealizado: integer read FProcRealizado write FProcRealizado;
    property obsProc: string read FObsProc write FObsProc;
    property ordExame: tpOrdExame read FOrdExame write FOrdExame;
    property indResult: tpIndResult read FIndResult write FIndResult;
  end;

  TRespMonit = class
  private
    FCPFResp: String;
    FNMResp: String;
    FNRCRM: String;
    FUFCRM: tpuf;
  public
    property cpfResp: String read FCPFResp write FCPFResp;
    property nmResp: String read FNMResp write FNMResp;
    property nrCRM: String read FNRCRM write FNRCRM;
    property ufCRM: tpuf read FUFCRM write FUFCRM;
  end;

  TMedico = class
  private
    FNmMed: string;
    FCPFMed : String;
    FNISMed : String;
    FnrCRM: String;
    FufCRM: tpuf;
  public
    property cpfMed: String read FCPFMed write FCPFMed;
    property nisMed: String read FNISMed write FNISMed;
    property NmMed: String read FNmMed write FNmMed;
    property nrCRM: String read FnrCRM write FnrCRM;
    property ufCRM: tpuf read FufCRM write FufCRM;
  end;

implementation

uses
  IniFiles,
  ACBreSocial;

{ TS2220Collection }

function TS2220Collection.Add: TS2220CollectionItem;
begin
  Result := TS2220CollectionItem(inherited Add);
  Result.Create(TComponent(Self.Owner));
end;

function TS2220Collection.GetItem(Index: Integer): TS2220CollectionItem;
begin
  Result := TS2220CollectionItem(inherited GetItem(Index));
end;

procedure TS2220Collection.SetItem(Index: Integer;
  Value: TS2220CollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TS2220CollectionItem }

constructor TS2220CollectionItem.Create(AOwner: TComponent);
begin
  FTipoEvento := teS2220;
  FevtMonit     := TevtMonit.Create(AOwner);
end;

destructor TS2220CollectionItem.Destroy;
begin
  FevtMonit.Free;

  inherited;
end;

{ TAso }

constructor TAso.create;
begin
  inherited;
  FExame := TExameCollection.Create(self);
  FMedico := TMedico.Create;
end;

destructor TAso.destroy;
begin
  FExame.Free;
  FMedico.Free;
  inherited;
end;

{ TExameColecao }

function TExameCollection.Add: TExameCollectionItem;
begin
  Result := TExameCollectionItem(inherited Add);
  Result.Create(self);
end;

constructor TExameCollection.Create(AOwner: TPersistent);
begin
  inherited Create(TExameCollectionItem);
end;

function TExameCollection.GetItem(Index: Integer): TExameCollectionItem;
begin
  Result := TExameCollectionItem(inherited GetItem(Index));
end;

procedure TExameCollection.SetItem(Index: Integer;
  const Value: TExameCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TevtMonit }

constructor TevtMonit.Create(AACBreSocial: TObject);
begin
  inherited Create(AACBreSocial);

  FIdeEvento     := TIdeEvento2.Create;
  FIdeEmpregador := TIdeEmpregador.Create;
  FIdeVinculo    := TIdeVinculo.Create;
  FexMedOcup     := TexMedOcup.Create;
end;

destructor TevtMonit.destroy;
begin
  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FIdeVinculo.Free;
  FexMedOcup.Free;

  inherited;
end;

procedure TevtMonit.GerarAso;
begin
  Gerador.wGrupo('aso');

  Gerador.wCampo(tcDat, '', 'dtAso',  10, 10, 1, self.exMedOcup.Aso.DtAso);
  Gerador.wCampo(tcStr, '', 'resAso',  1,  1, 1, eSResAsoToStr(self.exMedOcup.Aso.ResAso));

  GerarExame;
  GerarMedico;

  Gerador.wGrupo('/aso');
end;

procedure TevtMonit.GerarMedico;
begin
  Gerador.wGrupo('medico');

  if Trim(self.exMedOcup.Aso.Medico.cpfMed) <> '' then
  begin
    Gerador.wCampo(tcStr, '', 'cpfMed', 11, 11, 0, self.exMedOcup.Aso.Medico.cpfMed);
  end;

  if Trim(self.exMedOcup.Aso.Medico.nisMed) <> '' then
  begin
    Gerador.wCampo(tcStr, '', 'nisMed', 1, 11, 0, self.exMedOcup.Aso.Medico.nisMed);
  end;

  Gerador.wCampo(tcStr, '', 'nmMed', 1, 70, 1, self.exMedOcup.Aso.Medico.NmMed);

  Gerador.wCampo(tcStr, '', 'nrCRM', 1, 8, 1, self.exMedOcup.Aso.Medico.nrCRM);
  Gerador.wCampo(tcStr, '', 'ufCRM', 2, 2, 1, eSufToStr(self.exMedOcup.Aso.Medico.ufCRM));

  Gerador.wGrupo('/medico');
end;

procedure TevtMonit.GerarExame;
var
  i: integer;
begin
  for i:= 0 to self.exMedOcup.Aso.Exame.Count-1 do
  begin
    Gerador.wGrupo('exame');

    Gerador.wCampo(tcDat, '', 'dtExm',         10,  10, 1, self.exMedOcup.Aso.Exame.Items[i].dtExm);
    Gerador.wCampo(tcStr, '', 'procRealizado',  1,   4, 1, self.exMedOcup.Aso.Exame.Items[i].procRealizado);
    Gerador.wCampo(tcStr, '', 'obsProc',        1, 999, 0, self.exMedOcup.Aso.Exame.Items[i].obsProc);
    Gerador.wCampo(tcInt, '', 'ordExame',       1,   1, 1, eSOrdExameToStr(self.exMedOcup.Aso.Exame.Items[i].ordExame));
    Gerador.wCampo(tcInt, '', 'indResult',      1,   1, 0, eSIndResultToStr(self.exMedOcup.Aso.Exame.Items[i].indResult));
    Gerador.wGrupo('/exame');
  end;

  if self.exMedOcup.Aso.Exame.Count > 99 then
    Gerador.wAlerta('', 'exame', 'Lista de Exames', ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TevtMonit.GerarExMedOcup;
begin
  Gerador.wGrupo('exMedOcup');

  Gerador.wCampo(tcStr, '', 'tpExameOcup',   1,  1, 1, eSTpExameOcupToStr(self.exMedOcup.FtpExameOcup));

  GerarASO;
  GerarRespMonit;

  Gerador.wGrupo('/exMedOcup');
end;

procedure TevtMonit.GerarRespMonit;
begin
  Gerador.wGrupo('respMonit');

  if Trim(self.exMedOcup.RespMonit.cpfResp) <> '' then
  begin
    Gerador.wCampo(tcStr, '', 'cpfResp', 11, 11, 0, self.exMedOcup.RespMonit.cpfResp);
  end;

  Gerador.wCampo(tcStr, '', 'nmResp', 1, 70, 1, self.exMedOcup.RespMonit.nmResp);

  Gerador.wCampo(tcStr, '', 'nrCRM', 1, 8, 1, self.exMedOcup.RespMonit.nrCRM);
  Gerador.wCampo(tcStr, '', 'ufCRM', 2, 2, 1, eSufToStr(self.exMedOcup.RespMonit.ufCRM));

  Gerador.wGrupo('/respMonit');
end;

function TevtMonit.GerarXML: boolean;
begin
  try
    Self.VersaoDF := TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF;

    Self.Id := GerarChaveEsocial(now, self.ideEmpregador.NrInsc, self.Sequencial);

    GerarCabecalho('evtMonit');
    Gerador.wGrupo('evtMonit Id="' + Self.Id + '"');

    GerarIdeEvento2(self.IdeEvento);
    GerarIdeEmpregador(self.IdeEmpregador);
    GerarIdeVinculo(self.IdeVinculo);
    GerarExMedOcup;

    Gerador.wGrupo('/evtMonit');

    GerarRodape;

    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtMonit');

    Validar(schevtMonit);
  except on e:exception do
    raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '')
end;

function TevtMonit.LerArqIni(const AIniString: String): Boolean;
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
      sSecao := 'evtMonit';
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

      sSecao := 'aso';
      exMedOcup.aso.DtAso  := StringToDateTime(INIRec.ReadString(sSecao, 'dtAso', '0'));
      exMedOcup.tpExameOcup  := eSStrToTpExameOcup(Ok, INIRec.ReadString(sSecao, 'tpAso', '0'));
      exMedOcup.aso.ResAso := eSStrToResAso(Ok, INIRec.ReadString(sSecao, 'resAso', '1'));

      I := 1;
      while true do
      begin
        // de 01 até 99
        sSecao := 'exame' + IntToStrZero(I, 2);
        sFim   := INIRec.ReadString(sSecao, 'dtExm', 'FIM');

        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break;

        with exMedOcup.aso.exame.Add do
        begin
          dtExm         := StringToDateTime(sFim);
          ProcRealizado := INIRec.ReadInteger(sSecao, 'procRealizado', 0);
          obsProc       := INIRec.ReadString(sSecao, 'obsProc', EmptyStr);
          ordExame      := eSStrToOrdExame(Ok, INIRec.ReadString(sSecao, 'ordExame', '1'));
          indResult     := eSStrToIndResult(Ok, INIRec.ReadString(sSecao, 'indResult', '1'));
        end;

        Inc(I);
      end;


      // I vai vir com o o valor do último exame + 1
      sSecao := 'respMonit' + IntToStrZero(I-1, 2);
      exMedOcup.RespMonit.cpfResp := INIRec.ReadString(sSecao, 'cpfResp', EmptyStr);
      exMedOcup.RespMonit.nmResp := INIRec.ReadString(sSecao, 'nmResp', EmptyStr);
      exMedOcup.RespMonit.nrCRM := INIRec.ReadString(sSecao, 'nrCRM', EmptyStr);
      exMedOcup.RespMonit.ufCRM := eSStrTouf(Ok, INIRec.ReadString(sSecao, 'ufCRM', 'SP'));


      sSecao := 'medico';
      exMedOcup.Aso.medico.cpfMed := INIRec.ReadString(sSecao, 'cpfMed', EmptyStr);
      exMedOcup.Aso.medico.nisMed := INIRec.ReadString(sSecao, 'nisMed', EmptyStr);
      exMedOcup.Aso.medico.NmMed := INIRec.ReadString(sSecao, 'nmMed', EmptyStr);
      exMedOcup.Aso.medico.nrCRM := INIRec.ReadString(sSecao, 'nrCRM', EmptyStr);
      exMedOcup.Aso.medico.ufCRM := eSStrTouf(Ok, INIRec.ReadString(sSecao, 'ufCRM', 'SP'));
    end;

    GerarXML;
  finally
     INIRec.Free;
  end;
end;

{ TexMedOcup }

constructor TexMedOcup.create;
begin
  FAso := TAso.create;
  FRespMonit := TRespMonit.Create;
end;

destructor TexMedOcup.destroy;
begin
  FAso.Free;
  FRespMonit.Free;
  inherited;
end;

end.

