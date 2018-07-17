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

unit pcesS2241;

interface

uses
  SysUtils, Classes,
  pcnConversao, pcnGerador, ACBrUtil,
  pcesCommon, pcesConversaoeSocial, pcesGerador;

type
  TS2241Collection = class;
  TS2241CollectionItem = class;
  TEvtInsApo = class;
  TInsPerApo = class;//base para ini,alt e fim de insalperic e aposentesp
  TInsalPeric = class;
  TiniInsalPeric = class;
  TaltInsalPeric = class;
  TfimInsalPeric = class;
  TAposentEsp = class;
  TiniAposentEsp = class;
  TaltAposentEsp = class;
  TfimAposentEsp = class;

  TS2241Collection = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TS2241CollectionItem;
    procedure SetItem(Index: Integer; Value: TS2241CollectionItem);
  public
    function Add: TS2241CollectionItem;
    property Items[Index: Integer]: TS2241CollectionItem read GetItem write SetItem; default;
  end;

  TS2241CollectionItem = class(TCollectionItem)
  private
    FTipoEvento: TTipoEvento;
    FEvtInsApo: TEvtInsApo;
    procedure setEvtInsApo(const Value: TEvtInsApo);
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor  Destroy; override;
  published
    property TipoEvento: TTipoEvento read FTipoEvento;
    property EvtInsApo: TEvtInsApo read FEvtInsApo write setEvtInsApo;
  end;

  TEvtInsApo = class(TeSocialEvento)
  private
    FIdeEvento: TIdeEvento2;
    FIdeEmpregador: TIdeEmpregador;
    FIdeVinculo: TIdeVinculo;
    FInsalPeric: TInsalPeric;
    FAposentEsp: TAposentEsp;
    FACBreSocial: TObject;

    procedure GerarInsalPeric(objInsalPeric: TInsalPeric);
    procedure GerarIniInsalPeric(objIniInsPer: TiniInsalPeric);
    procedure GerarAltInsalPeric(objAltInsPer: TaltInsalPeric);
    procedure GerarFimInsalPeric(objFimInsPer: TfimInsalPeric);
    procedure GerarAposentEsp(objAposentEsp: TAposentEsp);
    procedure GerarIniAposentEsp(objIniApoEsp: TiniAposentEsp);
    procedure GerarAltAposentEsp(objAltApoEsp: TaltAposentEsp);
    procedure GerarFimAposentEsp(objFimApoEsp: TfimAposentEsp);
    procedure GerarInfoAmb(Sender: TInsPerApo; objInfoAmb: TInfoAmbCollection;
         Grupo: String = 'infoAmb');
    procedure GerarFatRisco(objFatRisco: TFatRiscoCollection);
  public
    constructor Create(AACBreSocial: TObject);overload;
    destructor  Destroy; override;

    function GerarXML: boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property IdeEvento: TIdeEvento2 read FIdeEvento write FIdeEvento;
    property IdeEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property IdeVinculo: TIdeVinculo read FIdeVinculo write FIdeVinculo;
    property InsalPeric: TInsalPeric read FInsalPeric write FInsalPeric;
    property AposentEsp: TAposentEsp read FAposentEsp write FAposentEsp;
  end;

  TInsPerApo = class(TPersistent)
  private
    FDtCondicao : TDateTime;
    FInfoAmb : TInfoAmbCollection;

    procedure setInfoAmb(const Value: TInfoAmbCollection);
  public
    constructor Create;
    destructor  Destroy; override;
  end;

  TiniInsalPeric = class(TInsPerApo)
  public
    property DtiniCondicao : TDateTime read FDtCondicao write FDtCondicao;
    property InfoAmb : TInfoAmbCollection read FInfoAmb write setInfoAmb;
  end;

  TaltInsalPeric = class(TInsPerApo)
  public
    property DtaltCondicao : TDateTime read FDtCondicao write FDtCondicao;
    property InfoAmb : TInfoAmbCollection read FInfoAmb write setInfoAmb;
  end;

  TfimInsalPeric = class(TInsPerApo)
  public
    property DtfimCondicao : TDateTime read FDtCondicao write FDtCondicao;
    property InfoAmb : TInfoAmbCollection read FInfoAmb write setInfoAmb;
  end;

  TInsalPeric = class(TPersistent)
  private
    FiniInsalPeric : TiniInsalPeric;
    FaltInsalPeric : TaltInsalPeric;
    FfimInsalPeric : TfimInsalPeric;
  public
    constructor Create;
    destructor  Destroy; Override;

    property iniInsalPeric : TiniInsalPeric read FiniInsalPeric write FiniInsalPeric;
    property altInsalPeric : TaltInsalPeric read FaltInsalPeric write FaltInsalPeric;
    property fimInsalPeric : TfimInsalPeric read FfimInsalPeric write FfimInsalPeric;
  end;

  TiniAposentEsp = class(TInsPerApo)
  public
    property DtiniCondicao : TDateTime read FDtCondicao write FDtCondicao;
    property InfoAmb : TInfoAmbCollection read FInfoAmb write setInfoAmb;
  end;

  TaltAposentEsp = class(TInsPerApo)
  public
    property DtaltCondicao : TDateTime read FDtCondicao write FDtCondicao;
    property InfoAmb : TInfoAmbCollection read FInfoAmb write setInfoAmb;
  end;

  TfimAposentEsp = class(TInsPerApo)
  public
    property DtfimCondicao : TDateTime read FDtCondicao write FDtCondicao;
    property InfoAmb : TInfoAmbCollection read FInfoAmb write setInfoAmb;
  end;

  TAposentEsp = class(TPersistent)
  private
    FiniAposentEsp : TiniAposentEsp;
    FaltAposentEsp : TaltAposentEsp;
    FfimAposentEsp : TfimAposentEsp;
  public
    constructor Create;
    destructor  Destroy; Override;

    property iniAposentEsp : TiniAposentEsp read FiniAposentEsp write FiniAposentEsp;
    property altAposentEsp : TaltAposentEsp read FaltAposentEsp write FaltAposentEsp;
    property fimAposentEsp : TfimAposentEsp read FfimAposentEsp write FfimAposentEsp;
  end;

implementation

uses
  IniFiles,
  ACBreSocial, ACBrDFeUtil;

{ TS2241CollectionItem }

constructor TS2241CollectionItem.Create(AOwner: TComponent);
begin
  FTipoEvento := teS2241;
  FEvtInsApo := TEvtInsApo.Create(AOwner);
end;

destructor TS2241CollectionItem.Destroy;
begin
  FEvtInsApo.Free;

  inherited;
end;

procedure TS2241CollectionItem.setEvtInsApo(const Value: TEvtInsApo);
begin
  FEvtInsApo.Assign(Value);
end;

{ TInsPerApo }

constructor TInsPerApo.create;
begin
  inherited;

  FInfoAmb := TInfoAmbCollection.create;
end;

destructor TInsPerApo.destroy;
begin
  FInfoAmb.Free;

  inherited;
end;

procedure TInsPerApo.setInfoAmb(const Value: TInfoAmbCollection);
begin
  FInfoAmb.Assign(Value);
end;

{ TInsalPeric }

constructor TInsalPeric.Create;
begin
  inherited;

  FiniInsalperic := TiniInsalPeric.create;
  FaltInsalPeric := TaltInsalPeric.create;
  FfimInsalPeric := TfimInsalPeric.create;
end;

destructor TInsalPeric.Destroy;
begin
  FiniInsalPeric.Free;
  FaltInsalPeric.Free;
  FfimInsalPeric.Free;

  inherited;
end;

{ TAposentEsp }

constructor TAposentEsp.Create;
begin
  inherited;

  FiniAposentEsp := TiniAposentEsp.create;
  FaltAposentEsp := TaltAposentEsp.create;
  FfimAposentEsp := TfimAposentEsp.create;
end;

destructor TAposentEsp.Destroy;
begin
  FiniAposentEsp.Free;
  FaltAposentEsp.Free;
  FfimAposentEsp.Free;

  inherited;
end;

{ TS2241Collection }

function TS2241Collection.Add: TS2241CollectionItem;
begin
  Result := TS2241CollectionItem(inherited Add);
  Result.Create(TComponent(Self.Owner));
end;

function TS2241Collection.GetItem(Index: Integer): TS2241CollectionItem;
begin
  Result := TS2241CollectionItem(inherited GetItem(Index));
end;

procedure TS2241Collection.SetItem(Index: Integer; Value: TS2241CollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TEvtInsApo }

constructor TEvtInsApo.Create(AACBreSocial: TObject);
begin
  inherited;

  FACBreSocial := AACBreSocial;
  FIdeEvento := TIdeEvento2.Create;
  FIdeEmpregador := TIdeEmpregador.Create;
  FIdeVinculo := TIdeVinculo.Create;
  FInsalPeric := TInsalPeric.Create;
  FAposentEsp := TAposentEsp.Create;
end;

destructor TEvtInsApo.destroy;
begin
  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FIdeVinculo.Free;
  FInsalPeric.Free;
  FAposentEsp.Free;

  inherited;
end;

procedure TEvtInsApo.GerarAltAposentEsp(objAltApoEsp: TaltAposentEsp);
begin
  Gerador.wGrupo('altAposentEsp');

  Gerador.wCampo(tcDat, '', 'dtAltCondicao', 10, 10, 1, objAltApoEsp.dtAltCondicao);

  GerarInfoAmb(objAltApoEsp, objAltApoEsp.InfoAmb, 'infoamb');

  Gerador.wGrupo('/altAposentEsp');
end;

procedure TEvtInsApo.GerarAltInsalPeric(objAltInsPer: TaltInsalPeric);
begin
  Gerador.wGrupo('altInsalPeric');

  Gerador.wCampo(tcDat, '', 'dtAltCondicao', 10, 10, 1, objAltInsPer.dtAltCondicao);

  GerarInfoAmb(objAltInsPer, objAltInsPer.InfoAmb, 'infoamb');

  Gerador.wGrupo('/altInsalPeric');
end;

procedure TEvtInsApo.GerarAposentEsp(objAposentEsp: TAposentEsp);
begin
  Gerador.wGrupo('aposentEsp');

  if objAposentEsp.iniAposentEsp.DtiniCondicao > 0 then
    GerariniAposentEsp(objAposentEsp.iniAposentEsp);

  if objAposentEsp.altAposentEsp.DtaltCondicao > 0 then
    GeraraltAposentEsp(objAposentEsp.altAposentEsp);

  if objAposentEsp.fimAposentEsp.DtfimCondicao > 0 then
    GerarfimAposentEsp(objAposentEsp.fimAposentEsp);

  Gerador.wGrupo('/aposentEsp');
end;

procedure TEvtInsApo.GerarFatRisco(objFatRisco: TFatRiscoCollection);
var
  i: Integer;
begin
  for i := 0 to objFatRisco.count - 1 do
  begin
    Gerador.wGrupo('fatRisco');

    Gerador.wCampo(tcStr, '', 'codFatRis', 1, 10, 1, objFatRisco.items[i].codFatRis);

    Gerador.wGrupo('/fatRisco');
  end;

  if objFatRisco.Count > 999 then
    Gerador.wAlerta('', 'fatRisco', 'Lista de Fatores de Riscos', ERR_MSG_MAIOR_MAXIMO + '999');
end;

procedure TEvtInsApo.GerarFimAposentEsp(objFimApoEsp: TfimAposentEsp);
begin
  Gerador.wGrupo('fimAposentEsp');

  Gerador.wCampo(tcDat, '', 'dtFimCondicao', 10, 10, 1, objFimApoEsp.dtfimCondicao);

  GerarInfoAmb(objFimApoEsp, objFimApoEsp.InfoAmb);

  Gerador.wGrupo('/fimAposentEsp');
end;

procedure TEvtInsApo.GerarFimInsalPeric(objFimInsPer: TfimInsalPeric);
begin
  Gerador.wGrupo('fimInsalPeric');

  Gerador.wCampo(tcDat, '', 'dtFimCondicao', 10, 10, 1, objFimInsPer.dtfimCondicao);

  GerarInfoAmb(objFimInsPer, objFimInsPer.InfoAmb);

  Gerador.wGrupo('/fimInsalPeric');
end;

procedure TEvtInsApo.GerarInfoAmb(Sender: TInsPerApo; objInfoAmb: TInfoAmbCollection;
   Grupo: String);
var
  i: Integer;
begin
  for i := 0 to objInfoAmb.count - 1 do
  begin
    Gerador.wGrupo(Grupo);

    Gerador.wCampo(tcStr, '', 'codAmb', 1, 30, 1, objInfoAmb.items[i].codAmb);

    if (not ((Sender is TfimAposentEsp) or (Sender is TfimInsalPeric))) then
      GerarFatRisco(objInfoAmb.items[i].FatRisco);

    Gerador.wGrupo('/' + Grupo);
  end;

  if objInfoAmb.Count > 99 then
    Gerador.wAlerta('', Grupo, 'Lista de Informações Ambientais', ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TEvtInsApo.GerarIniAposentEsp(objIniApoEsp: TiniAposentEsp);
begin
  Gerador.wGrupo('iniAposentEsp');

  Gerador.wCampo(tcDat, '', 'dtIniCondicao', 10, 10, 1, objIniApoEsp.dtIniCondicao);

  GerarInfoAmb(objIniApoEsp, objIniApoEsp.InfoAmb);

  Gerador.wGrupo('/iniAposentEsp');
end;

procedure TEvtInsApo.GerarIniInsalPeric(objIniInsPer: TiniInsalPeric);
begin
  Gerador.wGrupo('iniInsalPeric');

  Gerador.wCampo(tcDat, '', 'dtIniCondicao', 10, 10, 1, objIniInsPer.dtIniCondicao);

  GerarInfoAmb(objIniInsPer, objIniInsPer.InfoAmb);

  Gerador.wGrupo('/iniInsalPeric');
end;

procedure TEvtInsApo.GerarInsalPeric(objInsalPeric: TInsalPeric);
begin
  Gerador.wGrupo('insalPeric');

  if objInsalPeric.iniInsalPeric.DtiniCondicao > 0 then
    GerariniInsalPeric(objInsalPeric.iniInsalPeric);

  if objInsalPeric.altInsalPeric.DtaltCondicao > 0 then
    GeraraltInsalPeric(objInsalPeric.altInsalPeric);

  if objInsalPeric.fimInsalPeric.DtfimCondicao > 0 then
    GerarfimInsalPeric(objInsalPeric.fimInsalPeric);

  Gerador.wGrupo('/insalPeric');
end;

function TEvtInsApo.GerarXML: boolean;
begin
  try
    Self.VersaoDF := TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF;
     
    Self.Id := GerarChaveEsocial(now, self.ideEmpregador.NrInsc, self.Sequencial);

    GerarCabecalho('evtInsApo');
    Gerador.wGrupo('evtInsApo Id="' + Self.Id + '"');

    GerarIdeEvento2(self.IdeEvento);
    GerarIdeEmpregador(self.IdeEmpregador);
    GerarIdeVinculo(self.IdeVinculo);

    if Assigned(FInsalPeric) then
      GerarInsalPeric(self.InsalPeric);

    if Assigned(FAposentEsp) then
      GerarAposentEsp(self.AposentEsp);

    Gerador.wGrupo('/evtInsApo');

    GerarRodape;

    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtInsApo');

    Validar(schevtInsApo);
  except on e:exception do
    raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '')
end;

function TEvtInsApo.LerArqIni(const AIniString: String): Boolean;
var
  INIRec: TMemIniFile;
  Ok: Boolean;
  sSecao, sFim: String;
  I, J: Integer;
begin
  Result := False;

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);

    with Self do
    begin
      sSecao := 'evtInsApo';
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

      sSecao := 'iniInsalPeric';
      if INIRec.ReadString(sSecao, 'dtIniCondicao', '') <> '' then
      begin
        InsalPeric.iniInsalPeric.DtiniCondicao := StringToDateTime(INIRec.ReadString(sSecao, 'dtIniCondicao', '0'));

        I := 1;
        while true do
        begin
          // de 01 até 99
          sSecao := 'infoAmb' + IntToStrZero(I, 2);
          sFim   := INIRec.ReadString(sSecao, 'codAmb', 'FIM');

          if (sFim = 'FIM') or (Length(sFim) <= 0) then
            break;

          with InsalPeric.iniInsalPeric.infoAmb.Add do
          begin
            codAmb := sFim;

            J := 1;
            while true do
            begin
              // de 001 até 999
              sSecao := 'fatRisco' + IntToStrZero(I, 2) + IntToStrZero(J, 3);
              sFim   := INIRec.ReadString(sSecao, 'codFatRis', 'FIM');

              if (sFim = 'FIM') or (Length(sFim) <= 0) then
                break;

              with fatRisco.Add do
              begin
                codFatRis := sFim;
              end;

              Inc(J);
            end;

          end;

          Inc(I);
        end;
      end;

      sSecao := 'altInsalPeric';
      if INIRec.ReadString(sSecao, 'dtAltCondicao', '') <> '' then
      begin
        InsalPeric.altInsalPeric.DtaltCondicao := StringToDateTime(INIRec.ReadString(sSecao, 'dtAltCondicao', '0'));

        I := 1;
        while true do
        begin
          // de 01 até 99
          sSecao := 'infoAmb' + IntToStrZero(I, 2);
          sFim   := INIRec.ReadString(sSecao, 'codAmb', 'FIM');

          if (sFim = 'FIM') or (Length(sFim) <= 0) then
            break;

          with InsalPeric.altInsalPeric.infoAmb.Add do
          begin
            codAmb := sFim;

            J := 1;
            while true do
            begin
              // de 001 até 999
              sSecao := 'fatRisco' + IntToStrZero(I, 2) + IntToStrZero(J, 3);
              sFim   := INIRec.ReadString(sSecao, 'codFatRis', 'FIM');

              if (sFim = 'FIM') or (Length(sFim) <= 0) then
                break;

              with fatRisco.Add do
              begin
                codFatRis := sFim;
              end;

              Inc(J);
            end;

          end;

          Inc(I);
        end;
      end;

      sSecao := 'fimInsalPeric';
      if INIRec.ReadString(sSecao, 'dtFimCondicao', '') <> '' then
      begin
        InsalPeric.fimInsalPeric.dtFimCondicao := StringToDateTime(INIRec.ReadString(sSecao, 'dtFimCondicao', '0'));

        I := 1;
        while true do
        begin
          // de 01 até 99
          sSecao := 'infoAmb' + IntToStrZero(I, 2);
          sFim   := INIRec.ReadString(sSecao, 'codAmb', 'FIM');

          if (sFim = 'FIM') or (Length(sFim) <= 0) then
            break;

          with InsalPeric.fimInsalPeric.infoAmb.Add do
          begin
            codAmb := sFim;
          end;

          Inc(I);
        end;
      end;

      sSecao := 'iniAposentEsp';
      if INIRec.ReadString(sSecao, 'dtIniCondicao', '') <> '' then
      begin
        AposentEsp.iniAposentEsp.DtiniCondicao := StringToDateTime(INIRec.ReadString(sSecao, 'dtIniCondicao', '0'));

        I := 1;
        while true do
        begin
          // de 01 até 99
          sSecao := 'infoAmb' + IntToStrZero(I, 2);
          sFim   := INIRec.ReadString(sSecao, 'codAmb', 'FIM');

          if (sFim = 'FIM') or (Length(sFim) <= 0) then
            break;

          with AposentEsp.iniAposentEsp.infoAmb.Add do
          begin
            codAmb := sFim;

            J := 1;
            while true do
            begin
              // de 001 até 999
              sSecao := 'fatRisco' + IntToStrZero(I, 2) + IntToStrZero(J, 3);
              sFim   := INIRec.ReadString(sSecao, 'codFatRis', 'FIM');

              if (sFim = 'FIM') or (Length(sFim) <= 0) then
                break;

              with fatRisco.Add do
              begin
                codFatRis := sFim;
              end;

              Inc(J);
            end;

          end;

          Inc(I);
        end;
      end;

      sSecao := 'altAposentEsp';
      if INIRec.ReadString(sSecao, 'dtAltCondicao', '') <> '' then
      begin
        AposentEsp.altAposentEsp.DtaltCondicao := StringToDateTime(INIRec.ReadString(sSecao, 'dtAltCondicao', '0'));

        I := 1;
        while true do
        begin
          // de 01 até 99
          sSecao := 'infoAmb' + IntToStrZero(I, 2);
          sFim   := INIRec.ReadString(sSecao, 'codAmb', 'FIM');

          if (sFim = 'FIM') or (Length(sFim) <= 0) then
            break;

          with AposentEsp.altAposentEsp.infoAmb.Add do
          begin
            codAmb := sFim;

            J := 1;
            while true do
            begin
              // de 001 até 999
              sSecao := 'fatRisco' + IntToStrZero(I, 2) + IntToStrZero(J, 3);
              sFim   := INIRec.ReadString(sSecao, 'codFatRis', 'FIM');

              if (sFim = 'FIM') or (Length(sFim) <= 0) then
                break;

              with fatRisco.Add do
              begin
                codFatRis := sFim;
              end;

              Inc(J);
            end;

          end;

          Inc(I);
        end;
      end;

      sSecao := 'fimAposentEsp';
      if INIRec.ReadString(sSecao, 'dtFimCondicao', '') <> '' then
      begin
        AposentEsp.fimAposentEsp.DtfimCondicao := StringToDateTime(INIRec.ReadString(sSecao, 'dtFimCondicao', '0'));

        I := 1;
        while true do
        begin
          // de 01 até 99
          sSecao := 'infoAmb' + IntToStrZero(I, 2);
          sFim   := INIRec.ReadString(sSecao, 'codAmb', 'FIM');

          if (sFim = 'FIM') or (Length(sFim) <= 0) then
            break;

          with AposentEsp.fimAposentEsp.infoAmb.Add do
          begin
            codAmb := sFim;
          end;

          Inc(I);
        end;
      end;

    end;

    GerarXML;

    Result := True;
  finally
     INIRec.Free;
  end;
end;

end.
