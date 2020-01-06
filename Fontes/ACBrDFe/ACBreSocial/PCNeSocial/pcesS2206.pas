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
|* 01/03/2016: Alterações para validação com o XSD
******************************************************************************}
{$I ACBr.inc}

unit pcesS2206;

interface

uses
  SysUtils, Classes, Contnrs,
  pcnConversao, ACBrUtil,
  pcesCommon, pcesConversaoeSocial, pcesGerador;

type

  TS2206CollectionItem = class;
  TEvtAltContratual = class;
  TAltContratual = class;
  TServPubl = class;
  TInfoContratoS2206 = class;

  TS2206Collection = class(TeSocialCollection)
  private
    function GetItem(Index: Integer): TS2206CollectionItem;
    procedure SetItem(Index: Integer; Value: TS2206CollectionItem);
  public
    function Add: TS2206CollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TS2206CollectionItem;
    property Items[Index: Integer]: TS2206CollectionItem read GetItem write SetItem; default;
  end;

  TS2206CollectionItem = class(TObject)
  private
    FTipoEvento : TTipoEvento;
    FEvtAltContratual: TEvtAltContratual;
  public
    constructor Create(AOwner: TComponent);
    destructor  Destroy; override;
    property TipoEvento : TTipoEvento read FTipoEvento;
    property EvtAltContratual : TEvtAltContratual read FEvtAltContratual write FEvtAltContratual;
  end;

  TEvtAltContratual = class(TeSocialEvento)
  private
    FIdeEvento: TIdeEvento2;
    FIdeEmpregador: TIdeEmpregador;
    FIdeVinculo : TIdeVinculo;
    FAltContratual: TAltContratual;

    {Geradores da Classe - Necessários pois os geradores de ACBreSocialGerador
     possuem campos excedentes que não se aplicam ao S2206}
    procedure GerarAltContratual(objAltContratual: TAltContratual);
    procedure GerarInfoCeletista(objInfoCeletista : TInfoCeletista);
    procedure GerarInfoEstatutario(pInfoEstatutario: TInfoEstatutario);
    procedure GerarInfoContrato(ObjInfoContrato : TInfoContratoS2206; pTipo: Integer; pInfoCeletista: TInfoCeletista);
    procedure GerarTrabTemp(pTrabTemp: TTrabTemporario);
    procedure GerarServPubl(pServPubl: TServPubl);
    function  GetAltContratual : TAltContratual;
  public
    constructor Create(AACBreSocial: TObject); override;
    destructor Destroy; override;

    function GerarXML: boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property IdeEvento : TIdeEvento2 read FIdeEvento write FIdeEvento;
    property IdeEmpregador : TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property IdeVinculo : TIdeVinculo read FIdeVinculo write FIdeVInculo;
    property AltContratual : TAltContratual read GetAltContratual write FAltContratual;
  end;

  TServPubl = class(TObject)
  private
    FMtvAlter: tpMtvAlt;
  public
    property mtvAlter: tpMtvAlt read FMtvAlter write FMtvAlter;
  end;

  TInfoContratoS2206 = class(TInfoContrato)
  private
    FServPubl: TServPubl;

    function getServPubl: TServPubl;
  public
    constructor Create;
    destructor Destroy; override;
    function servPublInst: boolean;
    property servPubl: TServPubl read getServPubl write FServPubl;
  end;

  TAltContratual = class(TObject)
  private
    FdtAlteracao : TDateTime;
    FDtEf: TDateTime;
    FDscAlt: string;
    FVinculo     : TVinculo;
    FinfoRegimeTrab : TinfoRegimeTrab;
    FinfoContrato   : TInfoContratoS2206;
  public
    constructor Create;
    destructor  Destroy; override;
    property dtALteracao : TDateTime read FdtAlteracao write FdtAlteracao;
    property dtEf: TDateTime read FDtEf write FDtEf;
    property dscAlt: string read FDscAlt write FDscAlt;
    property Vinculo : TVInculo read FVinculo write FVinculo;
    property infoRegimeTrab : TinfoRegimeTrab read FinfoRegimeTrab write FinfoRegimeTrab;
    property infoContrato : TInfoContratoS2206 read FinfoContrato write FinfoContrato;
  end;

implementation

uses
  IniFiles,
  ACBreSocial;

{ TS2206Collection }

function TS2206Collection.Add: TS2206CollectionItem;
begin
  Result := Self.New;
end;

function TS2206Collection.GetItem(Index: Integer): TS2206CollectionItem;
begin
   Result := TS2206CollectionItem(inherited GetItem(Index));
end;

procedure TS2206Collection.SetItem(Index: Integer; Value: TS2206CollectionItem);
begin
  inherited SetItem(Index, Value);
end;

function TS2206Collection.New: TS2206CollectionItem;
begin
  Result := TS2206CollectionItem.Create(FACBreSocial);
  Self.Add(Result);
end;

{ TS2206CollectionItem }

constructor TS2206CollectionItem.Create(AOwner: TComponent);
begin
  inherited Create;
  FTipoEvento       := teS2206;
  FEvtAltContratual := TEvtAltContratual.Create(AOwner);
end;

destructor TS2206CollectionItem.Destroy;
begin
  FEvtAltContratual.Free;

  inherited;
end;

{ TAltContratual }

constructor TAltContratual.Create;
begin
  inherited;

  FVinculo        := TVinculo.Create;
  FinfoRegimeTrab := TinfoRegimeTrab.Create;
  FinfoContrato   := TInfoContratoS2206.Create;
end;

destructor TAltContratual.Destroy;
begin
  FVinculo.Free;
  FinfoRegimeTrab.Free;
  FinfoContrato.Free;

  inherited;
end;

{ TInfoContratoS2206 }

constructor TInfoContratoS2206.Create;
begin
  inherited;

  FServPubl := nil;
end;

destructor TInfoContratoS2206.Destroy;
begin
  FreeAndNil(FServPubl);

  inherited;
end;

function TInfoContratoS2206.getServPubl: TServPubl;
begin
  if not Assigned(FServPubl) then
    FServPubl := TServPubl.Create;
  Result := FServPubl;
end;

function TInfoContratoS2206.servPublInst: boolean;
begin
  result := Assigned(FServPubl);
end;

{ TEvtAltContratual }

constructor TEvtAltContratual.Create(AACBreSocial: TObject);
begin
  inherited Create(AACBreSocial);

  FIdeEvento     := TIdeEvento2.Create;
  FIdeEmpregador := TIdeEmpregador.Create;
  FIdeVinculo    := TIdeVinculo.Create;
  FAltContratual := TAltContratual.Create;
end;

destructor TEvtAltContratual.destroy;
begin
  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FAltContratual.Free;

  inherited;
end;

procedure TEvtAltContratual.GerarInfoEstatutario(pInfoEstatutario: TInfoEstatutario);
begin
  if eSTpPlanRPToStr(pInfoEstatutario.tpPlanRP) <> '0' then
  begin
    Gerador.wGrupo('infoEstatutario');

    Gerador.wCampo(tcInt, '', 'tpPlanRP', 1, 1, 1, eSTpPlanRPToStr(pInfoEstatutario.tpPlanRP));

    Gerador.wGrupo('/infoEstatutario');
  end;
end;

procedure TEvtAltContratual.GerarAltContratual(objAltContratual: TAltContratual);
begin
  Gerador.wGrupo('altContratual');

  Gerador.wCampo(tcDat, '', 'dtAlteracao', 10,  10, 1, objAltContratual.dtALteracao);
  Gerador.wCampo(tcDat, '', 'dtEf',        10,  10, 0, objAltContratual.dtEf);
  Gerador.wCampo(tcStr, '', 'dscAlt',       1, 150, 0, objAltContratual.dscAlt);

  GerarVinculo(objAltContratual.Vinculo, 3);

  Gerador.wGrupo('infoRegimeTrab');

  if objAltContratual.infoRegimeTrab.InfoCeletista.cnpjSindCategProf <> '' then
    GerarInfoCeletista(objAltContratual.infoRegimeTrab.InfoCeletista)
  else
    GerarInfoEstatutario(objAltContratual.infoRegimeTrab.InfoEstatutario);

  Gerador.wGrupo('/infoRegimeTrab');

  GerarInfoContrato(objAltContratual.InfoContrato, 3, objAltContratual.infoRegimeTrab.InfoCeletista);

  Gerador.wGrupo('/altContratual');
end;

procedure TEvtAltContratual.GerarTrabTemp(pTrabTemp: TTrabTemporario);
begin
  if pTrabTemp.justProrr <> '' then
  begin
    Gerador.wGrupo('trabTemp');

    Gerador.wCampo(tcStr, '', 'justProrr', 1, 999, 1, pTrabTemp.justProrr);

    Gerador.wGrupo('/trabTemp');
  end;
end;

procedure TEvtAltContratual.GerarInfoCeletista(objInfoCeletista: TInfoCeletista);
begin
  Gerador.wGrupo('infoCeletista');

  Gerador.wCampo(tcStr, '', 'tpRegJor',           1,  1, 1, eSTpRegJorToStr(objInfoCeletista.TpRegJor));
  Gerador.wCampo(tcStr, '', 'natAtividade',       1,  1, 1, eSNatAtividadeToStr(objInfoCeletista.NatAtividade));

  if objInfoCeletista.dtBase > 0  then
    Gerador.wCampo(tcStr, '', 'dtBase',             1,  2, 0, FormatFloat('00',objInfoCeletista.dtBase));

  Gerador.wCampo(tcStr, '', 'cnpjSindCategProf', 14, 14, 1, objInfoCeletista.cnpjSindCategProf);

  GerarTrabTemp(objInfoCeletista.TrabTemporario);
  GerarInfoAprend(objInfoCeletista.aprend);

  Gerador.wGrupo('/infoCeletista');
end;

procedure TEvtAltContratual.GerarServPubl(pServPubl: TServPubl);
begin
  Gerador.wGrupo('servPubl');

  Gerador.wCampo(tcInt, '', 'mtvAlter', 1, 1, 1, eSTpMtvAltToStr(pServPubl.mtvAlter));

  Gerador.wGrupo('/servPubl');
end;

procedure TEvtAltContratual.GerarInfoContrato(ObjInfoContrato: TInfoContratoS2206; pTipo: Integer; pInfoCeletista: TInfoCeletista);
begin
  Gerador.wGrupo('infoContrato');

  Gerador.wCampo(tcStr, '', 'codCargo',     1, 30, 0, objInfoContrato.CodCargo);
  Gerador.wCampo(tcStr, '', 'codFuncao',    1, 30, 0, objInfoContrato.CodFuncao);
  Gerador.wCampo(tcInt, '', 'codCateg',     1,  3, 1, objInfoContrato.CodCateg);
  Gerador.wCampo(tcStr, '', 'codCarreira',  1, 30, 0, objInfoContrato.codCarreira);
  Gerador.wCampo(tcDat, '', 'dtIngrCarr',  10, 10, 0, objInfoContrato.dtIngrCarr);

  GerarRemuneracao(objInfoContrato.Remuneracao);
  GerarDuracao(objInfoContrato.Duracao, pTipo);
  GerarLocalTrabalho(objInfoContrato.LocalTrabalho);

  //Informações do Horário Contratual do Trabalhador. O preenchimento é obrigatório se {tpRegJor} = [1]
  if (pInfoCeletista.TpRegJor = rjSubmetidosHorarioTrabalho) then
    GerarHorContratual(objInfoContrato.HorContratual);

  GerarFiliacaoSindical(objInfoContrato.FiliacaoSindical);
  GerarAlvaraJudicial(objInfoContrato.AlvaraJudicial);
  GerarObservacoes(objInfoContrato.observacoes);

  if objInfoContrato.servPublInst then
    GerarServPubl(objInfoContrato.servPubl);

  Gerador.wGrupo('/infoContrato');
end;

function TEvtAltContratual.GerarXML: boolean;
begin
  try
    Self.VersaoDF := TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF;
     
    Self.Id := GerarChaveEsocial(now, self.ideEmpregador.NrInsc, self.Sequencial);

    GerarCabecalho('evtAltContratual');

    Gerador.wGrupo('evtAltContratual Id="' + Self.Id + '"');

    GerarIdeEvento2(self.IdeEvento);
    GerarIdeEmpregador(self.IdeEmpregador);
    GerarIdeVinculo(Self.IdeVinculo);
    GerarAltContratual(FAltContratual);

    Gerador.wGrupo('/evtAltContratual');

    GerarRodape;

    FXML := Gerador.ArquivoFormatoXML;
//    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtAltContratual');

//    Validar(schevtAltContratual);
  except on e:exception do
    raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '')
end;

function TEvtAltContratual.GetAltContratual: TAltContratual;
begin
  if Not(Assigned(FAltContratual)) then
    FAltContratual := TAltContratual.Create;
  Result := FAltContratual;
end;

function TEvtAltContratual.LerArqIni(const AIniString: String): Boolean;
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
      sSecao := 'evtAltContratual';
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

      sSecao := 'altContratual';
      altContratual.dtAlteracao := StringToDateTime(INIRec.ReadString(sSecao, 'dtAlteracao', '0'));
      altContratual.dtEf        := StringToDateTime(INIRec.ReadString(sSecao, 'dtEf', '0'));
      altContratual.dscAlt      := INIRec.ReadString(sSecao, 'dscAlt', EmptyStr);

      sSecao := 'vinculo';
      altContratual.vinculo.TpRegPrev := eSStrTotpRegPrev(Ok, INIRec.ReadString(sSecao, 'tpRegPrev', '1'));

      sSecao := 'infoCeletista';
      if INIRec.ReadString(sSecao, 'tpRegJor', '') <> '' then
      begin
        altContratual.InfoRegimeTrab.InfoCeletista.TpRegJor          := eSStrToTpRegJor(Ok, INIRec.ReadString(sSecao, 'tpRegJor', '1'));
        altContratual.InfoRegimeTrab.InfoCeletista.NatAtividade      := eSStrToNatAtividade(Ok, INIRec.ReadString(sSecao, 'natAtividade', '1'));
        altContratual.InfoRegimeTrab.InfoCeletista.dtBase            := INIRec.ReadInteger(sSecao, 'dtBase', 0);
        altContratual.InfoRegimeTrab.InfoCeletista.cnpjSindCategProf := INIRec.ReadString(sSecao, 'cnpjSindCategProf', '');

        sSecao := 'trabTemp';
        if INIRec.ReadString(sSecao, 'justContr', '') <> '' then
          altContratual.InfoRegimeTrab.InfoCeletista.trabTemporario.justContr := INIRec.ReadString(sSecao, 'justContr', '');

        sSecao := 'aprend';
        if INIRec.ReadString(sSecao, 'tpInsc', '') <> '' then
        begin
          altContratual.InfoRegimeTrab.InfoCeletista.aprend.TpInsc := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
          altContratual.InfoRegimeTrab.InfoCeletista.aprend.NrInsc := INIRec.ReadString(sSecao, 'nrInsc', '');
        end;
      end;

      sSecao := 'infoEstatutario';
      if INIRec.ReadString(sSecao, 'tpPlanRP', '') <> '' then
        altContratual.InfoRegimeTrab.infoEstatutario.tpPlanRP := eSStrToTpPlanRP(Ok, INIRec.ReadString(sSecao, 'tpPlanRP', '1'));

      sSecao := 'infoContrato';
      altContratual.infoContrato.CodCargo    := INIRec.ReadString(sSecao, 'codCargo', '');
      altContratual.infoContrato.CodFuncao   := INIRec.ReadString(sSecao, 'codFuncao', '');
      altContratual.infoContrato.CodCateg    := INIRec.ReadInteger(sSecao, 'codCateg', 0);
      altContratual.infoContrato.codCarreira := INIRec.ReadString(sSecao, 'codCarreira', '');
      altContratual.infoContrato.dtIngrCarr  := StringToDateTime(INIRec.ReadString(sSecao, 'dtIngrCarr', '0'));

      sSecao := 'remuneracao';
      altContratual.infoContrato.remuneracao.VrSalFx    := StringToFloatDef(INIRec.ReadString(sSecao, 'vrSalFx', ''), 0);
      altContratual.infoContrato.remuneracao.UndSalFixo := eSStrToUndSalFixo(Ok, INIRec.ReadString(sSecao, 'undSalFixo', ''));
      altContratual.infoContrato.remuneracao.DscSalVar  := INIRec.ReadString(sSecao, 'dscSalVar', '');

      sSecao := 'duracao';
      altContratual.infoContrato.duracao.TpContr := eSStrToTpContr(Ok, INIRec.ReadString(sSecao, 'tpContr', '1'));
      altContratual.infoContrato.duracao.dtTerm  := StringToDateTime(INIRec.ReadString(sSecao, 'dtTerm', '0'));

      sSecao := 'localTrabGeral';
      if INIRec.ReadString(sSecao, 'tpInsc', '') <> '' then
      begin
        altContratual.infoContrato.LocalTrabalho.LocalTrabGeral.TpInsc   := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
        altContratual.infoContrato.LocalTrabalho.LocalTrabGeral.NrInsc   := INIRec.ReadString(sSecao, 'nrInsc', '');
        altContratual.infoContrato.LocalTrabalho.LocalTrabGeral.DescComp := INIRec.ReadString(sSecao, 'descComp', '');
      end;

      sSecao := 'localTrabDom';
      if INIRec.ReadString(sSecao, 'tpLograd', '') <> '' then
      begin
        altContratual.infoContrato.LocalTrabalho.localTrabDom.TpLograd    := INIRec.ReadString(sSecao, 'tpLograd', '');
        altContratual.infoContrato.LocalTrabalho.localTrabDom.DscLograd   := INIRec.ReadString(sSecao, 'dscLograd', '');
        altContratual.infoContrato.LocalTrabalho.localTrabDom.NrLograd    := INIRec.ReadString(sSecao, 'nrLograd', '');
        altContratual.infoContrato.LocalTrabalho.localTrabDom.Complemento := INIRec.ReadString(sSecao, 'complemento', '');
        altContratual.infoContrato.LocalTrabalho.localTrabDom.Bairro      := INIRec.ReadString(sSecao, 'bairro', '');
        altContratual.infoContrato.LocalTrabalho.localTrabDom.Cep         := INIRec.ReadString(sSecao, 'cep', '');
        altContratual.infoContrato.LocalTrabalho.localTrabDom.CodMunic    := INIRec.ReadInteger(sSecao, 'CodMunic', 0);
        altContratual.infoContrato.LocalTrabalho.localTrabDom.uf          := eSStrTouf(Ok, INIRec.ReadString(sSecao, 'uf', 'SP'));
      end;

      sSecao := 'horContratual';
      if INIRec.ReadString(sSecao, 'qtdHrsSem', '') <> '' then
      begin
        altContratual.infoContrato.horContratual.QtdHrsSem := INIRec.ReadInteger(sSecao, 'qtdHrsSem', 0);
        altContratual.infoContrato.horContratual.TpJornada := eSStrToTpJornada(Ok, INIRec.ReadString(sSecao, 'tpJornada', '1'));
        altContratual.infoContrato.horContratual.DscTpJorn := INIRec.ReadString(sSecao, 'dscTpJorn', '');
        altContratual.infoContrato.horContratual.tmpParc   := StrTotpTmpParc(Ok, INIRec.ReadString(sSecao, 'tmpParc', '0'));
      end;

      I := 1;
      while true do
      begin
        // de 00 até 99
        sSecao := 'horario' + IntToStrZero(I, 2);
        sFim   := INIRec.ReadString(sSecao, 'dia', 'FIM');

        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break;

        with altContratual.infoContrato.horContratual.horario.New do
        begin
          dia           := eSStrToTpDia(Ok, sFim);
          CodHorContrat := INIRec.ReadString(sSecao, 'codHorContrat', '');
        end;

        Inc(I);
      end;

      I := 1;
      while true do
      begin
        // de 0 até 2
        sSecao := 'filiacaoSindical' + IntToStrZero(I, 1);
        sFim   := INIRec.ReadString(sSecao, 'cnpjSindTrab', 'FIM');

        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break;

        with altContratual.infoContrato.filiacaoSindical.Add do
        begin
          cnpjSindTrab := sFim;
        end;

        Inc(I);
      end;

      sSecao := 'alvaraJudicial';
      if INIRec.ReadString(sSecao, 'nrProcJud', '') <> '' then
        altContratual.infoContrato.alvaraJudicial.NrProcJud := INIRec.ReadString(sSecao, 'nrProcJud', '');

      I := 1;
      while true do
      begin
        // de 00 até 99
        sSecao := 'observacoes' + IntToStrZero(I, 2);
        sFim   := INIRec.ReadString(sSecao, 'observacao', 'FIM');

        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break;

        with altContratual.infoContrato.observacoes.New do
        begin
          observacao := sFim;
        end;

        Inc(I);
      end;

      sSecao := 'servPubl';
      if INIRec.ReadString(sSecao, 'mtvAlter', '') <> '' then
        altContratual.infoContrato.servPubl.mtvAlter := eSStrToTpMtvAlt(Ok, INIRec.ReadString(sSecao, 'mtvAlter', '1'));
    end;

    GerarXML;
    XML := FXML;
  finally
    INIRec.Free;
  end;
end;

end.
