{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
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
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 27/10/2015: Jean Carlo Cantu, Tiago Ravache
|*  - Doação do componente para o Projeto ACBr
|* 28/08/2017: Leivio Fontenele - leivio@yahoo.com.br
|*  - Implementação comunicação, envelope, status e retorno do componente com webservice.
******************************************************************************}

{$I ACBr.inc}

unit pcesS2206;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$ELSE}
   Contnrs,
  {$IFEND}
  ACBrBase, pcnConversao,
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
  ACBrUtil.Base,
  ACBrUtil.FilesIO,
  ACBrUtil.DateTime,
  ACBreSocial;

{ TS2206Collection }

function TS2206Collection.Add: TS2206CollectionItem;
begin
  Result := Self.New;
end;

function TS2206Collection.GetItem(Index: Integer): TS2206CollectionItem;
begin
   Result := TS2206CollectionItem(inherited Items[Index]);
end;

procedure TS2206Collection.SetItem(Index: Integer; Value: TS2206CollectionItem);
begin
  inherited Items[Index] := Value;
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
  Gerador.wGrupo('infoEstatutario');

  Gerador.wCampo(tcInt, '', 'tpPlanRP', 1, 1, 1, eSTpPlanRPToStr(pInfoEstatutario.tpPlanRP));

  if VersaoDF > ve02_05_00 then
  begin
    if pInfoEstatutario.indTetoRGPS <> snfNada then
      Gerador.wCampo(tcStr, '', 'indTetoRGPS', 0, 1, 0,  eSSimNaoFacultativoToStr(pInfoEstatutario.indTetoRGPS));

    if pInfoEstatutario.indAbonoPerm <> snfNada then
      Gerador.wCampo(tcStr, '', 'indAbonoPerm', 0, 1, 0,  eSSimNaoFacultativoToStr(pInfoEstatutario.indAbonoPerm));
  end;

  Gerador.wGrupo('/infoEstatutario');
end;

procedure TEvtAltContratual.GerarAltContratual(objAltContratual: TAltContratual);
begin
  Gerador.wGrupo('altContratual');

  Gerador.wCampo(tcDat, '', 'dtAlteracao', 10,  10, 1, objAltContratual.dtALteracao);
  Gerador.wCampo(tcDat, '', 'dtEf',        10,  10, 0, objAltContratual.dtEf);
  Gerador.wCampo(tcStr, '', 'dscAlt',       1, 150, 0, objAltContratual.dscAlt);

  if VersaoDF <= ve02_05_00 then
    GerarVinculo(objAltContratual.Vinculo, 3)
  else
  begin
    Gerador.wGrupo('vinculo');

    Gerador.wCampo(tcStr, '', 'tpRegPrev', 1, 1, 1, eSTpRegPrevToStr(objAltContratual.vinculo.tpRegPrev));
  end;  
   
  Gerador.wGrupo('infoRegimeTrab');
  
  if objAltContratual.infoRegimeTrab.InfoCeletista.cnpjSindCategProf <> '' then
    GerarInfoCeletista(objAltContratual.infoRegimeTrab.InfoCeletista)
  else
    if(objAltContratual.FVinculo.tpRegPrev = rpRPPS)then
      GerarInfoEstatutario(objAltContratual.infoRegimeTrab.InfoEstatutario);
  
  Gerador.wGrupo('/infoRegimeTrab');
 
  GerarInfoContrato(objAltContratual.InfoContrato, 3, objAltContratual.infoRegimeTrab.InfoCeletista);

  if VersaoDF > ve02_05_00 then
    Gerador.wGrupo('/vinculo');
  
  Gerador.wGrupo('/altContratual');
end;

procedure TEvtAltContratual.GerarTrabTemp(pTrabTemp: TTrabTemporario);
begin
  if pTrabTemp.justProrr <> '' then
  begin
    Gerador.wGrupo('trabTemporario');

    Gerador.wCampo(tcStr, '', 'justProrr', 1, 999, 1, pTrabTemp.justProrr);

    Gerador.wGrupo('/trabTemporario');
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

  if VersaoDF <= ve02_05_00 then
  begin
    Gerador.wCampo(tcStr, '', 'codCargo',     1, 30, 0, objInfoContrato.CodCargo);
    Gerador.wCampo(tcStr, '', 'codFuncao',    1, 30, 0, objInfoContrato.CodFuncao);
  end
  else
  begin
    Gerador.wCampo(tcStr, '', 'nmCargo',     0, 100, 0, objInfoContrato.nmCargo);
    Gerador.wCampo(tcStr, '', 'CBOCargo',    0,   6, 0, objInfoContrato.CBOCargo);
    Gerador.wCampo(tcDat, '', 'dtIngrCargo', 0,  10, 0, objInfoContrato.dtIngrCargo);
    Gerador.wCampo(tcStr, '', 'nmFuncao',    0, 100, 0, objInfoContrato.nmFuncao);
    Gerador.wCampo(tcStr, '', 'CBOFuncao',   0,   6, 0, objInfoContrato.CBOFuncao);
    Gerador.wCampo(tcStr, '', 'acumCargo',   0,   1, 0, eSSimNaoFacultativoToStr(objInfoContrato.acumCargo));
  end;
  
  Gerador.wCampo(tcInt, '', 'codCateg',     1,  3, 1, objInfoContrato.CodCateg);
  
  if VersaoDF <= ve02_05_00 then
  begin
    Gerador.wCampo(tcStr, '', 'codCarreira',  1, 30, 0, objInfoContrato.codCarreira);
    Gerador.wCampo(tcDat, '', 'dtIngrCarr',  10, 10, 0, objInfoContrato.dtIngrCarr);
  end;
  if(NaoEstaVazio(pInfoCeletista.cnpjSindCategProf))then
  begin
    GerarRemuneracao(objInfoContrato.Remuneracao);
    GerarDuracao(objInfoContrato.Duracao, pTipo);
  end;
  GerarLocalTrabalho(objInfoContrato.LocalTrabalho);

//  Informações do Horário Contratual do Trabalhador. O preenchimento é obrigatório se {tpRegJor} = [1]
  if(NaoEstaVazio(pInfoCeletista.cnpjSindCategProf))then
  begin
    if (pInfoCeletista.TpRegJor = rjSubmetidosHorarioTrabalho) then
      GerarHorContratual(objInfoContrato.HorContratual);
  end;
  if VersaoDF < veS01_00_00 then
     GerarFiliacaoSindical(objInfoContrato.FiliacaoSindical);
  GerarAlvaraJudicial(objInfoContrato.AlvaraJudicial);
  GerarObservacoes(objInfoContrato.observacoes);

  if objInfoContrato.servPublInst then
    GerarServPubl(objInfoContrato.servPubl);

  if VersaoDF > ve02_05_00 then
    if objInfoContrato.treiCapInst() then
      GerarTreinamentoCapacitacao(objInfoContrato.treiCap);
    
  Gerador.wGrupo('/infoContrato');
end;

function TEvtAltContratual.GerarXML: boolean;
begin
  try
    inherited GerarXML;
    Self.VersaoDF := TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF;
     
    Self.Id := GerarChaveEsocial(now, self.ideEmpregador.NrInsc, self.Sequencial);

    GerarCabecalho('evtAltContratual');

    Gerador.wGrupo('evtAltContratual Id="' + Self.Id + '"');

    GerarIdeEvento2(self.IdeEvento);
    GerarIdeEmpregador(self.IdeEmpregador);
    GerarIdeVinculo2206(Self.IdeVinculo, False);
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

        sSecao := 'trabTemporario';
        sFim := INIRec.ReadString(sSecao, 'justProrr', '');
        if sFim <> '' then
          altContratual.InfoRegimeTrab.InfoCeletista.trabTemporario.justProrr := sFim
        else
        begin
          sSecao := 'trabTemp';
          sFim := INIRec.ReadString(sSecao, 'justProrr', '');

          if sFim <> '' then
            altContratual.InfoRegimeTrab.InfoCeletista.trabTemporario.justProrr := sFim;
        end;

        sSecao := 'aprend';

        Ok := False;
        if (TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF >= veS01_02_00) then
        begin
          if INIRec.ReadString(sSecao, 'indAprend', '') = '1' then
            Ok := (INIRec.ReadString(sSecao, 'cnpjEntQual', '') <> EmptyStr)
          else
            Ok := (INIRec.ReadString(sSecao, 'tpInsc', '') <> EmptyStr);
        end
        else
          Ok := (INIRec.ReadString(sSecao, 'tpInsc', '') <> EmptyStr);

        if Ok then
        begin
          altContratual.InfoRegimeTrab.InfoCeletista.aprend.indAprend := eSStrTotpIndAprend(Ok, INIRec.ReadString(sSecao, 'indAprend', '1'));
          altContratual.InfoRegimeTrab.InfoCeletista.aprend.cnpjEntQual := INIRec.ReadString(sSecao, 'cnpjEntQual', '');
          altContratual.InfoRegimeTrab.InfoCeletista.aprend.TpInsc := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
          altContratual.InfoRegimeTrab.InfoCeletista.aprend.NrInsc := INIRec.ReadString(sSecao, 'nrInsc', '');
          altContratual.InfoRegimeTrab.InfoCeletista.aprend.cnpjPrat := INIRec.ReadString(sSecao, 'cnpjPrat', '');
        end;
      end;

      sSecao := 'infoEstatutario';
      if INIRec.ReadString(sSecao, 'tpPlanRP', '') <> '' then
      begin
        altContratual.InfoRegimeTrab.infoEstatutario.tpPlanRP := eSStrToTpPlanRP(Ok, INIRec.ReadString(sSecao, 'tpPlanRP', '1'));
        altContratual.InfoRegimeTrab.infoEstatutario.indTetoRGPS := eSStrToSimNaoFacultativo(Ok, INIRec.ReadString(sSecao, 'indTetoRGPS', ''));
        altContratual.InfoRegimeTrab.infoEstatutario.indAbonoPerm := eSStrToSimNaoFacultativo(Ok, INIRec.ReadString(sSecao, 'indAbonoPerm', ''));
      end;

      sSecao := 'infoContrato';
      altContratual.infoContrato.CodCargo    := INIRec.ReadString(sSecao, 'codCargo', '');
      altContratual.infoContrato.CodFuncao   := INIRec.ReadString(sSecao, 'codFuncao', '');
      altContratual.infoContrato.CodCateg    := INIRec.ReadInteger(sSecao, 'codCateg', 0);
      altContratual.infoContrato.codCarreira := INIRec.ReadString(sSecao, 'codCarreira', '');
      altContratual.infoContrato.dtIngrCarr  := StringToDateTime(INIRec.ReadString(sSecao, 'dtIngrCarr', '0'));

      altContratual.infoContrato.nmCargo      := INIRec.ReadString(sSecao, 'nmCargo', '');
      altContratual.infoContrato.CBOCargo     := INIRec.ReadString(sSecao, 'CBOCargo', '');
      altContratual.infoContrato.dtIngrCargo  := StringToDateTime(INIRec.ReadString(sSecao, 'dtIngrCargo', '0'));
      altContratual.infoContrato.nmFuncao     := INIRec.ReadString(sSecao, 'nmFuncao', '');
      altContratual.infoContrato.CBOFuncao    := INIRec.ReadString(sSecao, 'CBOFuncao', '');
      altContratual.infoContrato.acumCargo    := eSStrToSimNaoFacultativo(Ok, INIRec.ReadString(sSecao, 'acumCargo', ''));

      sSecao := 'remuneracao';
      altContratual.infoContrato.remuneracao.VrSalFx    := StringToFloatDef(INIRec.ReadString(sSecao, 'vrSalFx', ''), 0);
      altContratual.infoContrato.remuneracao.UndSalFixo := eSStrToUndSalFixo(Ok, INIRec.ReadString(sSecao, 'undSalFixo', ''));
      altContratual.infoContrato.remuneracao.DscSalVar  := INIRec.ReadString(sSecao, 'dscSalVar', '');

      sSecao := 'duracao';
      altContratual.infoContrato.duracao.TpContr := eSStrToTpContr(Ok, INIRec.ReadString(sSecao, 'tpContr', '1'));
      altContratual.infoContrato.duracao.dtTerm  := StringToDateTime(INIRec.ReadString(sSecao, 'dtTerm', '0'));
      altContratual.infoContrato.duracao.objDet  := INIRec.ReadString(sSecao, 'objDet', '');

      sSecao := 'localTrabGeral';
      if INIRec.ReadString(sSecao, 'tpInsc', '') <> '' then
      begin
        altContratual.infoContrato.LocalTrabalho.LocalTrabGeral.TpInsc   := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
        altContratual.infoContrato.LocalTrabalho.LocalTrabGeral.NrInsc   := INIRec.ReadString(sSecao, 'nrInsc', '');
        altContratual.infoContrato.LocalTrabalho.LocalTrabGeral.DescComp := INIRec.ReadString(sSecao, 'descComp', '');
      end;

      sSecao := 'localTempDom';
      if INIRec.ReadString(sSecao, 'tpLograd', '') <> '' then
      begin
        altContratual.infoContrato.LocalTrabalho.localTempDom.TpLograd    := INIRec.ReadString(sSecao, 'tpLograd', '');
        altContratual.infoContrato.LocalTrabalho.localTempDom.DscLograd   := INIRec.ReadString(sSecao, 'dscLograd', '');
        altContratual.infoContrato.LocalTrabalho.localTempDom.NrLograd    := INIRec.ReadString(sSecao, 'nrLograd', '');
        altContratual.infoContrato.LocalTrabalho.localTempDom.Complemento := INIRec.ReadString(sSecao, 'complemento', '');
        altContratual.infoContrato.LocalTrabalho.localTempDom.Bairro      := INIRec.ReadString(sSecao, 'bairro', '');
        altContratual.infoContrato.LocalTrabalho.localTempDom.Cep         := INIRec.ReadString(sSecao, 'cep', '');
        altContratual.infoContrato.LocalTrabalho.localTempDom.CodMunic    := INIRec.ReadInteger(sSecao, 'CodMunic', 0);
        altContratual.infoContrato.LocalTrabalho.localTempDom.uf          := INIRec.ReadString(sSecao, 'uf', 'SP');
      end;

      sSecao := 'horContratual';
      if INIRec.ReadString(sSecao, 'qtdHrsSem', '') <> '' then
      begin
        altContratual.infoContrato.horContratual.QtdHrsSem := StrtoFloatDef(IniRec.ReadString(sSecao,'qtdHrsSem','0'),0);
        altContratual.infoContrato.horContratual.TpJornada := eSStrToTpJornada(Ok, INIRec.ReadString(sSecao, 'tpJornada', '1'));
        altContratual.infoContrato.horContratual.DscTpJorn := INIRec.ReadString(sSecao, 'dscTpJorn', '');
        altContratual.infoContrato.horContratual.dscJorn   := INIRec.ReadString(sSecao, 'dscJorn', '');
        altContratual.infoContrato.horContratual.tmpParc   := StrTotpTmpParc(Ok, INIRec.ReadString(sSecao, 'tmpParc', '0'));
        altContratual.infoContrato.horContratual.horNoturno:= eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'horNoturno', 'S'));  //26/01/2022
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

      I := 1;
      while true do
      begin
        // de 00 até 99
        sSecao := 'treiCap' + IntToStrZero(I, 2);
        sFim   := INIRec.ReadString(sSecao, 'codTreiCap', 'FIM');

        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break;

        with altContratual.infoContrato.treiCap.New do
        begin
          codTreiCap := StrToInt(sFim);
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
