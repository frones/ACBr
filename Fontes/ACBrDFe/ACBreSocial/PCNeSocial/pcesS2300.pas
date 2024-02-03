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

unit pcesS2300;

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
  TS2300CollectionItem = class;
  TEvtTSVInicio = class;
  TinfoTSVInicio = class;
  TinfoComplementares = class;
  TinfoDirSind = class;
  TinfoTrabCedido = class;
  TTermino = class;
  TinfoMandElet = class;
  
  TS2300Collection = class(TeSocialCollection)
  private
    function GetItem(Index: Integer): TS2300CollectionItem;
    procedure SetItem(Index: Integer; Value: TS2300CollectionItem);
  public
    function Add: TS2300CollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TS2300CollectionItem;
    property Items[Index: Integer]: TS2300CollectionItem read GetItem write SetItem; default;
  end;

  TS2300CollectionItem = class(TObject)
  private
    FTipoEvento: TTipoEvento;
    FEvtTSVInicio: TEvtTSVInicio;
  public
    constructor Create(AOwner: TComponent);
    destructor  Destroy; override;
    property TipoEvento: TTipoEvento read FTipoEvento;
    property EvtTSVInicio: TEvtTSVInicio read FEvtTSVInicio write FEvtTSVInicio;
  end;

  TEvtTSVInicio = class(TeSocialEvento)
  private
    FIdeEvento: TIdeEvento2;
    FIdeEmpregador: TIdeEmpregador;
    FTrabalhador: TTrabalhador;
    FinfoTSVInicio : TinfoTSVInicio;

    procedure GerarInfoTSVInicio(obj : TinfoTSVInicio);
    procedure GerarInfoComplementares(obj: TinfoComplementares);
    procedure GerarCargoFuncao(obj: TcargoFuncao);
    procedure GerarRemuneracao(obj: TRemuneracao);
    procedure GerarFGTS(obj: TFGTS);
    procedure GerarinfoDirSind(obj: TinfoDirSind);
    procedure GerarinfoTrabCedido(obj: TinfoTrabCedido);
    procedure GerarinfoEstagiario(obj: TinfoEstagiario);
    procedure GerarInstEnsino(obj: TinstEnsino);
    procedure GerarageIntegracao(obj: TageIntegracao);
    procedure GerarsupervisorEstagio(obj: TsupervisorEstagio);
    procedure GerarTermino(obj: TTermino);
    procedure GerarInfoMandElet(obj: TinfoMandElet);
  public
    constructor Create(AACBreSocial: TObject); override;
    destructor  Destroy; override;

    function GerarXML: boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property IdeEvento: TIdeEvento2 read FIdeEvento write FIdeEvento;
    property IdeEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property Trabalhador: TTrabalhador read FTrabalhador write FTrabalhador;
    property infoTSVInicio : TinfoTSVInicio read FinfoTSVInicio write FInfoTSVInicio;
  end;

  TinfoTSVInicio = class(TObject)
  private
    FcadIni: tpSimNao;
    FcodCateg : Integer;
    FdtInicio : TDateTime;
    FnatAtividade : tpNatAtividade;
    FinfoComplementares : TinfoComplementares;
    Fafastamento: TAfastamento;
    Ftermino: TTermino;
    FMudancaCPF: TMudancaCPF2;
    Fmatricula: String;
    FnrProcTrab: String;
  public
    constructor Create;
    destructor  Destroy; override;

    property cadIni: tpSimNao read FcadIni write FcadIni;
    property codCateg : Integer read FcodCateg write FcodCateg;
    property dtInicio : TDateTime read FdtInicio write FdtInicio;
    property natAtividade : tpNatAtividade read FnatAtividade write FnatAtividade;
    property infoComplementares : TinfoComplementares read FinfoComplementares write FinfoComplementares;
    property mudancaCPF: TMudancaCPF2 read FMudancaCPF write FMudancaCPF;
    property afastamento: TAfastamento read Fafastamento write Fafastamento;
    property termino: TTermino read Ftermino write Ftermino;
    property matricula: String read Fmatricula write FMatricula;
    property nrProcTrab: String read FnrProcTrab write FnrProcTrab;
  end;

  TinfoComplementares = class(TObject)
  private
    FcargoFuncao: TcargoFuncao;
    FRemuneracao: TRemuneracao;
    FFgts: TFGTS;
    FinfoDirSind: TinfoDirSind;
    FinfoTrabCedido: TinfoTrabCedido;
    FinfoEstagiario: TinfoEstagiario;
    FinfoMandElet: TinfoMandElet;
    FlocalTrabGeral: TLocalTrabGeral;
  public
    constructor Create;
    destructor  Destroy; override;

    property cargoFuncao: TcargoFuncao read FcargoFuncao write FcargoFuncao;
    property Remuneracao: TRemuneracao read FRemuneracao write FRemuneracao;
    property Fgts: TFGTS read FFgts write FFgts;
    property infoDirSind: TinfoDirSind read FinfoDirSind write FinfoDirSind;
    property infoTrabCedido: TinfoTrabCedido read FinfoTrabCedido write FinfoTrabCedido;
    property infoEstagiario: TinfoEstagiario read FinfoEstagiario write FinfoEstagiario;
    property infoMandElet: TinfoMandElet read FinfoMandElet write FinfoMandElet;
    property localTrabGeral: TLocalTrabGeral read FLocalTrabGeral write FLocalTrabGeral;
  end;

  TinfoDirSind = class(TObject)
  private
    FcategOrig : Integer;
    FtpInsc: tpTpInsc;
    FnrInsc: String;
    FdtAdmOrig: TDateTime;
    FmatricOrig: String;
    FtpRegTrab: tpTpRegTrab;
    FtpRegPrev: tpTpRegPrev;
  public
    property categOrig: Integer read FcategOrig write FcategOrig;
    property cnpjOrigem: String read FnrInsc write FnrInsc;
    property dtAdmOrig: TDateTime read FdtAdmOrig write FdtAdmOrig;
    property matricOrig: String read FmatricOrig write FmatricOrig;
    property tpInsc: tpTpInsc read FtpInsc write FtpInsc;
    property nrInsc: string read FnrInsc write FnrInsc;
    property tpRegTrab: tpTpRegTrab read FtpRegTrab write FtpRegTrab;
    property tpRegPrev: tpTpRegPrev read FtpRegPrev write FtpRegPrev;
  end;

  TinfoTrabCedido = class(TObject)
  private
    FcategOrig : Integer;
    FcnpjCednt : String;
    FmatricCed : String;
    FdtAdmCed : TDateTime;
    FTpRegTrab: tpTpRegTrab;
    FTpRegPrev: tpTpRegPrev;
    FinfOnus : tpInfOnus;
  public
    property categOrig: Integer read FcategOrig write FcategOrig;
    property cnpjCednt: String read FcnpjCednt write FcnpjCednt;
    property matricCed: String read FmatricCed write FmatricCed;
    property dtAdmCed: TDateTime read FdtAdmCed write FdtAdmCed;
    property tpRegTrab: tpTpRegTrab read FTpRegTrab write FTpRegTrab;
    property tpRegPrev: tpTpRegPrev read FTpRegPrev write FTpRegPrev;
    property infOnus: tpInfOnus read FinfOnus write FinfOnus;
  end;

  TinfoMandElet = class(TObject)
  private
    FindRemunCargo: tpSimNaoFacultativo;
    FtpRegTrab: tpTpRegTrab;
    FtpRegPrev: tpTpRegPrev;
    FcnpjOrig: String;
    FcategOrig: Integer;
    FmatricOrig: String;
    FdtExercOrig: TDateTime;
  public
    property categOrig: Integer read FcategOrig write FcategOrig;
    property cnpjOrig: String read FcnpjOrig write FcnpjOrig;
    property matricOrig: String read FmatricOrig write FmatricOrig;
    property dtExercOrig: TDateTime read FdtExercOrig write FdtExercOrig;
    property indRemunCargo: tpSimNaoFacultativo read FindRemunCargo write FindRemunCargo;
    property tpRegTrab: tpTpRegTrab read FtpRegTrab write FtpRegTrab;
    property tpRegPrev: tpTpRegPrev read FtpRegPrev write FtpRegPrev;
  end;
  
  TTermino = class(TObject)
  private
    FdtTerm: TDateTime;
  public
    property dtTerm: TDateTime read FdtTerm write FdtTerm;
  end;

implementation

uses
  IniFiles,
  ACBrUtil.Base,
  ACBrUtil.FilesIO,
  ACBrUtil.DateTime,
  ACBreSocial;

{ TS2300Collection }

function TS2300Collection.Add: TS2300CollectionItem;
begin
  Result := Self.New;
end;

function TS2300Collection.GetItem(Index: Integer): TS2300CollectionItem;
begin
  Result := TS2300CollectionItem(inherited Items[Index]);
end;

procedure TS2300Collection.SetItem(Index: Integer; Value: TS2300CollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TS2300Collection.New: TS2300CollectionItem;
begin
  Result := TS2300CollectionItem.Create(FACBreSocial);
  Self.Add(Result);
end;

{ TS2300CollectionItem }

constructor TS2300CollectionItem.Create(AOwner: TComponent);
begin
  inherited Create;
  FTipoEvento   := teS2300;
  FEvtTSVInicio := TEvtTSVInicio.Create(AOwner);
end;

destructor TS2300CollectionItem.Destroy;
begin
  FEvtTSVInicio.Free;

  inherited;
end;

{ TinfoTSVInicio }

constructor TinfoTSVInicio.Create;
begin
  inherited;

  FinfoComplementares := TinfoComplementares.Create;
  Fafastamento        := TAfastamento.Create;
  FMudancaCPF         := TMudancaCPF2.Create;
  Ftermino            := TTermino.Create;
end;

destructor TinfoTSVInicio.Destroy;
begin
  FinfoComplementares.Free;
  Fafastamento.Free;
  FMudancaCPF.Free;
  Ftermino.Free;

  inherited;
end;

{ TinfoComplementares }

constructor TinfoComplementares.Create;
begin
  inherited;

  FcargoFuncao := TcargoFuncao.Create;
  FRemuneracao := TRemuneracao.Create;
  FFgts := TFGTS.Create;
  FinfoDirSind := TinfoDirSind.Create;
  FinfoTrabCedido := TinfoTrabCedido.Create;
  FinfoEstagiario := TinfoEstagiario.Create;
  FinfoMandElet := TinfoMandElet.Create;
  FlocalTrabGeral := TLocalTrabGeral.Create;
end;

destructor TinfoComplementares.Destroy;
begin
  FcargoFuncao.Free;
  FRemuneracao.Free;
  FFgts.Free;
  FinfoDirSind.Free;
  FinfoTrabCedido.Free;
  FinfoEstagiario.Free;
  FinfoMandElet.Free;
  FLocalTrabGeral.Free;

  inherited;
end;

{ TEvtTSVInicio }

constructor TEvtTSVInicio.Create(AACBreSocial: TObject);
begin
  inherited Create(AACBreSocial);

  FIdeEvento     := TIdeEvento2.Create;
  FIdeEmpregador := TIdeEmpregador.Create;
  FTrabalhador   := TTrabalhador.Create;
  FinfoTSVInicio := TinfoTSVInicio.Create;
end;

destructor TEvtTSVInicio.Destroy;
begin
  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FTrabalhador.Free;
  FinfoTSVInicio.Free;

  inherited;
end;

procedure TEvtTSVInicio.GerarageIntegracao(obj: TageIntegracao);
begin
  if obj.cnpjAgntInteg <> EmptyStr then
  begin
	Gerador.wGrupo('ageIntegracao');

    Gerador.wCampo(tcStr, '', 'cnpjAgntInteg', 14,  14, 1, obj.cnpjAgntInteg);

    if VersaoDF <= ve02_05_00 then
    begin
      Gerador.wCampo(tcStr, '', 'nmRazao',        1, 100, 1, obj.nmRazao);
      Gerador.wCampo(tcStr, '', 'dscLograd',      1,  80, 1, obj.dscLograd);
      Gerador.wCampo(tcStr, '', 'nrLograd',       1,  10, 1, obj.nrLograd);
      Gerador.wCampo(tcStr, '', 'bairro',         1,  60, 0, obj.bairro);
      Gerador.wCampo(tcStr, '', 'cep',            1,   8, 1, obj.cep);
      Gerador.wCampo(tcStr, '', 'codMunic',       7,   7, 0, obj.codMunic);
      Gerador.wCampo(tcStr, '', 'uf',             2,   2, 1, obj.uf);
    end;
    
	Gerador.wGrupo('/ageIntegracao');
  end;
end;

procedure TEvtTSVInicio.GerarCargoFuncao(obj: TcargoFuncao);
begin
  if (obj.codCargo <> EmptyStr) or 
     (obj.nmCargo <> EmptyStr) or
     (obj.CBOCargo <> EmptyStr) or
     (obj.nmFuncao <> EmptyStr) or
     (obj.CBOFuncao <> EmptyStr) then
  begin
    Gerador.wGrupo('cargoFuncao');

    if VersaoDF <= ve02_05_00 then
    begin
      Gerador.wCampo(tcStr, '', 'codCargo',  1, 30, 1, obj.codCargo);
      Gerador.wCampo(tcStr, '', 'codFuncao', 1, 30, 0, obj.codFuncao);
    end
    else
    begin
      Gerador.wCampo(tcStr, '', 'nmCargo',     0, 100, 0, obj.nmCargo);
      Gerador.wCampo(tcStr, '', 'CBOCargo',    0,   6, 0, obj.CBOCargo);
      Gerador.wCampo(tcStr, '', 'nmFuncao',    0, 100, 0, obj.nmFuncao);
      Gerador.wCampo(tcStr, '', 'CBOFuncao',   0,   6, 0, obj.CBOFuncao);
    end;
    
    Gerador.wGrupo('/cargoFuncao');
  end;
end;

procedure TEvtTSVInicio.GerarFGTS(obj: TFGTS);
begin
  if obj.dtOpcFGTS > 0 then
  begin
    if VersaoDF <= ve02_05_00 then
      Gerador.wGrupo('fgts')
    else
      Gerador.wGrupo('FGTS');

    if VersaoDF <= ve02_05_00 then
      Gerador.wCampo(tcStr, '', 'opcFGTS',    1,  1, 1, eSOpcFGTSToStr(obj.OpcFGTS));

    //if VersaoDF <= ve02_05_00 then
      Gerador.wCampo(tcDat, '', 'dtOpcFGTS', 10, 10, 0, obj.dtOpcFGTS);

    if VersaoDF <= ve02_05_00 then
      Gerador.wGrupo('/fgts')
    else
      Gerador.wGrupo('/FGTS');
  end;
end;

procedure TEvtTSVInicio.GerarInfoComplementares(obj: TinfoComplementares);
begin
  if ((obj.cargoFuncao.codCargo <> EmptyStr) and (VersaoDF <= ve02_05_00)) or 
     ((obj.cargoFuncao.nmCargo <> EmptyStr) and (VersaoDF > ve02_05_00)) or
     ((obj.cargoFuncao.CBOCargo <> EmptyStr) and (VersaoDF > ve02_05_00)) or
     ((obj.cargoFuncao.nmFuncao <> EmptyStr) and (VersaoDF > ve02_05_00))or
     ((obj.cargoFuncao.CBOFuncao <> EmptyStr) and (VersaoDF > ve02_05_00)) or
     (obj.Remuneracao.VrSalFx > 0) or (obj.Remuneracao.UndSalFixo = sfNaoaplicavel) or
     (obj.FGTS.DtOpcFGTS > 0) or (obj.infoDirSind.categOrig > 0) or
     (obj.infoTrabCedido.dtAdmCed > 0) or (obj.infoEstagiario.dtPrevTerm > 0) or
     (obj.localTrabGeral.nrInsc <> '') then
  begin
    Gerador.wGrupo('infoComplementares');
    
    GerarCargoFuncao(obj.cargoFuncao);
    GerarRemuneracao(obj.Remuneracao);
    GerarFGTS(obj.FGTS);
    GerarinfoDirSind(obj.infoDirSind);
    GerarinfoTrabCedido(obj.infoTrabCedido);
    
    if VersaoDF > ve02_05_00 then
      GerarInfoMandElet(obj.infoMandElet);
      
    GerarinfoEstagiario(obj.infoEstagiario);

    if VersaoDF >= veS01_02_00 then
      GerarLocalTrabGeral(obj.localTrabGeral);

    Gerador.wGrupo('/infoComplementares');
  end;
end;

procedure TEvtTSVInicio.GerarinfoDirSind(obj: TinfoDirSind);
begin
  if obj.categOrig > 0 then
  begin
    Gerador.wGrupo('infoDirigenteSindical');

    Gerador.wCampo(tcStr, '', 'categOrig',     1,  3, 1, obj.categOrig);
    
    if VersaoDF <= ve02_05_00 then
      Gerador.wCampo(tcStr, '', 'cnpjOrigem', 14, 14, 0, obj.nrInsc)
    else 
    if obj.nrInsc <> EmptyStr then
    begin
      Gerador.wCampo(tcStr, '', 'tpInsc',      1,  1, 1, eSTpInscricaoToStr(obj.tpInsc));
      Gerador.wCampo(tcStr, '', 'nrInsc',     14, 14, 1, obj.nrInsc)
    end;
    
    Gerador.wCampo(tcDat, '', 'dtAdmOrig',    10, 10, 0, obj.dtAdmOrig);
    Gerador.wCampo(tcStr, '', 'matricOrig',    1, 30, 0, obj.matricOrig);
    
    if VersaoDF > ve02_05_00 then
    begin
      //preenchimento é obrigatório e exclusivo se infoDirigenteSindical/categOrig corresponder a "Empregado" ou "Agente Público"
      case obj.categOrig of
        101..199, 301..399 : Gerador.wCampo(tcStr, '', 'tpRegTrab',   1,  1, 0, eSTpRegTrabToStr(obj.tpRegTrab));
      end;
      Gerador.wCampo(tcStr, '', 'tpRegPrev',   1,  1, 1, eSTpRegPrevToStr(obj.tpRegPrev));
    end;

    Gerador.wGrupo('/infoDirigenteSindical');
  end;
end;

procedure TEvtTSVInicio.GerarinfoEstagiario(obj: TinfoEstagiario);
begin
  if obj.dtPrevTerm > 0 then
  begin
    Gerador.wGrupo('infoEstagiario');

    Gerador.wCampo(tcStr, '', 'natEstagio',   1,  1, 1, eSTpNatEstagioToStr(obj.natEstagio));
    Gerador.wCampo(tcStr, '', 'nivEstagio',   1,  1, 1, eStpNivelEstagioToStr(obj.nivEstagio));
    Gerador.wCampo(tcStr, '', 'areaAtuacao',  1, 50, 0, obj.areaAtuacao);
    Gerador.wCampo(tcStr, '', 'nrApol',       1, 30, 0, obj.nrApol);

    if VersaoDF <= ve02_05_00 then
      Gerador.wCampo(tcDe2, '', 'vlrBolsa',   1, 14, 0, obj.vlrBolsa);
      
    Gerador.wCampo(tcDat, '', 'dtPrevTerm',  10, 10, 1, obj.dtPrevTerm);

    GerarInstEnsino(obj.instEnsino);
    GerarageIntegracao(obj.ageIntegracao);
    GerarsupervisorEstagio(obj.supervisorEstagio);

    Gerador.wGrupo('/infoEstagiario');
  end;
end;

procedure TEvtTSVInicio.GerarinfoTrabCedido(obj: TinfoTrabCedido);
begin
  if obj.dtAdmCed > 0 then
  begin
    Gerador.wGrupo('infoTrabCedido');

    Gerador.wCampo(tcStr, '', 'categOrig',  1,  3, 1, obj.categOrig);
    Gerador.wCampo(tcStr, '', 'cnpjCednt', 14, 14, 1, obj.cnpjCednt);
    Gerador.wCampo(tcStr, '', 'matricCed',  1, 30, 1, obj.matricCed);
    Gerador.wCampo(tcDat, '', 'dtAdmCed',  10, 10, 1, obj.dtAdmCed);
    Gerador.wCampo(tcInt, '', 'tpRegTrab',  1,  1, 1, eSTpRegTrabToStr(obj.tpRegTrab));
    Gerador.wCampo(tcInt, '', 'tpRegPrev',  1,  1, 1, eSTpRegPrevToStr(obj.tpRegPrev));
    
    if VersaoDF <= ve02_05_00 then
      Gerador.wCampo(tcStr, '', 'infOnus',    1,  1, 1, tpInfOnusToStr(obj.infOnus));

    Gerador.wGrupo('/infoTrabCedido');
  end;
end;

procedure TEvtTSVInicio.GerarInfoTSVInicio(obj: TinfoTSVInicio);
begin
  Gerador.wGrupo('infoTSVInicio');

  Gerador.wCampo(tcStr, '', 'cadIni',        1,  1, 1, eSSimNaoToStr(obj.cadIni));

  if VersaoDF > ve02_05_00 then
    Gerador.wCampo(tcStr, '', 'matricula',   1, 30, 0, obj.matricula);

  Gerador.wCampo(tcStr, '', 'codCateg',      0,  3, 1, obj.codCateg);
  Gerador.wCampo(tcDat, '', 'dtInicio',     10, 10, 1, obj.dtInicio);

  if VersaoDF > ve02_05_00 then
    Gerador.wCampo(tcStr, '', 'nrProcTrab',   1, 20, 0, obj.nrProcTrab);

  //    Validação: **Preenchimento obrigatório** para as categorias de avulso, cooperado e dirigente sindical.
  //               Não deve ser preenchido para as categorias Diretor não empregado, servidor público indicado a conselho, membro de conselho tutelar e estagiário.

  if
    (obj.codCateg <> 721) and // Diretor não empregado com FGTS
    (obj.codCateg <> 722) and // Diretor não empregado sem FGTS
    (obj.codCateg <> 305) and // Servidor Publico Indicado a Conselho
    (obj.codCateg <> 771) and // Membro conselho tutelar
    (obj.codCateg <> 901) and // Estagiario
    (obj.natAtividade <> navNaoInformar) then
    Gerador.wCampo(tcStr, '', 'natAtividade',  1,  1, 0, eSNatAtividadeToStr(obj.natAtividade));

  GerarInfoComplementares(obj.InfoComplementares);

  if (obj.cadIni <> tpSim) then
    GerarMudancaCPF2(obj.mudancaCPF);

  GerarAfastamento(obj.afastamento);
  GerarTermino(obj.termino);

  Gerador.wGrupo('/infoTSVInicio');
end;

procedure TEvtTSVInicio.GerarInstEnsino(obj: TinstEnsino);
begin
  Gerador.wGrupo('instEnsino');

  Gerador.wCampo(tcStr, '', 'cnpjInstEnsino', 14,  14, 0, obj.cnpjInstEnsino);

  if VersaoDF <= ve02_05_00 then
  begin
    Gerador.wCampo(tcStr, '', 'nmRazao',         1, 100, 1, obj.nmRazao);
    Gerador.wCampo(tcStr, '', 'dscLograd',       1,  80, 0, obj.dscLograd);
    Gerador.wCampo(tcStr, '', 'nrLograd',        1,  10, 0, obj.nrLograd);
    Gerador.wCampo(tcStr, '', 'bairro',          1,  60, 0, obj.bairro);
    Gerador.wCampo(tcStr, '', 'cep',             1,   8, 0, obj.cep);
    Gerador.wCampo(tcInt, '', 'codMunic',        7,   7, 0, obj.codMunic);
    Gerador.wCampo(tcStr, '', 'uf',              2,   2, 0, obj.uf);
  end;
  
  Gerador.wGrupo('/instEnsino');
end;

procedure TEvtTSVInicio.GerarRemuneracao(obj: TRemuneracao);
begin
  if (obj.vrSalFx > 0) or (obj.UndSalFixo = sfNaoaplicavel) then
  begin
    Gerador.wGrupo('remuneracao');

    Gerador.wCampo(tcDe2, '', 'vrSalFx',    1,  14, 1, obj.vrSalFx);
    Gerador.wCampo(tcStr, '', 'undSalFixo', 1,   1, 1, eSUndSalFixoToStr(obj.undSalFixo));
    Gerador.wCampo(tcStr, '', 'dscSalVar',  1, 255, 0, obj.dscSalVar);

    Gerador.wGrupo('/remuneracao');
  end;
end;

procedure TEvtTSVInicio.GerarsupervisorEstagio(obj: TsupervisorEstagio);
begin
  if obj.cpfSupervisor <> EmptyStr then
  begin
    Gerador.wGrupo('supervisorEstagio');

    Gerador.wCampo(tcStr, '', 'cpfSupervisor', 11, 11, 1, obj.cpfSupervisor);
    
    if VersaoDF <= ve02_05_00 then
      Gerador.wCampo(tcStr, '', 'nmSuperv',     1, 70, 1, obj.nmSuperv);

    Gerador.wGrupo('/supervisorEstagio');
  end;
end;

procedure TEvtTSVInicio.GerarTermino(obj: TTermino);
begin
  if obj.dtTerm > 0 then
  begin
    Gerador.wGrupo('termino');

    Gerador.wCampo(tcDat, '', 'dtTerm', 10, 10, 1, obj.dtTerm);

    Gerador.wGrupo('/termino');
  end;
end;

procedure TEvtTSVInicio.GerarInfoMandElet(obj: TinfoMandElet);
begin
  if (self.FinfoTSVInicio.FcodCateg = 304) then
  begin
    Gerador.wGrupo('infoMandElet');
    Gerador.wCampo(tcStr, '', 'categOrig',      0,  3, 1, obj.categOrig);
    Gerador.wCampo(tcStr, '', 'cnpjOrig',      14, 14, 1, obj.cnpjOrig);
    Gerador.wCampo(tcStr, '', 'matricOrig',     0, 30, 1, obj.matricOrig);
    Gerador.wCampo(tcDat, '', 'dtExercOrig',   10, 10, 1, obj.dtExercOrig);
    Gerador.wCampo(tcStr, '', 'indRemunCargo',  1,  1, 0, eSSimNaoFacultativoToStr(obj.indRemunCargo));
    Gerador.wCampo(tcStr, '', 'tpRegTrab',      1,  1, 1, eSTpRegTrabToStr(obj.tpRegTrab));
    Gerador.wCampo(tcStr, '', 'tpRegPrev',      1,  1, 1, eSTpRegPrevToStr(obj.tpRegPrev));
    Gerador.wGrupo('/infoMandElet');
  end;
end;

function TEvtTSVInicio.GerarXML: boolean;
begin
  try
    inherited GerarXML;
    Self.VersaoDF := TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF;
     
    Self.Id := GerarChaveEsocial(now, self.ideEmpregador.NrInsc, self.Sequencial);

    GerarCabecalho('evtTSVInicio');
    Gerador.wGrupo('evtTSVInicio Id="' + Self.Id + '"');

    GerarIdeEvento2(self.IdeEvento);
    GerarIdeEmpregador(self.IdeEmpregador);

    GerarTrabalhador(self.Trabalhador, Self.infoTSVInicio.cadIni,
      'trabalhador', 3, Self.FinfoTSVInicio.codCateg);
    GerarInfoTSVInicio(self.infoTSVInicio);

    Gerador.wGrupo('/evtTSVInicio');

    GerarRodape;

    FXML := Gerador.ArquivoFormatoXML;
//    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtTSVInicio');
//    Validar(schevtTSVInicio);
  except on e:exception do
    raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '');
end;

function TEvtTSVInicio.LerArqIni(const AIniString: String): Boolean;
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
      sSecao := 'evtTSVInicio';
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

      sSecao := 'trabalhador';
      trabalhador.CpfTrab    := INIRec.ReadString(sSecao, 'cpfTrab', EmptyStr);
      trabalhador.NisTrab    := INIRec.ReadString(sSecao, 'nisTrab', EmptyStr);
      trabalhador.NmTrab     := INIRec.ReadString(sSecao, 'nmTrab', EmptyStr);
      trabalhador.Sexo       := INIRec.ReadString(sSecao, 'sexo', EmptyStr);
      trabalhador.RacaCor    := INIRec.ReadInteger(sSecao, 'racaCor', 1);
      trabalhador.EstCiv     := INIRec.ReadInteger(sSecao, 'estCiv', 1);
      trabalhador.GrauInstr  := INIRec.ReadString(sSecao, 'grauInstr', '01');
      trabalhador.nmSoc      := INIRec.ReadString(sSecao, 'nmSoc', EmptyStr);

      sSecao := 'nascimento';
      trabalhador.Nascimento.dtNascto   := StringToDateTime(INIRec.ReadString(sSecao, 'dtNascto', '0'));
      trabalhador.Nascimento.codMunic   := INIRec.ReadInteger(sSecao, 'codMunic', 0);
      trabalhador.Nascimento.UF         := INIRec.ReadString(sSecao, 'uf', 'SP');
      trabalhador.Nascimento.PaisNascto := INIRec.ReadString(sSecao, 'paisNascto', '');
      trabalhador.Nascimento.PaisNac    := INIRec.ReadString(sSecao, 'paisNac', '');
      trabalhador.Nascimento.NmMae      := INIRec.ReadString(sSecao, 'nmMae', '');
      trabalhador.Nascimento.NmPai      := INIRec.ReadString(sSecao, 'nmPai', '');

      sSecao := 'CTPS';
      if INIRec.ReadString(sSecao, 'nrCtps', '') <> '' then
      begin
        trabalhador.documentos.CTPS.NrCtps    := INIRec.ReadString(sSecao, 'nrCtps', '');
        trabalhador.documentos.CTPS.SerieCtps := INIRec.ReadString(sSecao, 'serieCtps', '');
        trabalhador.documentos.CTPS.UfCtps    := INIRec.ReadString(sSecao, 'ufCtps', 'SP');
      end;

      sSecao := 'RIC';
      if INIRec.ReadString(sSecao, 'nrRic', '') <> '' then
      begin
        trabalhador.documentos.RIC.NrRic        := INIRec.ReadString(sSecao, 'nrRic', '');
        trabalhador.documentos.RIC.OrgaoEmissor := INIRec.ReadString(sSecao, 'orgaoEmissor', '');
        trabalhador.documentos.RIC.DtExped      := StringToDateTime(INIRec.ReadString(sSecao, 'dtExped', '0'));
      end;

      sSecao := 'RG';
      if INIRec.ReadString(sSecao, 'nrRg', '') <> '' then
      begin
        trabalhador.documentos.rg.NrRg         := INIRec.ReadString(sSecao, 'nrRg', '');
        trabalhador.documentos.rg.OrgaoEmissor := INIRec.ReadString(sSecao, 'orgaoEmissor', '');
        trabalhador.documentos.rg.DtExped      := StringToDateTime(INIRec.ReadString(sSecao, 'dtExped', '0'));
      end;

      sSecao := 'RNE';
      if INIRec.ReadString(sSecao, 'nrRne', '') <> '' then
      begin
        trabalhador.documentos.RNE.NrRne        := INIRec.ReadString(sSecao, 'nrRne', '');
        trabalhador.documentos.RNE.OrgaoEmissor := INIRec.ReadString(sSecao, 'orgaoEmissor', '');
        trabalhador.documentos.RNE.DtExped      := StringToDateTime(INIRec.ReadString(sSecao, 'dtExped', '0'));
      end;

      sSecao := 'OC';
      if INIRec.ReadString(sSecao, 'nrOc', '') <> '' then
      begin
        trabalhador.documentos.OC.NrOc         := INIRec.ReadString(sSecao, 'nrOc', '');
        trabalhador.documentos.OC.OrgaoEmissor := INIRec.ReadString(sSecao, 'orgaoEmissor', '');
        trabalhador.documentos.OC.DtExped      := StringToDateTime(INIRec.ReadString(sSecao, 'dtExped', '0'));
        trabalhador.documentos.OC.DtValid      := StringToDateTime(INIRec.ReadString(sSecao, 'dtValid', '0'));
      end;

      sSecao := 'CNH';
      if INIRec.ReadString(sSecao, 'nrRegCnh', '') <> '' then
      begin
        trabalhador.documentos.CNH.nrRegCnh     := INIRec.ReadString(sSecao, 'nrRegCnh', '');
        trabalhador.documentos.CNH.DtExped      := StringToDateTime(INIRec.ReadString(sSecao, 'dtExped', '0'));
        trabalhador.documentos.CNH.ufCnh        := INIRec.ReadString(sSecao, 'ufCnh', 'SP');
        trabalhador.documentos.CNH.DtValid      := StringToDateTime(INIRec.ReadString(sSecao, 'dtValid', '0'));
        trabalhador.documentos.CNH.dtPriHab     := StringToDateTime(INIRec.ReadString(sSecao, 'dtPriHab', '0'));
        trabalhador.documentos.CNH.categoriaCnh := eSStrToCnh(Ok, INIRec.ReadString(sSecao, 'categoriaCnh', 'A'));
      end;

      sSecao := 'enderecoBrasil';
      if INIRec.ReadString(sSecao, 'tpLograd', '') <> '' then
      begin
        trabalhador.Endereco.Brasil.TpLograd    := INIRec.ReadString(sSecao, 'tpLograd', '');
        trabalhador.Endereco.Brasil.DscLograd   := INIRec.ReadString(sSecao, 'dscLograd', '');
        trabalhador.Endereco.Brasil.NrLograd    := INIRec.ReadString(sSecao, 'nrLograd', '');
        trabalhador.Endereco.Brasil.Complemento := INIRec.ReadString(sSecao, 'complemento', '');
        trabalhador.Endereco.Brasil.Bairro      := INIRec.ReadString(sSecao, 'bairro', '');
        trabalhador.Endereco.Brasil.Cep         := INIRec.ReadString(sSecao, 'cep', '');
        trabalhador.Endereco.Brasil.CodMunic    := INIRec.ReadInteger(sSecao, 'codMunic', 0);
        trabalhador.Endereco.Brasil.UF          := INIRec.ReadString(sSecao, 'uf', 'SP');
      end;

      sSecao := 'enderecoExterior';
      if INIRec.ReadString(sSecao, 'paisResid', '') <> '' then
      begin
        trabalhador.Endereco.Exterior.PaisResid   := INIRec.ReadString(sSecao, 'paisResid', '');
        trabalhador.Endereco.Exterior.DscLograd   := INIRec.ReadString(sSecao, 'dscLograd', '');
        trabalhador.Endereco.Exterior.NrLograd    := INIRec.ReadString(sSecao, 'nrLograd', '');
        trabalhador.Endereco.Exterior.Complemento := INIRec.ReadString(sSecao, 'complemento', '');
        trabalhador.Endereco.Exterior.Bairro      := INIRec.ReadString(sSecao, 'bairro', '');
        trabalhador.Endereco.Exterior.NmCid       := INIRec.ReadString(sSecao, 'nmCid', '');
        trabalhador.Endereco.Exterior.CodPostal   := INIRec.ReadString(sSecao, 'codPostal', '');
      end;

      sSecao := 'trabImig';
      trabalhador.trabImig.tmpResid := StrTotpTmpResid(Ok, INIRec.ReadString(sSecao, 'tmpResid', EmptyStr));
      trabalhador.trabImig.condIng  := StrTotpCondIng(Ok, INIRec.ReadString(sSecao, 'condIng', EmptyStr));

      sSecao := 'trabEstrangeiro';
      if INIRec.ReadString(sSecao, 'dtChegada', '') <> '' then
      begin
        trabalhador.TrabEstrangeiro.DtChegada        := StringToDateTime(INIRec.ReadString(sSecao, 'dtChegada', '0'));
        trabalhador.TrabEstrangeiro.ClassTrabEstrang := eSStrToClassTrabEstrang(Ok, INIRec.ReadString(sSecao, 'classTrabEstrang', '1'));
        trabalhador.TrabEstrangeiro.CasadoBr         := INIRec.ReadString(sSecao, 'casadoBr', 'S');
        trabalhador.TrabEstrangeiro.FilhosBr         := INIRec.ReadString(sSecao, 'filhosBr', 'S');
      end;

      sSecao := 'infoDeficiencia';
      if INIRec.ReadString(sSecao, 'defFisica', '') <> '' then
      begin
        trabalhador.infoDeficiencia.DefFisica      := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'defFisica', 'S'));
        trabalhador.infoDeficiencia.DefVisual      := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'defVisual', 'S'));
        trabalhador.infoDeficiencia.DefAuditiva    := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'defAuditiva', 'S'));
        trabalhador.infoDeficiencia.DefMental      := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'defMental', 'S'));
        trabalhador.infoDeficiencia.DefIntelectual := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'defIntelectual', 'S'));
        trabalhador.infoDeficiencia.ReabReadap     := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'reabReadap', 'S'));
        trabalhador.infoDeficiencia.Observacao     := INIRec.ReadString(sSecao, 'observacao', '');
      end;

      I := 1;
      while true do
      begin
        // de 00 até 99
        sSecao := 'dependente' + IntToStrZero(I, 2);
        sFim   := INIRec.ReadString(sSecao, 'nmDep', 'FIM');

        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break;

        with trabalhador.Dependente.New do
        begin
          tpDep    := eSStrToTpDep(Ok, INIRec.ReadString(sSecao, 'tpDep', ''));
          nmDep    := sFim;
          dtNascto := StringToDateTime(INIRec.ReadString(sSecao, 'dtNascto', '0'));
          cpfDep   := INIRec.ReadString(sSecao, 'cpfDep', '');
          sexoDep  := INIRec.ReadString(sSecao, 'sexoDep', '');
          depIRRF  := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'depIRRF', 'S'));
          depSF    := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'depSF', 'S'));
          incTrab  := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'incTrab', 'S'));
          descrDep := INIRec.ReadString(sSecao, 'descrDep', '');
        end;

        Inc(I);
      end;

      sSecao := 'contato';
      if INIRec.ReadString(sSecao, 'fonePrinc', '') <> '' then
      begin
        trabalhador.contato.FonePrinc     := INIRec.ReadString(sSecao, 'fonePrinc', '');
        trabalhador.contato.FoneAlternat  := INIRec.ReadString(sSecao, 'foneAlternat', 'S');
        trabalhador.contato.EmailPrinc    := INIRec.ReadString(sSecao, 'emailPrinc', 'S');
        trabalhador.contato.EmailAlternat := INIRec.ReadString(sSecao, 'emailAlternat', 'S');
      end;

      sSecao := 'infoTSVInicio';
      infoTSVInicio.cadIni         := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'cadIni', 'S'));
      infoTSVInicio.codCateg       := INIRec.ReadInteger(sSecao, 'codCateg', 0);
      infoTSVInicio.dtInicio       := StringToDateTime(INIRec.ReadString(sSecao, 'dtInicio', '0'));
      infoTSVInicio.nrProcTrab     := INIRec.ReadString(sSecao, 'nrProcTrab', EmptyStr);
      infoTSVInicio.natAtividade   := eSStrToNatAtividade(Ok, INIRec.ReadString(sSecao, 'natAtividade', '1'));
      infoTSVInicio.matricula      := INIRec.ReadString(sSecao, 'matricula', EmptyStr);

      sSecao := 'cargoFuncao';
      infoTSVInicio.infoComplementares.cargoFuncao.CodCargo    := INIRec.ReadString(sSecao, 'codCargo', '');
      infoTSVInicio.infoComplementares.cargoFuncao.CodFuncao   := INIRec.ReadString(sSecao, 'codFuncao', '');
      infoTSVInicio.infoComplementares.cargoFuncao.nmCargo     := INIRec.ReadString(sSecao, 'nmCargo', '');
      infoTSVInicio.infoComplementares.cargoFuncao.CBOCargo    := INIRec.ReadString(sSecao, 'CBOCargo', '');
      infoTSVInicio.infoComplementares.cargoFuncao.nmFuncao    := INIRec.ReadString(sSecao, 'nmFuncao', '');
      infoTSVInicio.infoComplementares.cargoFuncao.CBOFuncao   := INIRec.ReadString(sSecao, 'CBOFuncao', '');

      sSecao := 'remuneracao';
      if INIRec.ReadString(sSecao, 'vrSalFx', '') <> '' then
      begin
        infoTSVInicio.infoComplementares.remuneracao.VrSalFx    := StringToFloatDef(INIRec.ReadString(sSecao, 'vrSalFx', ''), 0);
        infoTSVInicio.infoComplementares.remuneracao.UndSalFixo := eSStrToUndSalFixo(Ok, INIRec.ReadString(sSecao, 'undSalFixo', ''));
        infoTSVInicio.infoComplementares.remuneracao.DscSalVar  := INIRec.ReadString(sSecao, 'dscSalVar', '');
      end;

      sSecao := 'FGTS';
      if INIRec.ReadString(sSecao, 'opcFGTS', '') <> '' then
      begin
        infoTSVInicio.infoComplementares.FGTS.OpcFGTS   := eSStrToOpcFGTS(Ok, INIRec.ReadString(sSecao, 'opcFGTS', '1'));
        infoTSVInicio.infoComplementares.FGTS.DtOpcFGTS := StringToDateTime(INIRec.ReadString(sSecao, 'dtOpcFGTS', '0'));
      end;
      infoTSVInicio.infoComplementares.FGTS.DtOpcFGTS := StringToDateTime(INIRec.ReadString(sSecao, 'dtOpcFGTS', '0'));

      sSecao := 'infoDirigenteSindical';
      if INIRec.ReadString(sSecao, 'categOrig', '') <> '' then
      begin
        infoTSVInicio.infoComplementares.infoDirSind.categOrig  := INIRec.ReadInteger(sSecao, 'categOrig', 1);
        infoTSVInicio.infoComplementares.infoDirSind.cnpjOrigem := INIRec.ReadString(sSecao, 'cnpjOrigem', '');
        infoTSVInicio.infoComplementares.infoDirSind.dtAdmOrig  := StringToDateTime(INIRec.ReadString(sSecao, 'dtAdmOrig', '0'));
        infoTSVInicio.infoComplementares.infoDirSind.matricOrig := INIRec.ReadString(sSecao, 'matricOrig', '');

        infoTSVInicio.infoComplementares.infoDirSind.tpInsc     := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
        infoTSVInicio.infoComplementares.infoDirSind.nrInsc     := INIRec.ReadString(sSecao, 'nrInsc', '');
        infoTSVInicio.infoComplementares.infoDirSind.tpRegTrab  := eSStrtoTpRegTrab(Ok, INIRec.ReadString(sSecao, 'tpTpRegTrab', '0'));
        infoTSVInicio.infoComplementares.infoDirSind.tpRegPrev  := eSStrtoTpRegPrev(Ok, INIRec.ReadString(sSecao, 'tpTpRegPrev', '0'));
      end;

      sSecao := 'infoTrabCedido';
      if INIRec.ReadString(sSecao, 'categOrig', '') <> '' then
      begin
        infoTSVInicio.infoComplementares.infoTrabCedido.categOrig := INIRec.ReadInteger(sSecao, 'categOrig', 1);
        infoTSVInicio.infoComplementares.infoTrabCedido.cnpjCednt := INIRec.ReadString(sSecao, 'cnpjCednt', '');
        infoTSVInicio.infoComplementares.infoTrabCedido.matricCed := INIRec.ReadString(sSecao, 'matricCed', '');
        infoTSVInicio.infoComplementares.infoTrabCedido.dtAdmCed  := StringToDateTime(INIRec.ReadString(sSecao, 'dtAdmCed', '0'));
        infoTSVInicio.infoComplementares.infoTrabCedido.tpRegTrab := eSStrToTpRegTrab(Ok, INIRec.ReadString(sSecao, 'tpRegTrab', '1'));
        infoTSVInicio.infoComplementares.infoTrabCedido.tpRegPrev := eSStrTotpRegPrev(Ok, INIRec.ReadString(sSecao, 'tpRegPrev', '1'));
        infoTSVInicio.infoComplementares.infoTrabCedido.infOnus   := StrTotpInfOnus(Ok, INIRec.ReadString(sSecao, 'infOnus', '1'));
      end;

      sSecao := 'infoMandElet';
      if INIRec.ReadString( sSecao, 'categOrig', '' ) <> '' then
      begin
        infoTSVInicio.infoComplementares.infoMandElet.categOrig     := INIRec.ReadInteger( sSecao, 'categOrig', 1 );
        infoTSVInicio.infoComplementares.infoMandElet.cnpjOrig      := INIRec.ReadString( sSecao, 'cnpjOrig', '' );
        infoTSVInicio.infoComplementares.infoMandElet.matricOrig    := INIRec.ReadString( sSecao, 'matricOrig', '' );
        infoTSVInicio.infoComplementares.infoMandElet.dtExercOrig   := StringToDateTime( INIRec.ReadString( sSecao, 'dtExercOrig', '0' ) );
        infoTSVInicio.infoComplementares.infoMandElet.indRemunCargo := eSStrToSimNaoFacultativo(Ok, INIRec.ReadString(sSecao, 'indRemunCargo', '0'));
        infoTSVInicio.infoComplementares.infoMandElet.tpRegTrab     := eSStrToTpRegTrab(Ok, INIRec.ReadString(sSecao, 'tpRegTrab', '0'));
        infoTSVInicio.infoComplementares.infoMandElet.tpRegPrev     := eSStrToTpRegPrev(Ok, INIRec.ReadString(sSecao, 'tpTpRegPrev', '0'));
      end;

      sSecao := 'infoEstagiario';
      if INIRec.ReadString(sSecao, 'natEstagio', '') <> '' then
      begin
        infoTSVInicio.infoComplementares.infoEstagiario.natEstagio  := eSStrToTpNatEstagio(Ok, INIRec.ReadString(sSecao, 'natEstagio', 'O'));
        infoTSVInicio.infoComplementares.infoEstagiario.nivEstagio  := eSStrTotpNivelEstagio(Ok, INIRec.ReadString(sSecao, 'nivEstagio', '1'));
        infoTSVInicio.infoComplementares.infoEstagiario.areaAtuacao := INIRec.ReadString(sSecao, 'areaAtuacao', '');
        infoTSVInicio.infoComplementares.infoEstagiario.nrApol      := INIRec.ReadString(sSecao, 'nrApol', '');
        infoTSVInicio.infoComplementares.infoEstagiario.vlrBolsa    := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrBolsa', ''), 0);
        infoTSVInicio.infoComplementares.infoEstagiario.dtPrevTerm  := StringToDateTime(INIRec.ReadString(sSecao, 'dtPrevTerm', '0'));

        sSecao := 'instEnsino';
        infoTSVInicio.infoComplementares.infoEstagiario.instEnsino.cnpjInstEnsino := INIRec.ReadString(sSecao, 'cnpjInstEnsino', '');
        infoTSVInicio.infoComplementares.infoEstagiario.instEnsino.nmRazao        := INIRec.ReadString(sSecao, 'nmRazao', '');
        infoTSVInicio.infoComplementares.infoEstagiario.instEnsino.dscLograd      := INIRec.ReadString(sSecao, 'dscLograd', '');
        infoTSVInicio.infoComplementares.infoEstagiario.instEnsino.nrLograd       := INIRec.ReadString(sSecao, 'nrLograd', '');
        infoTSVInicio.infoComplementares.infoEstagiario.instEnsino.bairro         := INIRec.ReadString(sSecao, 'bairro', '');
        infoTSVInicio.infoComplementares.infoEstagiario.instEnsino.Cep            := INIRec.ReadString(sSecao, 'cep', '');
        infoTSVInicio.infoComplementares.infoEstagiario.instEnsino.codMunic       := INIRec.ReadInteger(sSecao, 'codMunic', 0);
        infoTSVInicio.infoComplementares.infoEstagiario.instEnsino.uf             := INIRec.ReadString(sSecao, 'uf', '');

        sSecao := 'ageIntegracao';
        if INIRec.ReadString(sSecao, 'cnpjAgntInteg', '') <> '' then
        begin
          infoTSVInicio.infoComplementares.infoEstagiario.ageIntegracao.cnpjAgntInteg := INIRec.ReadString(sSecao, 'cnpjAgntInteg', '');
          infoTSVInicio.infoComplementares.infoEstagiario.ageIntegracao.nmRazao       := INIRec.ReadString(sSecao, 'nmRazao', '');
          infoTSVInicio.infoComplementares.infoEstagiario.ageIntegracao.dscLograd     := INIRec.ReadString(sSecao, 'dscLograd', '');
          infoTSVInicio.infoComplementares.infoEstagiario.ageIntegracao.nrLograd      := INIRec.ReadString(sSecao, 'nrLograd', '');
          infoTSVInicio.infoComplementares.infoEstagiario.ageIntegracao.bairro        := INIRec.ReadString(sSecao, 'bairro', '');
          infoTSVInicio.infoComplementares.infoEstagiario.ageIntegracao.Cep           := INIRec.ReadString(sSecao, 'cep', '');
          infoTSVInicio.infoComplementares.infoEstagiario.ageIntegracao.codMunic      := INIRec.ReadInteger(sSecao, 'codMunic', 0);
          infoTSVInicio.infoComplementares.infoEstagiario.ageIntegracao.uf            := INIRec.ReadString(sSecao, 'uf', 'SP');
        end;

        sSecao := 'supervisorEstagio';
        if INIRec.ReadString(sSecao, 'cpfSupervisor', '') <> '' then
        begin
          infoTSVInicio.infoComplementares.infoEstagiario.supervisorEstagio.cpfSupervisor := INIRec.ReadString(sSecao, 'cpfSupervisor', '');
          infoTSVInicio.infoComplementares.infoEstagiario.supervisorEstagio.nmSuperv      := INIRec.ReadString(sSecao, 'nmSuperv', '');
        end;
      end;

      sSecao := 'localTrabGeral';
      if INIRec.ReadString(sSecao, 'tpInsc', '') <> '' then
      begin
        infoTSVInicio.infoComplementares.LocalTrabGeral.TpInsc   := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
        infoTSVInicio.infoComplementares.LocalTrabGeral.NrInsc   := INIRec.ReadString(sSecao, 'nrInsc', '');
        infoTSVInicio.infoComplementares.LocalTrabGeral.DescComp := INIRec.ReadString(sSecao, 'descComp', '');
      end;

      sSecao := 'mudancaCPF';
      if INIRec.ReadString(sSecao, 'cpfAnt', '') <> '' then
      begin
        infoTSVInicio.mudancaCPF.cpfAnt := INIRec.ReadString(sSecao, 'cpfAnt', EmptyStr);
        infoTSVInicio.mudancaCPF.matricAnt := INIRec.ReadString(sSecao, 'matricAnt', EmptyStr);
        infoTSVInicio.mudancaCPF.dtAltCPF := StringToDateTime(INIRec.ReadString(sSecao, 'dtAltCPF', '0'));
        infoTSVInicio.mudancaCPF.observacao := INIRec.ReadString(sSecao, 'observacao', EmptyStr);
      end;

      sSecao := 'afastamento';
      if INIRec.ReadString(sSecao, 'dtIniAfast', '') <> '' then
      begin
        infoTSVInicio.afastamento.DtIniAfast  := StringToDateTime(INIRec.ReadString(sSecao, 'dtIniAfast', '0'));
        infoTSVInicio.afastamento.codMotAfast := eSStrTotpMotivosAfastamento(Ok, INIRec.ReadString(sSecao, 'codMotAfast', '00'));
      end;

      sSecao := 'termino';
      if INIRec.ReadString(sSecao, 'dtTerm', '') <> '' then
        infoTSVInicio.termino.dtTerm  := StringToDateTime(INIRec.ReadString(sSecao, 'dtTerm', '0'));
    end;

    GerarXML;
    XML := FXML;
  finally
    INIRec.Free;
  end;
end;

end.
