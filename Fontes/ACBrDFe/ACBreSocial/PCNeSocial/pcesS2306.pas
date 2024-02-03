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

unit pcesS2306;

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
  TS2306CollectionItem = class;
  TEvtTSVAltContr = class;
  TinfoTSVAlteracao = class;
  TinfoComplementares = class;
  TinfoMandElet = class;
  TinfoDirSind = class;
  TinfoTrabCedido = class;
  
  TS2306Collection = class(TeSocialCollection)
  private
    function GetItem(Index: Integer): TS2306CollectionItem;
    procedure SetItem(Index: Integer; Value: TS2306CollectionItem);
  public
    function Add: TS2306CollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TS2306CollectionItem;
    property Items[Index: Integer]: TS2306CollectionItem read GetItem write SetItem; default;
  end;

  TS2306CollectionItem = class(TObject)
  private
    FTipoEvento: TTipoEvento;
    FEvtTSVAltContr: TEvtTSVAltContr;
  public
    constructor Create(AOwner: TComponent);
    destructor  Destroy; override;
    property TipoEvento: TTipoEvento read FTipoEvento;
    property EvtTSVAltContr: TEvtTSVAltContr read FEvtTSVAltContr write FEvtTSVAltContr;
  end;

  TEvtTSVAltContr = class(TeSocialEvento)
  private
    FIdeEvento: TIdeEvento2;
    FIdeEmpregador: TIdeEmpregador;
    FideTrabSemVinc: TideTrabSemVinc;
    FinfoTSVAlteracao : TinfoTSVAlteracao;

    procedure GerarideTrabSemVinc(obj : TideTrabSemVinc);
    procedure GerarInfoTSVAlteracao(obj: TinfoTSVAlteracao);
    procedure GerarinfoComplementares(obj: TinfoComplementares);
    procedure GerarinfoEstagiario(obj: TinfoEstagiario);
    procedure GerarInstEnsino(obj: TinstEnsino);
    procedure GerarageIntegracao(obj: TageIntegracao);
    procedure GerarsupervisorEstagio(obj: TsupervisorEstagio);
    procedure GerarcargoFuncao(obj: TcargoFuncao);
    procedure GerarRemuneracao(obj: TRemuneracao);
    procedure GerarInfoMandElet(obj: TinfoMandElet);
    procedure GerarinfoDirigenteSindical(obj: TinfoDirSind);
    procedure GerarinfoTrabCedido(obj: TinfoTrabCedido);
  public
    constructor Create(AACBreSocial: TObject); override;
    destructor  Destroy; override;

    function GerarXML: boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property IdeEvento: TIdeEvento2 read FIdeEvento write FIdeEvento;
    property IdeEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property IdeTrabSemVinc: TideTrabSemVinc read FideTrabSemVinc write FideTrabSemVinc;
    property infoTSVAlteracao : TinfoTSVAlteracao read FinfoTSVAlteracao write FinfoTSVAlteracao;
  end;

  TinfoTSVAlteracao = class(TObject)
  private
    FdtAlteracao : TDateTime;
    FnatAtividade: tpNatAtividade;
    FinfoComplementares : TinfoComplementares;
  public
    constructor Create;
    destructor  Destroy; override;

    property dtAlteracao : TDateTime read FdtAlteracao write FdtAlteracao;
    property natAtividade : tpNatAtividade read FnatAtividade write FnatAtividade;
    property infoComplementares : TinfoComplementares read FinfoComplementares write FinfoComplementares;
  end;

  TinfoComplementares = class(TObject)
  private
    FcargoFuncao : TcargoFuncao;
    FRemuneracao : TRemuneracao;
    FinfoEstagiario : TinfoEstagiario;
    FinfoMandElet : TinfoMandElet;
    FinfoDirSind : TinfoDirSind;
    FinfoTrabCedido : TinfoTrabCedido;
    FlocalTrabGeral: TLocalTrabGeral;
  public
    constructor Create;
    destructor  Destroy; override;

    property cargoFuncao : TcargoFuncao read FcargoFuncao write FcargoFuncao;
    property Remuneracao : TRemuneracao read FRemuneracao write FRemuneracao;
    property infoEstagiario : TinfoEstagiario read FinfoEstagiario write FinfoEstagiario;
    property infoMandElet : TinfoMandElet read FinfoMandElet write FinfoMandElet;
    property infoDirigenteSindical : TinfoDirSind read FinfoDirSind write FinfoDirSind;
    property infoTrabCedido : TinfoTrabCedido read FinfoTrabCedido write FinfoTrabCedido;
    property localTrabGeral: TLocalTrabGeral read FLocalTrabGeral write FLocalTrabGeral;
  end;

  TinfoMandElet = class(TObject)
  private
    FindRemunCargo: tpSimNaoFacultativo;
    FtpRegPrev: tpTpRegPrev;
  public
    property indRemunCargo: tpSimNaoFacultativo read FindRemunCargo write FindRemunCargo;
    property tpRegPrev: tpTpRegPrev read FtpRegPrev write FtpRegPrev;
  end;
  
  TinfoDirSind = class(TObject)
  private
   FtpRegPrev : tpTpRegPrevFacultativo;
  public
   property tpRegPrev: tpTpRegPrevFacultativo read FtpRegPrev write FtpRegPrev;
  end;
  
  TinfoTrabCedido = class(TObject)
  private
    FTpRegPrev: tpTpRegPrevFacultativo;
  public
    property tpRegPrev: tpTpRegPrevFacultativo read FTpRegPrev write FTpRegPrev;
  end;
  
implementation

uses
  IniFiles,
  ACBrUtil.Base,
  ACBrUtil.FilesIO,
  ACBrUtil.DateTime,
  ACBreSocial;

{ TS2306Collection }

function TS2306Collection.Add: TS2306CollectionItem;
begin
  Result := Self.New;
end;

function TS2306Collection.GetItem(Index: Integer): TS2306CollectionItem;
begin
  Result := TS2306CollectionItem(inherited Items[Index]);
end;

procedure TS2306Collection.SetItem(Index: Integer; Value: TS2306CollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TS2306Collection.New: TS2306CollectionItem;
begin
  Result := TS2306CollectionItem.Create(FACBreSocial);
  Self.Add(Result);
end;

{ TS2306CollectionItem }

constructor TS2306CollectionItem.Create(AOwner: TComponent);
begin
  inherited Create;
  FTipoEvento     := teS2306;
  FEvtTSVAltContr := TEvtTSVAltContr.Create(AOwner);
end;

destructor TS2306CollectionItem.Destroy;
begin
  FEvtTSVAltContr.Free;

  inherited;
end;

{ TinfoComplementares }

constructor TinfoComplementares.Create;
begin
  inherited;

  FcargoFuncao    := TcargoFuncao.Create;
  FRemuneracao    := TRemuneracao.Create;
  FinfoEstagiario := TinfoEstagiario.Create;
  FinfoMandElet   := TinfoMandElet.Create;
  FinfoDirSind    := TinfoDirSind.Create;
  FinfoTrabCedido := TinfoTrabCedido.Create;
  FlocalTrabGeral := TLocalTrabGeral.Create;
end;

destructor TinfoComplementares.Destroy;
begin
  FcargoFuncao.Free;
  FRemuneracao.Free;
  FinfoEstagiario.Free;
  FinfoMandElet.Free;
  FinfoDirSind.Free;
  FinfoTrabCedido.Free;
  FlocalTrabGeral.Free;

  inherited;
end;

{ TinfoTSVAlteracao }

constructor TinfoTSVAlteracao.Create;
begin
  inherited;

  FinfoComplementares := TinfoComplementares.Create;
end;

destructor TinfoTSVAlteracao.Destroy;
begin
  FinfoComplementares.Free;

  inherited;
end;

{ TEvtTSVAltContr }

constructor TEvtTSVAltContr.Create(AACBreSocial: TObject);
begin
  inherited Create(AACBreSocial);

  FIdeEvento        := TIdeEvento2.Create;
  FIdeEmpregador    := TIdeEmpregador.Create;
  FideTrabSemVinc   := TideTrabSemVinc.Create;
  FinfoTSVAlteracao := TinfoTSVAlteracao.Create;
end;

destructor TEvtTSVAltContr.Destroy;
begin
  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FIdeTrabSemVinc.Free;
  FinfoTSVAlteracao.Free;

  inherited;
end;

procedure TEvtTSVAltContr.GerarageIntegracao(obj: TageIntegracao);
begin
  if obj.cnpjAgntInteg <> EmptyStr then
  begin
    Gerador.wGrupo('ageIntegracao');

    Gerador.wCampo(tcStr, '', 'cnpjAgntInteg', 14,  14, 1, obj.cnpjAgntInteg);

    if (VersaoDF <= ve02_05_00) then
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

procedure TEvtTSVAltContr.GerarcargoFuncao(obj: TcargoFuncao);
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

procedure TEvtTSVAltContr.GerarideTrabSemVinc(obj: TideTrabSemVinc);
begin
  Gerador.wGrupo('ideTrabSemVinculo');

  Gerador.wCampo(tcStr, '', 'cpfTrab',    11, 11, 1, obj.cpfTrab);
  
  if VersaoDF <= ve02_05_00 then
    Gerador.wCampo(tcStr, '', 'nisTrab',   1, 11, 0, obj.nisTrab)
  else
    Gerador.wCampo(tcStr, '', 'matricula', 1, 30, 0, obj.matricula);
  
  if (VersaoDF <= ve02_05_00) or (obj.matricula = '') then
      Gerador.wCampo(tcStr, '', 'codCateg',    1,  3, 1, obj.codCateg);

  Gerador.wGrupo('/ideTrabSemVinculo');
end;

procedure TEvtTSVAltContr.GerarinfoComplementares(obj: TinfoComplementares);
begin
  if (obj.cargoFuncao.codCargo <> EmptyStr) or
     (obj.Remuneracao.VrSalFx > 0) or
     (obj.infoEstagiario.dtPrevTerm > 0) or
     (obj.localTrabGeral.nrInsc <> '') then
  begin
    Gerador.wGrupo('infoComplementares');

    GerarcargoFuncao(obj.cargoFuncao);
    GerarRemuneracao(obj.Remuneracao);
    
    if VersaoDF > ve02_05_00 then
    begin
      GerarInfoMandElet(obj.infoMandElet);
      GerarinfoDirigenteSindical(obj.infoDirigenteSindical);
      GerarinfoTrabCedido(obj.infoTrabCedido);
    end;
    
    GerarinfoEstagiario(obj.infoEstagiario);

    if VersaoDF >= veS01_02_00 then
      GerarLocalTrabGeral(obj.localTrabGeral);

    Gerador.wGrupo('/infoComplementares');
  end;
end;

procedure TEvtTSVAltContr.GerarinfoEstagiario(obj: TinfoEstagiario);
begin
  if obj.dtPrevTerm > 0 then
  begin
    Gerador.wGrupo('infoEstagiario');

    Gerador.wCampo(tcStr, '', 'natEstagio',   1,  1, 1, eSTpNatEstagioToStr(obj.natEstagio));
    Gerador.wCampo(tcStr, '', 'nivEstagio',   1,  1, 1, eStpNivelEstagioToStr(obj.nivEstagio));
    Gerador.wCampo(tcStr, '', 'areaAtuacao',  1, 50, 0, obj.areaAtuacao);
    Gerador.wCampo(tcStr, '', 'nrApol',       1, 30, 0, obj.nrApol);

    if VersaoDF <= ve02_05_00 then
      Gerador.wCampo(tcDe2, '', 'vlrBolsa',     1, 14, 0, obj.vlrBolsa);

    Gerador.wCampo(tcDat, '', 'dtPrevTerm',  10, 10, 1, obj.dtPrevTerm);

    GerarInstEnsino(obj.instEnsino);
    GerarageIntegracao(obj.ageIntegracao);
    GerarsupervisorEstagio(obj.supervisorEstagio);

    Gerador.wGrupo('/infoEstagiario');
  end;
end;

procedure TEvtTSVAltContr.GerarInfoTSVAlteracao(obj: TinfoTSVAlteracao);
begin
  Gerador.wGrupo('infoTSVAlteracao');

  Gerador.wCampo(tcDat, '', 'dtAlteracao',  10, 10, 1, obj.dtAlteracao);

  //    Validação: **Preenchimento obrigatório** para as categorias de avulso, cooperado e dirigente sindical.
  //               Não deve ser preenchido para as categorias Diretor não empregado, servidor público indicado a conselho, membro de conselho tutelar e estagiário.
  if
    (Self.FideTrabSemVinc.codCateg <> 305) and // Servidor Publico Indicado a Conselho
    (Self.FideTrabSemVinc.codCateg <> 721) and // Diretor não empregado com FGTS
    (Self.FideTrabSemVinc.codCateg <> 722) and // Diretor não empregado sem FGTS
    (Self.FideTrabSemVinc.codCateg <> 771) and // Membro conselho tutelar
    (Self.FideTrabSemVinc.codCateg <> 901) and // Estagiario
    (obj.natAtividade <> navNaoInformar)
  then
    Gerador.wCampo(tcStr, '', 'natAtividade',  1,  1, 0, eSNatAtividadeToStr(obj.natAtividade));

  GerarinfoComplementares(obj.infoComplementares);

  Gerador.wGrupo('/infoTSVAlteracao');
end;

procedure TEvtTSVAltContr.GerarInstEnsino(obj: TinstEnsino);
begin
  Gerador.wGrupo('instEnsino');

  Gerador.wCampo(tcStr, '', 'cnpjInstEnsino', 14,  14, 0, obj.cnpjInstEnsino);

  if (obj.cnpjInstEnsino = '') or (VersaoDF <= ve02_05_00) then   // Preenchendo somente se não tiver CNPJ
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

procedure TEvtTSVAltContr.GerarRemuneracao(obj: TRemuneracao);
begin
  if obj.vrSalFx > 0 then
  begin
    Gerador.wGrupo('remuneracao');

    Gerador.wCampo(tcDe2, '', 'vrSalFx',    1,  14, 1, obj.vrSalFx);
    Gerador.wCampo(tcStr, '', 'undSalFixo', 1,   1, 1, eSUndSalFixoToStr(obj.undSalFixo));
    Gerador.wCampo(tcStr, '', 'dscSalVar',  1, 255, 0, obj.dscSalVar);

    Gerador.wGrupo('/remuneracao');
  end;
end;

procedure TEvtTSVAltContr.GerarInfoMandElet(obj: TinfoMandElet);
begin
  if obj.indRemunCargo <> snfNada then
  begin
    Gerador.wGrupo('infoMandElet');
  
    Gerador.wCampo(tcStr, '', 'indRemunCargo',  1,  1,  0, eSSimNaoFacultativoToStr(obj.indRemunCargo));
    Gerador.wCampo(tcStr, '', 'tpRegPrev',      1,  1,  1, eSTpRegPrevToStr(obj.tpRegPrev));
  
    Gerador.wGrupo('/infoMandElet');
  end;  
end;

procedure TEvtTSVAltContr.GerarinfoTrabCedido(obj: TinfoTrabCedido);
begin
  if obj.tpRegPrev <> rpfNenhum then
  begin
    Gerador.wGrupo('infoTrabCedido');

    Gerador.wCampo(tcInt, '', 'tpRegPrev',      1,  1, 1, eSTpRegPrevFacultativoToStr(obj.tpRegPrev));

    Gerador.wGrupo('/infoTrabCedido');
  end;  
end;

procedure TEvtTSVAltContr.GerarinfoDirigenteSindical(obj: TinfoDirSind);
begin
  if obj.tpRegPrev <> rpfNenhum then
  begin
    Gerador.wGrupo('infoDirigenteSindical');

    Gerador.wCampo(tcStr, '', 'tpRegPrev',      1,  1,  1, eSTpRegPrevFacultativoToStr(obj.tpRegPrev));

    Gerador.wGrupo('/infoDirigenteSindical');
  end;
end;

procedure TEvtTSVAltContr.GerarsupervisorEstagio(obj: TsupervisorEstagio);
begin
  if obj.cpfSupervisor <> EmptyStr then
  begin
    Gerador.wGrupo('supervisorEstagio');

    Gerador.wCampo(tcStr, '', 'cpfSupervisor', 11, 11, 1, obj.cpfSupervisor);

    if VersaoDF <= ve02_05_00 then
      Gerador.wCampo(tcStr, '', 'nmSuperv',      1,  70, 1, obj.nmSuperv);

    Gerador.wGrupo('/supervisorEstagio');
  end;
end;

function TEvtTSVAltContr.GerarXML: boolean;
begin
  try
    inherited GerarXML;
    Self.VersaoDF := TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF;
     
    Self.Id := GerarChaveEsocial(now, self.ideEmpregador.NrInsc, self.Sequencial);

    GerarCabecalho('evtTSVAltContr');
    Gerador.wGrupo('evtTSVAltContr Id="' + Self.Id + '"');

    GerarIdeEvento2(self.IdeEvento);
    GerarIdeEmpregador(self.IdeEmpregador);
    GerarideTrabSemVinc(self.IdeTrabSemVinc);
    GerarInfoTSVAlteracao(self.infoTSVAlteracao);

    Gerador.wGrupo('/evtTSVAltContr');

    GerarRodape;

    FXML := Gerador.ArquivoFormatoXML;
//    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtTSVAltContr');

//    Validar(schevtTSVAltContr);
  except on e:exception do
    raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '')
end;

function TEvtTSVAltContr.LerArqIni(const AIniString: String): Boolean;
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
      sSecao := 'evtTSVAltContr';
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

      sSecao := 'ideTrabSemVinculo';
      ideTrabSemVinc.CpfTrab    := INIRec.ReadString(sSecao, 'cpfTrab', EmptyStr);
      ideTrabSemVinc.NisTrab    := INIRec.ReadString(sSecao, 'nisTrab', EmptyStr);
      ideTrabSemVinc.codCateg   := INIRec.ReadInteger(sSecao, 'codCateg', 0);
      ideTrabSemVinc.matricula  := INIRec.ReadString(sSecao, 'matricula', EmptyStr);

      sSecao := 'infoTSVAlteracao';
      infoTSVAlteracao.dtAlteracao    := StringToDateTime(INIRec.ReadString(sSecao, 'dtAlteracao', '0'));
      infoTSVAlteracao.natAtividade := eSStrToNatAtividade(Ok, INIRec.ReadString(sSecao, 'natAtividade', '1'));

      sSecao := 'cargoFuncao';
      if INIRec.ReadString(sSecao, 'codCargo', '') <> '' then
      begin
        infoTSVAlteracao.infoComplementares.cargoFuncao.CodCargo  := INIRec.ReadString(sSecao, 'codCargo', '');
        infoTSVAlteracao.infoComplementares.cargoFuncao.CodFuncao := INIRec.ReadString(sSecao, 'codFuncao', '');
      end;
      infoTSVAlteracao.infoComplementares.cargoFuncao.nmCargo   := INIRec.ReadString(sSecao, 'nmCargo', '');  //01/06/2021
      infoTSVAlteracao.infoComplementares.cargoFuncao.CBOCargo  := INIRec.ReadString(sSecao, 'CBOCargo', '');  //01/06/2021
      infoTSVAlteracao.infoComplementares.cargoFuncao.nmFuncao  := INIRec.ReadString(sSecao, 'nmFuncao', '');  //01/06/2021
      infoTSVAlteracao.infoComplementares.cargoFuncao.CBOFuncao := INIRec.ReadString(sSecao, 'CBOFuncao', '');  //01/06/2021

      sSecao := 'remuneracao';
      if INIRec.ReadString(sSecao, 'vrSalFx', '') <> '' then
      begin
        infoTSVAlteracao.infoComplementares.remuneracao.VrSalFx    := StringToFloatDef(INIRec.ReadString(sSecao, 'vrSalFx', ''), 0);
        infoTSVAlteracao.infoComplementares.remuneracao.UndSalFixo := eSStrToUndSalFixo(Ok, INIRec.ReadString(sSecao, 'undSalFixo', ''));
        infoTSVAlteracao.infoComplementares.remuneracao.DscSalVar  := INIRec.ReadString(sSecao, 'dscSalVar', '');
      end;

      sSecao := 'infoDirigenteSindical';
      infoTSVAlteracao.infoComplementares.infoDirigenteSindical.tpRegPrev  := eSStrTotpRegPrevFacultativo(Ok, INIRec.ReadString(sSecao, 'tpRegPrev', '0'));

      sSecao := 'infoTrabCedido';
      infoTSVAlteracao.infoComplementares.infoTrabCedido.tpRegPrev := eSStrTotpRegPrevFacultativo(Ok, INIRec.ReadString(sSecao, 'tpRegPrev', '0'));

      sSecao := 'infoMandElet';
      infoTSVAlteracao.infoComplementares.infoMandElet.indRemunCargo := eSStrToSimNaoFacultativo(Ok, INIRec.ReadString(sSecao, 'indRemunCargo', '0'));
      infoTSVAlteracao.infoComplementares.infoMandElet.tpRegPrev     := eSStrToTpRegPrev(Ok, INIRec.ReadString(sSecao, 'tpRegPrev', '0'));

      sSecao := 'infoEstagiario';
      if INIRec.ReadString(sSecao, 'natEstagio', '') <> '' then
      begin
        infoTSVAlteracao.infoComplementares.infoEstagiario.natEstagio  := eSStrToTpNatEstagio(Ok, INIRec.ReadString(sSecao, 'natEstagio', 'O'));
        infoTSVAlteracao.infoComplementares.infoEstagiario.nivEstagio  := eSStrTotpNivelEstagio(Ok, INIRec.ReadString(sSecao, 'nivEstagio', '1'));
        infoTSVAlteracao.infoComplementares.infoEstagiario.areaAtuacao := INIRec.ReadString(sSecao, 'areaAtuacao', '');
        infoTSVAlteracao.infoComplementares.infoEstagiario.nrApol      := INIRec.ReadString(sSecao, 'nrApol', '');
        infoTSVAlteracao.infoComplementares.infoEstagiario.vlrBolsa    := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrBolsa', ''), 0);
        infoTSVAlteracao.infoComplementares.infoEstagiario.dtPrevTerm  := StringToDateTime(INIRec.ReadString(sSecao, 'dtPrevTerm', '0'));

        sSecao := 'instEnsino';
        infoTSVAlteracao.infoComplementares.infoEstagiario.instEnsino.cnpjInstEnsino := INIRec.ReadString(sSecao, 'cnpjInstEnsino', '');
        infoTSVAlteracao.infoComplementares.infoEstagiario.instEnsino.nmRazao        := INIRec.ReadString(sSecao, 'nmRazao', '');
        infoTSVAlteracao.infoComplementares.infoEstagiario.instEnsino.dscLograd      := INIRec.ReadString(sSecao, 'dscLograd', '');
        infoTSVAlteracao.infoComplementares.infoEstagiario.instEnsino.nrLograd       := INIRec.ReadString(sSecao, 'nrLograd', '');
        infoTSVAlteracao.infoComplementares.infoEstagiario.instEnsino.bairro         := INIRec.ReadString(sSecao, 'bairro', '');
        infoTSVAlteracao.infoComplementares.infoEstagiario.instEnsino.Cep            := INIRec.ReadString(sSecao, 'cep', '');
        infoTSVAlteracao.infoComplementares.infoEstagiario.instEnsino.codMunic       := INIRec.ReadInteger(sSecao, 'codMunic', 0);
        infoTSVAlteracao.infoComplementares.infoEstagiario.instEnsino.uf             := INIRec.ReadString(sSecao, 'uf', '');

        sSecao := 'ageIntegracao';
        if INIRec.ReadString(sSecao, 'cnpjAgntInteg', '') <> '' then
        begin
          infoTSVAlteracao.infoComplementares.infoEstagiario.ageIntegracao.cnpjAgntInteg := INIRec.ReadString(sSecao, 'cnpjAgntInteg', '');
          infoTSVAlteracao.infoComplementares.infoEstagiario.ageIntegracao.nmRazao       := INIRec.ReadString(sSecao, 'nmRazao', '');
          infoTSVAlteracao.infoComplementares.infoEstagiario.ageIntegracao.dscLograd     := INIRec.ReadString(sSecao, 'dscLograd', '');
          infoTSVAlteracao.infoComplementares.infoEstagiario.ageIntegracao.nrLograd      := INIRec.ReadString(sSecao, 'nrLograd', '');
          infoTSVAlteracao.infoComplementares.infoEstagiario.ageIntegracao.bairro        := INIRec.ReadString(sSecao, 'bairro', '');
          infoTSVAlteracao.infoComplementares.infoEstagiario.ageIntegracao.Cep           := INIRec.ReadString(sSecao, 'cep', '');
          infoTSVAlteracao.infoComplementares.infoEstagiario.ageIntegracao.codMunic      := INIRec.ReadInteger(sSecao, 'codMunic', 0);
          infoTSVAlteracao.infoComplementares.infoEstagiario.ageIntegracao.uf            := INIRec.ReadString(sSecao, 'uf', 'SP');
        end;

        sSecao := 'supervisorEstagio';
        if INIRec.ReadString(sSecao, 'cpfSupervisor', '') <> '' then
        begin
          infoTSVAlteracao.infoComplementares.infoEstagiario.supervisorEstagio.cpfSupervisor := INIRec.ReadString(sSecao, 'cpfSupervisor', '');
          infoTSVAlteracao.infoComplementares.infoEstagiario.supervisorEstagio.nmSuperv      := INIRec.ReadString(sSecao, 'nmSuperv', '');
        end;
      end;

      sSecao := 'localTrabGeral';
      if INIRec.ReadString(sSecao, 'tpInsc', '') <> '' then
      begin
        infoTSVAlteracao.infoComplementares.LocalTrabGeral.TpInsc   := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
        infoTSVAlteracao.infoComplementares.LocalTrabGeral.NrInsc   := INIRec.ReadString(sSecao, 'nrInsc', '');
        infoTSVAlteracao.infoComplementares.LocalTrabGeral.DescComp := INIRec.ReadString(sSecao, 'descComp', '');
      end;
    end;

    GerarXML;
    XML := FXML;
  finally
    INIRec.Free;
  end;
end;

end.
