{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
{                              Jean Carlo Cantu                                }
{                              Tiago Ravache                                   }
{                              Guilherme Costa                                 }
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

unit pcesS1070;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IfEnd}
  ACBrBase,
  ACBrDFeConsts,
  pcnConversao, pcnGerador,
  pcesCommon, pcesConversaoeSocial, pcesGerador;


type
  TS1070Collection = class;
  TS1070CollectionItem = class;
  TEvtTabProcesso = class;
  TDadosProcJud = class;
  TDadosProc = class;
  TInfoProcesso = class;
  TIdeProcesso = class;

  TS1070Collection = class(TeSocialCollection)
  private
    function GetItem(Index: Integer): TS1070CollectionItem;
    procedure SetItem(Index: Integer; Value: TS1070CollectionItem);
  public
    function Add: TS1070CollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TS1070CollectionItem;
    property Items[Index: Integer]: TS1070CollectionItem read GetItem write SetItem; default;
  end;

  TS1070CollectionItem = class(TObject)
  private
    FTipoEvento: TTipoEvento;
    FEvtTabProcesso: TEvtTabProcesso;
  public
    constructor Create(AOwner: TComponent);
    destructor  Destroy; override;
    property TipoEvento: TTipoEvento read FTipoEvento;
    property EvtTabProcesso: TEvtTabProcesso read FEvtTabProcesso write FEvtTabProcesso;
  end;

  TIdeProcesso = class(TObject)
  private
    FTpProc : tpTpProc;
    FNrProc : string;
    FIniValid: string;
    FFimValid: string;
  public
    property tpProc :tpTpProc read FTpProc write FTpProc;
    property nrProc :string read FNrProc write FNrProc;
    property iniValid: string read FIniValid write FIniValid;
    property fimValid: string read FFimValid write FFimValid;
  end;

  TEvtTabProcesso = class(TESocialEvento)
  private
    FModoLancamento: TModoLancamento;
    fIdeEvento: TIdeEvento;
    fIdeEmpregador: TIdeEmpregador;
    fInfoProcesso: TInfoProcesso;

    {Geradores específicos da classe}
    procedure GerarIdeProcesso;
    procedure GerarDadosProcJud;
    procedure GerarDadosProc;
    procedure GerarDadosInfoSusp;
  public
    constructor Create(AACBreSocial: TObject); override;
    destructor  Destroy; override;

    function GerarXML: boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property ModoLancamento: TModoLancamento read FModoLancamento write FModoLancamento;
    property IdeEvento: TIdeEvento read fIdeEvento write fIdeEvento;
    property IdeEmpregador: TIdeEmpregador read fIdeEmpregador write fIdeEmpregador;
    property InfoProcesso: TInfoProcesso read fInfoProcesso write fInfoProcesso;
  end;

  TInfoSuspCollectionItem = class(TObject)
  private
    FCodSusp: String;
    FIndSusp: tpIndSusp;
    FDTDecisao: TDateTime;
    FIndDeposito: tpSimNao;
  public
    property codSusp: String read FCodSusp write FCodSusp;
    property indSusp: tpIndSusp read FIndSusp write FIndSusp;
    property dtDecisao: TDateTime read FDTDecisao write FDTDecisao;
    property indDeposito: tpSimNao read FIndDeposito write FIndDeposito;
  end;

  TInfoSuspCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TInfoSuspCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfoSuspCollectionItem);
  public
    function Add: TInfoSuspCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TInfoSuspCollectionItem;
    property Items[Index: Integer]: TInfoSuspCollectionItem read GetItem write SetItem; default;
  end;

  TDadosProcJud = class(TObject)
  private
    FUfVara: string;
    FCodMunic: integer;
    FIdVara: string;
  public
    property UfVara: string read FUfVara write FUfVara;
    property codMunic: integer read FCodMunic write FCodMunic;
    property idVara: string read FIdVara write FIdVara;
  end;

  TDadosProc = class(TObject)
  private
    FIndAutoria: tpindAutoria;
    FIndMatProc: tpIndMatProc;
    Fobservacao: String;
    FDadosProcJud : TDadosProcJud;
    FInfoSusp: TInfoSuspCollection;

    function getDadosProcJud: TDadosProcJud;
    function getInfoSusp(): TInfoSuspCollection;
  public
    constructor Create;
    destructor Destroy; override;

    function dadosProcJudInst(): Boolean;
    function infoSuspInst(): Boolean;

    property indAutoria: tpindAutoria read FIndAutoria write FIndAutoria;
    property indMatProc: tpIndMatProc read FIndMatProc write FIndMatProc;
    property observacao: String read Fobservacao write Fobservacao;

    property DadosProcJud: TDadosProcJud read getDadosProcJud write FDadosProcJud;
    property infoSusp: TInfoSuspCollection read getInfoSusp write FInfoSusp;
  end;

  TInfoProcesso = class
  private
    FIdeProcesso: TIdeProcesso;
    FDadosProc: TDadosProc;
    FNovaValidade: TIdePeriodo;

    function getDadosProc(): TDadosProc;
    function getNovaValidade(): TIdePeriodo;
  public
    constructor Create;
    destructor Destroy; override;

    function dadosProcsInst(): Boolean;
    function novaValidadeInst(): Boolean;

    property ideProcesso: TIdeProcesso read FIdeProcesso write FIdeProcesso;
    property dadosProc: TDadosProc read getDadosProc write FDadosProc;
    property novaValidade: TIdePeriodo read getNovaValidade write FNovaValidade;
  end;

implementation

uses
  IniFiles,
  ACBrUtil.Base,
  ACBrUtil.DateTime,
  ACBrUtil.FilesIO,
  ACBreSocial;

{ TS1070Collection }

function TS1070Collection.Add: TS1070CollectionItem;
begin
  Result := Self.New;
end;

function TS1070Collection.GetItem(Index: Integer): TS1070CollectionItem;
begin
  Result := TS1070CollectionItem(inherited Items[Index]);
end;

procedure TS1070Collection.SetItem(Index: Integer;
  Value: TS1070CollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TS1070Collection.New: TS1070CollectionItem;
begin
  Result := TS1070CollectionItem.Create(FACBreSocial);
  Self.Add(Result);
end;

{ TS1070CollectionItem }

constructor TS1070CollectionItem.Create(AOwner: TComponent);
begin
  inherited Create;
  FTipoEvento     := teS1070;
  FEvtTabProcesso := TEvtTabProcesso.Create(AOwner);
end;

destructor TS1070CollectionItem.Destroy;
begin
  FEvtTabProcesso.Free;
  inherited;
end;

{ TInfoSuspCollection }

function TInfoSuspCollection.Add: TInfoSuspCollectionItem;
begin
  Result := Self.New;
end;

function TInfoSuspCollection.GetItem(
  Index: Integer): TInfoSuspCollectionItem;
begin
  Result := TInfoSuspCollectionItem(inherited Items[Index]);
end;

procedure TInfoSuspCollection.SetItem(Index: Integer;
  Value: TInfoSuspCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TInfoSuspCollection.New: TInfoSuspCollectionItem;
begin
  Result := TInfoSuspCollectionItem.Create;
  Self.Add(Result);
end;

{ TDadosProc }

constructor TDadosProc.Create;
begin
  inherited Create;
  fDadosProcJud := nil;
  FInfoSusp     := nil;
end;

function TDadosProc.dadosProcJudInst: Boolean;
begin
  Result := Assigned(fDadosProcJud);
end;

destructor TDadosProc.Destroy;
begin
  FreeAndNil(fDadosProcJud);
  FreeAndNil(FInfoSusp);
  inherited;
end;

function TDadosProc.getDadosProcJud: TDadosProcJud;
begin
  if Not(Assigned(fDadosProcJud)) then
    fDadosProcJud := TDadosProcJud.Create;
  Result := fDadosProcJud;
end;

function TDadosProc.getInfoSusp: TInfoSuspCollection;
begin
  if Not(Assigned(FInfoSusp)) then
    FInfoSusp := TInfoSuspCollection.Create;
  Result := FInfoSusp;
end;

function TDadosProc.infoSuspInst: Boolean;
begin
  Result := Assigned(FInfoSusp);
end;

{ TInfoProcesso }

constructor TInfoProcesso.Create;
begin
  FIdeProcesso := TIdeProcesso.Create;
  FDadosProc := nil;
  FNovaValidade := nil;
end;

function TInfoProcesso.dadosProcsInst: Boolean;
begin
  Result := Assigned(FDadosProc);
end;

destructor TInfoProcesso.destroy;
begin
  FIdeProcesso.Free;
  FreeAndNil(FDadosProc);
  FreeAndNil(FNovaValidade);
  inherited;
end;

function TInfoProcesso.getDadosProc: TdadosProc;
begin
  if Not(Assigned(FDadosProc)) then
    FdadosProc := TDadosProc.create;
  Result := FDadosProc;
end;

function TInfoProcesso.getNovaValidade: TIdePeriodo;
begin
  if Not(Assigned(FNovaValidade)) then
    FNovaValidade := TIdePeriodo.Create;
  Result := FNovaValidade;
end;

function TInfoProcesso.novaValidadeInst: Boolean;
begin
  Result := Assigned(FNovaValidade);
end;

{ TEvtTabProcesso }

constructor TEvtTabProcesso.Create(AACBreSocial: TObject);
begin
  inherited Create(AACBreSocial);

  fIdeEvento     := TIdeEvento.Create;
  fIdeEmpregador := TIdeEmpregador.Create;
  fInfoProcesso  := TInfoProcesso.Create;
end;

destructor TEvtTabProcesso.Destroy;
begin
  fIdeEvento.Free;
  fIdeEmpregador.Free;
  fInfoProcesso.Free;
  inherited;
end;

procedure TEvtTabProcesso.GerarDadosInfoSusp;
var
  i: Integer;
begin
  if InfoProcesso.dadosProc.infoSuspInst() then
  begin
    for i := 0 to InfoProcesso.dadosProc.infoSusp.Count - 1 do
    begin
      Gerador.wGrupo('infoSusp');

      Gerador.wCampo(tcStr, '', 'codSusp',      1, 14, 1, InfoProcesso.dadosProc.infoSusp.GetItem(i).codSusp);
      Gerador.wCampo(tcStr, '', 'indSusp',      2,  2, 1, eSIndSuspToStr(InfoProcesso.dadosProc.infoSusp.GetItem(i).indSusp));
      Gerador.wCampo(tcDat, '', 'dtDecisao',   10, 10, 1, InfoProcesso.dadosProc.infoSusp.GetItem(i).dtDecisao);
      Gerador.wCampo(tcStr, '', 'indDeposito',  1,  1, 1, eSSimNaoToStr(InfoProcesso.dadosProc.infoSusp.GetItem(i).indDeposito));

      Gerador.wGrupo('/infoSusp');
    end;

    if InfoProcesso.dadosProc.infoSusp.Count > 99 then
      Gerador.wAlerta('', 'infoSusp', 'Lista de Informações de Suspensão', ERR_MSG_MAIOR_MAXIMO + '99');
  end;
end;

procedure TEvtTabProcesso.GerarDadosProc;
begin
  Gerador.wGrupo('dadosProc');

  Gerador.wCampo(tcInt, '', 'indAutoria', 1, 1, 1, eSindAutoriaToStr(InfoProcesso.dadosProc.indAutoria));
  Gerador.wCampo(tcInt, '', 'indMatProc', 1, 2, 1, eSTpIndMatProcToStr(InfoProcesso.dadosProc.indMatProc));

  if VersaoDF >= ve02_04_02 then
    Gerador.wCampo(tcStr, '', 'observacao', 1, 255, 0, InfoProcesso.dadosProc.observacao);

  GerarDadosProcJud;
  GerarDadosInfoSusp;

  Gerador.wGrupo('/dadosProc');
end;

procedure TEvtTabProcesso.GerarDadosProcJud;
begin
  if (InfoProcesso.dadosProc.dadosProcJudInst()) then
  begin
    Gerador.wGrupo('dadosProcJud');

    Gerador.wCampo(tcStr, '', 'ufVara',   2, 2, 1, self.InfoProcesso.dadosProc.DadosProcJud.ufVara);
    Gerador.wCampo(tcStr, '', 'codMunic', 7, 7, 1, self.InfoProcesso.dadosProc.DadosProcJud.codMunic);
    Gerador.wCampo(tcStr, '', 'idVara',   1, 4, 1, self.InfoProcesso.dadosProc.DadosProcJud.idVara);

    Gerador.wGrupo('/dadosProcJud');
  end;
end;

procedure TEvtTabProcesso.GerarIdeProcesso;
begin
  Gerador.wGrupo('ideProcesso');

  Gerador.wCampo(tcStr, '', 'tpProc',   1,  1, 1, eSTpProcessoToStr(self.InfoProcesso.ideProcesso.tpProc));
  Gerador.wCampo(tcStr, '', 'nrProc',   1, 21, 1, self.InfoProcesso.ideProcesso.nrProc);
  Gerador.wCampo(tcStr, '', 'iniValid', 7,  7, 1, self.InfoProcesso.ideProcesso.iniValid);
  Gerador.wCampo(tcStr, '', 'fimValid', 7,  7, 0, self.InfoProcesso.ideProcesso.fimValid);

  Gerador.wGrupo('/ideProcesso');
end;

function TEvtTabProcesso.GerarXML: boolean;
begin
  try
    inherited GerarXML;
    Self.VersaoDF := TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF;
     
    Self.Id := GerarChaveEsocial(now, self.ideEmpregador.NrInsc, self.Sequencial);

    GerarCabecalho('evtTabProcesso');
    Gerador.wGrupo('evtTabProcesso Id="' + Self.Id + '"');

    GerarIdeEvento(self.IdeEvento);
    GerarIdeEmpregador(self.IdeEmpregador);

    Gerador.wGrupo('infoProcesso');

    GerarModoAbertura(Self.ModoLancamento);
    GerarIdeProcesso;

    if Self.ModoLancamento <> mlExclusao then
    begin
      GerarDadosProc;

      if Self.ModoLancamento = mlAlteracao then
        if (InfoProcesso.novaValidadeInst()) then
          GerarIdePeriodo(self.InfoProcesso.NovaValidade,'novaValidade');
    end;

    GerarModoFechamento(Self.ModoLancamento);

    Gerador.wGrupo('/infoProcesso');
    Gerador.wGrupo('/evtTabProcesso');

    GerarRodape;

    FXML := Gerador.ArquivoFormatoXML;
//    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtTabProcesso');

//    Validar(schevtTabProcesso);
  except on e:exception do
    raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '')
end;

function TEvtTabProcesso.LerArqIni(const AIniString: String): Boolean;
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
      sSecao := 'evtTabProcesso';
      Id             := INIRec.ReadString(sSecao, 'Id', '');
      Sequencial     := INIRec.ReadInteger(sSecao, 'Sequencial', 0);
      ModoLancamento := eSStrToModoLancamento(Ok, INIRec.ReadString(sSecao, 'ModoLancamento', 'inclusao'));

      sSecao := 'ideEvento';
      ideEvento.ProcEmi := eSStrToProcEmi(Ok, INIRec.ReadString(sSecao, 'procEmi', '1'));
      ideEvento.VerProc := INIRec.ReadString(sSecao, 'verProc', EmptyStr);

      sSecao := 'ideEmpregador';
      ideEmpregador.OrgaoPublico := (TACBreSocial(FACBreSocial).Configuracoes.Geral.TipoEmpregador = teOrgaoPublico);
      ideEmpregador.TpInsc       := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
      ideEmpregador.NrInsc       := INIRec.ReadString(sSecao, 'nrInsc', EmptyStr);

      sSecao := 'ideProcesso';
      infoProcesso.ideProcesso.tpProc   := eSStrToTpProcesso(Ok, INIRec.ReadString(sSecao, 'tpProc', '1'));
      infoProcesso.ideProcesso.nrProc   := INIRec.ReadString(sSecao, 'nrProc', EmptyStr);
      infoProcesso.ideProcesso.IniValid := INIRec.ReadString(sSecao, 'iniValid', EmptyStr);
      infoProcesso.ideProcesso.FimValid := INIRec.ReadString(sSecao, 'fimValid', EmptyStr);

      if (ModoLancamento <> mlExclusao) then
      begin
        sSecao := 'dadosProc';
        infoProcesso.dadosProc.indAutoria := eSStrToindAutoria(Ok, INIRec.ReadString(sSecao, 'indAutoria', '1'));
        infoProcesso.dadosProc.indMatProc := eSStrToTpIndMatProc(Ok, INIRec.ReadString(sSecao, 'indMatProc', '1'));

        sSecao := 'dadosProcJud';
        if INIRec.ReadString(sSecao, 'UfVara', '') <> '' then
        begin
          infoProcesso.dadosProc.DadosProcJud.UfVara   := INIRec.ReadString(sSecao, 'UfVara', '');
          infoProcesso.dadosProc.DadosProcJud.codMunic := INIRec.ReadInteger(sSecao, 'codMunic', 0);
          infoProcesso.dadosProc.DadosProcJud.idVara   := INIRec.ReadString(sSecao, 'idVara', '');
        end;

        I := 1;
        while true do
        begin
          // de 01 até 99
          sSecao := 'infoSusp' + IntToStrZero(I, 2);
          sFim   := INIRec.ReadString(sSecao, 'codSusp', 'FIM');

          if (sFim = 'FIM') or (Length(sFim) <= 0) then
            break;

          with infoProcesso.dadosProc.infoSusp.New do
          begin
            codSusp     := sFim;
            indSusp     := eSStrToIndSusp(Ok, INIRec.ReadString(sSecao, 'indSusp', '01'));
            dtDecisao   := StringToDateTime(INIRec.ReadString(sSecao, 'dtDecisao', '0'));
            indDeposito := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'indDeposito', 'S'));
          end;

          Inc(I);
        end;

        if ModoLancamento = mlAlteracao then
        begin
          sSecao := 'novaValidade';
          infoProcesso.novaValidade.IniValid := INIRec.ReadString(sSecao, 'iniValid', EmptyStr);
          infoProcesso.novaValidade.FimValid := INIRec.ReadString(sSecao, 'fimValid', EmptyStr);
        end;
      end;
    end;

    GerarXML;
    XML := FXML;
  finally
    INIRec.Free;
  end;
end;

end.
