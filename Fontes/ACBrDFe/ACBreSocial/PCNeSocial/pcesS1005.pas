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

unit pcesS1005;

interface

uses
  SysUtils, Classes, Controls,
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
  TS1005Collection = class;
  TS1005CollectionItem = class;
  TEvtTabEstab = class;

  {Classes específicas deste evento}
  TInfoPCD = class;
  TInfoEntEducCollection = class;
  TInfoEntEducCollectionItem = class;
  TInfoApr = class;
  TInfoTrab = class;
  TInfoObra = class;
  TinfoCaepf = class; //introduzido no layout2.1
  TDadosEstab = class;
  TIdeEstab = class;
  TInfoEstab = class;

  TS1005Collection = class(TeSocialCollection)
  private
    function GetItem(Index: Integer): TS1005CollectionItem;
    procedure SetItem(Index: Integer; Value: TS1005CollectionItem);
  public
    function Add: TS1005CollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TS1005CollectionItem;
    property Items[Index: Integer]: TS1005CollectionItem read GetItem write SetItem; default;
  end;

  TS1005CollectionItem = class(TObject)
  private
    FTipoEvento: TTipoEvento;
    FevtTabEstab: TevtTabEstab;
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;
    property TipoEvento: TTipoEvento read FTipoEvento;
    property evtTabEstab: TevtTabEstab read FevtTabEstab write FevtTabEstab;
  end;

  TInfoEntEducCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TInfoEntEducCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfoEntEducCollectionItem);
  public
    function Add: TInfoEntEducCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TInfoEntEducCollectionItem;
    property Items[Index: Integer]: TInfoEntEducCollectionItem read GetItem write SetItem; default;
  end;

  TInfoEntEducCollectionItem = class(TObject)
  private
    FNrInsc: string;
  public
    property nrInsc: string read FNrInsc write FNrInsc;
  end;

  TInfoApr = class(TObject)
  private
    FNrProcJud: string;
    FContEntEd: tpSimNaoFacultativo;
    FInfoEntEduc: TInfoEntEducCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property nrProcJud: String read FNrProcJud write FNrProcJud;
    property contEntEd: tpSimNaoFacultativo read FContEntEd write FContEntEd;
    property infoEntEduc: TInfoEntEducCollection read FInfoEntEduc write FInfoEntEduc;
  end;

  TInfoPCD = class(TObject)
  private
    FNrProcJud: string;
  public
    property nrProcJud: string read FNrProcJud write FNrProcJud;
  end;

  TInfoTrab = class(TObject)
  private
    FInfoApr: TInfoApr;
    FInfoPCD: TInfoPCD;

    function getInfoPCD: TInfoPCD;
    function getInfoApr: TInfoApr;
  public
    constructor Create;
    destructor Destroy; override;

    function infoPCDInst(): Boolean;
    function infoAprInst(): Boolean;

    property infoApr: TInfoApr read getInfoApr write FInfoApr;
    property infoPCD: TInfoPCD read getInfoPCD write FInfoPCD;
  end;

  TInfoObra = class(TObject)
  private
    FIndSubstPatrObra: tpIndSubstPatronalObra;
  public
    property indSubstPatrObra: tpIndSubstPatronalObra read FIndSubstPatrObra write FIndSubstPatrObra;
  end;

  TinfoCaepf = class(TObject)
  private
    FtpCaepf: tpCaepf;
  public
    property tpCaepf: tpCaepf read FtpCaepf write FtpCaepf;
  end;

  TDadosEstab = class(TObject)
  private
    FCnaePrep: string;
    FcnpjResp: string;
    FAliqGilrat: TAliqGilRat;
    FinfoCaepf : TinfoCaepf;
    FInfoObra: TInfoObra;
    FInfoTrab: TInfoTrab;

    function getInfoObra: TInfoObra;
    function getinfoCaepf : TInfoCaepf;
  public
    constructor Create;
    destructor Destroy; override;

    function infoObraInst(): Boolean;
    function infoCaepfInst(): Boolean;

    property cnaePrep: string read FCnaePrep write FCnaePrep;
    property cnpjResp: string read FcnpjResp write FcnpjResp;
    property aliqGilrat: TAliqGilRat read FAliqGilrat write FAliqGilrat;
    property infoObra: TInfoObra read getInfoObra write FInfoObra;
    property infoCaepf : TinfoCaepf read getinfoCaepf write FinfoCaepf;
    property infoTrab: TInfoTrab read FInfoTrab write FInfoTrab;
  end;

  TIdeEstab = class(TObject)
  private
    FTpInsc: tpTpInsc;
    FNrInsc: string;
    FIniValid: string;
    FFimValid: string;
  public
    property tpInsc: tpTpInsc read FTpInsc write FTpInsc;
    property nrInsc: string read FNrInsc write FNrInsc;
    property iniValid: string read FIniValid write FIniValid;
    property fimValid: string read FFimValid write FFimValid;
  end;

  TInfoEstab = class(TObject)
  private
    FIdeEstab: TIdeEstab;
    FDadosEstab: TDadosEstab;
    FNovaValidade: TidePeriodo;

    function getDadosEstab: TDadosEstab;
    function getNovaValidade: TidePeriodo;
  public
    constructor Create;
    destructor Destroy; override;

    function dadosEstabInst(): boolean;
    function NovaValidadeInst(): boolean;

    property IdeEstab: TIdeEstab read FIdeEstab write FIdeEstab;
    property DadosEstab: TDadosEstab read getDadosEstab write FDadosEstab;
    property NovaValidade: TidePeriodo read getNovaValidade write FNovaValidade;
  end;

  TEvtTabEstab = class(TeSocialEvento)
  private
    FModoLancamento: TModoLancamento;
    FIdeEvento: TIdeEvento;
    FIdeEmpregador: TIdeEmpregador;
    FInfoEstab: TInfoEstab;

    {. Geradores especificos desta classe .}
    procedure GerarInfoEntEduc;
    procedure GerarInfoApr;
    procedure GerarInfoPCD;
    procedure GerarInfoTrab;
    procedure GerarIdeEstab;
    procedure GerarInfoObra;
    procedure GerarInfoCaepf;
    procedure GerarDadosEstab;
  public
    constructor Create(AACBreSocial: TObject); override;
    destructor Destroy; override;

    function GerarXML: boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property ModoLancamento: TModoLancamento read FModoLancamento write FModoLancamento;
    property ideEvento: TIdeEvento read FIdeEvento write FIdeEvento;
    property ideEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property infoEstab: TInfoEstab read FInfoEstab write FInfoEstab;
  end;

implementation

uses
  IniFiles,
  ACBrUtil.Base,
  ACBrUtil.FilesIO,
  ACBreSocial;

{ TDadosEstab }

constructor TDadosEstab.Create;
begin
  inherited Create;
  FAliqGilrat := TAliqGilRat.Create;
  FInfoTrab   := TInfoTrab.Create;
  FInfoObra   := nil;
  FinfoCaepf  := nil;
end;

destructor TDadosEstab.Destroy;
begin
  FAliqGilrat.Free;
  FInfoTrab.Free;
  FreeAndNil(FInfoObra);
  FreeAndNil(FinfoCaepf);

  inherited;
end;

function TDadosEstab.getinfoCaepf: TInfoCaepf;
begin
  if Not(Assigned(FinfoCaepf)) then
    FInfoCaepf := TinfoCaepf.Create;
  Result := FinfoCaepf;
end;

function TDadosEstab.getInfoObra: TInfoObra;
begin
  if Not(Assigned(FInfoObra)) then
    FInfoObra := TInfoObra.Create;
  Result := FInfoObra;
end;

function TDadosEstab.infoCaepfInst: Boolean;
begin
  Result := Assigned(FinfoCaepf);
end;

function TDadosEstab.infoObraInst: Boolean;
begin
  Result := Assigned(FInfoObra);
end;

{ TInfoApr }

constructor TInfoApr.Create;
begin
  inherited;

  FInfoEntEduc := TInfoEntEducCollection.Create;
end;

destructor TInfoApr.Destroy;
begin
  FInfoEntEduc.Free;

  inherited;
end;

{ TInfoTrab }

constructor TInfoTrab.Create;
begin
  inherited;

  FInfoApr := nil;
  FInfoPCD := nil;

end;

destructor TInfoTrab.Destroy;
begin
  FreeAndNil(FInfoApr);
  FreeAndNil(FInfoPCD);

  inherited;
end;

function TInfoTrab.getInfoPCD: TInfoPCD;
begin
  if not(Assigned(FInfoPCD)) then
    FInfoPCD := TInfoPCD.Create;
  result := FInfoPCD;
end;

function TInfoTrab.getInfoApr: TInfoApr;
begin
  if not(Assigned(FInfoApr)) then
    FInfoApr := TInfoApr.Create;
  result := FInfoApr;
end;

function TInfoTrab.infoPCDInst: boolean;
begin
  result := Assigned(FInfoPCD);
end;

function TInfoTrab.infoAprInst: boolean;
begin
  result := Assigned(FInfoApr);
end;

{ TInfoEstab }

constructor TInfoEstab.Create;
begin
  inherited Create;
  FIdeEstab     := TIdeEstab.Create;
  FDadosEstab   := nil;
  FNovaValidade := nil;
end;

function TInfoEstab.dadosEstabInst: boolean;
begin
  result := Assigned(FDadosEstab);
end;

destructor TInfoEstab.Destroy;
begin
  FIdeEstab.Free;
  FreeAndNil(FDadosEstab);
  FreeAndNil(FNovaValidade);

  inherited;
end;

function TInfoEstab.getDadosEstab: TDadosEstab;
begin
  if Not(Assigned(FDadosEstab)) then
    FDadosEstab := TDadosEstab.create;
  Result := FDadosEstab;
end;

function TInfoEstab.getNovaValidade: TidePeriodo;
begin
  if Not(Assigned(FNovaValidade)) then
    FNovaValidade := TIdePeriodo.Create;
  Result := FNovaValidade;
end;

function TInfoEstab.NovaValidadeInst: boolean;
begin
  result := Assigned(FNovaValidade);
end;

{ TEvtTabEstab }

constructor TEvtTabEstab.Create(AACBreSocial: TObject);
begin
  inherited Create(AACBreSocial);

  FIdeEvento     := TIdeEvento.Create;
  FIdeEmpregador := TIdeEmpregador.Create;
  FInfoEstab     := TInfoEstab.Create;
end;

destructor TEvtTabEstab.Destroy;
begin
  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FInfoEstab.Free;

  inherited;
end;

procedure TEvtTabEstab.GerarDadosEstab;
begin
  Gerador.wGrupo('dadosEstab');

  Gerador.wCampo(tcStr, '', 'cnaePrep', 1, 7, 1, infoEstab.DadosEstab.cnaePrep);

  if (VersaoDF > ve02_05_00) and
     (infoEstab.DadosEstab.cnpjResp <> '') then
    Gerador.wCampo(tcStr, '', 'cnpjResp', 1, 14, 1, infoEstab.DadosEstab.cnpjResp);

  GerarAliqGilRat(FIdeEmpregador, infoEstab.FIdeEstab.tpInsc, infoEstab.DadosEstab.aliqGilrat, 'aliqGilrat');
  GerarInfoCaepf;
  GerarInfoObra;
  GerarInfoTrab;

  Gerador.wGrupo('/dadosEstab');
end;

procedure TEvtTabEstab.GerarIdeEstab;
begin
  Gerador.wGrupo('ideEstab');

  Gerador.wCampo(tcStr, '', 'tpInsc',   1,  1, 1, eSTpInscricaoToStr(Self.infoEstab.ideEstab.tpInsc));
  Gerador.wCampo(tcStr, '', 'nrInsc',   1, 15, 1, Self.infoEstab.ideEstab.nrInsc);
  Gerador.wCampo(tcStr, '', 'iniValid', 7,  7, 1, Self.infoEstab.IdeEstab.iniValid);
  Gerador.wCampo(tcStr, '', 'fimValid', 7,  7, 0, Self.infoEstab.IdeEstab.fimValid);

  Gerador.wGrupo('/ideEstab');
end;

procedure TEvtTabEstab.GerarInfoCaepf;
begin
  if infoEstab.DadosEstab.infoCaepfInst() and
     (infoEstab.DadosEstab.infoCaepf.tpCaepf <> tcVazio) then
  begin
    Gerador.wGrupo('infoCaepf');

    Gerador.wCampo(tcStr, '', 'tpCaepf', 1, 1, 1, eStpCaepfToStr(infoEstab.DadosEstab.infoCaepf.tpCaepf));

    Gerador.wGrupo('/infoCaepf');
  end;
end;

procedure TEvtTabEstab.GerarInfoObra;
begin
  if infoEstab.DadosEstab.infoObraInst() and
    (infoEstab.DadosEstab.InfoObra.indSubstPatrObra <> ispVazio) then
  begin
    Gerador.wGrupo('infoObra');

    Gerador.wCampo(tcStr, '', 'indSubstPatrObra', 1, 1, 1, eSIndSubstPatronalObraToStr(infoEstab.DadosEstab.InfoObra.indSubstPatrObra));

    Gerador.wGrupo('/infoObra');
  end;
end;

procedure TEvtTabEstab.GerarInfoTrab;
var
  L: Boolean;
begin
  if (infoEstab.DadosEstab.infoTrab.infoAprInst()) or 
     (infoEstab.DadosEstab.infoTrab.infoPCDInst()) or 
     (VersaoDF <= ve02_05_00) then
  begin
    L := False;

    if VersaoDF <= ve02_05_00 then
      L := True;
      
    if infoEstab.DadosEstab.infoTrab.infoAprInst() then
      if (infoEstab.DadosEstab.infoTrab.infoApr.nrProcJud <> '') or
         (Assigned(infoEstab.DadosEstab.infoTrab.infoApr.FInfoEntEduc)) then
        L := True;
        
    if infoEstab.DadosEstab.infoTrab.infoPCDInst() then
      if infoEstab.DadosEstab.infoTrab.infoPCD.nrProcJud <> '' then
        L := True;
        
    if L then
    begin
      Gerador.wGrupo('infoTrab');

      GerarInfoApr;
      GerarInfoPCD;

      Gerador.wGrupo('/infoTrab');
    end;  
  end;
end;

procedure TEvtTabEstab.GerarInfoPCD;
begin
  if infoEstab.DadosEstab.infoTrab.infoPCDInst() then
  begin
    if (VersaoDF <= ve02_05_00) or 
       (infoEstab.DadosEstab.infoTrab.infoPCD.nrProcJud <> '') then
    begin
      Gerador.wGrupo('infoPCD');

      Gerador.wCampo(tcStr, '', 'nrProcJud', 0, 20, 0, infoEstab.DadosEstab.infoTrab.infoPCD.nrProcJud);

      Gerador.wGrupo('/infoPCD');
    end;  
  end;
end;

procedure TEvtTabEstab.GerarInfoApr;
begin
  if infoEstab.DadosEstab.infoTrab.infoAprInst() then
  begin
    if (VersaoDF <= ve02_05_00) or 
       (infoEstab.DadosEstab.infoTrab.infoApr.nrProcJud <> '') or
       (Assigned(infoEstab.DadosEstab.infoTrab.infoApr.FInfoEntEduc)) then
    begin
      Gerador.wGrupo('infoApr');

      if (infoEstab.DadosEstab.infoTrab.infoApr.nrProcJud <> '') then
        Gerador.wCampo(tcStr, '', 'nrProcJud', 1, 20, 0, infoEstab.DadosEstab.infoTrab.infoApr.nrProcJud);

      GerarInfoEntEduc;

      Gerador.wGrupo('/infoApr');
    end;  
  end;
end;

procedure TEvtTabEstab.GerarInfoEntEduc;
var
  i: Integer;
begin
  if Assigned(infoEstab.DadosEstab.infoTrab.infoApr.FInfoEntEduc) then
  begin
    for i := 0 to infoEstab.DadosEstab.infoTrab.infoApr.FInfoEntEduc.Count - 1 do
    begin
      Gerador.wGrupo('infoEntEduc');

      Gerador.wCampo(tcStr, '', 'nrInsc', 1, 15, 1, infoEstab.DadosEstab.infoTrab.infoApr.FInfoEntEduc[i].nrInsc);

      Gerador.wGrupo('/infoEntEduc');
    end;

    if infoEstab.DadosEstab.infoTrab.infoApr.FInfoEntEduc.Count > 99 then
      Gerador.wAlerta('', 'infoEntEduc', 'Lista de Informações das Entidades Educativas', ERR_MSG_MAIOR_MAXIMO + '99');
  end;
end;

function TEvtTabEstab.GerarXML: boolean;
begin
  try
    inherited GerarXML;
    Self.VersaoDF := TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF;
     
    Self.Id := GerarChaveEsocial(now, self.ideEmpregador.NrInsc, self.Sequencial);

    GerarCabecalho('evtTabEstab');
    Gerador.wGrupo('evtTabEstab Id="' + Self.Id + '"');

    GerarIdeEvento(Self.FIdeEvento, True);
    GerarIdeEmpregador(Self.FIdeEmpregador);

    Gerador.wGrupo('infoEstab');

    GerarModoAbertura(Self.ModoLancamento);
    GerarIdeEstab;

    if (FModoLancamento <> mlExclusao) then
    begin
      GerarDadosEstab;

      if Self.ModoLancamento = mlAlteracao then
        if (infoEstab.NovaValidadeInst()) then
          GerarIdePeriodo(infoEstab.NovaValidade, 'novaValidade');
    end;

    GerarModoFechamento(Self.ModoLancamento);

    Gerador.wGrupo('/infoEstab');
    Gerador.wGrupo('/evtTabEstab');

    GerarRodape;

    FXML := Gerador.ArquivoFormatoXML;
//    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtTabEstab');

//    Validar(schevtTabEstab);
  except on e:exception do
    raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '')
end;

function TEvtTabEstab.LerArqIni(const AIniString: String): Boolean;
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
      sSecao := 'evtTabEstab';
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

      sSecao := 'ideEstab';
      infoEstab.ideEstab.tpInsc   := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
      infoEstab.ideEstab.NrInsc   := INIRec.ReadString(sSecao, 'nrInsc', EmptyStr);
      infoEstab.ideEstab.IniValid := INIRec.ReadString(sSecao, 'iniValid', EmptyStr);
      infoEstab.ideEstab.FimValid := INIRec.ReadString(sSecao, 'fimValid', EmptyStr);

      if (FModoLancamento <> mlExclusao) then
      begin
        sSecao := 'dadosEstab';
        infoEstab.DadosEstab.cnaePrep   := INIRec.ReadString(sSecao, 'cnaePrep', EmptyStr);
        infoEstab.DadosEstab.cnpjResp   := INIRec.ReadString(sSecao, 'cnpjResp', EmptyStr);

        sSecao := 'aliqGilrat';
        infoEstab.DadosEstab.aliqGilrat.AliqRat      :=eSStrToAliqRat(Ok, INIRec.ReadString(sSecao, 'aliqRat', '1'));
        infoEstab.DadosEstab.aliqGilrat.Fap          := StringToFloatDef(INIRec.ReadString(sSecao, 'fap', ''), 0);
        infoEstab.DadosEstab.aliqGilrat.AliqRatAjust := StringToFloatDef(INIRec.ReadString(sSecao, 'aliqRatAjust', ''), 0);

        sSecao := 'procAdmJudRat';
        if INIRec.ReadString(sSecao, 'tpProc', '') <> '' then
        begin
          infoEstab.DadosEstab.aliqGilrat.ProcAdmJudRat.tpProc  := eSStrToTpProcesso(Ok, INIRec.ReadString(sSecao, 'tpProc', '1'));
          infoEstab.DadosEstab.aliqGilrat.ProcAdmJudRat.nrProc  := INIRec.ReadString(sSecao, 'nrProc', EmptyStr);
          infoEstab.DadosEstab.aliqGilrat.ProcAdmJudRat.codSusp := INIRec.ReadString(sSecao, 'codSusp', EmptyStr);
        end;

        sSecao := 'procAdmJudFap';
        if INIRec.ReadString(sSecao, 'tpProc', '') <> '' then
        begin
          infoEstab.DadosEstab.aliqGilrat.ProcAdmJudFap.tpProc  := eSStrToTpProcesso(Ok, INIRec.ReadString(sSecao, 'tpProc', '1'));
          infoEstab.DadosEstab.aliqGilrat.ProcAdmJudFap.nrProc  := INIRec.ReadString(sSecao, 'nrProc', EmptyStr);
          infoEstab.DadosEstab.aliqGilrat.ProcAdmJudFap.codSusp := INIRec.ReadString(sSecao, 'codSusp', EmptyStr);
        end;

        sSecao := 'infoCaepf';
        if INIRec.ReadString(sSecao, 'tpCaepf', '') <> '' then
          infoEstab.DadosEstab.infoCaepf.tpCaepf := eSStrTotpCaepf(Ok, INIRec.ReadString(sSecao, 'tpCaepf', '1'));

        sSecao := 'infoObra';
        if INIRec.ReadString(sSecao, 'indSubstPatrObra', '') <> '' then
          infoEstab.DadosEstab.InfoObra.indSubstPatrObra := eSStrToIndSubstPatronalObra(Ok, INIRec.ReadString(sSecao, 'indSubstPatrObra', '1'));

        sSecao := 'infoApr';
        infoEstab.DadosEstab.infoTrab.infoApr.nrProcJud := INIRec.ReadString(sSecao, 'nrProcJud', EmptyStr);

        I := 1;
        while true do
        begin
          // de 01 até 99
          sSecao := 'infoEntEduc' + IntToStrZero(I, 2);
          sFim   := INIRec.ReadString(sSecao, 'nrInsc', 'FIM');

          if (sFim = 'FIM') or (Length(sFim) <= 0) then
            break;

          with infoEstab.DadosEstab.infoTrab.infoApr.FInfoEntEduc.New do
          begin
            nrInsc := sFim;
          end;

          Inc(I);
        end;

        sSecao := 'infoPCD';
        infoEstab.DadosEstab.infoTrab.infoPCD.nrProcJud := INIRec.ReadString(sSecao, 'nrProcJud', EmptyStr);

        if ModoLancamento = mlAlteracao then
        begin
          sSecao := 'novaValidade';
          infoEstab.NovaValidade.IniValid := INIRec.ReadString(sSecao, 'iniValid', EmptyStr);
          infoEstab.NovaValidade.FimValid := INIRec.ReadString(sSecao, 'fimValid', EmptyStr);
        end;
      end;  
    end;

    GerarXML;
    XML := FXML;
  finally
    INIRec.Free;
  end;
end;

{ TS1005CollectionItem }

constructor TS1005CollectionItem.Create(AOwner: TComponent);
begin
  inherited Create;
  FTipoEvento  := teS1005;
  FevtTabEstab := TevtTabEstab.Create(AOwner);
end;

destructor TS1005CollectionItem.Destroy;
begin
  FevtTabEstab.Free;

  inherited;
end;

{ TS1005Collection }

function TS1005Collection.Add: TS1005CollectionItem;
begin
  Result := Self.New;
end;

function TS1005Collection.GetItem(Index: Integer): TS1005CollectionItem;
begin
  Result := TS1005CollectionItem(inherited Items[Index]);
end;

procedure TS1005Collection.SetItem(Index: Integer; Value: TS1005CollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TS1005Collection.New: TS1005CollectionItem;
begin
  Result := TS1005CollectionItem.Create(FACBreSocial);
  Self.Add(Result);
end;

{ TInfoEntEducCollection }
function TInfoEntEducCollection.Add: TInfoEntEducCollectionItem;
begin
  Result := Self.New;
end;

function TInfoEntEducCollection.GetItem(
  Index: Integer): TInfoEntEducCollectionItem;
begin
  Result := TInfoEntEducCollectionItem(inherited Items[Index]);
end;

procedure TInfoEntEducCollection.SetItem(Index: Integer;
  Value: TInfoEntEducCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TInfoEntEducCollection.New: TInfoEntEducCollectionItem;
begin
  Result := TInfoEntEducCollectionItem.Create;
  Self.Add(Result);
end;

end.
