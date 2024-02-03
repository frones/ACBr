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

unit pcesS1060;

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
  ACBrBase,
  pcnConversao, pcnGerador,
  pcesCommon, pcesConversaoeSocial, pcesGerador,
  pcnLeitor;

type
  TS1060CollectionItem = class;
  TInfoAmbiente = class;
  TEvtTabAmbiente = class;


  TS1060Collection = class(TeSocialCollection)
  private
    function GetItem(Index: Integer): TS1060CollectionItem;
    procedure SetItem(Index: Integer; Value: TS1060CollectionItem);
  public
    function Add: TS1060CollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TS1060CollectionItem;
    property Items[Index: Integer]: TS1060CollectionItem read GetItem write SetItem; default;
  end;

  TS1060CollectionItem = class(TObject)
  private
    FTipoEvento: TTipoEvento;
    FEvtTabAmbiente: TEvtTabAmbiente;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    property TipoEvento: TTipoEvento read FTipoEvento;
    property EvtTabAmbiente: TEvtTabAmbiente read FEvtTabAmbiente write FEvtTabAmbiente;
  end;

  TEvtTabAmbiente = class(TeSocialEvento)
  private
    FModoLancamento: TModoLancamento;
    FIdeEvento: TIdeEvento;
    FIdeEmpregador: TIdeEmpregador;
    FInfoAmbiente: TInfoAmbiente;

    procedure GerarIdeAmbiente;
    procedure GerarDadosAmbiente;
  public
    constructor Create(AACBreSocial: TObject); override;
    destructor  Destroy; override;

    function GerarXML: boolean; override;
    function LerXML : Boolean;
    function LerArqIni(const AIniString: String): Boolean;

    property ModoLancamento: TModoLancamento read FModoLancamento write FModoLancamento;
    property ideEvento: TIdeEvento read FIdeEvento write FIdeEvento;
    property ideEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property infoAmbiente: TInfoAmbiente read FInfoAmbiente write FInfoAmbiente;
  end;

  TIdeAmbiente = class(TObject)
  private
    FCodAmb: string;
    FIniValid: string;
    FFimValid: string;
  public
    property codAmb: string read FCodAmb write FCodAmb;
    property iniValid: string read FIniValid write FIniValid;
    property fimValid: string read FFimValid write FFimValid;
  end;

  TDadosAmbiente = class(TObject)
  private
    FNmAmb: String;
    FDscAmb: String;
    FLocalAmb: tpLocalAmb;
    FTpInsc: tpTpInsc;
    FNrInsc: String;
    FCodLotacao: String;
  public
    property nmAmb: string read FNmAmb write FNmAmb;
    property dscAmb: string read FDscAmb write FDscAmb;
    property localAmb: tpLocalAmb read FLocalAmb write FLocalAmb;
    property tpInsc: tpTpInsc read FTpInsc write FTpInsc;
    property nrInsc: string read FNrInsc write FNrInsc;
    property codLotacao: String read FCodLotacao write FCodLotacao;
  end;

  TInfoAmbiente = class(TObject)
  private
    FIdeAmbiente: TIdeAmbiente;
    FDadosAmbiente: TDadosAmbiente;
    FNovaValidade: TIdePeriodo;

    function getDadosAmbiente: TDadosAmbiente;
    function getNovaValidade: TIdePeriodo;
  public
    constructor Create;
    destructor Destroy; override;

    function dadosAmbienteInst(): Boolean;
    function novaValidadeInst(): Boolean;

    property ideAmbiente: TIdeAmbiente read FIdeAmbiente write FIdeAmbiente;
    property dadosAmbiente: TDadosAmbiente read getDadosAmbiente write FDadosAmbiente;
    property novaValidade: TIdePeriodo read getNovaValidade write FNovaValidade;
  end;

implementation

uses
  IniFiles,
  ACBrUtil.FilesIO,
  ACBreSocial;

{ TS1060Collection }

function TS1060Collection.Add: TS1060CollectionItem;
begin
  Result := Self.New;
end;

function TS1060Collection.GetItem(Index: Integer): TS1060CollectionItem;
begin
  Result := TS1060CollectionItem(inherited Items[Index]);
end;

procedure TS1060Collection.SetItem(Index: Integer; Value: TS1060CollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TS1060Collection.New: TS1060CollectionItem;
begin
  Result := TS1060CollectionItem.Create(FACBreSocial);
  Self.Add(Result);
end;

{ TS1060CollectionItem }

constructor TS1060CollectionItem.Create(AOwner: TComponent);
begin
  inherited Create;
  FTipoEvento     := teS1060;
  FEvtTabAmbiente := TEvtTabAmbiente.Create(AOwner);
end;

destructor TS1060CollectionItem.Destroy;
begin
  FEvtTabAmbiente.Free;

  inherited;
end;

{ TInfoAmbiente }

constructor TInfoAmbiente.Create;
begin
  inherited Create;
  FIdeAmbiente   := TIdeAmbiente.Create;
  FDadosAmbiente := nil;
  FNovaValidade  := nil;
end;

function TInfoAmbiente.dadosAmbienteInst: Boolean;
begin
  Result := Assigned(FDadosAmbiente);
end;

destructor TInfoAmbiente.Destroy;
begin
  FIdeAmbiente.Free;
  FreeAndNil(FDadosAmbiente);
  FreeAndNil(FNovaValidade);

  inherited;
end;

function TInfoAmbiente.getDadosAmbiente: TDadosAmbiente;
begin
  if Not(Assigned(FDadosAmbiente)) then
    FDadosAmbiente := TDadosAmbiente.create;
  Result := FDadosAmbiente;
end;

function TInfoAmbiente.getNovaValidade: TIdePeriodo;
begin
  if Not(Assigned(FNovaValidade)) then
    FNovaValidade := TIdePeriodo.Create;
  Result := FNovaValidade;
end;

function TInfoAmbiente.novaValidadeInst: Boolean;
begin
  Result := Assigned(FNovaValidade);
end;

{ TEvtTabAmbiente }

constructor TEvtTabAmbiente.Create(AACBreSocial: TObject);
begin
  inherited Create(AACBreSocial);

  FIdeEvento     := TIdeEvento.Create;
  FIdeEmpregador := TIdeEmpregador.Create;
  FInfoAmbiente  := TInfoAmbiente.create;
end;

destructor TEvtTabAmbiente.Destroy;
begin
  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FInfoAmbiente.Free;

  inherited;
end;

procedure TEvtTabAmbiente.GerarDadosAmbiente;
begin
  Gerador.wGrupo('dadosAmbiente');

  Gerador.wCampo(tcStr, '', 'nmAmb',     1,   100, 1, infoAmbiente.dadosAmbiente.nmAmb);
  Gerador.wCampo(tcStr, '', 'dscAmb',     1, 8000, 1, infoAmbiente.dadosAmbiente.dscAmb);
  Gerador.wCampo(tcStr, '', 'localAmb',   1,    1, 1, eSLocalAmbToStr(infoAmbiente.dadosAmbiente.localAmb));
  Gerador.wCampo(tcStr, '', 'tpInsc',     1,    1, 0, eSTpInscricaoToStr(infoAmbiente.dadosAmbiente.tpInsc));
  Gerador.wCampo(tcStr, '', 'nrInsc',     1,   15, 0, infoAmbiente.dadosAmbiente.nrInsc);
  Gerador.wCampo(tcStr, '', 'codLotacao', 1,   30, 0, infoAmbiente.dadosAmbiente.codLotacao);

  Gerador.wGrupo('/dadosAmbiente');
end;

procedure TEvtTabAmbiente.GerarIdeAmbiente;
begin
  Gerador.wGrupo('ideAmbiente');

  Gerador.wCampo(tcStr, '', 'codAmb',   1, 30, 1, infoAmbiente.ideAmbiente.codAmb);
  Gerador.wCampo(tcStr, '', 'iniValid', 7,  7, 1, infoAmbiente.ideAmbiente.iniValid);
  Gerador.wCampo(tcStr, '', 'fimValid', 7,  7, 0, infoAmbiente.ideAmbiente.fimValid);

  Gerador.wGrupo('/ideAmbiente');
end;

function TEvtTabAmbiente.GerarXML: boolean;
begin
  try
    inherited GerarXML;
    Self.VersaoDF := TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF;
     
    Self.Id := GerarChaveEsocial(now, self.ideEmpregador.NrInsc, self.Sequencial);

    GerarCabecalho('evtTabAmbiente');
    Gerador.wGrupo('evtTabAmbiente Id="' + Self.Id + '"');

    GerarIdeEvento(self.IdeEvento);
    GerarIdeEmpregador(self.IdeEmpregador);

    Gerador.wGrupo('infoAmbiente');

    GerarModoAbertura(Self.ModoLancamento);
    GerarIdeAmbiente;

    if Self.ModoLancamento <> mlExclusao then
    begin
      GerarDadosAmbiente;

      if Self.ModoLancamento = mlAlteracao then
        if (infoAmbiente.novaValidadeInst()) then
          GerarIdePeriodo(infoAmbiente.novaValidade, 'novaValidade');
    end;

    GerarModoFechamento(Self.ModoLancamento);

    Gerador.wGrupo('/infoAmbiente');
    Gerador.wGrupo('/evtTabAmbiente');

    GerarRodape;

    FXML := Gerador.ArquivoFormatoXML;
//    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtTabAmbiente');

//    Validar(schevtTabAmbiente);
  except on e:exception do
    raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '')
end;

function TEvtTabAmbiente.LerArqIni(const AIniString: String): Boolean;
var
  INIRec: TMemIniFile;
  Ok: Boolean;
  sSecao{, sFim}: String;
//  I: Integer;
begin
  Result := True;

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);

    with Self do
    begin
      sSecao := 'evtTabAmbiente';
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

      sSecao := 'ideAmbiente';
      infoAmbiente.ideAmbiente.codAmb   := INIRec.ReadString(sSecao, 'codAmb', EmptyStr);
      infoAmbiente.ideAmbiente.IniValid := INIRec.ReadString(sSecao, 'iniValid', EmptyStr);
      infoAmbiente.ideAmbiente.FimValid := INIRec.ReadString(sSecao, 'fimValid', EmptyStr);

      if (ModoLancamento <> mlExclusao) then
      begin
        sSecao := 'dadosAmbiente';
        infoAmbiente.dadosAmbiente.nmAmb      := INIRec.ReadString(sSecao, 'nmAmb', EmptyStr);
        infoAmbiente.dadosAmbiente.dscAmb     := INIRec.ReadString(sSecao, 'dscAmb', EmptyStr);
        infoAmbiente.dadosAmbiente.localAmb   := eSStrToLocalAmb(Ok, INIRec.ReadString(sSecao, 'localAmb', '1'));
        infoAmbiente.dadosAmbiente.tpInsc     := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
        infoAmbiente.dadosAmbiente.nrInsc     := INIRec.ReadString(sSecao, 'nrInsc', '');
        infoAmbiente.dadosAmbiente.codLotacao := INIRec.ReadString(sSecao, 'codLotacao', '');

        if ModoLancamento = mlAlteracao then
        begin
          sSecao := 'novaValidade';
          infoAmbiente.novaValidade.IniValid := INIRec.ReadString(sSecao, 'iniValid', EmptyStr);
          infoAmbiente.novaValidade.FimValid := INIRec.ReadString(sSecao, 'fimValid', EmptyStr);
        end;
      end;
    end;

    GerarXML;
    XML := FXML;
  finally
    INIRec.Free;
  end;
end;

function TEvtTabAmbiente.LerXML: Boolean;
var
  Leitor: TLeitor;
  bOK: Boolean;
begin
  Result := True;
  Leitor := TLeitor.Create;
  Leitor.Arquivo := XML;
  Leitor.Grupo := Leitor.Arquivo;
  try
    if Leitor.rExtrai(1, 'ideEvento') <> '' then
    begin
      Self.FIdeEvento.ProcEmi := eSStrToprocEmi(bOK, Leitor.rCampo(tcStr, 'procEmi'));
      Self.FIdeEvento.VerProc := Leitor.rCampo(tcStr, 'verProc');
    end;

    if Leitor.rExtrai(1, 'ideEmpregador') <> '' then
    begin
      Self.FIdeEmpregador.TpInsc := eSStrToTpInscricao(bOK, Leitor.rCampo(tcStr, 'tpInsc'));
      Self.FIdeEmpregador.NrInsc := Leitor.rCampo(tcStr, 'nrInsc');
    end;

    if Leitor.rExtrai(1, 'dadosAmbiente') <> '' then
    begin
      Self.FInfoAmbiente.DadosAmbiente.nmAmb      := Leitor.rCampo(tcStr, 'nmAmb');
      Self.FInfoAmbiente.DadosAmbiente.dscAmb     := Leitor.rCampo(tcStr, 'dscAmb');
      Self.FInfoAmbiente.DadosAmbiente.localAmb   := eSStrToLocalAmb(bOK, Leitor.rCampo(tcStr, 'localAmb'));
      Self.FInfoAmbiente.DadosAmbiente.tpInsc     := eSStrToTpInscricao(bOK, Leitor.rCampo(tcStr, 'tpInsc'));
      Self.FInfoAmbiente.DadosAmbiente.nrInsc     := Leitor.rCampo(tcStr, 'nrInsc');
      Self.FInfoAmbiente.DadosAmbiente.codLotacao := Leitor.rCampo(tcStr, 'codLotacao');
    end;

    if Leitor.rExtrai(1, 'ideAmbiente') <> '' then
    begin
      Self.FInfoAmbiente.ideAmbiente.codAmb   := Leitor.rCampo(tcStr, 'codAmb');
      Self.FInfoAmbiente.ideAmbiente.iniValid := Leitor.rCampo(tcStr, 'iniValid');
      Self.FInfoAmbiente.ideAmbiente.fimValid := Leitor.rCampo(tcStr, 'fimValid');
    end;

    Self.ModoLancamento := mlInclusao;

    if Leitor.rExtrai(1, 'alteracao') <> '' then
    begin
      Self.ModoLancamento := mlAlteracao;

      if Leitor.rExtrai(1, 'alteracao') <> '' then
      begin
        Self.infoAmbiente.novaValidade.IniValid :=  Leitor.rCampo(tcStr, 'iniValid');
        Self.infoAmbiente.novaValidade.FimValid :=  Leitor.rCampo(tcStr, 'fimValid');
      end;
    end
    else if Leitor.rExtrai(1, 'exclusao') <> '' then
    begin
      Self.ModoLancamento := mlExclusao;
    end;
  finally
    Leitor.Free;
  end;
end;

end.
