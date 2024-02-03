{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Renato Tanchela Rubinho                         }
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

{$I ACBr.inc}

unit pcnReinfR1050;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IFEND}
  ACBrBase,
  pcnConversao, pcnGerador,
  ACBrUtil.Strings, ACBrUtil.FilesIO, ACBrUtil.Base, ACBrUtil.DateTime,
  pcnCommonReinf, pcnConversaoReinf, pcnGeradorReinf;

type
  {Classes específicas deste evento}

  TR1050Collection = class;
  TR1050CollectionItem = class;
  TevtTabLig = class;
  TinfoLig = class;
  TideEntLig = class;

  TevtTabLig = class(TReinfEvento) //Classe do elemento principal do XML do evento!
  private
    FModoLancamento: TTipoOperacao;
    FIdeEvento: TIdeEvento;
    FideContri: TideContri;
    FinfoLig: TinfoLig;

    {Geradores específicos desta classe}
    procedure GerarideEntLig(pEmp: TideEntLig);
  public
    constructor Create(AACBrReinf: TObject); override;
    destructor  Destroy; override;

    function GerarXML: Boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property ModoLancamento: TTipoOperacao read FModoLancamento write FModoLancamento;
    property ideEvento: TIdeEvento read FIdeEvento write FIdeEvento;
    property ideContri: TideContri read FideContri write FideContri;
    property infoLig: TinfoLig read FinfoLig write FinfoLig;
  end;

  TR1050CollectionItem = class(TObject)
  private
    FTipoEvento: TTipoEvento;
    FevtTabLig: TevtTabLig;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    property TipoEvento: TTipoEvento read FTipoEvento;
    property evtTabLig: TevtTabLig read FevtTabLig write FevtTabLig;
  end;

  TR1050Collection = class(TReinfCollection)
  private
    function GetItem(Index: Integer): TR1050CollectionItem;
    procedure SetItem(Index: Integer; Value: TR1050CollectionItem);
  public
    function Add: TR1050CollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TR1050CollectionItem;

    property Items[Index: Integer]: TR1050CollectionItem read GetItem write SetItem; default;
  end;

  TinfoLig = class(TObject)
  private
    FideEntLig: TideEntLig;
    FNovaValidade: TidePeriodo;

    function getNovaValidade: TIdePeriodo;
  public
    constructor Create;
    destructor Destroy; override;

    function novaValidadeInst(): Boolean;

    property ideEntLig: TideEntLig read FideEntLig write FideEntLig;
    property novaValidade: TIdePeriodo read getNovaValidade write FnovaValidade;
  end;

  TideEntLig = class(TObject)
  private
    FtpEntLig: TtpEntLig;
    FcnpjLig: string;
    FIniValid: string;
    FFimValid: string;
  public
    property tpEntLig: TtpEntLig read FtpEntLig write FtpEntLig;
    property cnpjLig: string read FcnpjLig write FcnpjLig;
    property IniValid: string read FIniValid write FIniValid;
    property FimValid: string read FFimValid write FFimValid;
  end;

implementation

uses
  IniFiles,
  ACBrReinf, ACBrDFeUtil;

{ TR1050Collection }

function TR1050Collection.Add: TR1050CollectionItem;
begin
  Result := Self.New;
end;

function TR1050Collection.GetItem(Index: Integer): TR1050CollectionItem;
begin
  Result := TR1050CollectionItem(inherited Items[Index]);
end;

function TR1050Collection.New: TR1050CollectionItem;
begin
  Result := TR1050CollectionItem.Create(FACBrReinf);
  Self.Add(Result);
end;

procedure TR1050Collection.SetItem(Index: Integer; Value: TR1050CollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TR1050CollectionItem }

constructor TR1050CollectionItem.Create(AOwner: TComponent);
begin
  inherited Create;

  FTipoEvento     := teR1050;
  FevtTabLig := TevtTabLig.Create(AOwner);
end;

destructor TR1050CollectionItem.Destroy;
begin
  inherited;

  FevtTabLig.Free;
end;

procedure TevtTabLig.GerarideEntLig(pEmp: TideEntLig);
begin
  Gerador.wGrupo('ideEntLig');

  if ModoLancamento <> toExclusao then
    Gerador.wCampo(tcStr, '', 'tpEntLig',  1,  1, 1, tpEntLigToStr(pEmp.tpEntLig));

  Gerador.wCampo(tcStr, '', 'cnpjLig',   1, 14, 1, pEmp.cnpjLig);
  Gerador.wCampo(tcStr, '', 'iniValid',  7,  7, 1, pEmp.IniValid);
  Gerador.wCampo(tcStr, '', 'fimValid',  7,  7, 0, pEmp.FimValid);
  Gerador.wGrupo('/ideEntLig');
end;

constructor TevtTabLig.Create(AACBrReinf: TObject);
begin
  inherited;

  FideContri := TideContri.Create;
  FIdeEvento := TIdeEvento.Create;
  FinfoLig   := TinfoLig.Create;
end;

destructor TevtTabLig.Destroy;
begin
  FideContri.Free;
  FIdeEvento.Free;
  FinfoLig.Free;

  inherited;
end;

function TevtTabLig.GerarXML: Boolean;
begin
  try
    Self.VersaoDF := TACBrReinf(FACBrReinf).Configuracoes.Geral.VersaoDF;

    Self.Id := GerarChaveReinf(now, self.ideContri.NrInsc, self.Sequencial, self.ideContri.TpInsc);

    GerarCabecalho('evt1050TabLig');
    Gerador.wGrupo('evtTabLig id="' + Self.Id + '"');

    GerarIdeEvento(Self.IdeEvento);
    GerarideContri(Self.ideContri);

    Gerador.wGrupo('infoLig');

    GerarModoAbertura(Self.ModoLancamento);
    
    GerarideEntLig(Self.infoLig.ideEntLig);

    if (ModoLancamento = toAlteracao) and (infoLig.novaValidadeInst()) then
      GerarIdePeriodo(infoLig.novaValidade, 'novaValidade');

    GerarModoFechamento(Self.ModoLancamento);

    Gerador.wGrupo('/infoLig');
    Gerador.wGrupo('/evtTabLig');

    GerarRodape;

    FXML := Gerador.ArquivoFormatoXML;
  except on e:exception do
    raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '');
end;

function TevtTabLig.LerArqIni(const AIniString: String): Boolean;
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
      sSecao := 'evtTabLig';
      Id             := INIRec.ReadString(sSecao, 'Id', '');
      Sequencial     := INIRec.ReadInteger(sSecao, 'Sequencial', 0);
      ModoLancamento := StrToTipoOperacao(Ok, INIRec.ReadString(sSecao, 'ModoLancamento', 'inclusao'));

      sSecao := 'ideEvento';
      ideEvento.ProcEmi := StrToProcEmiReinf(Ok, INIRec.ReadString(sSecao, 'procEmi', '1'));
      ideEvento.VerProc := INIRec.ReadString(sSecao, 'verProc', EmptyStr);

      sSecao := 'ideContri';
      ideContri.OrgaoPublico := (TACBrReinf(FACBrReinf).Configuracoes.Geral.TipoContribuinte = tcOrgaoPublico);
      ideContri.TpInsc       := StrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
      ideContri.NrInsc       := INIRec.ReadString(sSecao, 'nrInsc', EmptyStr);

      sSecao := 'ideEntLig';

      if ModoLancamento <> toExclusao then
        infoLig.ideEntLig.tpEntLig := StrToTpEntLig(Ok, INIRec.ReadString(sSecao, 'tpEntLig', '1'));

      infoLig.ideEntLig.cnpjLig  := INIRec.ReadString(sSecao, 'cnpjLig', EmptyStr);
      infoLig.ideEntLig.IniValid := INIRec.ReadString(sSecao, 'iniValid', EmptyStr);
      infoLig.ideEntLig.FimValid := INIRec.ReadString(sSecao, 'fimValid', EmptyStr);

      if ModoLancamento = toAlteracao then
      begin
        sSecao := 'novaValidade';
        infoLig.novaValidade.IniValid := INIRec.ReadString(sSecao, 'iniValid', EmptyStr);
        infoLig.novaValidade.FimValid := INIRec.ReadString(sSecao, 'fimValid', EmptyStr);
      end;
    end;

    GerarXML;
    XML := FXML;
  finally
    INIRec.Free;
  end;
end;

{ TinfoLig }

constructor TinfoLig.Create;
begin
  FideEntLig := TideEntLig.Create;
  FNovaValidade := nil;
end;

destructor TinfoLig.Destroy;
begin
  FideEntLig.Free;
  FreeAndNil(FNovaValidade);

  inherited;
end;

function TinfoLig.getNovaValidade: TIdePeriodo;
begin
  if not(Assigned(FNovaValidade)) then
    FNovaValidade := TIdePeriodo.Create;
  Result := FNovaValidade;
end;

function TinfoLig.novaValidadeInst: Boolean;
begin
  Result := Assigned(FNovaValidade);
end;

end.
