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

unit pcesS2190;

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
  TS2190CollectionItem = class;
  TEvtAdmPrelim = class;
  TInfoRegPrelim = class;

  TS2190Collection = class(TeSocialCollection)
  private
    function GetItem(Index: Integer): TS2190CollectionItem;
    procedure SetItem(Index: Integer; Value: TS2190CollectionItem);
  public
    function Add: TS2190CollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TS2190CollectionItem;
    property Items[Index: Integer]: TS2190CollectionItem read GetItem write SetItem; default;
  end;

  TS2190CollectionItem = class(TObject)
  private
    FTipoEvento: TTipoEvento;
    FEvtAdmPrelim: TEvtAdmPrelim;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    property TipoEvento: TTipoEvento read FTipoEvento;
    property EvtAdmPrelim: TEvtAdmPrelim read FEvtAdmPrelim write  FEvtAdmPrelim;
  end;

  TEvtAdmPrelim = class(TeSocialEvento)
  private
    FIdeEvento: TIdeEvento2;
    FIdeEmpregador: TIdeEmpregador;
    FInfoRegPrelim: TInfoRegPrelim;

    procedure GerarInfoRegPrelim;
  public
    constructor Create(AACBreSocial: TObject); override;
    destructor Destroy; override;

    function GerarXML: boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property IdeEvento: TIdeEvento2 read FIdeEvento write FIdeEvento;
    property IdeEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property InfoRegPrelim: TInfoRegPrelim read FInfoRegPrelim write FInfoRegPrelim;
  end;
  
  TInfoRegPrelim = class(TObject)
  private
    FcpfTrab: string;
    FdtNascto: TDateTime;
    FdtAdm: TDateTime;
    Fmatricula: String;
    FcodCateg: Integer;
    FnatAtividade: tpNatAtividade;
    FinfoRegCTPS: TinfoRegCTPS;
  public
    constructor Create;
    destructor Destroy; override;

    property cpfTrab: string read FcpfTrab write FcpfTrab;
    property dtNascto: TDateTime read FdtNascto write FdtNascto;
    property dtAdm: TDateTime read FdtAdm write FdtAdm;
    property matricula: String read Fmatricula write Fmatricula;
    property codCateg: Integer read FcodCateg write FcodCateg;
    property natAtividade: tpNatAtividade read FnatAtividade write FnatAtividade;
    property infoRegCTPS: TinfoRegCTPS read FinfoRegCTPS write FinfoRegCTPS;
  end;

implementation

uses
  IniFiles,
  ACBrUtil.Base,
  ACBrUtil.FilesIO,
  ACBrUtil.DateTime,
  ACBreSocial;

{ TS2190Collection }

function TS2190Collection.Add: TS2190CollectionItem;
begin
  Result := Self.New;
end;

function TS2190Collection.GetItem(Index: Integer): TS2190CollectionItem;
begin
  Result := TS2190CollectionItem(inherited Items[Index]);
end;

procedure TS2190Collection.SetItem(Index: Integer;
  Value: TS2190CollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TS2190Collection.New: TS2190CollectionItem;
begin
  Result := TS2190CollectionItem.Create(FACBreSocial);
  Self.Add(Result);
end;

{ TS2190CollectionItem }
constructor TS2190CollectionItem.Create(AOwner: TComponent);
begin
  inherited Create;
  
  FTipoEvento := teS2190;
  FEvtAdmPrelim := TEvtAdmPrelim.Create(AOwner);
end;

destructor TS2190CollectionItem.Destroy;
begin
  FEvtAdmPrelim.Free;

  inherited;
end;

{ TEvtAdmissao }
constructor TEvtAdmPrelim.Create(AACBreSocial: TObject);
begin
  inherited Create(AACBreSocial);

  FIdeEvento     := TIdeEvento2.Create;
  FIdeEmpregador := TIdeEmpregador.Create;
  FInfoRegPrelim := TInfoRegPrelim.Create;
end;

destructor TEvtAdmPrelim.Destroy;
begin
  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FInfoRegPrelim.Free;

  inherited;
end;

{ TInfoRegPrelim }

constructor TInfoRegPrelim.Create;
begin
  inherited Create;
  
  FinfoRegCTPS := TinfoRegCTPS.Create;
end;

destructor TInfoRegPrelim.Destroy;
begin
  FinfoRegCTPS.Free;

  inherited;
end;

procedure TEvtAdmPrelim.GerarInfoRegPrelim;
begin
  Gerador.wGrupo('infoRegPrelim');

  Gerador.wCampo(tcStr, '', 'cpfTrab',  11, 11, 1, infoRegPrelim.cpfTrab);
  Gerador.wCampo(tcDat, '', 'dtNascto', 10, 10, 1, infoRegPrelim.dtNascto);
  Gerador.wCampo(tcDat, '', 'dtAdm',    10, 10, 1, infoRegPrelim.dtAdm);

  if VersaoDF > ve02_05_00 then
  begin
    Gerador.wCampo(tcStr, '', 'matricula', 1, 30, 1, infoRegPrelim.matricula);
    Gerador.wCampo(tcInt, '', 'codCateg',  1,  3, 1, infoRegPrelim.codCateg);

    if infoRegPrelim.natAtividade <> navNaoInformar then
     Gerador.wCampo(tcStr, '', 'natAtividade',  1,  1, 1, eSNatAtividadeToStr(infoRegPrelim.natAtividade));

    if infoRegPrelim.infoRegCTPS.CBOCargo <> '' then
    begin
      Gerador.wGrupo('infoRegCTPS');

      Gerador.wCampo(tcStr, '', 'CBOCargo',   6,  6, 1, infoRegPrelim.infoRegCTPS.CBOCargo);
      Gerador.wCampo(tcDe2, '', 'vrSalFx',    1, 14, 1, infoRegPrelim.infoRegCTPS.VrSalFx);
      Gerador.wCampo(tcStr, '', 'undSalFixo', 1,  1, 1, eSUndSalFixoToStr(infoRegPrelim.infoRegCTPS.UndSalFixo));
      Gerador.wCampo(tcStr, '', 'tpContr',    1,  1, 1, eSTpContrToStr(infoRegPrelim.infoRegCTPS.tpContr));

      if infoRegPrelim.infoRegCTPS.dtTerm <> 0 then
        Gerador.wCampo(tcDat, '', 'dtTerm',    10, 10, 0, infoRegPrelim.infoRegCTPS.dtTerm);
    
      Gerador.wGrupo('/infoRegCTPS');
    end;
  end;

  Gerador.wGrupo('/infoRegPrelim');
end;

function TEvtAdmPrelim.GerarXML: boolean;
begin
  try
    inherited GerarXML;
    Self.VersaoDF := TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF;
     
    Self.Id := GerarChaveEsocial(now, self.ideEmpregador.NrInsc, self.Sequencial);

    GerarCabecalho('evtAdmPrelim');
    Gerador.wGrupo('evtAdmPrelim Id="' + Self.Id + '"');

    if VersaoDF > ve02_05_00 then
      GerarIdeEvento2(self.IdeEvento)
    else
      GerarIdeEvento(self.IdeEvento);
      
    GerarIdeEmpregador(self.IdeEmpregador);
    GerarInfoRegPrelim;

    Gerador.wGrupo('/evtAdmPrelim');

    GerarRodape;

    FXML := Gerador.ArquivoFormatoXML;
//    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtAdmPrelim');

//    Validar(schevtAdmPrelim);
  except on e:exception do
    raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '')
end;

function TEvtAdmPrelim.LerArqIni(const AIniString: String): Boolean;
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
      sSecao := 'evtAdmPrelim';
      Id         := INIRec.ReadString(sSecao, 'Id', '');
      Sequencial := INIRec.ReadInteger(sSecao, 'Sequencial', 0);

      sSecao := 'ideEvento';
      ideEvento.ProcEmi     := eSStrToProcEmi(Ok, INIRec.ReadString(sSecao, 'procEmi', '1'));
      ideEvento.VerProc     := INIRec.ReadString(sSecao, 'verProc', EmptyStr);
      ideEvento.indRetif    := eSStrToIndRetificacao(ok, INIRec.ReadString(sSecao, 'indRetif', '1'));
      ideEvento.NrRecibo    := INIRec.ReadString(sSecao, 'nrRecibo', EmptyStr);

      sSecao := 'ideEmpregador';
      ideEmpregador.OrgaoPublico := (TACBreSocial(FACBreSocial).Configuracoes.Geral.TipoEmpregador = teOrgaoPublico);
      ideEmpregador.TpInsc       := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
      ideEmpregador.NrInsc       := INIRec.ReadString(sSecao, 'nrInsc', EmptyStr);

      sSecao := 'infoRegPrelim';
      infoRegPrelim.cpfTrab       := INIRec.ReadString(sSecao, 'cpfTrab', EmptyStr);
      infoRegPrelim.dtNascto      := StringToDateTime(INIRec.ReadString(sSecao, 'dtNascto', '0'));
      infoRegPrelim.dtAdm         := StringToDateTime(INIRec.ReadString(sSecao, 'dtAdm', '0'));
      infoRegPrelim.codCateg      := INIRec.ReadInteger(sSecao, 'codCateg', 0);
      infoRegPrelim.matricula     := INIRec.ReadString(sSecao, 'matricula', EmptyStr);
      infoRegPrelim.natAtividade  := eSStrToNatAtividade(ok, INIRec.ReadString(sSecao, 'natAtividade', '1'));

      sSecao := 'infoRegCTPS';
      InfoRegPrelim.infoRegCTPS.CBOCargo  := INIRec.ReadString(sSecao, 'CBOCargo', EmptyStr);
      InfoRegPrelim.infoRegCTPS.vrSalFx   := StringToFloatDef(INIRec.ReadString(sSecao, 'vrSalFx', ''), 0);
      InfoRegPrelim.infoRegCTPS.undSalFixo:= eSStrToUndSalFixo(Ok, INIRec.ReadString(sSecao, 'undSalFixo', '1'));
      InfoRegPrelim.infoRegCTPS.tpContr   := eSStrToTpContr(Ok, INIRec.ReadString(sSecao, 'tpContr', '1'));
      InfoRegPrelim.infoRegCTPS.dtTerm    := StringToDateTime(INIRec.ReadString(sSecao, 'dtTerm', '0'));
    end;

    GerarXML;
    XML := FXML;
  finally
    INIRec.Free;
  end;
end;

end.
