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

unit pcesS1299;

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
  TS1299CollectionItem = class;
  TEvtFechaEvPer = class;
  TInfoFech = class;

  TS1299Collection = class(TeSocialCollection)
  private
    function GetItem(Index: Integer): TS1299CollectionItem;
    procedure SetItem(Index: Integer; Value: TS1299CollectionItem);
  public
    function Add: TS1299CollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TS1299CollectionItem;
    property Items[Index: Integer]: TS1299CollectionItem read GetItem write SetItem; default;
  end;

  TS1299CollectionItem = class(TObject)
  private
    FTipoEvento: TTipoEvento;
    FEvtFechaEvPer: TEvtFechaEvPer;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    property TipoEvento: TTipoEvento read FTipoEvento;
    property EvtFechaEvPer: TEvtFechaEvPer read FEvtFechaEvPer write FEvtFechaEvPer;
  end;

  TEvtFechaEvPer = class(TESocialEvento)
  private
    FIdeEvento: TIdeEvento4;
    FIdeEmpregador: TIdeEmpregador;
    FIdeRespInf : TIdeRespInf;
    FInfoFech: TInfoFech;

    {Geradores específicos da classe}
    procedure GerarInfoFech;
  public
    constructor Create(AACBreSocial: TObject); override;
    destructor  Destroy; override;

    function GerarXML: boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property IdeEvento: TIdeEvento4 read FIdeEvento write FIdeEvento;
    property IdeEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property IdeRespInf: TIdeRespInf read FIdeRespInf write FIdeRespInf;
    property InfoFech: TInfoFech read FInfoFech write FInfoFech;
  end;

  TInfoFech = class
  private
    FevtRemun: TpSimNao;
    FevtPgtos: TpSimNao;
    FevtAqProd: TpSimNao;
    FevtComProd: TpSimNao;
    FevtContratAvNP: TpSimNao;
    FevtInfoComplPer: TpSimNao;
    FcompSemMovto : string;
    FindExcApur1250: tpSimNaoFacultativo;
    FtransDCTFWeb: tpSimNaoFacultativo;
    FNaoValid: TpSimNaoFacultativo;
  public
    constructor create;
    destructor Destroy; override;

    property evtRemun: TpSimNao read FevtRemun write FevtRemun;
    property evtPgtos: TpSimNao read FevtPgtos write FevtPgtos;
    property evtAqProd: TpSimNao read FevtAqProd write FevtAqProd;
    property evtComProd: TpSimNao read FevtComProd write FevtComProd;
    property evtContratAvNP: TpSimNao read FevtContratAvNP write FevtContratAvNP;
    property evtInfoComplPer: TpSimNao read FevtInfoComplPer write FevtInfoComplPer;
    property compSemMovto : string read FcompSemMovto write FcompSemMovto;
    property indExcApur1250: tpSimNaoFacultativo read FindExcApur1250 write FindExcApur1250;
    property transDCTFWeb: tpSimNaoFacultativo read FtransDCTFWeb write FtransDCTFWeb;
    property naoValid: TpSimNaoFacultativo read FNaoValid write FNaoValid;
  end;

implementation

uses
  IniFiles,
  ACBrUtil.FilesIO,
  ACBreSocial;

{ TS1299Collection }

function TS1299Collection.Add: TS1299CollectionItem;
begin
  Result := Self.New;
end;

function TS1299Collection.GetItem(Index: Integer): TS1299CollectionItem;
begin
  Result := TS1299CollectionItem(inherited Items[Index]);
end;

procedure TS1299Collection.SetItem(Index: Integer; Value: TS1299CollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TS1299Collection.New: TS1299CollectionItem;
begin
  Result := TS1299CollectionItem.Create(FACBreSocial);
  Self.Add(Result);
end;

{TS1299CollectionItem}
constructor TS1299CollectionItem.Create(AOwner: TComponent);
begin
  inherited Create;
  FTipoEvento    := teS1299;
  FEvtFechaEvPer := TEvtFechaEvPer.Create(AOwner);
end;

destructor TS1299CollectionItem.Destroy;
begin
  FEvtFechaEvPer.Free;

  inherited;
end;

{ TEvtSolicTotal }
constructor TEvtFechaEvPer.Create(AACBreSocial: TObject);
begin
  inherited Create(AACBreSocial);

  FIdeEvento     := TIdeEvento4.Create;
  FIdeEmpregador := TIdeEmpregador.Create;
  FIdeRespInf    := TIdeRespInf.Create;
  FInfoFech      := TInfoFech.Create;
end;

destructor TEvtFechaEvPer.destroy;
begin
  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FIdeRespInf.Free;
  FInfoFech.Free;

  inherited;
end;

procedure TEvtFechaEvPer.GerarInfoFech;
begin
  Gerador.wGrupo('infoFech');

  Gerador.wCampo(tcStr, '', 'evtRemun',        1, 1, 1, eSSimNaoToStr(self.InfoFech.evtRemun));

  if VersaoDF >= veS01_01_00 then
    Gerador.wCampo(tcStr, '', 'evtPgtos',      1, 1, 1, eSSimNaoToStr(self.InfoFech.evtPgtos));

  if VersaoDF <= ve02_05_00 then
    Gerador.wCampo(tcStr, '', 'evtAqProd',     1, 1, 1, eSSimNaoToStr(self.InfoFech.evtAqProd));
  
  Gerador.wCampo(tcStr, '', 'evtComProd',      1, 1, 1, eSSimNaoToStr(self.InfoFech.evtComProd));
  Gerador.wCampo(tcStr, '', 'evtContratAvNP',  1, 1, 1, eSSimNaoToStr(self.InfoFech.evtContratAvNP));
  Gerador.wCampo(tcStr, '', 'evtInfoComplPer', 1, 1, 1, eSSimNaoToStr(self.InfoFech.evtInfoComplPer));

  if VersaoDF <= ve02_05_00 then
  begin
    if ((eSSimNaoToStr(self.InfoFech.evtRemun)        = 'N') and
        (eSSimNaoToStr(self.InfoFech.evtPgtos)        = 'N') and
        (eSSimNaoToStr(self.InfoFech.evtAqProd)       = 'N') and
        (eSSimNaoToStr(self.InfoFech.evtComProd)      = 'N') and
        (eSSimNaoToStr(self.InfoFech.evtContratAvNP)  = 'N') and
        (eSSimNaoToStr(self.InfoFech.evtInfoComplPer) = 'N')) then
      Gerador.wCampo(tcStr, '', 'compSemMovto', 1, 7, 0, self.InfoFech.compSemMovto);
  end;

  if (VersaoDF >= ve02_05_00) and
     (Self.InfoFech.indExcApur1250 <> snfNada) and
     ((Self.ideEvento.IndApuracao = iapuMensal) and
      (Copy(Self.ideEvento.perApur,1,4)+Copy(Self.ideEvento.perApur,6,2) <= '202106'))
  then
    Gerador.wCampo(tcStr, '', 'indExcApur1250', 1, 1, 1, eSSimNaoFacultativoToStr(self.InfoFech.indExcApur1250));

  if VersaoDF >= veS01_00_00 then
  begin
    if Self.infoFech.transDCTFWeb = snfSim then
      Gerador.wCampo(tcStr, '', 'transDCTFWeb', 1, 1, 1, eSSimNaoFacultativoToStr(self.infoFech.transDCTFWeb));

    if Self.infoFech.naoValid = snfSim then
      Gerador.wCampo(tcStr, '', 'naoValid',     1, 1, 1, eSSimNaoFacultativoToStr(self.infoFech.naoValid));
  end;

  Gerador.wGrupo('/infoFech');
end;

function TEvtFechaEvPer.GerarXML: boolean;
begin
  try
    inherited GerarXML;
    Self.VersaoDF := TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF;

    Self.Id := GerarChaveEsocial(now, self.ideEmpregador.NrInsc, self.Sequencial);

    GerarCabecalho('evtFechaEvPer');
    Gerador.wGrupo('evtFechaEvPer Id="' + Self.Id + '"');

    GerarIdeEvento4(self.IdeEvento);
    GerarIdeEmpregador(self.IdeEmpregador);

    if VersaoDF <= ve02_05_00 then
      GerarIdeRespInf(Self.IdeRespInf);

    GerarInfoFech;

    Gerador.wGrupo('/evtFechaEvPer');

    GerarRodape;

    FXML := Gerador.ArquivoFormatoXML;
//    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtFechaEvPer');
//    Validar(schevtFechaEvPer);
  except on e:exception do
    raise Exception.Create('ID: ' + Self.Id + sLineBreak +'Nome Resp.: ' + Self.FIdeRespInf.nmResp + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '')
end;

{ TInfoFech }

constructor TInfoFech.create;
begin
  inherited;
end;

destructor TInfoFech.destroy;
begin
  inherited;
end;

function TEvtFechaEvPer.LerArqIni(const AIniString: String): Boolean;
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
      sSecao := 'evtFechaEvPer';
      Id         := INIRec.ReadString(sSecao, 'Id', '');
      Sequencial := INIRec.ReadInteger(sSecao, 'Sequencial', 0);

      sSecao := 'ideEvento';
      ideEvento.IndApuracao := eSStrToIndApuracao(Ok, INIRec.ReadString(sSecao, 'indApuracao', '1'));
      ideEvento.perApur     := INIRec.ReadString(sSecao, 'perApur', EmptyStr);
      ideEvento.indGuia     := INIRec.ReadString(sSecao, 'indGuia', EmptyStr);
      ideEvento.ProcEmi     := eSStrToProcEmi(Ok, INIRec.ReadString(sSecao, 'procEmi', '1'));
      ideEvento.VerProc     := INIRec.ReadString(sSecao, 'verProc', EmptyStr);

      sSecao := 'ideEmpregador';
      ideEmpregador.OrgaoPublico := (TACBreSocial(FACBreSocial).Configuracoes.Geral.TipoEmpregador = teOrgaoPublico);
      ideEmpregador.TpInsc       := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
      ideEmpregador.NrInsc       := INIRec.ReadString(sSecao, 'nrInsc', EmptyStr);

      sSecao := 'ideRespInf';
      if INIRec.ReadString(sSecao, 'nmResp', '') <> '' then
      begin
        ideRespInf.nmResp   := INIRec.ReadString(sSecao, 'nmResp', EmptyStr);
        ideRespInf.cpfResp  := INIRec.ReadString(sSecao, 'cpfResp', EmptyStr);
        ideRespInf.telefone := INIRec.ReadString(sSecao, 'telefone', EmptyStr);
        ideRespInf.email    := INIRec.ReadString(sSecao, 'email', EmptyStr);
      end;

      sSecao := 'infoFech';
      infoFech.evtRemun        := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'evtRemun', 'S'));
      infoFech.evtPgtos        := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'evtPgtos', 'S'));
      infoFech.evtAqProd       := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'evtAqProd', 'S'));
      infoFech.evtComProd      := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'evtComProd', 'S'));
      infoFech.evtContratAvNP  := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'evtContratAvNP', 'S'));
      infoFech.evtInfoComplPer := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'evtInfoComplPer', 'S'));
      infoFech.compSemMovto    := INIRec.ReadString(sSecao, 'compSemMovto', '');
      infoFech.indExcApur1250  := eSStrToSimNaoFacultativo(Ok, INIRec.ReadString(sSecao, 'indExcApur1250', 'S'));
      infoFech.transDCTFWeb    := eSStrToSimNaoFacultativo(Ok, INIRec.ReadString(sSecao, 'transDCTFWeb', 'N'));
      infoFech.naoValid        := eSStrToSimNaoFacultativo(Ok, INIRec.ReadString(sSecao, 'naoValid', 'S'));
    end;

    GerarXML;
    XML := FXML;
  finally
    INIRec.Free;
  end;
end;

end.
