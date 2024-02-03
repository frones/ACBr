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

unit pcesS2400;

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
  ACBrUtil.Base,
  ACBrUtil.FilesIO,
  ACBrUtil.DateTime,
  pcesCommon, pcesConversaoeSocial, pcesGerador;

type
  TS2400Collection = class;
  TS2400CollectionItem = class;
  TEvtCdBenefIn = class;
  TBeneficiario = class;

  TS2400Collection = class(TeSocialCollection)
  private
    function GetItem(Index: Integer): TS2400CollectionItem;
    procedure SetItem(Index: Integer; Value: TS2400CollectionItem);
  public
    function Add: TS2400CollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TS2400CollectionItem;
    property Items[Index: Integer]: TS2400CollectionItem read GetItem write SetItem; default;
  end;

  TS2400CollectionItem = class(TObject)
  private
    FTipoEvento: TTipoEvento;
    FEvtCdBenefIn : TEvtCdBenefIn;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    property TipoEvento: TTipoEvento read FTipoEvento;
    property EvtCdBenefIn: TEvtCdBenefIn read FEvtCdBenefIn write FEvtCdBenefIn;
  end;

  TEvtCdBenefIn = class(TeSocialEvento)
  private
    FIdeEvento: TIdeEvento2;
    FIdeEmpregador: TIdeEmpregador;
    FBeneficiario: TBeneficiario;
    
    procedure GerarBeneficiario(pBeneficiario: TBeneficiario);
  public
    constructor Create(AACBreSocial: TObject); override;
    destructor Destroy; override;

    function GerarXML: boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property IdeEvento: TIdeEvento2 read FIdeEvento write FIdeEvento;
    property IdeEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property Beneficiario: TBeneficiario read FBeneficiario write FBeneficiario;
  end;

  TBeneficiario = class(TObject)
  private
    FCpfBenef: string;
    FNmBenefic: string;
    FDtNascto: TDateTime;
    FDtInicio: TDateTime;
    FSexo: string;
    FRacaCor: integer;
    FEstCiv: integer;
    FIncFisMen: TpSimNao;
    FDtIncFisMen: TDateTime;
    FEndereco: TEndereco;
    FDependente: TDependenteCollection;
  public
    constructor Create;
    destructor Destroy; override;
    
    property cpfBenef: String read FCpfBEnef write FCpfBEnef;
    property nmBenefic: string read FNmBenefic write FNmBenefic;
    property dtNascto: TDateTime read FDtNascto write FDtNascto;
    property dtInicio: TDateTime read FDtInicio write FDtInicio;
    property sexo: string read FSexo write FSexo;
    property racaCor: integer read FRacaCor write FRacaCor;
    property estCiv: integer read FEstCiv write FEstCiv;
    property incFisMen: TpSimNao read FIncFisMen write FIncFisMen;
    property dtIncFisMen: TDateTime read FDtIncFisMen write FDtIncFisMen;
    property endereco: TEndereco read FEndereco write FEndereco;
    property dependente: TDependenteCollection read FDependente write FDependente;
  end;

implementation

uses
  IniFiles,
  ACBreSocial;

{ TS2400Collection }

function TS2400Collection.Add: TS2400CollectionItem;
begin
  Result := Self.New;
end;

function TS2400Collection.GetItem(Index: Integer): TS2400CollectionItem;
begin
  Result := TS2400CollectionItem(inherited Items[Index]);
end;

procedure TS2400Collection.SetItem(Index: Integer; Value: TS2400CollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TS2400Collection.New: TS2400CollectionItem;
begin
  Result := TS2400CollectionItem.Create(FACBreSocial);
  Self.Add(Result);
end;

{ TS2400CollectionItem }

constructor TS2400CollectionItem.Create(AOwner: TComponent);
begin
  inherited Create;
  FTipoEvento   := teS2400;
  FEvtCdBenefIn := TEvtCdBenefIn.Create(AOwner);
end;

destructor TS2400CollectionItem.Destroy;
begin
  FEvtCdBenefIn.Free;

  inherited;
end;

{ TBeneficiario }

constructor TBeneficiario.Create;
begin
  inherited Create;
  FEndereco := TEndereco.Create;
  FDependente := TDependenteCollection.Create;
end;

destructor TBeneficiario.Destroy;
begin
  FEndereco.Free;
  FDependente.Free;
  inherited;
end;

{ TEvtCdBenefIn }

constructor TEvtCdBenefIn.Create(AACBreSocial: TObject);
begin
  inherited Create(AACBreSocial);

  FIdeEvento     := TIdeEvento2.Create;
  FIdeEmpregador := TIdeEmpregador.Create;
  FBeneficiario  := TBeneficiario.Create;
end;

destructor TEvtCdBenefIn.Destroy;
begin
  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FBeneficiario.Free;

  inherited;
end;

procedure TEvtCdBenefIn.GerarBeneficiario(pBeneficiario: TBeneficiario);
begin
  Gerador.wGrupo('beneficiario');

  Gerador.wCampo(tcStr, '', 'cpfBenef',    11, 11, 1, pBeneficiario.cpfBenef);
  Gerador.wCampo(tcStr, '', 'nmBenefic',   70, 70, 1, pBeneficiario.nmBenefic);
  Gerador.wCampo(tcDat, '', 'dtNascto',    10, 10, 1, pBeneficiario.dtNascto);
  Gerador.wCampo(tcDat, '', 'dtInicio',    10, 10, 1, pBeneficiario.dtInicio);
  Gerador.wCampo(tcStr, '', 'sexo',         0,  1, 1, pBeneficiario.sexo);
  Gerador.wCampo(tcInt, '', 'racaCor',      1,  1, 1, pBeneficiario.racaCor);

  if ((pBeneficiario.EstCiv >= 1) and (pBeneficiario.EstCiv <= 5)) then
    Gerador.wCampo(tcInt, '', 'estCiv',     1,  1, 0, pBeneficiario.estCiv);

  Gerador.wCampo(tcStr, '', 'incFisMen',    1,  1, 1, eSSimNaoToStr(pBeneficiario.incFisMen));
  Gerador.wCampo(tcDat, '', 'dtIncFisMen',  0, 10, 0, pBeneficiario.dtIncFisMen);
    
  GerarEndereco(pBeneficiario.endereco, (pBeneficiario.endereco.exterior.paisResid <> ''));

  GerarDependente(pBeneficiario.dependente, true);
 
  Gerador.wGrupo('/beneficiario');
end;

function TEvtCdBenefIn.GerarXML: boolean;
begin
  try
    inherited GerarXML;
    Self.VersaoDF := TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF;
     
    Self.Id := GerarChaveEsocial(now, self.ideEmpregador.NrInsc, self.Sequencial);

    GerarCabecalho('evtCdBenefIn');
    Gerador.wGrupo('evtCdBenefIn Id="' + Self.Id + '"');

    GerarIdeEvento2(self.IdeEvento);
    GerarIdeEmpregador(self.IdeEmpregador);
    GerarBeneficiario(self.Beneficiario);
    
    Gerador.wGrupo('/evtCdBenefIn');

    GerarRodape;

    FXML := Gerador.ArquivoFormatoXML;
//    XML := Assinar(Gerador.ArquivoFormatoXML, 'EvtCdBenefIn');

//    Validar(schEvtCdBenefIn);
  except on e:exception do
    raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '')
end;

function TEvtCdBenefIn.LerArqIni(const AIniString: String): Boolean;
var
  INIRec: TMemIniFile;
  Ok: Boolean;
  sSecao, sFim: String;
  I : Integer;
  depend: TDependenteCollectionItem;
begin
  Result := True;

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);
    with Self do
    begin
      sSecao             := 'evtCdBenefln';
      Id                 := INIRec.ReadString(sSecao , 'Id'        , '');
      Sequencial         := INIRec.ReadInteger(sSecao, 'Sequencial',  0);

      sSecao             := 'ideEvento';
      ideEvento.indRetif := eSStrToIndRetificacao(Ok, INIRec.ReadString(sSecao, 'indRetif'  , '1'));
      ideEvento.NrRecibo := INIRec.ReadString(sSecao , 'nrRecibo'  , EmptyStr);
      ideEvento.ProcEmi  := eSStrToprocEmi(Ok, INIRec.ReadString(sSecao, 'procEmi', '1'));
      ideEvento.VerProc  := INIRec.ReadString(sSecao, 'verProc', EmptyStr);

      sSecao             := 'ideEmpregador';
      IdeEmpregador.OrgaoPublico := (TACBreSocial(FACBreSocial).Configuracoes.Geral.TipoEmpregador = teOrgaoPublico);
      IdeEmpregador.TpInsc := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
      IdeEmpregador.NrInsc := INIRec.ReadString(sSecao, 'nrInsc', EmptyStr);

      sSecao             := 'beneficiario';
      Beneficiario.cpfBenef  := INIRec.ReadString(sSecao, 'cpfBenef'  , EmptyStr);
      Beneficiario.nmBenefic := INIRec.ReadString(sSecao, 'nmBenefic' , EmptyStr);
      Beneficiario.dtNascto  := INIRec.ReadDate(sSecao, 'dtNascto', 0);
      Beneficiario.dtInicio  := INIRec.ReadDate(sSecao, 'dtInicio', 0);
      Beneficiario.sexo      := INIRec.ReadString(sSecao, 'sexo', EmptyStr);
      Beneficiario.racaCor   := INIRec.ReadInteger(sSecao, 'racaCor', 0);
      Beneficiario.estCiv    := INIRec.ReadInteger(sSecao, 'estCiv' , 0);
      Beneficiario.incFisMen := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'incFiscMen', EmptyStr));
      Beneficiario.dtIncFisMen := INIRec.ReadDate(sSecao, 'dtIncFisMen', 0);

      sSecao := 'enderecoBrasil';
      Beneficiario.endereco.Brasil.TpLograd    := INIRec.ReadString(sSecao , 'tpLograd'   , EmptyStr);
      Beneficiario.endereco.Brasil.dscLograd   := INIRec.ReadString(sSecao , 'dscLograd'  , EmptyStr);
      Beneficiario.endereco.Brasil.nrLograd    := INIRec.ReadString(sSecao , 'nrLograd'   , EmptyStr);
      Beneficiario.endereco.Brasil.complemento := INIRec.ReadString(sSecao , 'complemento', EmptyStr);
      Beneficiario.endereco.Brasil.bairro      := INIRec.ReadString(sSecao , 'bairro'     , EmptyStr);
      Beneficiario.endereco.Brasil.cep         := INIRec.ReadString(sSecao , 'cep'        , EmptyStr);
      Beneficiario.endereco.Brasil.codMunic    := INIRec.ReadInteger(sSecao, 'codMunic'   , 0       );
      Beneficiario.endereco.Brasil.uf          := INIRec.ReadString(sSecao , 'uf'         , EmptyStr);

      sSecao := 'enderecoExterior';
      Beneficiario.endereco.exterior.paisResid   := INIRec.ReadString(sSecao, 'paisResid'  , EmptyStr);
      Beneficiario.endereco.exterior.dscLograd   := INIRec.ReadString(sSecao, 'dscLograd'  , EmptyStr);
      Beneficiario.endereco.exterior.nrLograd    := INIRec.ReadString(sSecao, 'nrLograd'   , EmptyStr);
      Beneficiario.endereco.exterior.complemento := INIRec.ReadString(sSecao, 'complemento', EmptyStr);
      Beneficiario.endereco.exterior.bairro      := INIRec.ReadString(sSecao, 'bairro'     , EmptyStr);
      Beneficiario.endereco.exterior.nmCid       := INIRec.ReadString(sSecao, 'nmCid'      , EmptyStr);
      Beneficiario.endereco.exterior.codPostal   := INIRec.ReadString(sSecao, 'codPostal'  , EmptyStr);

      I := 1;
      sFim := EmptyStr;
      while true do
      begin
        //De 0 até 99;
        sSecao := 'dependente' + IntToStrZero(I, 2);
        sFim   := INIRec.ReadString(sSecao, 'nmDep', 'FIM');

        if(sFim = 'FIM') or (Length(sFIM) <= 0)then
          break;

        depend := Beneficiario.dependente.New;
        depend.tpDep     := eSStrToTpDep(Ok, INIRec.ReadString(sSecao, 'tpDep', ''));
        depend.nmDep     := sFim;
        depend.dtNascto  := INIRec.ReadDate(sSecao, 'dtNascto', 0);
        depend.cpfDep    := INIRec.ReadString(sSecao, 'cpfDep' , EmptyStr);
        depend.sexoDep   := INIRec.ReadString(sSecao, 'sexoDep', EmptyStr);
        depend.depIRRF   := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'depIRRF', EmptyStr));
        depend.incFisMen := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'incFisMen', EmptyStr));
        depend.descrDep  := INIRec.ReadString(sSecao, 'descrDep', '');

        Inc(I);
      end;

      GerarXML;
      XML := FXML;

    end;
  finally
    INIRec.Free;
  end;
end;

end.
