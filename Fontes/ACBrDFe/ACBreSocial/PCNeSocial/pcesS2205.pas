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

unit pcesS2205;

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
  TS2205CollectionItem = class;
  TEvtAltCadastral = class;

  TS2205Collection = class(TeSocialCollection)
  private
    function GetItem(Index: Integer): TS2205CollectionItem;
    procedure SetItem(Index: Integer; Value: TS2205CollectionItem);
  public
    function Add: TS2205CollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TS2205CollectionItem;
    property Items[Index: Integer]: TS2205CollectionItem read GetItem write SetItem; default;
  end;

  TS2205CollectionItem = class(TObject)
  private
    FTipoEvento: TTipoEvento;
    FEvtAltCadastral: TEvtAltCadastral;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    property TipoEvento: TTipoEvento read FTipoEvento;
    property EvtAltCadastral: TEvtAltCadastral read FEvtAltCadastral write FEvtAltCadastral;
  end;

  TEvtAltCadastral = class(TeSocialEvento)
  private
    FdtAlteracao: TDateTime;
    FCodCateg: integer;
    FIdeEvento: TIdeEvento2;
    FIdeEmpregador: TIdeEmpregador;
    FTrabalhador: TTrabalhador;
    FVinculo: TVinculo;
    FIdeTrabalhador: TideTrabalhador;

    procedure GerarInfoAltCadastral;
  public
    constructor Create(AACBreSocial: TObject); override;
    destructor Destroy; override;

    function GerarXML: boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property dtAlteracao: TDateTime read FdtAlteracao write FdtAlteracao;
    property codCateg: integer read FCodCateg write FCodCateg;
    property ideEvento: TIdeEvento2 read FIdeEvento write FIdeEvento;
    property ideEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property trabalhador: TTrabalhador read FTrabalhador write FTrabalhador;
    property vinculo: TVinculo read FVinculo write FVinculo;
    property ideTrabalhador: TideTrabalhador read FIdeTrabalhador write FIdeTrabalhador;
  end;

implementation

uses
  IniFiles,
  ACBrUtil.Base,
  ACBrUtil.FilesIO,
  ACBrUtil.DateTime,
  ACBreSocial;

{ TS2205Collection }

function TS2205Collection.Add: TS2205CollectionItem;
begin
  Result := Self.New;
end;

function TS2205Collection.GetItem(Index: Integer): TS2205CollectionItem;
begin
  Result := TS2205CollectionItem(inherited Items[Index]);
end;

procedure TS2205Collection.SetItem(Index: Integer;
  Value: TS2205CollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TS2205Collection.New: TS2205CollectionItem;
begin
  Result := TS2205CollectionItem.Create(FACBreSocial);
  Self.Add(Result);
end;

{ TS2205CollectionItem }

constructor TS2205CollectionItem.Create(AOwner: TComponent);
begin
  inherited Create;
  FTipoEvento      := teS2205;
  FEvtAltCadastral := TEvtAltCadastral.Create(AOwner);
end;

destructor TS2205CollectionItem.Destroy;
begin
  FEvtAltCadastral.Free;

  inherited;
end;

{ TEvtAltCadastral }

constructor TEvtAltCadastral.Create(AACBreSocial: TObject);
begin
  inherited Create(AACBreSocial);

  FCodCateg       := 0;
  FIdeEvento      := TIdeEvento2.Create;
  FIdeEmpregador  := TIdeEmpregador.Create;
  FTrabalhador    := TTrabalhador.Create;
  FVinculo        := TVinculo.Create;
  FIdeTrabalhador := TideTrabalhador.Create;
end;

destructor TEvtAltCadastral.destroy;
begin
  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FTrabalhador.Free;
  FVinculo.Free;
  FIdeTrabalhador.Free;

  inherited;
end;

procedure TEvtAltCadastral.GerarInfoAltCadastral;
begin
  GerarModoAbertura(mlAlteracao);

  Gerador.wCampo(tcDat, '', 'dtAlteracao', 10, 10, 1, self.dtAlteracao);

  GerarTrabalhador(self.Trabalhador, tpSim, 'dadosTrabalhador', 1, self.CodCateg);

  GerarModoFechamento(mlAlteracao);
end;

function TEvtAltCadastral.GerarXML: boolean;
begin
  try
    inherited GerarXML;
    Self.VersaoDF := TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF;
     
    Self.Id := GerarChaveEsocial(now, self.ideEmpregador.NrInsc, self.Sequencial);

    GerarCabecalho('evtAltCadastral');
    Gerador.wGrupo('evtAltCadastral Id="' + Self.Id + '"');

    GerarIdeEvento2(self.IdeEvento);
    GerarIdeEmpregador(self.IdeEmpregador);
    GerarIdeTrabalhador(self.IdeTrabalhador, True);
    GerarInfoAltCadastral;

    Gerador.wGrupo('/evtAltCadastral');

    GerarRodape;

    FXML := Gerador.ArquivoFormatoXML;
//    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtAltCadastral');

//    Validar(schevtAltCadastral);
  except on e:exception do
    raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '')
end;

function TEvtAltCadastral.LerArqIni(const AIniString: String): Boolean;
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
      sSecao := 'evtAltCadastral';
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

      sSecao := 'idetrabalhador';
      idetrabalhador.CpfTrab := INIRec.ReadString(sSecao, 'cpfTrab', EmptyStr);

      sSecao := 'alteracao';
      dtAlteracao := StringToDateTime(INIRec.ReadString(sSecao, 'dtAlteracao', '0'));

      sSecao := 'dadostrabalhador';
      trabalhador.NisTrab    := INIRec.ReadString(sSecao, 'nisTrab', EmptyStr);
      trabalhador.NmTrab     := INIRec.ReadString(sSecao, 'nmTrab', EmptyStr);
      trabalhador.Sexo       := INIRec.ReadString(sSecao, 'sexo', EmptyStr);
      trabalhador.RacaCor    := INIRec.ReadInteger(sSecao, 'racaCor', 1);
      trabalhador.EstCiv     := INIRec.ReadInteger(sSecao, 'estCiv', 1);
      trabalhador.GrauInstr  := INIRec.ReadString(sSecao, 'grauInstr', '01');
//      Trabalhador.IndPriEmpr := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'indPriEmpr', 'S'));
      trabalhador.nmSoc      := INIRec.ReadString(sSecao, 'nmSoc', EmptyStr);
      trabalhador.PaisNac    := INIRec.ReadString(sSecao, 'paisNac', '');

      sSecao := 'nascimento';
      trabalhador.Nascimento.dtNascto   := StringToDateTime(INIRec.ReadString(sSecao, 'dtNascto', '0'));
      trabalhador.Nascimento.codMunic   := INIRec.ReadInteger(sSecao, 'codMunic', 0);
      trabalhador.Nascimento.uf         := INIRec.ReadString(sSecao, 'uf', 'SP');
      trabalhador.Nascimento.paisNascto := INIRec.ReadString(sSecao, 'paisNascto', '');
      trabalhador.Nascimento.paisNac    := INIRec.ReadString(sSecao, 'paisNac', '');
      trabalhador.Nascimento.nmMae      := INIRec.ReadString(sSecao, 'nmMae', '');
      trabalhador.Nascimento.nmPai      := INIRec.ReadString(sSecao, 'nmPai', '');

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
        trabalhador.documentos.CNH.categoriaCnh := eSStrToCnh(Ok, INIRec.ReadString(sSecao, 'dtPriHab', 'A'));
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
      trabalhador.trabImig.tmpResid   := StrTotpTmpResid(Ok, INIRec.ReadString(sSecao, 'tmpResid', '0'));
      trabalhador.trabImig.condIng    := StrTotpCondIng(Ok, INIRec.ReadString(sSecao, 'condIng', '0'));

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
        trabalhador.infoDeficiencia.infoCota       := eSStrToSimNaoFacultativo(Ok, INIRec.ReadString(sSecao, 'infoCota', 'S'));
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

      sSecao := 'aposentadoria';
      if INIRec.ReadString(sSecao, 'trabAposent', '') <> '' then
        trabalhador.aposentadoria.TrabAposent := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'trabAposent', 'S'));

      sSecao := 'contato';
      if INIRec.ReadString(sSecao, 'fonePrinc', '') <> '' then
      begin
        trabalhador.contato.FonePrinc     := INIRec.ReadString(sSecao, 'fonePrinc', '');
        trabalhador.contato.FoneAlternat  := INIRec.ReadString(sSecao, 'foneAlternat', 'S');
        trabalhador.contato.EmailPrinc    := INIRec.ReadString(sSecao, 'emailPrinc', 'S');
        trabalhador.contato.EmailAlternat := INIRec.ReadString(sSecao, 'emailAlternat', 'S');
      end;
    end;

    GerarXML;
    XML := FXML;
  finally
    INIRec.Free;
  end;
end;

end.
