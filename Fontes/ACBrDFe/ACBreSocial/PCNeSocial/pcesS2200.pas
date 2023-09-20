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

unit pcesS2200;

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
  ACBrBase, pcnConversao, pcnConsts,
  pcesCommon, pcesConversaoeSocial, pcesGerador, pcnLeitor;

type
  TS2200Collection = class;
  TS2200CollectionItem = class;
  TEvtAdmissao = class;

  TS2200Collection = class(TeSocialCollection)
  private
    function GetItem(Index: Integer): TS2200CollectionItem;
    procedure SetItem(Index: Integer; Value: TS2200CollectionItem);
  public
    function Add: TS2200CollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TS2200CollectionItem;
    property Items[Index: Integer]: TS2200CollectionItem read GetItem write SetItem; default;
  end;

  TS2200CollectionItem = class(TObject)
  private
    FTipoEvento: TTipoEvento;
    FEvtAdmissao: TEvtAdmissao;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    property TipoEvento: TTipoEvento read FTipoEvento;
    property EvtAdmissao: TEvtAdmissao read FEvtAdmissao write FEvtAdmissao;
  end;

  TEvtAdmissao = class(TeSocialEvento)
  private
    FIdeEvento: TIdeEvento2;
    FIdeEmpregador: TIdeEmpregador;
    FTrabalhador: TTrabalhador;
    FVinculo: TVinculo;

  public
    constructor Create(AACBreSocial: TObject); override;
    destructor Destroy; override;

    function GerarXML: boolean; override;
    function LerXML: Boolean;
    function LerArqIni(const AIniString: String): Boolean;

    property IdeEvento: TIdeEvento2 read FIdeEvento write FIdeEvento;
    property IdeEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property Trabalhador: TTrabalhador read FTrabalhador write FTrabalhador;
    property Vinculo: TVinculo read FVinculo write FVinculo;
  end;

implementation

uses
  IniFiles,
  ACBrUtil.Base,
  ACBrUtil.FilesIO,
  ACBrUtil.DateTime,
  ACBreSocial;

{ TS2200Collection }

function TS2200Collection.Add: TS2200CollectionItem;
begin
  Result := Self.New;
end;

function TS2200Collection.GetItem(Index: Integer): TS2200CollectionItem;
begin
  Result := TS2200CollectionItem(inherited Items[Index]);
end;

procedure TS2200Collection.SetItem(Index: Integer;
  Value: TS2200CollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TS2200Collection.New: TS2200CollectionItem;
begin
  Result := TS2200CollectionItem.Create(FACBreSocial);
  Self.Add(Result);
end;

{ TS2200CollectionItem }
constructor TS2200CollectionItem.Create(AOwner: TComponent);
begin
  inherited Create;
  FTipoEvento  := teS2200;
  FEvtAdmissao := TEvtAdmissao.Create(AOwner);
end;

destructor TS2200CollectionItem.Destroy;
begin
  FEvtAdmissao.Free;

  inherited;
end;

{ TEvtAdmissao }
constructor TEvtAdmissao.Create(AACBreSocial: TObject);
begin
  inherited Create(AACBreSocial);

  FIdeEvento     := TIdeEvento2.Create;
  FIdeEmpregador := TIdeEmpregador.Create;
  FTrabalhador   := TTrabalhador.Create;
  FVinculo       := TVinculo.Create;
end;

destructor TEvtAdmissao.destroy;
begin
  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FTrabalhador.Free;
  FVinculo.Free;

  inherited;
end;

function TEvtAdmissao.GerarXML: boolean;
begin
  try
    inherited GerarXML;
    Self.VersaoDF := TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF;

    Self.Id := GerarChaveEsocial(now, self.ideEmpregador.NrInsc, self.Sequencial);

    GerarCabecalho('evtAdmissao');
    Gerador.wGrupo('evtAdmissao Id="' + Self.Id + '"');

    GerarIdeEvento2(self.IdeEvento);
    GerarIdeEmpregador(self.IdeEmpregador);

    GerarTrabalhador(self.Trabalhador, Self.Vinculo.cadIni, 'trabalhador', 2);
    GerarVinculo(self.Vinculo, 2);

    Gerador.wGrupo('/evtAdmissao');

    GerarRodape;

    FXML := Gerador.ArquivoFormatoXML;
//    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtAdmissao');
//    Validar(schevtAdmissao);
  except on e:exception do
    raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '')
end;

function TEvtAdmissao.LerArqIni(const AIniString: String): Boolean;
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
      sSecao := 'evtAdmissao';
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
      Trabalhador.IndPriEmpr := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'indPriEmpr', 'S'));
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

      sSecao := 'trabEstrangeiro';
      if INIRec.ReadString(sSecao, 'dtChegada', '') <> '' then
      begin
        trabalhador.TrabEstrangeiro.DtChegada        := StringToDateTime(INIRec.ReadString(sSecao, 'dtChegada', '0'));
        trabalhador.TrabEstrangeiro.ClassTrabEstrang := eSStrToClassTrabEstrang(Ok, INIRec.ReadString(sSecao, 'classTrabEstrang', '1'));
        trabalhador.TrabEstrangeiro.CasadoBr         := INIRec.ReadString(sSecao, 'casadoBr', 'S');
        trabalhador.TrabEstrangeiro.FilhosBr         := INIRec.ReadString(sSecao, 'filhosBr', 'S');
      end;

      sSecao := 'trabImig';
      if INIRec.ReadString(sSecao, 'condIng', '0') <> '0' then
      begin
        trabalhador.trabImig.tmpResid := StrTotpTmpResid(OK, INIRec.ReadString(sSecao, 'tmpResid', '0'));
        trabalhador.trabImig.condIng  := StrTotpCondIng(OK, INIRec.ReadString(sSecao, 'condIng', '0'));
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

      sSecao := 'vinculo';
      vinculo.Matricula      := INIRec.ReadString(sSecao, 'matricula', '');
      vinculo.TpRegTrab      := eSStrToTpRegTrab(Ok, INIRec.ReadString(sSecao, 'tpRegTrab', '1'));
      vinculo.TpRegPrev      := eSStrTotpRegPrev(Ok, INIRec.ReadString(sSecao, 'tpRegPrev', '1'));
      vinculo.NrRecInfPrelim := INIRec.ReadString(sSecao, 'nrRecInfPrelim', '');
      vinculo.cadIni         := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'cadIni', 'S'));

      sSecao := 'infoCeletista';
      if INIRec.ReadString(sSecao, 'dtAdm', '') <> '' then
      begin
        vinculo.InfoRegimeTrab.InfoCeletista.DtAdm             := StringToDateTime(INIRec.ReadString(sSecao, 'dtAdm', '0'));
        vinculo.InfoRegimeTrab.InfoCeletista.TpAdmissao        := eSStrToTpAdmissao(Ok, INIRec.ReadString(sSecao, 'tpAdmissao', '1'));
        vinculo.InfoRegimeTrab.InfoCeletista.IndAdmissao       := eSStrToTpIndAdmissao(Ok, INIRec.ReadString(sSecao, 'indAdmissao', '1'));
        vinculo.InfoRegimeTrab.InfoCeletista.nrProcTrab        := INIRec.ReadString(sSecao, 'nrProcTrab', '');
        vinculo.InfoRegimeTrab.InfoCeletista.TpRegJor          := eSStrToTpRegJor(Ok, INIRec.ReadString(sSecao, 'tpRegJor', '1'));
        vinculo.InfoRegimeTrab.InfoCeletista.NatAtividade      := eSStrToNatAtividade(Ok, INIRec.ReadString(sSecao, 'natAtividade', '1'));
        vinculo.InfoRegimeTrab.InfoCeletista.dtBase            := INIRec.ReadInteger(sSecao, 'dtBase', 0);
        vinculo.InfoRegimeTrab.InfoCeletista.cnpjSindCategProf := INIRec.ReadString(sSecao, 'cnpjSindCategProf', '');
        vinculo.InfoRegimeTrab.InfoCeletista.matAnotJud        := INIRec.ReadString(sSecao, 'matAnotJud', '');

        sSecao := 'FGTS';
        vinculo.InfoRegimeTrab.InfoCeletista.FGTS.OpcFGTS   := eSStrToOpcFGTS(Ok, INIRec.ReadString(sSecao, 'opcFGTS', '1'));
        vinculo.InfoRegimeTrab.InfoCeletista.FGTS.DtOpcFGTS := StringToDateTime(INIRec.ReadString(sSecao, 'dtOpcFGTS', '0'));
      end;

      sSecao := 'trabTemporario';
      if INIRec.ReadString(sSecao, 'hipLeg', '') <> '' then
      begin
        vinculo.InfoRegimeTrab.InfoCeletista.trabTemporario.hipLeg      := INIRec.ReadInteger(sSecao, 'hipLeg', 1);
        vinculo.InfoRegimeTrab.InfoCeletista.trabTemporario.justContr   := INIRec.ReadString(sSecao, 'justContr', '');

        sSecao := 'ideTomadorServ';
        vinculo.InfoRegimeTrab.InfoCeletista.trabTemporario.ideTomadorServ.TpInsc := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
        vinculo.InfoRegimeTrab.InfoCeletista.trabTemporario.ideTomadorServ.NrInsc := INIRec.ReadString(sSecao, 'nrInsc', '');
      end;

      sSecao := 'ideEstabVinc';
      if INIRec.ReadString(sSecao, 'tpInsc', '') <> '' then
      begin
        vinculo.InfoRegimeTrab.InfoCeletista.trabTemporario.IdeEstabVinc.TpInsc := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
        vinculo.InfoRegimeTrab.InfoCeletista.trabTemporario.IdeEstabVinc.NrInsc := INIRec.ReadString(sSecao, 'nrInsc', '');
      end;

      I := 1;
      while true do
      begin
        // de 0 até 9
        sSecao := 'ideTrabSubstituido' + IntToStrZero(I, 1);
        sFim   := INIRec.ReadString(sSecao, 'cpfTrabSubst', 'FIM');

        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break;

        with vinculo.InfoRegimeTrab.InfoCeletista.trabTemporario.IdeTrabSubstituido.New do
        begin
          cpfTrabSubst := sFim;
        end;

        Inc(I);
      end;

      sSecao := 'aprend';

      Ok := False;
      if (TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF >= veS01_02_00) then
      begin
        if INIRec.ReadString(sSecao, 'indAprend', '') = '1' then
          Ok := (INIRec.ReadString(sSecao, 'cnpjEntQual', '') <> EmptyStr)
        else
          Ok := (INIRec.ReadString(sSecao, 'tpInsc', '') <> EmptyStr);
      end
      else
        Ok := (INIRec.ReadString(sSecao, 'tpInsc', '') <> EmptyStr);

      if Ok then
      begin
        vinculo.InfoRegimeTrab.InfoCeletista.aprend.indAprend := eSStrTotpIndAprend(Ok, INIRec.ReadString(sSecao, 'indAprend', '1'));
        vinculo.InfoRegimeTrab.InfoCeletista.aprend.cnpjEntQual := INIRec.ReadString(sSecao, 'cnpjEntQual', '');
        vinculo.InfoRegimeTrab.InfoCeletista.aprend.TpInsc := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
        vinculo.InfoRegimeTrab.InfoCeletista.aprend.NrInsc := INIRec.ReadString(sSecao, 'nrInsc', '');
        vinculo.InfoRegimeTrab.InfoCeletista.aprend.cnpjPrat := INIRec.ReadString(sSecao, 'cnpjPrat', '');
      end;

      sSecao := 'infoEstatutario';
      if INIRec.ReadString(sSecao, 'tpProv', '') <> '' then
      begin
        vinculo.InfoRegimeTrab.infoEstatutario.IndProvim   := eSStrToIndProvim(Ok, INIRec.ReadString(sSecao, 'indProvim', '1'));
        vinculo.InfoRegimeTrab.infoEstatutario.TpProv      := eSStrToTpProv(Ok, INIRec.ReadString(sSecao, 'tpProv', '1'));
        vinculo.InfoRegimeTrab.infoEstatutario.DtNomeacao  := StringToDateTime(INIRec.ReadString(sSecao, 'dtNomeacao', '0'));
        vinculo.InfoRegimeTrab.infoEstatutario.DtPosse     := StringToDateTime(INIRec.ReadString(sSecao, 'dtPosse', '0'));
        vinculo.InfoRegimeTrab.infoEstatutario.DtExercicio := StringToDateTime(INIRec.ReadString(sSecao, 'dtExercicio', '0'));
        vinculo.InfoRegimeTrab.infoEstatutario.tpPlanRP    := eSStrToTpPlanRP(Ok, INIRec.ReadString(sSecao, 'tpPlanRP', '1'));
        vinculo.InfoRegimeTrab.infoEstatutario.infoDecJud.nrProcJud := INIRec.ReadString(sSecao, 'nrProcJud', '');
        vinculo.InfoRegimeTrab.infoEstatutario.indTetoRGPS := eSStrToSimNaoFacultativo(Ok, INIRec.ReadString(sSecao, 'indTetoRGPS', ''));
        vinculo.InfoRegimeTrab.infoEstatutario.indAbonoPerm:= eSStrToSimNaoFacultativo(Ok, INIRec.ReadString(sSecao, 'indAbonoPerm', ''));
        vinculo.InfoRegimeTrab.infoEstatutario.dtIniAbono  := StringToDateTime(INIRec.ReadString(sSecao, 'dtIniAbono', '0'));
      end;

      sSecao := 'infoContrato';
      vinculo.infoContrato.CodCargo    := INIRec.ReadString(sSecao, 'codCargo', '');
      vinculo.infoContrato.CodFuncao   := INIRec.ReadString(sSecao, 'codFuncao', '');
      vinculo.infoContrato.CodCateg    := INIRec.ReadInteger(sSecao, 'codCateg', 0);
      vinculo.infoContrato.codCarreira := INIRec.ReadString(sSecao, 'codCarreira', '');
      vinculo.infoContrato.dtIngrCarr  := StringToDateTime(INIRec.ReadString(sSecao, 'dtIngrCarr', '0'));

      vinculo.infoContrato.nmCargo      := INIRec.ReadString(sSecao, 'nmCargo', '');
      vinculo.infoContrato.CBOCargo     := INIRec.ReadString(sSecao, 'CBOCargo', '');
      vinculo.infoContrato.dtIngrCargo  := StringToDateTime(INIRec.ReadString(sSecao, 'dtIngrCargo', '0'));
      vinculo.infoContrato.nmFuncao     := INIRec.ReadString(sSecao, 'nmFuncao', '');
      vinculo.infoContrato.CBOFuncao    := INIRec.ReadString(sSecao, 'CBOFuncao', '');
      vinculo.infoContrato.acumCargo    := eSStrToSimNaoFacultativo(Ok, INIRec.ReadString(sSecao, 'acumCargo', ''));

      sSecao := 'remuneracao';
      vinculo.infoContrato.remuneracao.VrSalFx    := StringToFloatDef(INIRec.ReadString(sSecao, 'vrSalFx', ''), 0);
      vinculo.infoContrato.remuneracao.UndSalFixo := eSStrToUndSalFixo(Ok, INIRec.ReadString(sSecao, 'undSalFixo', ''));
      vinculo.infoContrato.remuneracao.DscSalVar  := INIRec.ReadString(sSecao, 'dscSalVar', '');

      sSecao := 'duracao';
      if INIRec.ReadString(sSecao, 'tpContr', '1') <> '3' then
      begin
      vinculo.infoContrato.duracao.TpContr   := eSStrToTpContr(Ok, INIRec.ReadString(sSecao, 'tpContr', '1'));
      vinculo.infoContrato.duracao.dtTerm    := StringToDateTime(INIRec.ReadString(sSecao, 'dtTerm', '0'));
      vinculo.infoContrato.duracao.clauAssec := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'clauAssec', 'S'));
      vinculo.infoContrato.duracao.objDet    := INIRec.ReadString(sSecao, 'objDet', '');
      end;

      sSecao := 'localTrabGeral';
      if INIRec.ReadString(sSecao, 'tpInsc', '') <> '' then
      begin
        vinculo.infoContrato.LocalTrabalho.LocalTrabGeral.TpInsc   := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
        vinculo.infoContrato.LocalTrabalho.LocalTrabGeral.NrInsc   := INIRec.ReadString(sSecao, 'nrInsc', '');
        vinculo.infoContrato.LocalTrabalho.LocalTrabGeral.DescComp := INIRec.ReadString(sSecao, 'descComp', '');
      end;

      sSecao := 'localTrabDom';
      if INIRec.ReadString(sSecao, 'tpLograd', '') <> '' then
      begin
        vinculo.infoContrato.LocalTrabalho.localTrabDom.TpLograd    := INIRec.ReadString(sSecao, 'tpLograd', '');
        vinculo.infoContrato.LocalTrabalho.localTrabDom.DscLograd   := INIRec.ReadString(sSecao, 'dscLograd', '');
        vinculo.infoContrato.LocalTrabalho.localTrabDom.NrLograd    := INIRec.ReadString(sSecao, 'nrLograd', '');
        vinculo.infoContrato.LocalTrabalho.localTrabDom.Complemento := INIRec.ReadString(sSecao, 'complemento', '');
        vinculo.infoContrato.LocalTrabalho.localTrabDom.Bairro      := INIRec.ReadString(sSecao, 'bairro', '');
        vinculo.infoContrato.LocalTrabalho.localTrabDom.Cep         := INIRec.ReadString(sSecao, 'cep', '');
        vinculo.infoContrato.LocalTrabalho.localTrabDom.CodMunic    := INIRec.ReadInteger(sSecao, 'CodMunic', 0);
        vinculo.infoContrato.LocalTrabalho.localTrabDom.uf          := INIRec.ReadString(sSecao, 'uf', 'SP');
      end;

      sSecao := 'localTempDom';
      if INIRec.ReadString(sSecao, 'tpLograd', '') <> '' then
      begin
        vinculo.infoContrato.LocalTrabalho.LocalTempDom.TpLograd    := INIRec.ReadString(sSecao, 'tpLograd', '');
        vinculo.infoContrato.LocalTrabalho.LocalTempDom.DscLograd   := INIRec.ReadString(sSecao, 'dscLograd', '');
        vinculo.infoContrato.LocalTrabalho.LocalTempDom.NrLograd    := INIRec.ReadString(sSecao, 'nrLograd', '');
        vinculo.infoContrato.LocalTrabalho.LocalTempDom.Complemento := INIRec.ReadString(sSecao, 'complemento', '');
        vinculo.infoContrato.LocalTrabalho.LocalTempDom.Bairro      := INIRec.ReadString(sSecao, 'bairro', '');
        vinculo.infoContrato.LocalTrabalho.LocalTempDom.Cep         := INIRec.ReadString(sSecao, 'cep', '');
        vinculo.infoContrato.LocalTrabalho.LocalTempDom.CodMunic    := INIRec.ReadInteger(sSecao, 'CodMunic', 0);
        vinculo.infoContrato.LocalTrabalho.LocalTempDom.uf          := INIRec.ReadString(sSecao, 'uf', 'SP');
      end;

      sSecao := 'horContratual';
      if INIRec.ReadString(sSecao, 'tpJornada', '') <> '' then
      begin
        vinculo.infoContrato.horContratual.QtdHrsSem := INIRec.ReadInteger(sSecao, 'qtdHrsSem', 0);
        vinculo.infoContrato.horContratual.TpJornada := eSStrToTpJornada(Ok, INIRec.ReadString(sSecao, 'tpJornada', '1'));
        vinculo.infoContrato.horContratual.DscTpJorn := INIRec.ReadString(sSecao, 'dscTpJorn', '');
        vinculo.infoContrato.horContratual.dscJorn   := INIRec.ReadString(sSecao, 'dscJorn', '');
        vinculo.infoContrato.horContratual.tmpParc   := StrTotpTmpParc(Ok, INIRec.ReadString(sSecao, 'tmpParc', '0'));
        vinculo.infoContrato.horContratual.horNoturno:= eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'horNoturno', 'S'));  //26/01/2022
      end;

      I := 1;
      while true do
      begin
        // de 00 até 99
        sSecao := 'horario' + IntToStrZero(I, 2);
        sFim   := INIRec.ReadString(sSecao, 'dia', 'FIM');

        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break;

        with vinculo.infoContrato.horContratual.horario.New do
        begin
          dia           := eSStrToTpDia(Ok, sFim);
          CodHorContrat := INIRec.ReadString(sSecao, 'codHorContrat', '');
        end;

        Inc(I);
      end;

      I := 1;
      while true do
      begin
        // de 0 até 2
        sSecao := 'filiacaoSindical' + IntToStrZero(I, 1);
        sFim   := INIRec.ReadString(sSecao, 'cnpjSindTrab', 'FIM');

        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break;

        with vinculo.infoContrato.filiacaoSindical.Add do
        begin
          cnpjSindTrab := sFim;
        end;

        Inc(I);
      end;

      sSecao := 'alvaraJudicial';
      if INIRec.ReadString(sSecao, 'nrProcJud', '') <> '' then
        vinculo.infoContrato.alvaraJudicial.NrProcJud := INIRec.ReadString(sSecao, 'nrProcJud', '');

      I := 1;
      while true do
      begin
        // de 00 até 99
        sSecao := 'observacoes' + IntToStrZero(I, 2);
        sFim   := INIRec.ReadString(sSecao, 'observacao', 'FIM');

        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break;

        with vinculo.infoContrato.observacoes.New do
        begin
          observacao := sFim;
        end;

        Inc(I);
      end;

      I := 1;
      while true do
      begin
        // de 00 até 99
        sSecao := 'treiCap' + IntToStrZero(I, 2);
        sFim   := INIRec.ReadString(sSecao, 'codTreiCap', 'FIM');

        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break;

        with vinculo.infoContrato.treiCap.New do
        begin
          codTreiCap := StrToInt(sFim);
        end;

        Inc(I);
      end;

      sSecao := 'sucessaoVinc';
      if ((INIRec.ReadString(sSecao, 'cnpjEmpregAnt', '') <> '') and (TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF <= ve02_05_00)) then
      begin
        vinculo.sucessaoVinc.tpInsc        := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
        vinculo.sucessaoVinc.nrInsc        := INIRec.ReadString(sSecao, 'nrInsc', '');
        vinculo.sucessaoVinc.tpInscAnt     := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInscAnt', '1'));
        vinculo.sucessaoVinc.cnpjEmpregAnt := INIRec.ReadString(sSecao, 'cnpjEmpregAnt', '');
        vinculo.sucessaoVinc.MatricAnt     := INIRec.ReadString(sSecao, 'matricAnt', '');
        vinculo.sucessaoVinc.dtTransf      := StringToDateTime(INIRec.ReadString(sSecao, 'dtTransf', '0'));
        vinculo.sucessaoVinc.Observacao    := INIRec.ReadString(sSecao, 'observacao', '');
      end;

      if ((INIRec.ReadString(sSecao, 'tpInsc', '') <> '') and (TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF > ve02_05_00)) then
      begin
        vinculo.sucessaoVinc.tpInsc       := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
        vinculo.sucessaoVinc.nrInsc       := INIRec.ReadString(sSecao, 'nrInsc', '');
        vinculo.sucessaoVinc.MatricAnt    := INIRec.ReadString(sSecao, 'matricAnt', '');
        vinculo.sucessaoVinc.dtTransf     := StringToDateTime(INIRec.ReadString(sSecao, 'dtTransf', '0'));
        vinculo.sucessaoVinc.Observacao   := INIRec.ReadString(sSecao, 'observacao', '');
      end;

      sSecao := 'transfDom';
      if INIRec.ReadString(sSecao, 'cpfSubstituido', '') <> '' then
      begin
        vinculo.transfDom.cpfSubstituido := INIRec.ReadString(sSecao, 'cpfSubstituido', '');
        vinculo.transfDom.MatricAnt      := INIRec.ReadString(sSecao, 'matricAnt', '');
        vinculo.transfDom.dtTransf       := StringToDateTime(INIRec.ReadString(sSecao, 'dtTransf', '0'));
      end;

      sSecao := 'mudancaCPF';
      if INIRec.ReadString(sSecao, 'cpfAnt', '') <> '' then
      begin
        vinculo.mudancaCPF.cpfAnt     := INIRec.ReadString(sSecao, 'cpfAnt', '');
        vinculo.mudancaCPF.matricAnt  := INIRec.ReadString(sSecao, 'matricAnt', '');
        vinculo.mudancaCPF.dtAltCPF   := StringToDateTime(INIRec.ReadString(sSecao, 'dtAltCPF', '0'));
        vinculo.mudancaCPF.observacao := INIRec.ReadString(sSecao, 'observacao', '');
      end;

      sSecao := 'afastamento';
      if INIRec.ReadString(sSecao, 'dtIniAfast', '') <> '' then
      begin
        vinculo.afastamento.DtIniAfast  := StringToDateTime(INIRec.ReadString(sSecao, 'dtIniAfast', '0'));
        vinculo.afastamento.codMotAfast := eSStrTotpMotivosAfastamento(Ok, INIRec.ReadString(sSecao, 'codMotAfast', '00'));
      end;

      sSecao := 'desligamento';
      if INIRec.ReadString(sSecao, 'dtDeslig', '') <> '' then
        vinculo.desligamento.DtDeslig := StringToDateTime(INIRec.ReadString(sSecao, 'dtDeslig', '0'));

      sSecao := 'cessao';
      if INIRec.ReadString(sSecao, 'dtIniCessao', '') <> '' then
        vinculo.cessao.dtIniCessao := StringToDateTime(INIRec.ReadString(sSecao, 'dtIniCessao', '0'));

    end;

    GerarXML;
    XML := FXML;
  finally
    INIRec.Free;
  end;
end;

function TEvtAdmissao.LerXML: Boolean;
var
  Leitor: TLeitor;
  bOK: Boolean;
  I: Integer;
begin
  Result := True;
  Leitor := TLeitor.Create;
  try
    Leitor.Arquivo := XML;

    if Leitor.rExtrai(1, 'evtAdmissao') <> '' then
    begin
      Id := Leitor.rCampo(tcStr, 'Id');

      if Leitor.rExtrai(2, 'ideEvento') <> '' then
      begin
        FIdeEvento.indRetif := eSStrToIndRetificacao(bOK, Leitor.rCampo(tcStr, 'indRetif'));
        FIdeEvento.ProcEmi  := eSStrToprocEmi(bOK, Leitor.rCampo(tcStr, 'procEmi'));
        FIdeEvento.VerProc  := Leitor.rCampo(tcStr, 'verProc');
      end;

      if Leitor.rExtrai(2, 'ideEmpregador') <> '' then
      begin
        FIdeEmpregador.TpInsc := eSStrToTpInscricao(bOK, Leitor.rCampo(tcStr, 'tpInsc'));
        FIdeEmpregador.NrInsc := Leitor.rCampo(tcStr, 'nrInsc');
      end;

      if Leitor.rExtrai(2, 'trabalhador') <> '' then
      begin
        Trabalhador.CpfTrab                     := Leitor.rCampo(tcStr, 'cpfTrab');
        Trabalhador.NisTrab                     := Leitor.rCampo(tcStr, 'nisTrab');
        Trabalhador.NmTrab                      := Leitor.rCampo(tcStr, 'nmTrab');
        Trabalhador.Sexo                        := Leitor.rCampo(tcStr, 'sexo');
        Trabalhador.RacaCor                     := StrToIntDef(Leitor.rCampo(tcStr, 'racaCor'),0); //integer
        Trabalhador.EstCiv                      := StrToIntDef(Leitor.rCampo(tcStr, 'estCiv'),0);//integer
        Trabalhador.GrauInstr                   := Leitor.rCampo(tcStr, 'grauInstr');
        Trabalhador.nmSoc                       := Leitor.rCampo(tcStr, 'nmSoc');

        if Leitor.rExtrai(3, 'nascimento') <> '' then
        begin
          Trabalhador.Nascimento.dtNascto         := StringToDateTime(Leitor.rCampo(tcDat, 'dtNascto'));
          trabalhador.Nascimento.codMunic          := StrToIntDef(Leitor.rCampo(tcStr, 'codMunic'),0);
          trabalhador.Nascimento.UF                := Leitor.rCampo(tcStr, 'uf');
          Trabalhador.Nascimento.PaisNascto       := Leitor.rCampo(tcStr, 'paisNascto');
          Trabalhador.Nascimento.PaisNac          := Leitor.rCampo(tcStr, 'paisNac');
          Trabalhador.Nascimento.NmMae            := Leitor.rCampo(tcStr, 'nmMae');
          Trabalhador.Nascimento.NmPai            := Leitor.rCampo(tcStr, 'nmPai');
        end;

        if Leitor.rExtrai(3, 'endereco') <> '' then
        begin
          if Leitor.rExtrai(4, 'brasil') <> '' then
          begin
            Trabalhador.Endereco.Brasil.TpLograd    := Leitor.rCampo(tcStr, 'tpLograd');
            Trabalhador.Endereco.Brasil.DscLograd   := Leitor.rCampo(tcStr, 'dscLograd');
            Trabalhador.Endereco.Brasil.NrLograd    := Leitor.rCampo(tcStr, 'nrLograd');
            Trabalhador.Endereco.Brasil.Complemento := Leitor.rCampo(tcStr, 'complemento');
            Trabalhador.Endereco.Brasil.Bairro      := Leitor.rCampo(tcStr, 'bairro');
            Trabalhador.Endereco.Brasil.Cep         := Leitor.rCampo(tcStr, 'cep');
            Trabalhador.Endereco.Brasil.CodMunic    := StrToIntDef(Leitor.rCampo(tcStr, 'codMunic'),0);  //integer
            Trabalhador.Endereco.Brasil.UF          := Leitor.rCampo(tcStr, 'uf');
          end;
          if Leitor.rExtrai(4, 'exterior') <> '' then
          begin
            Trabalhador.Endereco.Exterior.PaisResid   := Leitor.rCampo(tcStr, 'paisResid');
            Trabalhador.Endereco.Exterior.DscLograd   := Leitor.rCampo(tcStr, 'dscLograd');
            Trabalhador.Endereco.Exterior.NrLograd    := Leitor.rCampo(tcStr, 'nrLograd');
            Trabalhador.Endereco.Exterior.Complemento := Leitor.rCampo(tcStr, 'complemento');
            Trabalhador.Endereco.Exterior.Bairro      := Leitor.rCampo(tcStr, 'bairro');
            Trabalhador.Endereco.Exterior.NmCid       := Leitor.rCampo(tcStr, 'nmCid');
            Trabalhador.Endereco.Exterior.CodPostal   := Leitor.rCampo(tcStr, 'codPostal');
          end;
        end;

        if Leitor.rExtrai(3, 'trabImig') <> '' then
        begin
          trabalhador.trabImig.tmpResid := StrTotpTmpResid(bOK, Leitor.rCampo(tcStr, 'tmpResid'));
          trabalhador.trabImig.condIng  := StrTotpCondIng(bOK, Leitor.rCampo(tcStr, 'condIng'));
        end;

        if Leitor.rExtrai(3, 'infoDeficiencia') <> '' then
        begin
          trabalhador.infoDeficiencia.DefFisica      := eSStrToSimNao(bOk, Leitor.rCampo(tcStr, 'defFisica'));
          trabalhador.infoDeficiencia.DefVisual      := eSStrToSimNao(bOk, Leitor.rCampo(tcStr, 'defVisual'));
          trabalhador.infoDeficiencia.DefAuditiva    := eSStrToSimNao(bOk, Leitor.rCampo(tcStr, 'defAuditiva'));
          trabalhador.infoDeficiencia.DefMental      := eSStrToSimNao(bOk, Leitor.rCampo(tcStr, 'defMental'));
          trabalhador.infoDeficiencia.DefIntelectual := eSStrToSimNao(bOk, Leitor.rCampo(tcStr, 'defIntelectual'));
          trabalhador.infoDeficiencia.ReabReadap     := eSStrToSimNao(bOk, Leitor.rCampo(tcStr, 'reabReadap'));
          trabalhador.infoDeficiencia.infoCota       := eSStrToSimNaoFacultativo(bOk, Leitor.rCampo(tcStr, 'infoCota'));
          trabalhador.infoDeficiencia.Observacao     := Leitor.rCampo(tcStr, 'observacao');
        end;

        i := 0;
        while Leitor.rExtrai(3, 'dependente', '', i + 1) <> '' do
        begin
          with Trabalhador.Dependente.New do
          begin
            tpDep    := eSStrToTpDep(bOK, Leitor.rCampo(tcStr, 'tpDep'));
            nmDep    := Leitor.rCampo(tcStr, 'nmDep');
            dtNascto := StringToDateTime(Leitor.rCampo(tcDat, 'dtNascto'));
            cpfDep   := Leitor.rCampo(tcStr, 'cpfDep');
            depIRRF  := eSStrToSimNao(bOK, Leitor.rCampo(tcStr, 'depIRRF', 'S'));
            depSF    := eSStrToSimNao(bOK, Leitor.rCampo(tcStr, 'depSF', 'S'));
            incTrab  := eSStrToSimNao(bOK, Leitor.rCampo(tcStr, 'incTrab', 'S'));
          end;
          Inc(i);
        end;
      end;

      if Leitor.rExtrai(2, 'contato') <> '' then
      begin
        Trabalhador.contato.FonePrinc     := Leitor.rCampo(tcStr, 'fonePrinc');
        Trabalhador.contato.FoneAlternat  := Leitor.rCampo(tcStr, 'foneAlternat');
        Trabalhador.contato.EmailPrinc    := Leitor.rCampo(tcStr, 'emailPrinc');
        Trabalhador.contato.EmailAlternat := Leitor.rCampo(tcStr, 'emailAlternat');
      end;

      if Leitor.rExtrai(2, 'vinculo') <> '' then
      begin
        vinculo.Matricula      := Leitor.rCampo(tcStr, 'matricula');
        vinculo.TpRegTrab      := eSStrToTpRegTrab(bOk, Leitor.rCampo(tcStr, 'tpRegTrab'));
        vinculo.TpRegPrev      := eSStrTotpRegPrev(bOk, Leitor.rCampo(tcStr, 'tpRegPrev'));
        vinculo.NrRecInfPrelim := Leitor.rCampo(tcStr, 'nrRecInfPrelim');
        vinculo.cadIni         := eSStrToSimNao(bOk, Leitor.rCampo(tcStr, 'cadIni'));

        if Leitor.rExtrai(3, 'infoRegimeTrab') <> '' then
        begin
          if Leitor.rExtrai(4, 'infoCeletista') <> '' then
          begin
            vinculo.InfoRegimeTrab.InfoCeletista.DtAdm             := StringToDateTime(Leitor.rCampo(tcDat, 'dtAdm'));
            vinculo.InfoRegimeTrab.InfoCeletista.TpAdmissao        := eSStrToTpAdmissao(bOk, Leitor.rCampo(tcStr, 'tpAdmissao'));
            vinculo.InfoRegimeTrab.InfoCeletista.IndAdmissao       := eSStrToTpIndAdmissao(bOk, Leitor.rCampo(tcStr, 'indAdmissao'));
            vinculo.InfoRegimeTrab.InfoCeletista.nrProcTrab        := Leitor.rCampo(tcStr, 'nrProcTrab');
            vinculo.InfoRegimeTrab.InfoCeletista.TpRegJor          := eSStrToTpRegJor(bOk, Leitor.rCampo(tcStr, 'tpRegJor'));
            vinculo.InfoRegimeTrab.InfoCeletista.NatAtividade      := eSStrToNatAtividade(bOk, Leitor.rCampo(tcStr, 'natAtividade'));
            vinculo.InfoRegimeTrab.InfoCeletista.dtBase            := Leitor.rCampo(tcStr, 'dtBase');
            vinculo.InfoRegimeTrab.InfoCeletista.cnpjSindCategProf := Leitor.rCampo(tcStr, 'cnpjSindCategProf');
            vinculo.InfoRegimeTrab.InfoCeletista.matAnotJud        := Leitor.rCampo(tcStr, 'matAnotJud');

            if Leitor.rExtrai(5, 'FGTS') <> '' then
            begin
              vinculo.InfoRegimeTrab.InfoCeletista.FGTS.OpcFGTS   := eSStrToOpcFGTS(bOk, Leitor.rCampo(tcStr, 'opcFGTS'));
              vinculo.InfoRegimeTrab.InfoCeletista.FGTS.DtOpcFGTS := StringToDateTime(Leitor.rCampo(tcDat, 'dtOpcFGTS'));
            end;

            if Leitor.rExtrai(5, 'trabTemporario') <> '' then
            begin
              vinculo.InfoRegimeTrab.InfoCeletista.trabTemporario.hipLeg      := StrToIntDef(Leitor.rCampo(tcStr, 'hipLeg'), 0);
              vinculo.InfoRegimeTrab.InfoCeletista.trabTemporario.justContr   := Leitor.rCampo(tcStr, 'justContr');

              if Leitor.rExtrai(6, 'ideEstabVinc') <> '' then
              begin
                vinculo.InfoRegimeTrab.InfoCeletista.trabTemporario.IdeEstabVinc.TpInsc := eSStrToTpInscricao(bOk, Leitor.rCampo(tcStr, 'tpInsc'));
                vinculo.InfoRegimeTrab.InfoCeletista.trabTemporario.IdeEstabVinc.NrInsc := Leitor.rCampo(tcStr, 'nrInsc', '');
              end;

              i := 0;
              while Leitor.rExtrai(6, 'ideTrabSubstituido', '', i + 1) <> '' do
              begin
                with vinculo.InfoRegimeTrab.InfoCeletista.trabTemporario.IdeTrabSubstituido.New do
                begin
                  cpfTrabSubst := Leitor.rCampo(tcStr, 'cpfTrabSubst', '');
                end;
                Inc(i);
              end;
            end;

            if Leitor.rExtrai(5, 'aprend') <> '' then
            begin
              vinculo.InfoRegimeTrab.InfoCeletista.aprend.indAprend := eSStrTotpIndAprend(bOk, Leitor.rCampo(tcStr, 'indAprend'));
              vinculo.InfoRegimeTrab.InfoCeletista.aprend.cnpjEntQual := Leitor.rCampo(tcStr, 'cnpjEntQual', '');
              vinculo.InfoRegimeTrab.InfoCeletista.aprend.TpInsc := eSStrToTpInscricao(bOk, Leitor.rCampo(tcStr, 'tpInsc'));
              vinculo.InfoRegimeTrab.InfoCeletista.aprend.NrInsc := Leitor.rCampo(tcStr, 'nrInsc', '');
              vinculo.InfoRegimeTrab.InfoCeletista.aprend.cnpjPrat := Leitor.rCampo(tcStr, 'cnpjPrat', '');
            end;
          end;

          if Leitor.rExtrai(4, 'infoEstatutario') <> '' then
          begin
            vinculo.InfoRegimeTrab.infoEstatutario.IndProvim            := eSStrToIndProvim(bOk, Leitor.rCampo(tcStr, 'indProvim'));
            vinculo.InfoRegimeTrab.infoEstatutario.TpProv               := eSStrToTpProv(bOk, Leitor.rCampo(tcStr, 'tpProv'));
            vinculo.InfoRegimeTrab.infoEstatutario.DtNomeacao           := StringToDateTime(Leitor.rCampo(tcDat, 'dtNomeacao'));
            vinculo.InfoRegimeTrab.infoEstatutario.DtPosse              := StringToDateTime(Leitor.rCampo(tcDat, 'dtPosse'));
            vinculo.InfoRegimeTrab.infoEstatutario.DtExercicio          := StringToDateTime(Leitor.rCampo(tcDat, 'dtExercicio'));
            vinculo.InfoRegimeTrab.infoEstatutario.tpPlanRP             := eSStrToTpPlanRP(bOk, Leitor.rCampo(tcStr, 'tpPlanRP'));
            vinculo.InfoRegimeTrab.infoEstatutario.infoDecJud.nrProcJud := Leitor.rCampo(tcStr, 'nrProcJud');
            vinculo.InfoRegimeTrab.infoEstatutario.indTetoRGPS          := eSStrToSimNaoFacultativo(bOk, Leitor.rCampo(tcStr, 'indTetoRGPS'));
            vinculo.InfoRegimeTrab.infoEstatutario.indAbonoPerm         := eSStrToSimNaoFacultativo(bOk, Leitor.rCampo(tcStr, 'indAbonoPerm'));
            vinculo.InfoRegimeTrab.infoEstatutario.dtIniAbono           := StringToDateTime(Leitor.rCampo(tcDat, 'dtIniAbono'));
          end;
        end;

        if Leitor.rExtrai(3, 'infoContrato') <> '' then
        begin
          vinculo.infoContrato.CodCargo    := Leitor.rCampo(tcStr, 'codCargo');
          vinculo.infoContrato.CodFuncao   := Leitor.rCampo(tcStr, 'codFuncao');
          vinculo.infoContrato.CodCateg    := StrToIntDef(Leitor.rCampo(tcStr, 'codCateg'), 0);
          vinculo.infoContrato.codCarreira := Leitor.rCampo(tcStr, 'codCarreira');
          vinculo.infoContrato.dtIngrCarr  := StringToDateTime(Leitor.rCampo(tcDat, 'dtIngrCarr'));

          vinculo.infoContrato.nmCargo      := Leitor.rCampo(tcStr, 'nmCargo');
          vinculo.infoContrato.CBOCargo     := Leitor.rCampo(tcStr, 'CBOCargo');
          vinculo.infoContrato.dtIngrCargo  := StringToDateTime(Leitor.rCampo(tcDat, 'dtIngrCargo'));
          vinculo.infoContrato.nmFuncao     := Leitor.rCampo(tcStr, 'nmFuncao');
          vinculo.infoContrato.CBOFuncao    := Leitor.rCampo(tcStr, 'CBOFuncao');
          vinculo.infoContrato.acumCargo    := eSStrToSimNaoFacultativo(bOk, Leitor.rCampo(tcStr, 'acumCargo'));

          if Leitor.rExtrai(4, 'remuneracao') <> '' then
          begin
            vinculo.infoContrato.remuneracao.VrSalFx    := StringToFloatDef(Leitor.rCampo(tcStr, 'vrSalFx'), 0);
            vinculo.infoContrato.remuneracao.UndSalFixo := eSStrToUndSalFixo(bOk, Leitor.rCampo(tcStr, 'undSalFixo'));
            vinculo.infoContrato.remuneracao.DscSalVar  := Leitor.rCampo(tcStr, 'dscSalVar');
          end;

          if Leitor.rExtrai(4, 'duracao') <> '' then
          begin
            vinculo.infoContrato.duracao.TpContr   := eSStrToTpContr(bOk, Leitor.rCampo(tcStr, 'tpContr'));
            vinculo.infoContrato.duracao.dtTerm    := StringToDateTime(Leitor.rCampo(tcDat, 'dtTerm'));
            vinculo.infoContrato.duracao.clauAssec := eSStrToSimNao(bOk, Leitor.rCampo(tcStr, 'clauAssec'));
            vinculo.infoContrato.duracao.objDet    := Leitor.rCampo(tcStr, 'objDet');
          end;

          if Leitor.rExtrai(4, 'localTrabalho') <> '' then
          begin
            if Leitor.rExtrai(5, 'localTrabGeral') <> '' then
            begin
              vinculo.infoContrato.LocalTrabalho.LocalTrabGeral.TpInsc   := eSStrToTpInscricao(bOk, Leitor.rCampo(tcStr, 'tpInsc'));
              vinculo.infoContrato.LocalTrabalho.LocalTrabGeral.NrInsc   := Leitor.rCampo(tcStr, 'nrInsc');
              vinculo.infoContrato.LocalTrabalho.LocalTrabGeral.DescComp := Leitor.rCampo(tcStr, 'descComp');
            end;

            if Leitor.rExtrai(5, 'localTempDom') <> '' then
            begin
              vinculo.infoContrato.LocalTrabalho.LocalTempDom.TpLograd    := Leitor.rCampo(tcStr, 'tpLograd');
              vinculo.infoContrato.LocalTrabalho.LocalTempDom.DscLograd   := Leitor.rCampo(tcStr, 'dscLograd');
              vinculo.infoContrato.LocalTrabalho.LocalTempDom.NrLograd    := Leitor.rCampo(tcStr, 'nrLograd');
              vinculo.infoContrato.LocalTrabalho.LocalTempDom.Complemento := Leitor.rCampo(tcStr, 'complemento');
              vinculo.infoContrato.LocalTrabalho.LocalTempDom.Bairro      := Leitor.rCampo(tcStr, 'bairro');
              vinculo.infoContrato.LocalTrabalho.LocalTempDom.Cep         := Leitor.rCampo(tcStr, 'cep');
              vinculo.infoContrato.LocalTrabalho.LocalTempDom.CodMunic    := StrToIntDef(Leitor.rCampo(tcStr, 'CodMunic'), 0);
              vinculo.infoContrato.LocalTrabalho.LocalTempDom.uf          := Leitor.rCampo(tcStr, 'uf');
            end;
          end;

          if Leitor.rExtrai(4, 'horContratual') <> '' then
          begin
            vinculo.infoContrato.horContratual.QtdHrsSem := StrToIntDef(Leitor.rCampo(tcStr, 'qtdHrsSem'), 0);
            vinculo.infoContrato.horContratual.TpJornada := eSStrToTpJornada(bOk, Leitor.rCampo(tcStr, 'tpJornada'));
            vinculo.infoContrato.horContratual.DscTpJorn := Leitor.rCampo(tcStr, 'dscTpJorn');
            vinculo.infoContrato.horContratual.dscJorn   := Leitor.rCampo(tcStr, 'dscJorn');
            vinculo.infoContrato.horContratual.tmpParc   := StrTotpTmpParc(bOk, Leitor.rCampo(tcStr, 'tmpParc'));
            vinculo.infoContrato.horContratual.horNoturno:= eSStrToSimNao(bOk, Leitor.rCampo(tcStr, 'horNoturno'));
          end;

          if Leitor.rExtrai(4, 'alvaraJudicial') <> '' then
            vinculo.infoContrato.alvaraJudicial.NrProcJud := Leitor.rCampo(tcStr, 'nrProcJud');

          i := 0;
          while Leitor.rExtrai(4, 'observacoes', '', i + 1) <> '' do
          begin
            with vinculo.infoContrato.observacoes.New do
            begin
              observacao := Leitor.rCampo(tcStr, 'observacao', '');
            end;
            Inc(i);
          end;

          i := 0;
          while Leitor.rExtrai(4, 'treiCap', '', i + 1) <> '' do
          begin
            with vinculo.infoContrato.treiCap.New do
            begin
              codTreiCap := Leitor.rCampo(tcStr, 'codTreiCap', '');
            end;
            Inc(i);
          end;
        end;

        if ((Leitor.rExtrai(3, 'sucessaoVinc') <> '') and (TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF <= ve02_05_00)) then
        begin
          vinculo.sucessaoVinc.tpInsc        := eSStrToTpInscricao(bOk, Leitor.rCampo(tcStr, 'tpInsc'));
          vinculo.sucessaoVinc.nrInsc        := Leitor.rCampo(tcStr, 'nrInsc');
          vinculo.sucessaoVinc.tpInscAnt     := eSStrToTpInscricao(bOk, Leitor.rCampo(tcStr, 'tpInscAnt'));
          vinculo.sucessaoVinc.cnpjEmpregAnt := Leitor.rCampo(tcStr, 'cnpjEmpregAnt');
          vinculo.sucessaoVinc.MatricAnt     := Leitor.rCampo(tcStr, 'matricAnt');
          vinculo.sucessaoVinc.dtTransf      := StringToDateTime(Leitor.rCampo(tcDat, 'dtTransf'));
          vinculo.sucessaoVinc.Observacao    := Leitor.rCampo(tcStr, 'observacao');
        end;

        if Leitor.rExtrai(3, 'transfDom') <> '' then
        begin
          vinculo.transfDom.cpfSubstituido := Leitor.rCampo(tcStr, 'cpfSubstituido');
          vinculo.transfDom.MatricAnt      := Leitor.rCampo(tcStr, 'matricAnt');
          vinculo.transfDom.dtTransf       := StringToDateTime(Leitor.rCampo(tcDat, 'dtTransf'));
        end;

        if Leitor.rExtrai(3, 'mudancaCPF') <> '' then
        begin
          vinculo.mudancaCPF.cpfAnt     := Leitor.rCampo(tcStr, 'cpfAnt');
          vinculo.mudancaCPF.matricAnt  := Leitor.rCampo(tcStr, 'matricAnt');
          vinculo.mudancaCPF.dtAltCPF   := StringToDateTime(Leitor.rCampo(tcDat, 'dtAltCPF'));
          vinculo.mudancaCPF.observacao := Leitor.rCampo(tcStr, 'observacao');
        end;

        if Leitor.rExtrai(3, 'afastamento') <> '' then
        begin
          vinculo.afastamento.DtIniAfast  := StringToDateTime(Leitor.rCampo(tcDat, 'dtIniAfast'));
          vinculo.afastamento.codMotAfast := eSStrTotpMotivosAfastamento(bOk, Leitor.rCampo(tcStr, 'codMotAfast'));
        end;

        if Leitor.rExtrai(3, 'desligamento') <> '' then
          vinculo.desligamento.DtDeslig := StringToDateTime(Leitor.rCampo(tcDat, 'dtDeslig'));

        if Leitor.rExtrai(3, 'cessao') <> '' then
          vinculo.cessao.dtIniCessao := StringToDateTime(Leitor.rCampo(tcDat, 'dtIniCessao'));
      end;
    end;
  finally
    Leitor.Free;
  end;
end;

end.
