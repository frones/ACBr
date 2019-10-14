{******************************************************************************}
{ Projeto: Componente ACBreSocial                                              }
{  Biblioteca multiplataforma de componentes Delphi para envio dos eventos do  }
{ eSocial - http://www.esocial.gov.br/                                         }
{                                                                              }
{ Direitos Autorais Reservados (c) 2008 Wemerson Souto                         }
{                                       Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }
{                                                                              }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 27/10/2015: Jean Carlo Cantu, Tiago Ravache
|*  - Doação do componente para o Projeto ACBr
|* 01/03/2016: Guilherme Costa
|*  - Passado o namespace para geração no cabeçalho
******************************************************************************}
{$I ACBr.inc}

unit pcesS2200;

interface

uses
  SysUtils, Classes, Contnrs,
  pcnConversao, ACBrUtil,
  pcesCommon, pcesConversaoeSocial, pcesGerador;

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
    function LerArqIni(const AIniString: String): Boolean;

    property IdeEvento: TIdeEvento2 read FIdeEvento write FIdeEvento;
    property IdeEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property Trabalhador: TTrabalhador read FTrabalhador write FTrabalhador;
    property Vinculo: TVinculo read FVinculo write FVinculo;
  end;

implementation

uses
  IniFiles,
  ACBreSocial;

{ TS2200Collection }

function TS2200Collection.Add: TS2200CollectionItem;
begin
  Result := Self.New;
end;

function TS2200Collection.GetItem(Index: Integer): TS2200CollectionItem;
begin
  Result := TS2200CollectionItem(inherited GetItem(Index));
end;

procedure TS2200Collection.SetItem(Index: Integer;
  Value: TS2200CollectionItem);
begin
  inherited SetItem(Index, Value);
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

    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtAdmissao');
    Validar(schevtAdmissao);
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
        trabalhador.documentos.CNH.ufCnh        := eSStrTouf(Ok, INIRec.ReadString(sSecao, 'ufCnh', 'SP'));
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
        trabalhador.Endereco.Brasil.UF          := eSStrTouf(Ok, INIRec.ReadString(sSecao, 'uf', 'SP'));
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

      sSecao := 'infoDeficiencia';
      if INIRec.ReadString(sSecao, 'defFisica', '') <> '' then
      begin
        trabalhador.infoDeficiencia.DefFisica      := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'defFisica', 'S'));
        trabalhador.infoDeficiencia.DefVisual      := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'defVisual', 'S'));
        trabalhador.infoDeficiencia.DefAuditiva    := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'defAuditiva', 'S'));
        trabalhador.infoDeficiencia.DefMental      := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'defMental', 'S'));
        trabalhador.infoDeficiencia.DefIntelectual := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'defIntelectual', 'S'));
        trabalhador.infoDeficiencia.ReabReadap     := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'reabReadap', 'S'));
        trabalhador.infoDeficiencia.infoCota       := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'infoCota', 'S'));
        trabalhador.infoDeficiencia.Observacao     := INIRec.ReadString(sSecao, 'observacao', '');
      end;

      I := 1;
      while true do
      begin
        // de 00 até 99
        sSecao := 'dependente' + IntToStrZero(I, 2);
        sFim   := INIRec.ReadString(sSecao, 'tpDep', 'FIM');

        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break;

        with trabalhador.Dependente.New do
        begin
          tpDep    := eSStrToTpDep(Ok, sFim);
          nmDep    := INIRec.ReadString(sSecao, 'nmDep', '');
          dtNascto := StringToDateTime(INIRec.ReadString(sSecao, 'dtNascto', '0'));
          cpfDep   := INIRec.ReadString(sSecao, 'cpfDep', '');
          depIRRF  := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'depIRRF', 'S'));
          depSF    := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'depSF', 'S'));
          incTrab  := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'incTrab', 'S'));
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
        vinculo.InfoRegimeTrab.InfoCeletista.TpRegJor          := eSStrToTpRegJor(Ok, INIRec.ReadString(sSecao, 'tpRegJor', '1'));
        vinculo.InfoRegimeTrab.InfoCeletista.NatAtividade      := eSStrToNatAtividade(Ok, INIRec.ReadString(sSecao, 'natAtividade', '1'));
        vinculo.InfoRegimeTrab.InfoCeletista.dtBase            := INIRec.ReadInteger(sSecao, 'dtBase', 0);
        vinculo.InfoRegimeTrab.InfoCeletista.cnpjSindCategProf := INIRec.ReadString(sSecao, 'cnpjSindCategProf', '');

        sSecao := 'FGTS';
        vinculo.InfoRegimeTrab.InfoCeletista.FGTS.OpcFGTS   := eSStrToOpcFGTS(Ok, INIRec.ReadString(sSecao, 'opcFGTS', '1'));
        vinculo.InfoRegimeTrab.InfoCeletista.FGTS.DtOpcFGTS := StringToDateTime(INIRec.ReadString(sSecao, 'dtOpcFGTS', '0'));
      end;

      sSecao := 'trabTemporario';
      if INIRec.ReadString(sSecao, 'hipLeg', '') <> '' then
      begin
        vinculo.InfoRegimeTrab.InfoCeletista.trabTemporario.hipLeg      := INIRec.ReadInteger(sSecao, 'hipLeg', 1);
        vinculo.InfoRegimeTrab.InfoCeletista.trabTemporario.justContr   := INIRec.ReadString(sSecao, 'justContr', '');
        vinculo.InfoRegimeTrab.InfoCeletista.trabTemporario.tpinclContr := eSStrToTpInclContr(Ok, INIRec.ReadString(sSecao, 'tpinclContr', ''));

        sSecao := 'ideTomadorServ';
        vinculo.InfoRegimeTrab.InfoCeletista.trabTemporario.ideTomadorServ.TpInsc := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
        vinculo.InfoRegimeTrab.InfoCeletista.trabTemporario.ideTomadorServ.NrInsc := INIRec.ReadString(sSecao, 'nrInsc', '');
      end;

      sSecao := 'ideEstabVinc';
      if INIRec.ReadString(sSecao, 'tpInsc', '') <> '' then
      begin
        vinculo.InfoRegimeTrab.InfoCeletista.trabTemporario.ideTomadorServ.ideEstabVinc.TpInsc := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
        vinculo.InfoRegimeTrab.InfoCeletista.trabTemporario.ideTomadorServ.ideEstabVinc.NrInsc := INIRec.ReadString(sSecao, 'nrInsc', '');
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
      if INIRec.ReadString(sSecao, 'tpInsc', '') <> '' then
      begin
        vinculo.InfoRegimeTrab.InfoCeletista.aprend.TpInsc := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
        vinculo.InfoRegimeTrab.InfoCeletista.aprend.NrInsc := INIRec.ReadString(sSecao, 'nrInsc', '');
      end;

      sSecao := 'infoEstatutario';
      if INIRec.ReadString(sSecao, 'indProvim', '') <> '' then
      begin
        vinculo.InfoRegimeTrab.infoEstatutario.IndProvim   := eSStrToIndProvim(Ok, INIRec.ReadString(sSecao, 'indProvim', '1'));
        vinculo.InfoRegimeTrab.infoEstatutario.TpProv      := eSStrToTpProv(Ok, INIRec.ReadString(sSecao, 'tpProv', '1'));
        vinculo.InfoRegimeTrab.infoEstatutario.DtNomeacao  := StringToDateTime(INIRec.ReadString(sSecao, 'dtNomeacao', '0'));
        vinculo.InfoRegimeTrab.infoEstatutario.DtPosse     := StringToDateTime(INIRec.ReadString(sSecao, 'dtPosse', '0'));
        vinculo.InfoRegimeTrab.infoEstatutario.DtExercicio := StringToDateTime(INIRec.ReadString(sSecao, 'dtExercicio', '0'));
        vinculo.InfoRegimeTrab.infoEstatutario.tpPlanRP    := eSStrToTpPlanRP(Ok, INIRec.ReadString(sSecao, 'tpPlanRP', '1'));
        vinculo.InfoRegimeTrab.infoEstatutario.infoDecJud.nrProcJud := INIRec.ReadString(sSecao, 'nrProcJud', '');
      end;

      sSecao := 'infoContrato';
      vinculo.infoContrato.CodCargo    := INIRec.ReadString(sSecao, 'codCargo', '');
      vinculo.infoContrato.CodFuncao   := INIRec.ReadString(sSecao, 'codFuncao', '');
      vinculo.infoContrato.CodCateg    := INIRec.ReadInteger(sSecao, 'codCateg', 0);
      vinculo.infoContrato.codCarreira := INIRec.ReadString(sSecao, 'codCarreira', '');
      vinculo.infoContrato.dtIngrCarr  := StringToDateTime(INIRec.ReadString(sSecao, 'dtIngrCarr', '0'));

      sSecao := 'remuneracao';
      vinculo.infoContrato.remuneracao.VrSalFx    := StringToFloatDef(INIRec.ReadString(sSecao, 'vrSalFx', ''), 0);
      vinculo.infoContrato.remuneracao.UndSalFixo := eSStrToUndSalFixo(Ok, INIRec.ReadString(sSecao, 'undSalFixo', ''));
      vinculo.infoContrato.remuneracao.DscSalVar  := INIRec.ReadString(sSecao, 'dscSalVar', '');

      sSecao := 'duracao';
      vinculo.infoContrato.duracao.TpContr   := eSStrToTpContr(Ok, INIRec.ReadString(sSecao, 'tpContr', '1'));
      vinculo.infoContrato.duracao.dtTerm    := StringToDateTime(INIRec.ReadString(sSecao, 'dtTerm', '0'));
      vinculo.infoContrato.duracao.clauAssec := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'clauAssec', 'S'));
      vinculo.infoContrato.duracao.objDet    := INIRec.ReadString(sSecao, 'objDet', '');

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
        vinculo.infoContrato.LocalTrabalho.localTrabDom.uf          := eSStrTouf(Ok, INIRec.ReadString(sSecao, 'uf', 'SP'));
      end;

      sSecao := 'horContratual';
      if INIRec.ReadString(sSecao, 'tpJornada', '') <> '' then
      begin
        vinculo.infoContrato.horContratual.QtdHrsSem := INIRec.ReadInteger(sSecao, 'qtdHrsSem', 0);
        vinculo.infoContrato.horContratual.TpJornada := eSStrToTpJornada(Ok, INIRec.ReadString(sSecao, 'tpJornada', '1'));
        vinculo.infoContrato.horContratual.DscTpJorn := INIRec.ReadString(sSecao, 'dscTpJorn', '');
        vinculo.infoContrato.horContratual.tmpParc   := StrTotpTmpParc(Ok, INIRec.ReadString(sSecao, 'tmpParc', '0'));
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

      sSecao := 'sucessaoVinc';
      if INIRec.ReadString(sSecao, 'cnpjEmpregAnt', '') <> '' then
      begin
        vinculo.sucessaoVinc.tpInscAnt     := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInscAnt', '1'));
        vinculo.sucessaoVinc.cnpjEmpregAnt := INIRec.ReadString(sSecao, 'cnpjEmpregAnt', '');
        vinculo.sucessaoVinc.MatricAnt     := INIRec.ReadString(sSecao, 'matricAnt', '');
        vinculo.sucessaoVinc.dtTransf      := StringToDateTime(INIRec.ReadString(sSecao, 'dtTransf', '0'));
        vinculo.sucessaoVinc.Observacao    := INIRec.ReadString(sSecao, 'observacao', '');
      end;

      sSecao := 'transfDom';
      if INIRec.ReadString(sSecao, 'cpfSubstituido', '') <> '' then
      begin
        vinculo.transfDom.cpfSubstituido := INIRec.ReadString(sSecao, 'cpfSubstituido', '');
        vinculo.transfDom.MatricAnt      := INIRec.ReadString(sSecao, 'matricAnt', '');
        vinculo.transfDom.dtTransf       := StringToDateTime(INIRec.ReadString(sSecao, 'dtTransf', '0'));
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
    end;

    GerarXML;
  finally
     INIRec.Free;
  end;
end;

end.
