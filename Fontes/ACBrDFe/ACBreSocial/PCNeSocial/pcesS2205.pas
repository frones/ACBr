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
|*  - Passado o namespace para geração do cabeçalho
******************************************************************************}
{$I ACBr.inc}

unit pcesS2205;

interface

uses
  SysUtils, Classes,
  pcnConversao, ACBrUtil,
  pcesCommon, pcesConversaoeSocial, pcesGerador;

type
  TS2205Collection = class;
  TS2205CollectionItem = class;
  TEvtAltCadastral = class;

  TS2205Collection = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TS2205CollectionItem;
    procedure SetItem(Index: Integer; Value: TS2205CollectionItem);
  public
    function Add: TS2205CollectionItem;
    property Items[Index: Integer]: TS2205CollectionItem read GetItem write SetItem; default;
  end;

  TS2205CollectionItem = class(TCollectionItem)
  private
    FTipoEvento: TTipoEvento;
    FEvtAltCadastral: TEvtAltCadastral;
    procedure setEvtAltCadastral(const Value: TEvtAltCadastral);
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;
  published
    property TipoEvento: TTipoEvento read FTipoEvento;
    property EvtAltCadastral: TEvtAltCadastral read FEvtAltCadastral write setEvtAltCadastral;
  end;

  TEvtAltCadastral = class(TeSocialEvento)
  private
    FdtAlteracao: TDateTime;
    FIdeEvento: TIdeEvento2;
    FIdeEmpregador: TIdeEmpregador;
    FTrabalhador: TTrabalhador;
    FVinculo: TVinculo;
    FIdeTrabalhador: TideTrabalhador;
    FACBreSocial: TObject;

    procedure GerarInfoAltCadastral;
  public
    constructor Create(AACBreSocial: TObject);
    destructor destroy; override;

    function GerarXML: boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property dtAlteracao: TDateTime read FdtAlteracao write FdtAlteracao;
    property IdeEvento: TIdeEvento2 read FIdeEvento write FIdeEvento;
    property IdeEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property Trabalhador: TTrabalhador read FTrabalhador write FTrabalhador;
    property Vinculo: TVinculo read FVinculo write FVinculo;
    property IdeTrabalhador: TideTrabalhador read FIdeTrabalhador write FIdeTrabalhador;
  end;

implementation

uses
  IniFiles,
  ACBreSocial, ACBrDFeUtil;

{ TS2205Collection }

function TS2205Collection.Add: TS2205CollectionItem;
begin
  Result := TS2205CollectionItem(inherited Add);
  Result.Create(TComponent(Self.Owner));
end;

function TS2205Collection.GetItem(Index: Integer): TS2205CollectionItem;
begin
  Result := TS2205CollectionItem(inherited GetItem(Index));
end;

procedure TS2205Collection.SetItem(Index: Integer;
  Value: TS2205CollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TS2205CollectionItem }

constructor TS2205CollectionItem.Create(AOwner: TComponent);
begin
  FTipoEvento := teS2205;
  FEvtAltCadastral := TEvtAltCadastral.Create(AOwner);
end;

destructor TS2205CollectionItem.Destroy;
begin
  FEvtAltCadastral.Free;

  inherited;
end;

procedure TS2205CollectionItem.setEvtAltCadastral(
  const Value: TEvtAltCadastral);
begin
  FEvtAltCadastral.Assign(Value);
end;

{ TEvtAltCadastral }

constructor TEvtAltCadastral.Create(AACBreSocial: TObject);
begin
  inherited;

  FACBreSocial := AACBreSocial;
  FIdeEvento := TIdeEvento2.Create;
  FIdeEmpregador := TIdeEmpregador.Create;
  FTrabalhador := TTrabalhador.Create;
  FVinculo := TVinculo.Create;
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

  GerarTrabalhador(self.Trabalhador, 'dadosTrabalhador');

  GerarModoFechamento(mlAlteracao);
end;

function TEvtAltCadastral.GerarXML: boolean;
begin
  try
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

    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtAltCadastral');

    Validar(schevtAltCadastral);
  except on e:exception do
    raise Exception.Create(e.Message);
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
  Result := False;

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);

    with Self do
    begin
      sSecao := 'evtAltCadastral';
      Sequencial := INIRec.ReadInteger(sSecao, 'Sequencial', 0);

      sSecao := 'ideEvento';
      ideEvento.indRetif    := eSStrToIndRetificacao(Ok, INIRec.ReadString(sSecao, 'indRetif', '1'));
      ideEvento.NrRecibo    := INIRec.ReadString(sSecao, 'nrRecibo', EmptyStr);
      ideEvento.TpAmb       := eSStrTotpAmb(Ok, INIRec.ReadString(sSecao, 'tpAmb', '1'));
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

      sSecao := 'CTPS';
      trabalhador.documentos.CTPS.NrCtps    := INIRec.ReadString(sSecao, 'nrCtps', '');
      trabalhador.documentos.CTPS.SerieCtps := INIRec.ReadString(sSecao, 'serieCtps', '');
      trabalhador.documentos.CTPS.UfCtps    := INIRec.ReadString(sSecao, 'ufCtps', 'SP');

      sSecao := 'RIC';
      trabalhador.documentos.RIC.NrRic        := INIRec.ReadString(sSecao, 'nrRic', '');
      trabalhador.documentos.RIC.OrgaoEmissor := INIRec.ReadString(sSecao, 'orgaoEmissor', '');
      trabalhador.documentos.RIC.DtExped      := StringToDateTime(INIRec.ReadString(sSecao, 'dtExped', '0'));

      sSecao := 'RG';
      trabalhador.documentos.rg.NrRg         := INIRec.ReadString(sSecao, 'nrRg', '');
      trabalhador.documentos.rg.OrgaoEmissor := INIRec.ReadString(sSecao, 'orgaoEmissor', '');
      trabalhador.documentos.rg.DtExped      := StringToDateTime(INIRec.ReadString(sSecao, 'dtExped', '0'));

      sSecao := 'RNE';
      trabalhador.documentos.RNE.NrRne        := INIRec.ReadString(sSecao, 'nrRne', '');
      trabalhador.documentos.RNE.OrgaoEmissor := INIRec.ReadString(sSecao, 'orgaoEmissor', '');
      trabalhador.documentos.RNE.DtExped      := StringToDateTime(INIRec.ReadString(sSecao, 'dtExped', '0'));

      sSecao := 'OC';
      trabalhador.documentos.OC.NrOc         := INIRec.ReadString(sSecao, 'nrOc', '');
      trabalhador.documentos.OC.OrgaoEmissor := INIRec.ReadString(sSecao, 'orgaoEmissor', '');
      trabalhador.documentos.OC.DtExped      := StringToDateTime(INIRec.ReadString(sSecao, 'dtExped', '0'));
      trabalhador.documentos.OC.DtValid      := StringToDateTime(INIRec.ReadString(sSecao, 'dtValid', '0'));

      sSecao := 'CNH';
      trabalhador.documentos.CNH.nrRegCnh     := INIRec.ReadString(sSecao, 'nrRegCnh', '');
      trabalhador.documentos.CNH.DtExped      := StringToDateTime(INIRec.ReadString(sSecao, 'dtExped', '0'));
      trabalhador.documentos.CNH.ufCnh        := eSStrTouf(Ok, INIRec.ReadString(sSecao, 'ufCnh', 'SP'));
      trabalhador.documentos.CNH.DtValid      := StringToDateTime(INIRec.ReadString(sSecao, 'dtValid', '0'));
      trabalhador.documentos.CNH.dtPriHab     := StringToDateTime(INIRec.ReadString(sSecao, 'dtPriHab', '0'));
      trabalhador.documentos.CNH.categoriaCnh := eSStrToCnh(Ok, INIRec.ReadString(sSecao, 'dtPriHab', 'A'));

      sSecao := 'enderecoBrasil';
      trabalhador.Endereco.Brasil.TpLograd    := INIRec.ReadString(sSecao, 'tpLograd', '');
      trabalhador.Endereco.Brasil.DscLograd   := INIRec.ReadString(sSecao, 'dscLograd', '');
      trabalhador.Endereco.Brasil.NrLograd    := INIRec.ReadString(sSecao, 'nrLograd', '');
      trabalhador.Endereco.Brasil.Complemento := INIRec.ReadString(sSecao, 'complemento', '');
      trabalhador.Endereco.Brasil.Bairro      := INIRec.ReadString(sSecao, 'bairro', '');
      trabalhador.Endereco.Brasil.Cep         := INIRec.ReadString(sSecao, 'cep', '');
      trabalhador.Endereco.Brasil.CodMunic    := INIRec.ReadInteger(sSecao, 'codMunic', 0);
      trabalhador.Endereco.Brasil.UF          := eSStrTouf(Ok, INIRec.ReadString(sSecao, 'uf', 'SP'));

      sSecao := 'enderecoExterior';
      trabalhador.Endereco.Exterior.PaisResid   := INIRec.ReadString(sSecao, 'paisResid', '');
      trabalhador.Endereco.Exterior.DscLograd   := INIRec.ReadString(sSecao, 'dscLograd', '');
      trabalhador.Endereco.Exterior.NrLograd    := INIRec.ReadString(sSecao, 'nrLograd', '');
      trabalhador.Endereco.Exterior.Complemento := INIRec.ReadString(sSecao, 'complemento', '');
      trabalhador.Endereco.Exterior.Bairro      := INIRec.ReadString(sSecao, 'bairro', '');
      trabalhador.Endereco.Exterior.NmCid       := INIRec.ReadString(sSecao, 'nmCid', '');
      trabalhador.Endereco.Exterior.CodPostal   := INIRec.ReadString(sSecao, 'codPostal', '');

      sSecao := 'trabEstrangeiro';
      trabalhador.TrabEstrangeiro.DtChegada        := StringToDateTime(INIRec.ReadString(sSecao, 'dtChegada', '0'));
      trabalhador.TrabEstrangeiro.ClassTrabEstrang := eSStrToClassTrabEstrang(Ok, INIRec.ReadString(sSecao, 'classTrabEstrang', '1'));
      trabalhador.TrabEstrangeiro.CasadoBr         := INIRec.ReadString(sSecao, 'casadoBr', 'S');
      trabalhador.TrabEstrangeiro.FilhosBr         := INIRec.ReadString(sSecao, 'filhosBr', 'S');

      sSecao := 'infoDeficiencia';
      trabalhador.infoDeficiencia.DefFisica      := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'defFisica', 'S'));
      trabalhador.infoDeficiencia.DefVisual      := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'defVisual', 'S'));
      trabalhador.infoDeficiencia.DefAuditiva    := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'defAuditiva', 'S'));
      trabalhador.infoDeficiencia.DefMental      := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'defMental', 'S'));
      trabalhador.infoDeficiencia.DefIntelectual := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'defIntelectual', 'S'));
      trabalhador.infoDeficiencia.ReabReadap     := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'reabReadap', 'S'));
      trabalhador.infoDeficiencia.infoCota       := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'infoCota', 'S'));
      trabalhador.infoDeficiencia.Observacao     := INIRec.ReadString(sSecao, 'observacao', '');

      I := 1;
      while true do
      begin
        // de 00 até 99
        sSecao := 'dependente' + IntToStrZero(I, 2);
        sFim   := INIRec.ReadString(sSecao, 'tpDep', 'FIM');

        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break;

        with trabalhador.Dependente.Add do
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
      trabalhador.aposentadoria.TrabAposent := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'trabAposent', 'S'));

      sSecao := 'contato';
      trabalhador.contato.FonePrinc     := INIRec.ReadString(sSecao, 'fonePrinc', '');
      trabalhador.contato.FoneAlternat  := INIRec.ReadString(sSecao, 'foneAlternat', 'S');
      trabalhador.contato.EmailPrinc    := INIRec.ReadString(sSecao, 'emailPrinc', 'S');
      trabalhador.contato.EmailAlternat := INIRec.ReadString(sSecao, 'emailAlternat', 'S');
    end;

    GerarXML;

    Result := True;
  finally
     INIRec.Free;
  end;
end;

end.
