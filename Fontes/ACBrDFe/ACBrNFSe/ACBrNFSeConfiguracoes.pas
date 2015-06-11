{******************************************************************************}
{ Projeto: Componente ACBrNFSe                                                 }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Nota Fiscal}
{  de Serviço eletrônica - NFSe                                                }

{ Direitos Autorais Reservados (c) 2008 Wemerson Souto                         }
{                                       Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }

{ Colaboradores nesse arquivo:                                                 }

{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }


{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }

{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }

{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }

{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }

{******************************************************************************}

{$I ACBr.inc}

unit ACBrNFSeConfiguracoes;

interface

uses
  Classes, SysUtils,
  ACBrDFeConfiguracoes, pcnConversao, pnfsConversao;

type

 TConfigCidade = record
    VersaoSoap: String;
    Prefixo2: String;
    Prefixo3: String;
    Prefixo4: String;
    Identificador: String;
    NameSpaceEnvelope: String;
    AssinarRPS: Boolean;
    AssinarLote: Boolean;
    AssinarGerar: Boolean;
    QuebradeLinha: String;
 end;

 TConfigSchema = record
    VersaoCabecalho: String;
    VersaoDados: String;
    VersaoXML: String;
    NameSpaceXML: String;
    Cabecalho: String;
    ServicoEnviar: String;
    ServicoConSit: String;
    ServicoConLot: String;
    ServicoConRps: String;
    ServicoConSeqRps: String;
    ServicoConNfse: String;
    ServicoCancelar: String;
    ServicoGerar: String;
    ServicoEnviarSincrono: String;
    ServicoSubstituir: String;
    DefTipos: String;
  end;

 TConfigURL = record
    HomNomeCidade:String;
    HomRecepcaoLoteRPS: String;
    HomConsultaLoteRPS: String;
    HomConsultaNFSeRPS: String;
    HomConsultaSitLoteRPS: String;
    HomConsultaSeqRPS: String;
    HomConsultaNFSe: String;
    HomCancelaNFSe: String;
    HomGerarNFSe: String;
    HomRecepcaoSincrono: String;
    HomSubstituiNFSe: String;

    ProNomeCidade:String;
    ProRecepcaoLoteRPS: String;
    ProConsultaLoteRPS: String;
    ProConsultaSeqRPS: String;
    ProConsultaNFSeRPS: String;
    ProConsultaSitLoteRPS: String;
    ProConsultaNFSe: String;
    ProCancelaNFSe: String;
    ProGerarNFSe: String;
    ProRecepcaoSincrono: String;
    ProSubstituiNFSe: String;
  end;

  { TGeralConfNFSe }

  TGeralConfNFSe = class(TGeralConf)
  private
    FConfigCidade: TConfigCidade;
    FConfigSchema: TConfigSchema;
    FConfigURL: TConfigURL;

    FCodigoMunicipio: Integer;
    FProvedor: TnfseProvedor;
    FxProvedor: String;
    FSenhaWeb: AnsiString;
    FConsultaLoteAposEnvio: Boolean;
    FUserWeb: String;

  public
    constructor Create(AOwner: TConfiguracoes); override;
    procedure Assign(DeGeralConfNFSe: TGeralConfNFSe); overload;

    procedure SetConfigMunicipio;
  published
    property ConfigCidade: TConfigCidade read FConfigCidade;
    property ConfigSchema: TConfigSchema read FConfigSchema;
    property ConfigURL: TConfigURL read FConfigURL;

    property CodigoMunicipio: Integer read FCodigoMunicipio write FCodigoMunicipio;
    property Provedor: TnfseProvedor read FProvedor;
    property xProvedor: String read FxProvedor;
    property SenhaWeb: AnsiString read FSenhaWeb write FSenhaWeb;
    property UserWeb: String read FUserWeb write FUserWeb;
    property ConsultaLoteAposEnvio: Boolean read FConsultaLoteAposEnvio write FConsultaLoteAposEnvio;
  end;

  { TArquivosConfNFSe }

  TArquivosConfNFSe = class(TArquivosConf)
  private
    FEmissaoPathNFSe: boolean;
    FSalvarApenasNFSeProcessadas: boolean;
    FPathGer: String;
    FPathRPS: String;
    FPathNFSe: String;
    FPathCan: String;
    FNomeLongoNFSe: Boolean;
    FTabServicosExt: Boolean;
  public
    constructor Create(AOwner: TConfiguracoes); override;
    procedure Assign(DeArquivosConfNFSe: TArquivosConfNFSe); overload;

    function GetPathGer(Data: TDateTime = 0; CNPJ: String = ''): String;
    function GetPathRPS(Data: TDateTime = 0; CNPJ: String = ''): String;
    function GetPathNFSe(Data: TDateTime = 0; CNPJ: String = ''): String;
    function GetPathCan(Data: TDateTime = 0; CNPJ: String = ''): String;
  published
    property EmissaoPathNFSe: boolean read FEmissaoPathNFSe
      write FEmissaoPathNFSe default False;
    property SalvarApenasNFSeProcessadas: boolean
      read FSalvarApenasNFSeProcessadas write FSalvarApenasNFSeProcessadas default False;
    property PathGer: String read FPathGer write FPathGer;
    property PathRPS: String read FPathRPS  write FPathRPS;
    property PathNFSe: String read FPathNFSe write FPathNFSe;
    property PathCan: String read FPathCan write FPathCan;
    property NomeLongoNFSe: Boolean read FNomeLongoNFSe write FNomeLongoNFSe default False;
    property TabServicosExt: Boolean read FTabServicosExt write FTabServicosExt default False;
  end;

  { TConfiguracoesNFSe }

  TConfiguracoesNFSe = class(TConfiguracoes)
  private
    function GetArquivos: TArquivosConfNFSe;
    function GetGeral: TGeralConfNFSe;
  protected
    procedure CreateGeralConf; override;
    procedure CreateArquivosConf; override;

  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(DeConfiguracoesNFSe: TConfiguracoesNFSe); overload;

  published
    property Geral: TGeralConfNFSe read GetGeral;
    property Arquivos: TArquivosConfNFSe read GetArquivos;
    property WebServices;
    property Certificados;
  end;

implementation

uses
  ACBrUtil,
  DateUtils;

{ TConfiguracoesNFSe }

constructor TConfiguracoesNFSe.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  WebServices.ResourceName := 'ACBrNFSeServicos';
end;

procedure TConfiguracoesNFSe.Assign(DeConfiguracoesNFSe: TConfiguracoesNFSe);
begin
  Geral.Assign(DeConfiguracoesNFSe.Geral);
  WebServices.Assign(DeConfiguracoesNFSe.WebServices);
  Certificados.Assign(DeConfiguracoesNFSe.Certificados);
  Arquivos.Assign(DeConfiguracoesNFSe.Arquivos);
end;

function TConfiguracoesNFSe.GetArquivos: TArquivosConfNFSe;
begin
  Result := TArquivosConfNFSe(FPArquivos);
end;

function TConfiguracoesNFSe.GetGeral: TGeralConfNFSe;
begin
  Result := TGeralConfNFSe(FPGeral);
end;

procedure TConfiguracoesNFSe.CreateGeralConf;
begin
  FPGeral := TGeralConfNFSe.Create(Self);
end;

procedure TConfiguracoesNFSe.CreateArquivosConf;
begin
  FPArquivos := TArquivosConfNFSe.Create(self);
end;

{ TGeralConfNFSe }

constructor TGeralConfNFSe.Create(AOwner: TConfiguracoes);
begin
  inherited Create(AOwner);

  FProvedor := proNenhum;
end;

procedure TGeralConfNFSe.Assign(DeGeralConfNFSe: TGeralConfNFSe);
begin
  inherited Assign(DeGeralConfNFSe);

  FProvedor := DeGeralConfNFSe.Provedor;
end;

procedure TGeralConfNFSe.SetConfigMunicipio;
var
 Ok:           Boolean;
 ConfigCidade: TConfigCidade;
 ConfigSchema: TConfigSchema;
 ConfigURL:    TConfigURL;
begin
 FxProvedor := CodCidadeToProvedor(FCodigoMunicipio);
 FProvedor  := StrToProvedor(Ok, FxProvedor);

 if FProvedor = proNenhum
  then raise Exception.Create('Código do Municipio ['+ IntToStr(FCodigoMunicipio) +'] não Encontrado.');


end;

{ TArquivosConfNFSe }

constructor TArquivosConfNFSe.Create(AOwner: TConfiguracoes);
begin
  inherited Create(AOwner);

  FEmissaoPathNFSe := False;
  FSalvarApenasNFSeProcessadas := False;
  FPathGer := '';
  FPathRPS := '';
  FPathNFSe := '';
  FPathCan := '';
  FNomeLongoNFSe := False;
  FTabServicosExt := False;
end;

procedure TArquivosConfNFSe.Assign(DeArquivosConfNFSe: TArquivosConfNFSe);
begin
  inherited Assign(DeArquivosConfNFSe);

  EmissaoPathNFSe := DeArquivosConfNFSe.EmissaoPathNFSe;
  SalvarApenasNFSeProcessadas := DeArquivosConfNFSe.SalvarApenasNFSeProcessadas;
  PathGer := DeArquivosConfNFSe.PathGer;
  PathRPS := DeArquivosConfNFSe.PathRPS;
  PathNFSe := DeArquivosConfNFSe.PathNFSe;
  PathCan := DeArquivosConfNFSe.PathCan;
  NomeLongoNFSe := DeArquivosConfNFSe.NomeLongoNFSe;
  TabServicosExt := DeArquivosConfNFSe.TabServicosExt;
end;

function TArquivosConfNFSe.GetPathGer(Data: TDateTime;
  CNPJ: String): String;
begin
  Result := GetPath(FPathGer, 'Ger', CNPJ, Data);
end;

function TArquivosConfNFSe.GetPathRPS(Data: TDateTime;
  CNPJ: String): String;
begin
  Result := GetPath(FPathRPS, 'RPS', CNPJ, Data);
end;

function TArquivosConfNFSe.GetPathNFSe(Data: TDateTime = 0;
  CNPJ: String = ''): String;
begin
  Result := GetPath(FPathNFSe, 'NFSe', CNPJ, Data);
end;

function TArquivosConfNFSe.GetPathCan(Data: TDateTime = 0;
  CNPJ: String = ''): String;
begin
  Result := GetPath(FPathCan, 'Can', CNPJ, Data);
end;

end.
