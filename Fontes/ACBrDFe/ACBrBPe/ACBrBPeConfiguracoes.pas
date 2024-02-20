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

{$I ACBr.inc}

unit ACBrBPeConfiguracoes;

interface

uses
  Classes, SysUtils, IniFiles,
  ACBrDFeConfiguracoes, pcnConversao, ACBrBPeConversao;

type

  { TGeralConfBPe }

  TGeralConfBPe = class(TGeralConf)
  private
    FModeloDF: TModeloBPe;
    FModeloDFCodigo: integer;
    FVersaoDF: TVersaoBPe;

    procedure SetVersaoDF(const Value: TVersaoBPe);
    procedure SetModeloDF(const Value: TModeloBPe);
  public
    constructor Create(AOwner: TConfiguracoes); override;
    procedure Assign(DeGeralConfBPe: TGeralConfBPe); reintroduce;
    procedure GravarIni(const AIni: TCustomIniFile); override;
    procedure LerIni(const AIni: TCustomIniFile); override;

  published
    property ModeloDF: TModeloBPe read FModeloDF write SetModeloDF default moBPe;
    property ModeloDFCodigo: integer read FModeloDFCodigo;
    property VersaoDF: TVersaoBPe read FVersaoDF write SetVersaoDF default ve100;
  end;

  { TArquivosConfBPe }

  TArquivosConfBPe = class(TArquivosConf)
  private
    FEmissaoPathBPe: Boolean;
    FSalvarEvento: Boolean;
    FSalvarApenasBPeProcessadas: Boolean;
    FNormatizarMunicipios: Boolean;
    FPathBPe: String;
    FPathEvento: String;
    FPathArquivoMunicipios: String;
  public
    constructor Create(AOwner: TConfiguracoes); override;
    destructor Destroy; override;
    procedure Assign(DeArquivosConfBPe: TArquivosConfBPe); reintroduce;
    procedure GravarIni(const AIni: TCustomIniFile); override;
    procedure LerIni(const AIni: TCustomIniFile); override;

    function GetPathBPe(Data: TDateTime = 0; const CNPJ: String = '';
      const AIE: String = ''; Modelo: TModeloBPe = moBPe): String;
    function GetPathEvento(tipoEvento: TpcnTpEvento; const CNPJ: String = '';
      const AIE: String = ''; Data: TDateTime = 0): String;
  published
    property EmissaoPathBPe: Boolean read FEmissaoPathBPe
      write FEmissaoPathBPe default False;
    property SalvarEvento: Boolean read FSalvarEvento
      write FSalvarEvento default False;
    property SalvarApenasBPeProcessadas: Boolean
      read FSalvarApenasBPeProcessadas write FSalvarApenasBPeProcessadas default False;
    property NormatizarMunicipios: boolean read FNormatizarMunicipios
      write FNormatizarMunicipios default False;
    property PathBPe: String read FPathBPe write FPathBPe;
    property PathEvento: String read FPathEvento write FPathEvento;
    property PathArquivoMunicipios: String read FPathArquivoMunicipios write FPathArquivoMunicipios;
  end;

  { TConfiguracoesBPe }

  TConfiguracoesBPe = class(TConfiguracoes)
  private
    function GetArquivos: TArquivosConfBPe;
    function GetGeral: TGeralConfBPe;
  protected
    procedure CreateGeralConf; override;
    procedure CreateArquivosConf; override;

  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(DeConfiguracoesBPe: TConfiguracoesBPe); reintroduce;

  published
    property Geral: TGeralConfBPe read GetGeral;
    property Arquivos: TArquivosConfBPe read GetArquivos;
    property WebServices;
    property Certificados;
    property RespTec;
  end;

implementation

uses
  ACBrUtil.FilesIO,
  ACBrBPe,
  DateUtils;

{ TConfiguracoesBPe }

constructor TConfiguracoesBPe.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FPSessaoIni := 'BPe';
  WebServices.ResourceName := 'ACBrBPeServicos';
end;

procedure TConfiguracoesBPe.Assign(DeConfiguracoesBPe: TConfiguracoesBPe);
begin
  Geral.Assign(DeConfiguracoesBPe.Geral);
  WebServices.Assign(DeConfiguracoesBPe.WebServices);
  Certificados.Assign(DeConfiguracoesBPe.Certificados);
  Arquivos.Assign(DeConfiguracoesBPe.Arquivos);
  RespTec.Assign(DeConfiguracoesBPe.RespTec);
end;

function TConfiguracoesBPe.GetArquivos: TArquivosConfBPe;
begin
  Result := TArquivosConfBPe(FPArquivos);
end;

function TConfiguracoesBPe.GetGeral: TGeralConfBPe;
begin
  Result := TGeralConfBPe(FPGeral);
end;

procedure TConfiguracoesBPe.CreateGeralConf;
begin
  FPGeral := TGeralConfBPe.Create(Self);
end;

procedure TConfiguracoesBPe.CreateArquivosConf;
begin
  FPArquivos := TArquivosConfBPe.Create(self);
end;

{ TGeralConfBPe }

constructor TGeralConfBPe.Create(AOwner: TConfiguracoes);
begin
  inherited Create(AOwner);

  FModeloDF := moBPe;
  FModeloDFCodigo := StrToInt(ModeloBPeToStr(FModeloDF));
  FVersaoDF := ve100;
end;

procedure TGeralConfBPe.Assign(DeGeralConfBPe: TGeralConfBPe);
begin
  inherited Assign(DeGeralConfBPe);

  ModeloDF := DeGeralConfBPe.ModeloDF;
  VersaoDF := DeGeralConfBPe.VersaoDF;
end;

procedure TGeralConfBPe.SetModeloDF(const Value: TModeloBPe);
begin
  FModeloDF := Value;
  FModeloDFCodigo := StrToInt(ModeloBPeToStr(FModeloDF));
end;

procedure TGeralConfBPe.SetVersaoDF(const Value: TVersaoBPe);
begin
  FVersaoDF := Value;
end;

procedure TGeralConfBPe.GravarIni(const AIni: TCustomIniFile);
begin
  inherited GravarIni(AIni);

  AIni.WriteInteger(fpConfiguracoes.SessaoIni, 'ModeloDF', Integer(ModeloDF));
  AIni.WriteInteger(fpConfiguracoes.SessaoIni, 'VersaoDF', Integer(VersaoDF));
end;

procedure TGeralConfBPe.LerIni(const AIni: TCustomIniFile);
begin
  inherited LerIni(AIni);

  ModeloDF := TModeloBPe(AIni.ReadInteger(fpConfiguracoes.SessaoIni, 'ModeloDF', Integer(ModeloDF)));
  VersaoDF := TVersaoBPe(AIni.ReadInteger(fpConfiguracoes.SessaoIni, 'VersaoDF', Integer(VersaoDF)));
end;

{ TArquivosConfBPe }

constructor TArquivosConfBPe.Create(AOwner: TConfiguracoes);
begin
  inherited Create(AOwner);

  FEmissaoPathBPe := False;
  FSalvarEvento := False;
  FSalvarApenasBPeProcessadas := False;
  FNormatizarMunicipios := False;
  FPathBPe := '';
  FPathEvento := '';
  FPathArquivoMunicipios := '';
end;

destructor TArquivosConfBPe.Destroy;
begin

  inherited;
end;

procedure TArquivosConfBPe.Assign(DeArquivosConfBPe: TArquivosConfBPe);
begin
  inherited Assign(DeArquivosConfBPe);

  FEmissaoPathBPe             := DeArquivosConfBPe.EmissaoPathBPe;
  FSalvarEvento               := DeArquivosConfBPe.SalvarEvento;
  FSalvarApenasBPeProcessadas := DeArquivosConfBPe.SalvarApenasBPeProcessadas;
  FNormatizarMunicipios       := DeArquivosConfBPe.NormatizarMunicipios;
  FPathBPe                    := DeArquivosConfBPe.PathBPe;
  FPathEvento                 := DeArquivosConfBPe.PathEvento;
  FPathArquivoMunicipios      := DeArquivosConfBPe.PathArquivoMunicipios;
end;

function TArquivosConfBPe.GetPathEvento(tipoEvento: TpcnTpEvento; const CNPJ: String = '';
  const AIE: String = ''; Data: TDateTime = 0): String;
var
  Dir: String;
begin
  Dir := GetPath(FPathEvento, 'Evento', CNPJ, AIE, Data);

  if AdicionarLiteral then
    Dir := PathWithDelim(Dir) + TpEventoToDescStr(tipoEvento);

  if not DirectoryExists(Dir) then
    ForceDirectories(Dir);

  Result := Dir;
end;

function TArquivosConfBPe.GetPathBPe(Data: TDateTime = 0; const CNPJ: String = '';
  const AIE: String = ''; Modelo: TModeloBPe = moBPe): String;
var
  DescricaoModelo: String;
begin
  if Modelo = moBPe then
    DescricaoModelo := 'BPe'
  else
    DescricaoModelo := 'BPeTM';

  Result := GetPath(FPathBPe, DescricaoModelo, CNPJ, AIE, Data, DescricaoModelo);
end;

procedure TArquivosConfBPe.GravarIni(const AIni: TCustomIniFile);
begin
  inherited GravarIni(AIni);

  AIni.WriteBool(fpConfiguracoes.SessaoIni, 'SalvarEvento', SalvarEvento);
  AIni.WriteBool(fpConfiguracoes.SessaoIni, 'SalvarApenasBPeProcessadas', SalvarApenasBPeProcessadas);
  AIni.WriteBool(fpConfiguracoes.SessaoIni, 'EmissaoPathBPe', EmissaoPathBPe);
  AIni.WriteBool(fpConfiguracoes.SessaoIni, 'NormatizarMunicipios', NormatizarMunicipios);
  AIni.WriteString(fpConfiguracoes.SessaoIni, 'PathBPe', PathBPe);
  AIni.WriteString(fpConfiguracoes.SessaoIni, 'PathEvento', PathEvento);
  AIni.WriteString(fpConfiguracoes.SessaoIni, 'PathArquivoMunicipios', PathArquivoMunicipios);
end;

procedure TArquivosConfBPe.LerIni(const AIni: TCustomIniFile);
begin
  inherited LerIni(AIni);

  SalvarEvento := AIni.ReadBool(fpConfiguracoes.SessaoIni, 'SalvarEvento', SalvarEvento);
  SalvarApenasBPeProcessadas := AIni.ReadBool(fpConfiguracoes.SessaoIni, 'SalvarApenasBPeProcessadas', SalvarApenasBPeProcessadas);
  EmissaoPathBPe := AIni.ReadBool(fpConfiguracoes.SessaoIni, 'EmissaoPathBPe', EmissaoPathBPe);
  NormatizarMunicipios := AIni.ReadBool(fpConfiguracoes.SessaoIni, 'NormatizarMunicipios', NormatizarMunicipios);
  PathBPe := AIni.ReadString(fpConfiguracoes.SessaoIni, 'PathBPe', PathBPe);
  PathEvento := AIni.ReadString(fpConfiguracoes.SessaoIni, 'PathEvento', PathEvento);
  PathArquivoMunicipios := AIni.ReadString(fpConfiguracoes.SessaoIni, 'PathArquivoMunicipios', PathArquivoMunicipios);
end;

end.
