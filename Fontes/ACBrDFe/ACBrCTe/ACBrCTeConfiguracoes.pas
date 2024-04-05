{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Giurizzato Junior                         }
{                              Wemerson Souto                                  }
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

unit ACBrCTeConfiguracoes;

interface

uses
  Classes, Sysutils, IniFiles,
  ACBrDFeConfiguracoes, pcnConversao, pcteConversaoCTe;

type

  { TGeralConfCTe }

  TGeralConfCTe = class(TGeralConf)
  private
    FModeloDF: TModeloCTe;
    FModeloDFCodigo: integer;
    FVersaoDF: TVersaoCTe;

    procedure SetVersaoDF(const Value: TVersaoCTe);
    procedure SetModeloDF(const Value: TModeloCTe);
  public
    constructor Create(AOwner: TConfiguracoes); override;
    procedure Assign(DeGeralConfCTe: TGeralConfCTe); reintroduce;
    procedure GravarIni(const AIni: TCustomIniFile); override;
    procedure LerIni(const AIni: TCustomIniFile); override;

  published
    property ModeloDF: TModeloCTe read FModeloDF write SetModeloDF default moCTe;
    property ModeloDFCodigo: integer read FModeloDFCodigo;
    property VersaoDF: TVersaoCTe read FVersaoDF write SetVersaoDF default ve300;
  end;

  { TArquivosConfCTe }

  TArquivosConfCTe = class(TArquivosConf)
  private
    FEmissaoPathCTe: Boolean;
    FSalvarApenasCTeProcessados: Boolean;
    FNormatizarMunicipios: Boolean;
    FPathCTe: String;
    FPathInu: String;
    FPathEvento: String;
    FPathArquivoMunicipios: String;
  public
    constructor Create(AOwner: TConfiguracoes); override;
    procedure Assign(DeArquivosConfCTe: TArquivosConfCTe); reintroduce;
    destructor Destroy; override;
    procedure GravarIni(const AIni: TCustomIniFile); override;
    procedure LerIni(const AIni: TCustomIniFile); override;

    function GetPathCTe(Data: TDateTime = 0; const CNPJ: String = ''; const IE: String = ''; Modelo: Integer = 0): String;
    function GetPathInu(Data: TDateTime = 0; const CNPJ: String = ''; const IE: String = ''): String;
    function GetPathEvento(tipoEvento: TpcnTpEvento; const CNPJ: String = ''; const IE: String = ''; Data: TDateTime = 0): String;
  published
    property EmissaoPathCTe: Boolean     read FEmissaoPathCte write FEmissaoPathCTe default False;
    property SalvarApenasCTeProcessados: Boolean read FSalvarApenasCTeProcessados write FSalvarApenasCTeProcessados default False;
    property NormatizarMunicipios: boolean read FNormatizarMunicipios write FNormatizarMunicipios default False;
    property PathCTe: String             read FPathCTe        write FPathCTe;
    property PathInu: String             read FPathInu        write FPathInu;
    property PathEvento: String          read FPathEvento     write FPathEvento;
    property PathArquivoMunicipios: String read FPathArquivoMunicipios write FPathArquivoMunicipios;
  end;

  { TConfiguracoesCTe }

  TConfiguracoesCTe = class(TConfiguracoes)
  private
    function GetGeral: TGeralConfCTe;
    function GetArquivos: TArquivosConfCTe;
  protected
    procedure CreateGeralConf; override;
    procedure CreateArquivosConf; override;

  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(DeConfiguracoesCTe: TConfiguracoesCTe); reintroduce;

  published
    property Geral: TGeralConfCTe       read GetGeral;
    property Arquivos: TArquivosConfCTe read GetArquivos;
    property WebServices;
    property Certificados;
    property RespTec;
  end;

implementation

uses
  ACBrUtil.FilesIO, ACBrCTe, DateUtils;

{ TConfiguracoesCTe }

constructor TConfiguracoesCTe.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FPSessaoIni := 'CTe';
  WebServices.ResourceName := 'ACBrCTeServicos';
end;

function TConfiguracoesCTe.GetGeral: TGeralConfCTe;
begin
  Result := TGeralConfCTe(FPGeral);
end;

function TConfiguracoesCTe.GetArquivos: TArquivosConfCTe;
begin
  Result := TArquivosConfCTe(FPArquivos);
end;

procedure TConfiguracoesCTe.CreateGeralConf;
begin
  FPGeral := TGeralConfCTe.Create(Self);
end;

procedure TConfiguracoesCTe.CreateArquivosConf;
begin
  FPArquivos := TArquivosConfCTe.Create(Self);
end;

procedure TConfiguracoesCTe.Assign(DeConfiguracoesCTe: TConfiguracoesCTe);
begin
  Geral.Assign(DeConfiguracoesCTe.Geral);
  WebServices.Assign(DeConfiguracoesCTe.WebServices);
  Certificados.Assign(DeConfiguracoesCTe.Certificados);
  Arquivos.Assign(DeConfiguracoesCTe.Arquivos);
  RespTec.Assign(DeConfiguracoesCTe.RespTec);
end;

{ TGeralConfCTe }

procedure TGeralConfCTe.Assign(DeGeralConfCTe: TGeralConfCTe);
begin
  inherited Assign(DeGeralConfCTe);

  ModeloDF := DeGeralConfCTe.ModeloDF;
  FVersaoDF := DeGeralConfCTe.VersaoDF;
end;

constructor TGeralConfCTe.Create(AOwner: TConfiguracoes);
begin
  Inherited Create(AOwner);

  FModeloDF := moCTe;
  FModeloDFCodigo := StrToInt(ModeloCTeToStr(FModeloDF));
  FVersaoDF := ve300;
end;

procedure TGeralConfCTe.GravarIni(const AIni: TCustomIniFile);
begin
  inherited GravarIni(AIni);

  AIni.WriteInteger(fpConfiguracoes.SessaoIni, 'ModeloDF', Integer(ModeloDF));
  AIni.WriteInteger(fpConfiguracoes.SessaoIni, 'VersaoDF', Integer(VersaoDF));
end;

procedure TGeralConfCTe.LerIni(const AIni: TCustomIniFile);
begin
  inherited LerIni(AIni);

  ModeloDF := TModeloCTe(AIni.ReadInteger(fpConfiguracoes.SessaoIni, 'ModeloDF', Integer(ModeloDF)));
  VersaoDF := TVersaoCTe(AIni.ReadInteger(fpConfiguracoes.SessaoIni, 'VersaoDF', Integer(VersaoDF)));
end;

procedure TGeralConfCTe.SetModeloDF(const Value: TModeloCTe);
begin
  FModeloDF := Value;
  FModeloDFCodigo := StrToInt(ModeloCTeToStr(FModeloDF));
end;

procedure TGeralConfCTe.SetVersaoDF(const Value: TVersaoCTe);
begin
  FVersaoDF := Value;
end;

{ TArquivosConfCTe }

procedure TArquivosConfCTe.Assign(DeArquivosConfCTe: TArquivosConfCTe);
begin
  inherited Assign(DeArquivosConfCTe);

  FEmissaoPathCTe             := DeArquivosConfCTe.EmissaoPathCTe;
  FSalvarApenasCTeProcessados := DeArquivosConfCTe.SalvarApenasCTeProcessados;
  FNormatizarMunicipios       := DeArquivosConfCTe.NormatizarMunicipios;
  FPathCTe                    := DeArquivosConfCTe.PathCTe;
  FPathInu                    := DeArquivosConfCTe.PathInu;
  FPathEvento                 := DeArquivosConfCTe.PathEvento;
  FPathArquivoMunicipios      := DeArquivosConfCTe.PathArquivoMunicipios;
end;

constructor TArquivosConfCTe.Create(AOwner: TConfiguracoes);
begin
  inherited Create(AOwner);

  FEmissaoPathCTe := False;
  FSalvarApenasCTeProcessados := False;
  FNormatizarMunicipios := False;
  FPathCTe := '';
  FPathInu := '';
  FPathEvento := '';
  FPathArquivoMunicipios := '';
end;

destructor TArquivosConfCTe.Destroy;
begin

  inherited;
end;

function TArquivosConfCTe.GetPathCTe(Data: TDateTime = 0; const CNPJ: String = ''; const IE: String = ''; Modelo: Integer = 0): String;
var
  DescricaoModelo: String;
begin
  case Modelo of
     0:
       begin
         if Assigned(fpConfiguracoes.Owner) then
           DescricaoModelo := TACBrCTe(fpConfiguracoes.Owner).GetNomeModeloDFe
         else
           DescricaoModelo := 'CTe';
       end;
    57:
      DescricaoModelo := 'CTe';
    64:
      DescricaoModelo := 'GTVe';
    67:
      DescricaoModelo := 'CTeOS';
  end;

  Result := GetPath(FPathCTe, DescricaoModelo, CNPJ, IE, Data, DescricaoModelo);
end;

function TArquivosConfCTe.GetPathInu(Data: TDateTime = 0; const CNPJ: String = ''; const IE: String = ''): String;
begin
  Result := GetPath(FPathInu, 'Inu', CNPJ, IE, Data);
end;

function TArquivosConfCTe.GetPathEvento(tipoEvento: TpcnTpEvento;
  const CNPJ: String = ''; const IE: String = ''; Data: TDateTime = 0): String;
var
  Dir: String;
begin
  Dir := GetPath(FPathEvento, 'Evento', CNPJ, IE, Data);

  if AdicionarLiteral then
    Dir := PathWithDelim(Dir) + TpEventoToDescStr(tipoEvento);

  if not DirectoryExists(Dir) then
     ForceDirectories(Dir);

  Result := Dir;
end;

procedure TArquivosConfCTe.GravarIni(const AIni: TCustomIniFile);
begin
  inherited GravarIni(AIni);

  AIni.WriteBool(fpConfiguracoes.SessaoIni, 'SalvarApenasCTeProcessados', SalvarApenasCTeProcessados);
  AIni.WriteBool(fpConfiguracoes.SessaoIni, 'EmissaoPathCTe', EmissaoPathCTe);
  AIni.WriteBool(fpConfiguracoes.SessaoIni, 'NormatizarMunicipios', NormatizarMunicipios);
  AIni.WriteString(fpConfiguracoes.SessaoIni, 'PathCTe', PathCTe);
  AIni.WriteString(fpConfiguracoes.SessaoIni, 'PathInu', PathInu);
  AIni.WriteString(fpConfiguracoes.SessaoIni, 'PathEvento', PathEvento);
  AIni.WriteString(fpConfiguracoes.SessaoIni, 'PathArquivoMunicipios', PathArquivoMunicipios);
end;

procedure TArquivosConfCTe.LerIni(const AIni: TCustomIniFile);
begin
  inherited LerIni(AIni);

  SalvarApenasCTeProcessados := AIni.ReadBool(fpConfiguracoes.SessaoIni, 'SalvarApenasCTeProcessados', SalvarApenasCTeProcessados);
  EmissaoPathCTe := AIni.ReadBool(fpConfiguracoes.SessaoIni, 'EmissaoPathCTe', EmissaoPathCTe);
  NormatizarMunicipios := AIni.ReadBool(fpConfiguracoes.SessaoIni, 'NormatizarMunicipios', NormatizarMunicipios);
  PathCTe := AIni.ReadString(fpConfiguracoes.SessaoIni, 'PathCTe', PathCTe);
  PathInu := AIni.ReadString(fpConfiguracoes.SessaoIni, 'PathInu', PathInu);
  PathEvento := AIni.ReadString(fpConfiguracoes.SessaoIni, 'PathEvento', PathEvento);
  PathArquivoMunicipios := AIni.ReadString(fpConfiguracoes.SessaoIni, 'PathArquivoMunicipios', PathArquivoMunicipios);
end;

end.
