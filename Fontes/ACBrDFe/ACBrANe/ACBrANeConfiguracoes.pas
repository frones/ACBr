{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Giurizzato Junior                         }
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

unit ACBrANeConfiguracoes;

interface

uses
  Classes, SysUtils, IniFiles,
  ACBrDFeConfiguracoes, ACBrANe.Conversao;

type

  { TGeralConfANe }

  TGeralConfANe = class(TGeralConf)
  private
    FTipoDoc: TTipoDoc;
    FVersaoDF: TVersaoANe;
    FSeguradora: TSeguradora;
    FxSeguradora: string;
    FUsuario: string;
    FSenha: string;
    FCodATM: string;
    FCNPJEmitente: string;

    procedure SetVersaoDF(const Value: TVersaoANe);
    procedure SetSeguradora(const Value: TSeguradora);
  public
    constructor Create(AOwner: TConfiguracoes); override;

    procedure Assign(DeGeralConfANe: TGeralConfANe); reintroduce;

    procedure GravarIni(const AIni: TCustomIniFile); override;
    procedure LerIni(const AIni: TCustomIniFile); override;
    procedure LerParametros;

  published
    property TipoDoc: TTipoDoc read FTipoDoc write FTipoDoc;
    property VersaoDF: TVersaoANe read FVersaoDF write SetVersaoDF default ve200;
    property Seguradora: TSeguradora read FSeguradora write SetSeguradora;
    property xSeguradora: string read FxSeguradora;
    property Usuario: string read FUsuario write FUsuario;
    property Senha: string read FSenha write FSenha;
    property CodATM: string read FCodATM write FCodATM;
    property CNPJEmitente: string read FCNPJEmitente write FCNPJEmitente;
  end;

  { TArquivosConfANe }

  TArquivosConfANe = class(TArquivosConf)
  private
    FEmissaoPathANe: boolean;
    FPathANe: string;
  public
    constructor Create(AOwner: TConfiguracoes); override;
    procedure Assign(DeArquivosConfANe: TArquivosConfANe); reintroduce;

    procedure GravarIni(const AIni: TCustomIniFile); override;
    procedure LerIni(const AIni: TCustomIniFile); override;

    function GetPathANe(Data: TDateTime = 0; const CNPJ: string = '';
      const IE: string = ''): string;
  published
    property EmissaoPathANe: boolean read FEmissaoPathANe
      write FEmissaoPathANe default False;
    property PathANe: string read FPathANe write FPathANe;
  end;

  { TConfiguracoesANe }

  TConfiguracoesANe = class(TConfiguracoes)
  private
    function GetArquivos: TArquivosConfANe;
    function GetGeral: TGeralConfANe;
  protected
    procedure CreateGeralConf; override;
    procedure CreateArquivosConf; override;

  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(DeConfiguracoesANe: TConfiguracoesANe); reintroduce;

  published
    property Geral: TGeralConfANe read GetGeral;
    property Arquivos: TArquivosConfANe read GetArquivos;
    property WebServices;
    property Certificados;
  end;

implementation

uses
  ACBrUtil.FilesIO, ACBrUtil.Strings,
  ACBrANe, ACBrDFeException;

{ TConfiguracoesANe }

constructor TConfiguracoesANe.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FPSessaoIni := 'ANe';
  WebServices.ResourceName := 'ACBrANeServicos';
end;

procedure TConfiguracoesANe.Assign(DeConfiguracoesANe: TConfiguracoesANe);
begin
  Geral.Assign(DeConfiguracoesANe.Geral);
  WebServices.Assign(DeConfiguracoesANe.WebServices);
  Certificados.Assign(DeConfiguracoesANe.Certificados);
  Arquivos.Assign(DeConfiguracoesANe.Arquivos);
end;

function TConfiguracoesANe.GetArquivos: TArquivosConfANe;
begin
  Result := TArquivosConfANe(FPArquivos);
end;

function TConfiguracoesANe.GetGeral: TGeralConfANe;
begin
  Result := TGeralConfANe(FPGeral);
end;

procedure TConfiguracoesANe.CreateGeralConf;
begin
  FPGeral := TGeralConfANe.Create(Self);
end;

procedure TConfiguracoesANe.CreateArquivosConf;
begin
  FPArquivos := TArquivosConfANe.Create(self);
end;

{ TGeralConfANe }

constructor TGeralConfANe.Create(AOwner: TConfiguracoes);
begin
  inherited Create(AOwner);

  FVersaoDF := ve200;
  FTipoDoc := tdCTe;
  FUsuario := '';
  FSenha := '';
  FCodATM := '';
  FCNPJEmitente := '';
  FSeguradora := segATM;
end;

procedure TGeralConfANe.GravarIni(const AIni: TCustomIniFile);
begin
  inherited GravarIni(AIni);

  AIni.WriteInteger(fpConfiguracoes.SessaoIni, 'TipoDoc', Integer(TipoDoc));
  AIni.WriteInteger(fpConfiguracoes.SessaoIni, 'VersaoDF', Integer(VersaoDF));
  AIni.WriteString(fpConfiguracoes.SessaoIni, 'Usuario', Usuario);
  AIni.WriteString(fpConfiguracoes.SessaoIni, 'Senha', Senha);
  AIni.WriteString(fpConfiguracoes.SessaoIni, 'CodATM', CodATM);
  AIni.WriteString(fpConfiguracoes.SessaoIni, 'CNPJEmitente', CNPJEmitente);
  AIni.WriteInteger(fpConfiguracoes.SessaoIni, 'Seguradora', Integer(Seguradora));
end;

procedure TGeralConfANe.LerIni(const AIni: TCustomIniFile);
begin
  inherited LerIni(AIni);

  TipoDoc := TTipoDoc(AIni.ReadInteger(fpConfiguracoes.SessaoIni, 'TipoDoc', Integer(TipoDoc)));
  VersaoDF := TVersaoANe(AIni.ReadInteger(fpConfiguracoes.SessaoIni, 'VersaoDF', Integer(VersaoDF)));
  Usuario := AIni.ReadString(fpConfiguracoes.SessaoIni, 'Usuario', Usuario);
  Senha := AIni.ReadString(fpConfiguracoes.SessaoIni, 'Senha', Senha);
  CodATM := AIni.ReadString(fpConfiguracoes.SessaoIni, 'CodATM', CodATM);
  CNPJEmitente := AIni.ReadString(fpConfiguracoes.SessaoIni, 'CNPJEmitente', CNPJEmitente);
  Seguradora := TSeguradora(AIni.ReadInteger(fpConfiguracoes.SessaoIni, 'Seguradora', Integer(Seguradora)));
end;

procedure TGeralConfANe.LerParametros;
var
  ACBrANeLocal: TACBrANe;
begin
  if not Assigned(fpConfiguracoes.Owner) then
    Exit;

  // Carrega automaticamente o arquivo ACBrANeServicos se necessário.
  ACBrANeLocal := TACBrANe(fpConfiguracoes.Owner);
  if not (csDesigning in fpConfiguracoes.Owner.ComponentState) then
    ACBrANeLocal.LerSeguradoras;

  if FSeguradora = segNenhum then
    raise EACBrDFeException.Create('Seguradora não selecionada');

  FxSeguradora := SeguradoraToStr(FSeguradora);

  ACBrANeLocal.SetProvider;
end;

procedure TGeralConfANe.Assign(DeGeralConfANe: TGeralConfANe);
begin
  inherited Assign(DeGeralConfANe);

  FxSeguradora := DeGeralConfANe.xSeguradora;
  FUsuario := DeGeralConfANe.Usuario;
  FSenha := DeGeralConfANe.Senha;
  FCodATM := DeGeralConfANe.CodATM;
  FCNPJEmitente := DeGeralConfANe.CNPJEmitente;
  FVersaoDF := DeGeralConfANe.VersaoDF;

  Seguradora := DeGeralConfANe.Seguradora;
end;

procedure TGeralConfANe.SetSeguradora(const Value: TSeguradora);
begin
  FSeguradora := Value;

  if FSeguradora <> segNenhum then
    LerParametros;
end;

procedure TGeralConfANe.SetVersaoDF(const Value: TVersaoANe);
begin
  FVersaoDF := Value;
end;

{ TArquivosConfANe }

constructor TArquivosConfANe.Create(AOwner: TConfiguracoes);
begin
  inherited Create(AOwner);

  FEmissaoPathANe := False;
  FPathANe := '';
end;

procedure TArquivosConfANe.Assign(DeArquivosConfANe: TArquivosConfANe);
begin
  inherited Assign(DeArquivosConfANe);

  EmissaoPathANe := DeArquivosConfANe.EmissaoPathANe;
  PathANe := DeArquivosConfANe.PathANe;
end;

procedure TArquivosConfANe.GravarIni(const AIni: TCustomIniFile);
begin
  inherited GravarIni(AIni);

  AIni.WriteBool(fpConfiguracoes.SessaoIni, 'EmissaoPathANe', EmissaoPathANe);
  AIni.WriteString(fpConfiguracoes.SessaoIni, 'PathANe', PathANe);
end;

procedure TArquivosConfANe.LerIni(const AIni: TCustomIniFile);
begin
  inherited LerIni(AIni);

  EmissaoPathANe := AIni.ReadBool(fpConfiguracoes.SessaoIni, 'EmissaoPathANe', EmissaoPathANe);
  PathANe := AIni.ReadString(fpConfiguracoes.SessaoIni, 'PathANe', PathANe);
end;

function TArquivosConfANe.GetPathANe(Data: TDateTime = 0;
  const CNPJ: string = ''; const IE: string = ''): string;
begin
  Result := GetPath(FPathANe, 'ANe', CNPJ, IE, Data);
end;

end.
