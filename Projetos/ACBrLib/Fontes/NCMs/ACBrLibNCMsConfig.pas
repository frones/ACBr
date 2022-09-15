{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Elton Barbosa                                   }
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

unit ACBrLibNCMsConfig;

interface

uses
  Classes, SysUtils, IniFiles, SynaChar,
  ACBrLibConfig, ACBrNCMs;

type

  { TNCMsConfig }
  TNCMsConfig = class
  private
    FServidorProxy: String;
    FPortaProxy: String;
    FSenhaProxy: String;
    FUsuarioProxy: String;
    FArquivoCache      : String;
    FDiasValidadeCache : Integer;

  public
    constructor Create;
    destructor Destroy; override;

    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);

    property ServidorProxy: String read FServidorProxy write FServidorProxy;
    property PortaProxy: String read FPortaProxy write FPortaProxy;
    property SenhaProxy: String read FSenhaProxy write FSenhaProxy;
    property UsuarioProxy: String read FUsuarioProxy write FUsuarioProxy;
    property ArquivoCache: String read FArquivoCache write FArquivoCache;
    property DiasValidadeCache: Integer read FDiasValidadeCache write FDiasValidadeCache;
  end;

  { TLibNCMsConfig }
  TLibNCMsConfig = class(TLibConfig)
  private
    FNCMsConfig: TNCMsConfig;

  protected
    function AtualizarArquivoConfiguracao: Boolean; override;

    procedure INIParaClasse; override;
    procedure ClasseParaINI; override;
    procedure ClasseParaComponentes; override;

    procedure Travar; override;
    procedure Destravar; override;

  public
    constructor Create(AOwner: TObject; ANomeArquivo: String = ''; AChaveCrypt: AnsiString = ''); override;
    destructor Destroy; override;

    property NCMsConfig: TNCMsConfig read FNCMsConfig;
  end;

implementation

uses
  ACBrLibNCMsBase, ACBrLibNCMsConsts, ACBrLibConsts, {ACBrLibComum, }ACBrUtil.FilesIO;

{ TNCMsConfig }

constructor TNCMsConfig.Create;
begin
  inherited;
  FServidorProxy := '';
  FPortaProxy    := '';
  FSenhaProxy    := '';
  FUsuarioProxy  := '';
  FDiasValidadeCache := 0;
  FArquivoCache := CNCM_ARQUIVO_CACHE;
end;

destructor TNCMsConfig.Destroy;
begin
  inherited Destroy;
end;

procedure TNCMsConfig.LerIni(const AIni: TCustomIniFile);
begin
  FPortaProxy        := AIni.ReadString(CSessaoNCMs, CChavePorta, FPortaProxy);
  FSenhaProxy        := AIni.ReadString(CSessaoNCMs, CChaveSenhaProxy, FSenhaProxy);
  FUsuarioProxy      := AIni.ReadString(CSessaoNCMs, CChaveUsuarioProxy, FUsuarioProxy);
  FServidorProxy     := AIni.ReadString(CSessaoNCMs, CChaveServidorProxy, FServidorProxy);
  FArquivoCache      := AIni.ReadString(CSessaoNCMs, CChaveArquivoCache, FArquivoCache);
  FDiasValidadeCache := AIni.ReadInteger(CSessaoNCMs, CChaveDiasValidadeCache, FDiasValidadeCache);
end;

procedure TNCMsConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteString(CSessaoNCMs, CChavePorta, FPortaProxy);
  AIni.WriteString(CSessaoNCMs, CChaveSenhaProxy, FSenhaProxy);
  AIni.WriteString(CSessaoNCMs, CChaveUsuarioProxy, FUsuarioProxy);
  AIni.WriteString(CSessaoNCMs, CChaveServidorProxy, FServidorProxy);
  AIni.WriteString(CSessaoNCMs, CChaveArquivoCache, FArquivoCache);
  AIni.WriteInteger(CSessaoNCMs, CChaveDiasValidadeCache, FDiasValidadeCache);
end;

{ TLibNCMsConfig }
constructor TLibNCMsConfig.Create(AOwner: TObject; ANomeArquivo: String; AChaveCrypt: AnsiString);
begin
  inherited Create(AOwner, ANomeArquivo, AChaveCrypt);

  FNCMsConfig := TNCMsConfig.Create;
end;

destructor TLibNCMsConfig.Destroy;
begin
  FNCMsConfig.Free;

  inherited Destroy;
end;

function TLibNCMsConfig.AtualizarArquivoConfiguracao: Boolean;
var
  Versao: String;
begin
  Versao := Ini.ReadString(CSessaoVersao, CLibNCMsNome, '0');
  Result := (CompareVersions(CLibNCMsVersao, Versao) > 0) or
            (inherited AtualizarArquivoConfiguracao);
end;

procedure TLibNCMsConfig.INIParaClasse;
begin
  inherited INIParaClasse;

  FNCMsConfig.LerIni(Ini);
end;

procedure TLibNCMsConfig.ClasseParaINI;
begin
  inherited ClasseParaINI;

  Ini.WriteString(CSessaoVersao, CLibNCMsNome, CLibNCMsVersao);

  FNCMsConfig.GravarIni(Ini);
end;

procedure TLibNCMsConfig.ClasseParaComponentes;
begin
  if Assigned(Owner) then
    TACBrLibNCMs(Owner).NCMsDM.AplicarConfiguracoes;
end;

procedure TLibNCMsConfig.Travar;
begin
  if Assigned(Owner) then
  begin
    with TACBrLibNCMs(Owner) do
      NCMsDM.Travar;
  end;
end;

procedure TLibNCMsConfig.Destravar;
begin
  if Assigned(Owner) then
  begin
    with TACBrLibNCMs(Owner) do
      NCMsDM.Destravar;
  end;
end;

end.

