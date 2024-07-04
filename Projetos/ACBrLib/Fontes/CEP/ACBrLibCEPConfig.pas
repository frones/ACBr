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

unit ACBrLibCEPConfig;

interface

uses
  Classes, SysUtils, IniFiles, SynaChar,
  ACBrLibConfig, ACBrCEP, blcksock;

type

  { TCEPConfig }
  TCEPConfig = class
  private
    FChaveAcesso: String;
    FPesquisarIBGE: Boolean;
    FSenha: String;
    FUsuario: String;
    FWebService: TACBrCEPWebService;
    FSSLType: TSSLType;

  public
    constructor Create;
    destructor Destroy; override;
    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);

    property WebService: TACBrCEPWebService read FWebService write FWebService;
    property ChaveAcesso: String read FChaveAcesso write FChaveAcesso;
    property Usuario: String read FUsuario write FUsuario;
    property Senha: String read FSenha write FSenha;
    property PesquisarIBGE: Boolean read FPesquisarIBGE write FPesquisarIBGE; // Válido somente para wsCorreios e TACBrWSCorreiosSIGEP
    property SSLType: TSSLType read FSSLType write FSSLType;
  end;

  { TLibCEPConfig }
  TLibCEPConfig = class(TLibConfig)
  private
    FCEPConfig: TCEPConfig;

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

    property CEPConfig: TCEPConfig read FCEPConfig;
  end;

implementation

uses
  ACBrLibCEPBase, ACBrLibCEPConsts, ACBrLibConsts, ACBrLibComum, ACBrUtil.FilesIO;

{ TCEPConfig }

constructor TCEPConfig.Create;
begin
  FWebService    := wsNenhum;
  FChaveAcesso   := '';
  FUsuario       := '';
  FSenha         := '';
  FPesquisarIBGE := False;
  FSSLType       := LT_all;
end;

destructor TCEPConfig.Destroy;
begin
  inherited Destroy;
end;

procedure TCEPConfig.LerIni(const AIni: TCustomIniFile);
begin
  FWebService    := TACBrCEPWebService(AIni.ReadInteger(CSessaoCEP, CChaveWebService, Integer(FWebService)));
  FChaveAcesso   := AIni.ReadString(CSessaoCEP, CChaveChaveAcesso, FChaveAcesso);
  FUsuario       := AIni.ReadString(CSessaoCEP, CChaveUsuario, FUsuario);
  FSenha         := AIni.ReadString(CSessaoCEP, CChaveSenha, FSenha);
  FPesquisarIBGE := AIni.ReadBool(CSessaoCEP, CChavePesquisarIBGE, FPesquisarIBGE);
  FSSLType       := TSSLType(AIni.ReadInteger(CSessaoCEP, CChaveSSLType, Integer(FSSLType)));
end;

procedure TCEPConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteInteger(CSessaoCEP, CChaveWebService, Integer(FWebService));
  AIni.WriteString(CSessaoCEP, CChaveChaveAcesso, FChaveAcesso);
  AIni.WriteString(CSessaoCEP, CChaveUsuario, FUsuario);
  AIni.WriteString(CSessaoCEP, CChaveSenha, FSenha);
  AIni.WriteBool(CSessaoCEP, CChavePesquisarIBGE, FPesquisarIBGE);
  AIni.WriteInteger(CSessaoCEP, CChaveSSLType, Integer(FSSLType));
end;

{ TLibCEPConfig }

constructor TLibCEPConfig.Create(AOwner: TObject; ANomeArquivo: String; AChaveCrypt: AnsiString);
begin
  inherited Create(AOwner, ANomeArquivo, AChaveCrypt);

  FCEPConfig := TCEPConfig.Create;
end;

destructor TLibCEPConfig.Destroy;
begin
  FCEPConfig.Free;

  inherited Destroy;
end;

function TLibCEPConfig.AtualizarArquivoConfiguracao: Boolean;
var
  Versao: String;
begin
  Versao := Ini.ReadString(CSessaoVersao, CLibCEPNome, '0');
  Result := (CompareVersions(CLibCEPVersao, Versao) > 0) or
            (inherited AtualizarArquivoConfiguracao);
end;

procedure TLibCEPConfig.INIParaClasse;
begin
  inherited INIParaClasse;

  FCEPConfig.LerIni(Ini);
end;

procedure TLibCEPConfig.ClasseParaINI;
begin
  inherited ClasseParaINI;

  Ini.WriteString(CSessaoVersao, CLibCEPNome, CLibCEPVersao);

  FCEPConfig.GravarIni(Ini);
end;

procedure TLibCEPConfig.ClasseParaComponentes;
begin
  if Assigned(Owner) then
    TACBrLibCEP(Owner).CEPDM.AplicarConfiguracoes;
end;

procedure TLibCEPConfig.Travar;
begin
  if Assigned(Owner) then
  begin
    with TACBrLibCEP(Owner) do
      CEPDM.Travar;
  end;
end;

procedure TLibCEPConfig.Destravar;
begin
  if Assigned(Owner) then
  begin
    with TACBrLibCEP(Owner) do
      CEPDM.Destravar;
  end;
end;

end.

