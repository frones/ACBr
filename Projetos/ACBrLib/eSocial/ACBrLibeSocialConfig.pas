{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }

{ Direitos Autorais Reservados (c) 2018 Daniel Simoes de Almeida               }

{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }

{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }

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
{ http://www.opensource.org/licenses/gpl-license.php                           }

{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{        Rua Cel.Aureliano de Camargo, 973 - Tatuí - SP - 18270-170            }

{******************************************************************************}

{$I ACBr.inc}

unit ACBrLibeSocialConfig;

interface

uses
  Classes, SysUtils, IniFiles, SynaChar,
  ACBreSocialConfiguracoes,
  ACBrLibConfig;

type

  { TeSocialConfig }
  {
  TeSocialConfig = class
  private
    FeSocialConfig: TConfiguracoeseSocial;

  public
    constructor Create;
    destructor Destroy; override;
//    procedure LerIni(const AIni: TCustomIniFile);
//    procedure GravarIni(const AIni: TCustomIniFile);

    property eSocialConfig: TConfiguracoeseSocial read FeSocialConfig;
  end;
  }
  { TLibeSocialConfig }
  TLibeSocialConfig = class(TLibConfig)
  private
    FeSocialConfig: TConfiguracoeseSocial;

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

    property eSocialConfig: TConfiguracoeseSocial read FeSocialConfig;
  end;

implementation

uses
  ACBrLibeSocialClass, ACBrLibeSocialConsts, ACBrLibConsts, ACBrLibComum, ACBrUtil;

{ TeSocialConfig }
{
constructor TeSocialConfig.Create;
begin
  FeSocialConfig := TConfiguracoeseSocial.Create(nil);
end;

destructor TeSocialConfig.Destroy;
begin
  FeSocialConfig.Destroy;

  inherited Destroy;
end;
}
{
procedure TeSocialConfig.LerIni(const AIni: TCustomIniFile);
begin
  FCodContrato := AIni.ReadString(CSessaoeSocial, CChaveCodContrato, FCodContrato);
  FSenha       := AIni.ReadString(CSessaoeSocial, CChaveSenha, FSenha);
end;

procedure TeSocialConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteString(CSessaoeSocial, CChaveCodContrato, FCodContrato);
  AIni.WriteString(CSessaoeSocial, CChaveSenha, FSenha);
end;
}
{ TLibeSocialConfig }

constructor TLibeSocialConfig.Create(AOwner: TObject; ANomeArquivo: String; AChaveCrypt: AnsiString);
begin
  inherited Create(AOwner, ANomeArquivo, AChaveCrypt);

  FeSocialConfig := TConfiguracoeseSocial.Create(nil);
end;

destructor TLibeSocialConfig.Destroy;
begin
  FeSocialConfig.Free;

  inherited Destroy;
end;

function TLibeSocialConfig.AtualizarArquivoConfiguracao: Boolean;
var
  Versao: String;
begin
  Versao := Ini.ReadString(CSessaoVersao, CLibeSocialNome, '0');
  Result := (CompareVersions(CLibeSocialVersao, Versao) > 0) or
            (inherited AtualizarArquivoConfiguracao);
end;

procedure TLibeSocialConfig.INIParaClasse;
begin
  inherited INIParaClasse;

  FeSocialConfig.Arquivos.LerIni(Ini);
  FeSocialConfig.Certificados.LerIni(Ini);
  FeSocialConfig.Geral.LerIni(Ini);
  FeSocialConfig.WebServices.LerIni(Ini);
//  FeSocialConfig.LerIni(Ini);
end;

procedure TLibeSocialConfig.ClasseParaINI;
begin
  inherited ClasseParaINI;

  Ini.WriteString(CSessaoVersao, CLibeSocialNome, CLibeSocialVersao);

  FeSocialConfig.Arquivos.GravarIni(Ini);
  FeSocialConfig.Certificados.GravarIni(Ini);
  FeSocialConfig.Geral.GravarIni(Ini);
  FeSocialConfig.WebServices.GravarIni(Ini);
end;

procedure TLibeSocialConfig.ClasseParaComponentes;
begin
  if Assigned(Owner) then
    TACBrLibeSocial(Owner).eSocialDM.AplicarConfiguracoes;
end;

procedure TLibeSocialConfig.Travar;
begin
  if Assigned(Owner) then
  begin
    with TACBrLibeSocial(Owner) do
      eSocialDM.Travar;
  end;
end;

procedure TLibeSocialConfig.Destravar;
begin
  if Assigned(Owner) then
  begin
    with TACBrLibeSocial(Owner) do
      eSocialDM.Destravar;
  end;
end;

end.

