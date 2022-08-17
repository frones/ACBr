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

unit ACBrLibeSocialConfig;

interface

uses
  Classes, SysUtils, IniFiles, SynaChar,
  ACBreSocialConfiguracoes,
  ACBrLibConfig;

type

  { TLibeSocialConfig }
  TLibeSocialConfig = class(TLibConfig)
  private
    FeSocialConfig: TConfiguracoeseSocial;

  protected
    procedure Travar; override;
    procedure Destravar; override;

    procedure INIParaClasse; override;
    procedure ClasseParaINI; override;
    procedure ClasseParaComponentes; override;

  public
    constructor Create(AOwner: TObject; ANomeArquivo: String = ''; AChaveCrypt: AnsiString = ''); override;
    destructor Destroy; override;

    property eSocialConfig: TConfiguracoeseSocial read FeSocialConfig;

  end;

implementation

uses
  ACBrLibeSocialBase, ACBrLibeSocialConsts, ACBrLibConsts, ACBrLibComum,
  ACBrUtil.FilesIO, ACBrUtil.Strings;

{ TLibeSocialConfig }

constructor TLibeSocialConfig.Create(AOwner: TObject; ANomeArquivo: String; AChaveCrypt: AnsiString);
begin
  inherited Create(AOwner, ANomeArquivo, AChaveCrypt);

  FeSocialConfig := TConfiguracoeseSocial.Create(nil);
  FeSocialConfig.ChaveCryptINI := AChaveCrypt;

end;

destructor TLibeSocialConfig.Destroy;
begin
  FeSocialConfig.Destroy;

  inherited Destroy;
end;

procedure TLibeSocialConfig.INIParaClasse;
begin
  inherited INIParaClasse;

  FeSocialConfig.ChaveCryptINI := ChaveCrypt;
  FeSocialConfig.LerIni(Ini);

end;

procedure TLibeSocialConfig.ClasseParaINI;
begin
  inherited ClasseParaINI;

  FeSocialConfig.ChaveCryptINI := ChaveCrypt;
  FeSocialConfig.GravarIni(Ini);

end;

procedure TLibeSocialConfig.ClasseParaComponentes;
begin
  FeSocialConfig.ChaveCryptINI := ChaveCrypt;

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

