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

unit ACBrLibReinfConfig;

interface

uses
  Classes, SysUtils, IniFiles, SynaChar,
  ACBrReinfConfiguracoes,
  ACBrLibConfig;

type

  { TLibReinfConfig }
  TLibReinfConfig = class(TLibConfig)
  private
    FReinfConfig: TConfiguracoesReinf;

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

    property ReinfConfig: TConfiguracoesReinf read FReinfConfig;
  end;

implementation

uses
  ACBrLibReinfBase, ACBrLibReinfConsts, ACBrLibConsts, ACBrLibComum,
  ACBrUtil.FilesIO, ACBrUtil.Strings;

{ TLibReinfConfig }

constructor TLibReinfConfig.Create(AOwner: TObject; ANomeArquivo: String; AChaveCrypt: AnsiString);
begin
  inherited Create(AOwner, ANomeArquivo, AChaveCrypt);

  FReinfConfig := TConfiguracoesReinf.Create(nil);
end;

destructor TLibReinfConfig.Destroy;
begin
  FReinfConfig.Free;

  inherited Destroy;
end;

function TLibReinfConfig.AtualizarArquivoConfiguracao: Boolean;
var
  Versao: String;
begin
  Versao := Ini.ReadString(CSessaoVersao, CLibReinfNome, '0');
  Result := (CompareVersions(CLibReinfVersao, Versao) > 0) or
            (inherited AtualizarArquivoConfiguracao);
end;

procedure TLibReinfConfig.INIParaClasse;
begin
  inherited INIParaClasse;

  FReinfConfig.ChaveCryptINI := ChaveCrypt;
  FReinfConfig.LerIni(Ini);
end;

procedure TLibReinfConfig.ClasseParaINI;
begin
  inherited ClasseParaINI;

  FReinfConfig.ChaveCryptINI := ChaveCrypt;
  FReinfConfig.GravarIni(Ini);
end;

procedure TLibReinfConfig.ClasseParaComponentes;
begin
  FReinfConfig.ChaveCryptINI := ChaveCrypt;

  if Assigned(Owner) then
    TACBrLibReinf(Owner).ReinfDM.AplicarConfiguracoes;
end;

procedure TLibReinfConfig.Travar;
begin
  if Assigned(Owner) then
  begin
    with TACBrLibReinf(Owner) do
      ReinfDM.Travar;
  end;
end;

procedure TLibReinfConfig.Destravar;
begin
  if Assigned(Owner) then
  begin
    with TACBrLibReinf(Owner) do
      ReinfDM.Destravar;
  end;
end;

end.

