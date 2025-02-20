{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2025 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Antonio Carlos Junior                           }
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

unit ACBrLibCupomVerdeConfig;

interface

uses
  Classes, SysUtils, IniFiles, synachar,
  ACBrLibConfig, ACBrCupomVerde;

type

{ TCupomVerdeConfig }
TCupomVerdeConfig = class
  private

  public
    constructor Create;
    destructor Destroy; override;

    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);

end;

{ TLibCupomVerdeConfig }
TLibCupomVerdeConfig = class(TLibConfig)
  private
    FCupomVerdeConfig: TCupomVerdeConfig;

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

    property CupomVerdeConfig: TCupomVerdeConfig read FCupomVerdeConfig;
end;

implementation

Uses
  ACBrLibCupomVerdeBase, ACBrLibCupomVerdeConsts, ACBrLibConsts, ACBrUtil.FilesIO;

{ TCupomVerdeConfig }

constructor TCupomVerdeConfig.Create;
begin
  inherited;
end;

destructor TCupomVerdeConfig.Destroy;
begin
  inherited Destroy;
end;

procedure TCupomVerdeConfig.LerIni(const AIni: TCustomIniFile);
begin

end;

procedure TCupomVerdeConfig.GravarIni(const AIni: TCustomIniFile);
begin

end;

{ TLibCupomVerdeConfig }

function TLibCupomVerdeConfig.AtualizarArquivoConfiguracao: Boolean;
var
  Versao: String;
begin
  Versao := Ini.ReadString(CSessaoVersao, CLibCupomVerdeNome, '0');
  Result := (CompareVersions(CLibCupomVerdeVersao, Versao) > 0) or
            (inherited AtualizarArquivoConfiguracao);
end;

procedure TLibCupomVerdeConfig.INIParaClasse;
begin
  inherited INIParaClasse;

  FCupomVerdeConfig.LerIni(Ini);
end;

procedure TLibCupomVerdeConfig.ClasseParaINI;
begin
  inherited ClasseParaINI;

  Ini.WriteString(CSessaoVersao, CLibCupomVerdeNome, CLibCupomVerdeVersao);

  FCupomVerdeConfig.GravarIni(Ini);
end;

procedure TLibCupomVerdeConfig.ClasseParaComponentes;
begin
  if Assigned(Owner) then
  begin
    TACBrLibCupomVerde(Owner).CupomVerdeDM.AplicarConfiguracoes;
  end;
end;

procedure TLibCupomVerdeConfig.Travar;
begin
  if Assigned(Owner) then
  begin
    with TACBrLibCupomVerde(Owner) do
    CupomVerdeDM.Travar;
  end;
end;

procedure TLibCupomVerdeConfig.Destravar;
begin
  if Assigned(Owner) then
  begin
    with TACBrLibCupomVerde(Owner) do
    CupomVerdeDM.Destravar;
  end;
end;

constructor TLibCupomVerdeConfig.Create(AOwner: TObject; ANomeArquivo: String;
  AChaveCrypt: AnsiString);
begin
  inherited Create(AOwner, ANomeArquivo, AChaveCrypt);
end;

destructor TLibCupomVerdeConfig.Destroy;
begin
  FCupomVerdeConfig.Free;
  inherited Destroy;
end;

end.

