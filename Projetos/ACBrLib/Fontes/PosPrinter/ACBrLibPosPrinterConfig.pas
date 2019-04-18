{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }

{ Direitos Autorais Reservados (c) 2018 Daniel Simoes de Almeida               }

{ Colaboradores nesse arquivo: Rafael Teno Dias                                }

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

unit ACBrLibPosPrinterConfig;

interface

uses
  Classes, SysUtils, IniFiles,
  ACBrLibConfig, ACBrDeviceConfig;

type
  { TLibPosPrinterConfig }
  TLibPosPrinterConfig = class(TLibConfig)
  private
    FDeviceConfig: TDeviceConfig;

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

    property DeviceConfig: TDeviceConfig read FDeviceConfig;

  end;

implementation

uses
  ACBrLibPosPrinterClass, ACBrLibPosPrinterConsts, ACBrLibConsts, ACBrLibComum,
  ACBrUtil;

{ TLibPosPrinterConfig }

constructor TLibPosPrinterConfig.Create(AOwner: TObject; ANomeArquivo: String; AChaveCrypt: AnsiString);
begin
  inherited Create(AOwner, ANomeArquivo, AChaveCrypt);

  FDeviceConfig := TDeviceConfig.Create(CSessaoPosPrinterDevice);
end;

destructor TLibPosPrinterConfig.Destroy;
begin
  FDeviceConfig.Free;

  inherited Destroy;
end;

function TLibPosPrinterConfig.AtualizarArquivoConfiguracao: Boolean;
var
  Versao: String;
begin
  Versao := Ini.ReadString(CSessaoVersao, CLibPosPrinterNome, '0');
  Result := (CompareVersions(CLibPosPrinterVersao, Versao) > 0) or
            (inherited AtualizarArquivoConfiguracao);
end;

procedure TLibPosPrinterConfig.INIParaClasse;
begin
  inherited INIParaClasse;

  FDeviceConfig.LerIni(Ini);
end;

procedure TLibPosPrinterConfig.ClasseParaINI;
begin
  inherited ClasseParaINI;

  Ini.WriteString(CSessaoVersao, CLibPosPrinterNome, CLibPosPrinterVersao);

  FDeviceConfig.GravarIni(Ini);
end;

procedure TLibPosPrinterConfig.ClasseParaComponentes;
begin
  if Assigned(Owner) then
    TACBrLibPosPrinter(Owner).PosDM.AplicarConfiguracoes;
end;

procedure TLibPosPrinterConfig.Travar;
begin
  if Assigned(Owner) then
  begin
    with TACBrLibPosPrinter(Owner) do
      PosDM.Travar;
  end;
end;

procedure TLibPosPrinterConfig.Destravar;
begin
  if Assigned(Owner) then
  begin
    with TACBrLibPosPrinter(Owner) do
      PosDM.Destravar;
  end;
end;

end.

