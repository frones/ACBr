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

unit ACBrLibGAVConfig;

interface

uses
  Classes, SysUtils, IniFiles, SynaChar,
  ACBrLibConfig, ACBrDeviceConfig, ACBrGAV, ACBrDevice;

type

  { TGAVConfig }
  TGAVConfig = class
  private
    FAberturaAntecipada: TACBrGAVAberturaAntecipada;
    FAberturaIntervalo: Integer;
    FModelo: TACBrGAVModelo;
    FPorta: String;
    FStrComando: String;

  public
    constructor Create;
    destructor Destroy; override;
    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);

    property Modelo: TACBrGAVModelo     read FModelo            write FModelo;
    property Porta: String              read FPorta             write FPorta;
    property StrComando: String         read FStrComando        write FStrComando;
    property AberturaIntervalo: Integer read FAberturaIntervalo write FAberturaIntervalo;
    Property AberturaAntecipada: TACBrGAVAberturaAntecipada read FAberturaAntecipada write FAberturaAntecipada;
  end;

  { TLibGAVConfig }
  TLibGAVConfig = class(TLibConfig)
  private
    FGAVConfig: TGAVConfig;
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

    property GAVConfig: TGAVConfig read FGAVConfig;
    property DeviceConfig: TDeviceConfig read FDeviceConfig;

  end;

implementation

uses
  ACBrLibGAVClass, ACBrLibGAVConsts, ACBrLibConsts, ACBrLibComum, ACBrUtil;

{ TGAVConfig }

constructor TGAVConfig.Create;
begin
  FPorta              := '';
  FModelo             := gavNenhuma;
  FStrComando         := '';
  FAberturaIntervalo  := 300;
  FAberturaAntecipada := aaAguardar;

end;

destructor TGAVConfig.Destroy;
begin
  inherited Destroy;
end;

procedure TGAVConfig.LerIni(const AIni: TCustomIniFile);
begin
  FPorta              := AIni.ReadString(CSessaoGAV, CChavePorta, FPorta);
  FModelo             := TACBrGAVModelo(AIni.ReadInteger(CSessaoGAV, CChaveModelo, Integer(FModelo)));
  FStrComando         := AIni.ReadString(CSessaoGAV, CChaveStrComando, FStrComando);
  FAberturaIntervalo  := AIni.ReadInteger(CSessaoGAV, CChaveAberturaIntervalo, FAberturaIntervalo);
  FAberturaAntecipada := TACBrGAVAberturaAntecipada(AIni.ReadInteger(CSessaoGAV, CChaveAberturaAntecipada, Integer(FAberturaAntecipada)));
end;

procedure TGAVConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteString(CSessaoGAV, CChavePorta, FPorta);
  AIni.WriteInteger(CSessaoGAV, CChaveModelo, Integer(FModelo));
  AIni.WriteString(CSessaoGAV, CChaveStrComando, FStrComando);
  AIni.WriteInteger(CSessaoGAV, CChaveAberturaIntervalo, FAberturaIntervalo);
  AIni.WriteInteger(CSessaoGAV, CChaveAberturaAntecipada, Integer(FAberturaAntecipada));
end;

{ TLibGAVConfig }

constructor TLibGAVConfig.Create(AOwner: TObject; ANomeArquivo: String; AChaveCrypt: AnsiString);
begin
  inherited Create(AOwner, ANomeArquivo, AChaveCrypt);

  FGAVConfig := TGAVConfig.Create;
  FDeviceConfig := TDeviceConfig.Create('GAV_Device');
end;

destructor TLibGAVConfig.Destroy;
begin
  FGAVConfig.Free;
  FDeviceConfig.Free;

  inherited Destroy;
end;

function TLibGAVConfig.AtualizarArquivoConfiguracao: Boolean;
var
  Versao: String;
begin
  Versao := Ini.ReadString(CSessaoVersao, CLibGAVNome, '0');
  Result := (CompareVersions(CLibGAVVersao, Versao) > 0) or
            (inherited AtualizarArquivoConfiguracao);
end;

procedure TLibGAVConfig.INIParaClasse;
begin
  inherited INIParaClasse;

  FGAVConfig.LerIni(Ini);
end;

procedure TLibGAVConfig.ClasseParaINI;
begin
  inherited ClasseParaINI;

  Ini.WriteString(CSessaoVersao, CLibGAVNome, CLibGAVVersao);

  FGAVConfig.GravarIni(Ini);
end;

procedure TLibGAVConfig.ClasseParaComponentes;
begin
  if Assigned(Owner) then
    TACBrLibGAV(Owner).GAVDM.AplicarConfiguracoes;
end;

procedure TLibGAVConfig.Travar;
begin
  if Assigned(Owner) then
  begin
    with TACBrLibGAV(Owner) do
      GAVDM.Travar;
  end;
end;

procedure TLibGAVConfig.Destravar;
begin
  if Assigned(Owner) then
  begin
    with TACBrLibGAV(Owner) do
      GAVDM.Destravar;
  end;
end;

end.

