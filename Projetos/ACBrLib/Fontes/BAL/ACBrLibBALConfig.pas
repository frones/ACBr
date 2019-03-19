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

unit ACBrLibBALConfig;

interface

uses
  Classes, SysUtils, IniFiles, SynaChar,
  ACBrLibConfig, ACBrDeviceConfig, ACBrBAL;

type

  { TBALConfig }
  TBALConfig = class
  private
    FArqLog: String;
    FIntervalo: Integer;
    FModelo: TACBrBALModelo;
    FMonitorarBalanca: Boolean;
    FPorta: String;
    FPosFim: Integer;
    FPosini: Integer;

  public
    constructor Create;
    destructor Destroy; override;
    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);

    property ArqLog: String            read FArqLog           write FArqLog;
    property Modelo: TACBrBALModelo    read FModelo           write FModelo;
    property Porta: String             read FPorta            write FPorta;
    property Intervalo: Integer        read FIntervalo        write FIntervalo;
    property MonitorarBalanca: Boolean read FMonitorarBalanca write FMonitorarBalanca;
    property PosIni: Integer           read FPosini           write FPosIni;
    property PosFim: Integer           read FPosFim           write FPosFim;
  end;

  { TLibBALConfig }
  TLibBALConfig = class(TLibConfig)
  private
    FBALConfig: TBALConfig;
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

    property BALConfig: TBALConfig read FBALConfig;
    property DeviceConfig: TDeviceConfig read FDeviceConfig;
  end;

implementation

uses
  ACBrLibBALClass, ACBrLibBALConsts, ACBrLibConsts, ACBrLibComum, ACBrUtil;

{ TBALConfig }

constructor TBALConfig.Create;
begin
  FPorta            := '';
  FModelo           := balNenhum;
  FIntervalo        := 200;
  FMonitorarBalanca := False;
  FPosini           := 0;
  FPosFim           := 0;
end;

destructor TBALConfig.Destroy;
begin
  inherited Destroy;
end;

procedure TBALConfig.LerIni(const AIni: TCustomIniFile);
begin
  FArqLog    := AIni.ReadString(CSessaoBAL, CChaveLog, FArqLog);
  FPorta     := AIni.ReadString(CSessaoBAL, CChavePorta, FPorta);
  FModelo    := TACBrBALModelo(AIni.ReadInteger(CSessaoBAL, CChaveModelo, Integer(FModelo)));
  FIntervalo := AIni.ReadInteger(CSessaoBAL, CChaveIntervalo, FIntervalo);
  FPosini    := AIni.ReadInteger(CSessaoBAL, CChavePosIni, FPosini);
  FPosFim    := AIni.ReadInteger(CSessaoBAL, CChavePosFim, FPosFim);

  FMonitorarBalanca := AIni.ReadBool(CSessaoBAL, CChaveMonitorarBalanca, FMonitorarBalanca);
end;

procedure TBALConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteString(CSessaoBAL, CChaveLog, FArqLog);
  AIni.WriteString(CSessaoBAL, CChavePorta, FPorta);
  AIni.WriteInteger(CSessaoBAL, CChaveModelo, Integer(FModelo));
  AIni.WriteInteger(CSessaoBAL, CChaveIntervalo, FIntervalo);
  AIni.WriteInteger(CSessaoBAL, CChavePosini, FPosini);
  AIni.WriteInteger(CSessaoBAL, CChavePosFim, FPosFim);
  AIni.WriteBool(CSessaoBAL, CChaveMonitorarBalanca, FMonitorarBalanca);
end;

{ TLibBALConfig }

constructor TLibBALConfig.Create(AOwner: TObject; ANomeArquivo: String; AChaveCrypt: AnsiString);
begin
  inherited Create(AOwner, ANomeArquivo, AChaveCrypt);

  FBALConfig := TBALConfig.Create;
  FDeviceConfig := TDeviceConfig.Create('BAL_Device');
end;

destructor TLibBALConfig.Destroy;
begin
  FBALConfig.Free;
  FDeviceConfig.Free;

  inherited Destroy;
end;

function TLibBALConfig.AtualizarArquivoConfiguracao: Boolean;
var
  Versao: String;
begin
  Versao := Ini.ReadString(CSessaoVersao, CLibBALNome, '0');
  Result := (CompareVersions(CLibBALVersao, Versao) > 0) or
            (inherited AtualizarArquivoConfiguracao);
end;

procedure TLibBALConfig.INIParaClasse;
begin
  inherited INIParaClasse;

  FBALConfig.LerIni(Ini);
  FDeviceConfig.LerIni(Ini);
end;

procedure TLibBALConfig.ClasseParaINI;
begin
  inherited ClasseParaINI;

  Ini.WriteString(CSessaoVersao, CLibBALNome, CLibBALVersao);

  FBALConfig.GravarIni(Ini);
  FDeviceConfig.GravarIni(Ini);
end;

procedure TLibBALConfig.ClasseParaComponentes;
begin
  if Assigned(Owner) then
    TACBrLibBAL(Owner).BALDM.AplicarConfiguracoes;
end;

procedure TLibBALConfig.Travar;
begin
  if Assigned(Owner) then
  begin
    with TACBrLibBAL(Owner) do
      BALDM.Travar;
  end;
end;

procedure TLibBALConfig.Destravar;
begin
  if Assigned(Owner) then
  begin
    with TACBrLibBAL(Owner) do
      BALDM.Destravar;
  end;
end;

end.

