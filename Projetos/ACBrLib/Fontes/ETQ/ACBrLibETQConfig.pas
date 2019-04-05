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

unit ACBrLibETQConfig;

interface

uses
  Classes, SysUtils, IniFiles, SynaChar,
  ACBrLibConfig, ACBrDeviceConfig, ACBrDevice, ACBrETQ;

type

  { TETQConfig }
  TETQConfig = class
  private
    FArqLog: String;
    FAtivo: Boolean;
    FAvanco: Integer;
    FBackFeed: TACBrETQBackFeed;
    FDPI: TACBrETQDPI;
    FLimparMemoria: Boolean;
    FMargemEsquerda: Integer;
    FModelo: TACBrETQModelo;
    FOrigem: TACBrETQOrigem;
    FPorta: String;
    FTemperatura: Integer;
    FUnidade: TACBrETQUnidade;
    FVelocidade: Integer;

  public
    constructor Create;
    destructor Destroy; override;
    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);

    property ArqLog: String read FArqLog write FArqLog;
    property Unidade: TACBrETQUnidade read FUnidade write FUnidade;
    property Modelo: TACBrETQModelo read FModelo write FModelo;
    property BackFeed: TACBrETQBackFeed read FBackFeed write FBackFeed;
    property LimparMemoria: Boolean read FLimparMemoria write FLimparMemoria;
    property Temperatura: Integer read FTemperatura write FTemperatura;
    property Velocidade: Integer read FVelocidade write FVelocidade;
    property Origem: TACBrETQOrigem read FOrigem write FOrigem;
    property DPI: TACBrETQDPI read FDPI write FDPI;
    property Avanco: Integer read FAvanco write FAvanco;
    property MargemEsquerda: Integer read FMargemEsquerda write FMargemEsquerda;
    property Porta: String read FPorta write FPorta;
    property Ativo: Boolean read FAtivo write FAtivo;

  end;

  { TLibETQConfig }
  TLibETQConfig = class(TLibConfig)
  private
    FETQConfig: TETQConfig;
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

    property ETQConfig: TETQConfig read FETQConfig;
    property DeviceConfig: TDeviceConfig read FDeviceConfig;
  end;

implementation

uses
  ACBrLibETQClass, ACBrLibETQConsts, ACBrLibConsts, ACBrLibComum, ACBrUtil;

{ TETQConfig }

constructor TETQConfig.Create;
begin
  FUnidade        := etqDecimoDeMilimetros;
  FModelo         := etqNenhum;
  FBackFeed       := bfNone;
  FLimparMemoria  := True;
  FTemperatura    := 10;
  FVelocidade     := -1;
  FOrigem         := ogNone;
  FDPI            := dpi203;
  FAvanco         := 0;
  FMargemEsquerda := 0;
  FArqLog         := '';
  FPorta          := '';
  FAtivo          := False;
end;

destructor TETQConfig.Destroy;
begin
  inherited Destroy;
end;

procedure TETQConfig.LerIni(const AIni: TCustomIniFile);
begin
  FArqLog         := AIni.ReadString(CSessaoETQ, CChaveLog, FArqLog);
  FPorta          := AIni.ReadString(CSessaoETQ, CChavePorta, FPorta);
  FTemperatura    := AIni.ReadInteger(CSessaoETQ, CChaveTemperatura, FTemperatura);
  FVelocidade     := AIni.ReadInteger(CSessaoETQ, CChaveVelocidade, FVelocidade);
  FAvanco         := AIni.ReadInteger(CSessaoETQ, CChaveAvanco, FAvanco);
  FMargemEsquerda := AIni.ReadInteger(CSessaoETQ, CChaveMargemEsquerda, FMargemEsquerda);
  FLimparMemoria  := AIni.ReadBool(CSessaoETQ, CChaveLimparMemoria, FLimparMemoria);
  FAtivo          := AIni.ReadBool(CSessaoETQ, CChaveAtivo, FAtivo);
  FModelo         := TACBrETQModelo(AIni.ReadInteger(CSessaoETQ, CChaveModelo, Integer(FModelo)));
  FUnidade        := TACBrETQUnidade(AIni.ReadInteger(CSessaoETQ, CChaveUnidade, Integer(FUnidade)));
  FBackFeed       := TACBrETQBackFeed(AIni.ReadInteger(CSessaoETQ, CChaveBackFeed, Integer(FBackFeed)));
  FOrigem         := TACBrETQOrigem(AIni.ReadInteger(CSessaoETQ, CChaveOrigem, Integer(FOrigem)));
  FDPI            := TACBrETQDPI(AIni.ReadInteger(CSessaoETQ, CChaveDPI, Integer(FDPI)));
end;

procedure TETQConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteString(CSessaoETQ, CChaveLog, FArqLog);
  AIni.WriteString(CSessaoETQ, CChavePorta, FPorta);
  AIni.WriteInteger(CSessaoETQ, CChaveTemperatura, FTemperatura);
  AIni.WriteInteger(CSessaoETQ, CChaveVelocidade, FVelocidade);
  AIni.WriteInteger(CSessaoETQ, CChaveAvanco, FAvanco);
  AIni.WriteInteger(CSessaoETQ, CChaveMargemEsquerda, FMargemEsquerda);
  AIni.WriteBool(CSessaoETQ, CChaveLimparMemoria, FLimparMemoria);
  AIni.WriteBool(CSessaoETQ, CChaveAtivo, FAtivo);
  AIni.WriteInteger(CSessaoETQ, CChaveModelo, Integer(FModelo));
  AIni.WriteInteger(CSessaoETQ, CChaveUnidade, Integer(FUnidade));
  AIni.WriteInteger(CSessaoETQ, CChaveBackFeed, Integer(FBackFeed));
  AIni.WriteInteger(CSessaoETQ, CChaveOrigem, Integer(FOrigem));
  AIni.WriteInteger(CSessaoETQ, CChaveDPI, Integer(FDPI));
end;

{ TLibETQConfig }

constructor TLibETQConfig.Create(AOwner: TObject; ANomeArquivo: String; AChaveCrypt: AnsiString);
begin
  inherited Create(AOwner, ANomeArquivo, AChaveCrypt);

  FETQConfig := TETQConfig.Create;
  FDeviceConfig := TDeviceConfig.Create('ETQ_Device');
end;

destructor TLibETQConfig.Destroy;
begin
  FETQConfig.Free;
  FDeviceConfig.Free;

  inherited Destroy;
end;

function TLibETQConfig.AtualizarArquivoConfiguracao: Boolean;
var
  Versao: String;
begin
  Versao := Ini.ReadString(CSessaoVersao, CLibETQNome, '0');
  Result := (CompareVersions(CLibETQVersao, Versao) > 0) or
            (inherited AtualizarArquivoConfiguracao);
end;

procedure TLibETQConfig.INIParaClasse;
begin
  inherited INIParaClasse;

  FETQConfig.LerIni(Ini);
  FDeviceConfig.LerIni(Ini);
end;

procedure TLibETQConfig.ClasseParaINI;
begin
  inherited ClasseParaINI;

  Ini.WriteString(CSessaoVersao, CLibETQNome, CLibETQVersao);

  FETQConfig.GravarIni(Ini);
  FDeviceConfig.GravarIni(Ini);
end;

procedure TLibETQConfig.ClasseParaComponentes;
begin
  if Assigned(Owner) then
    TACBrLibETQ(Owner).ETQDM.AplicarConfiguracoes;
end;

procedure TLibETQConfig.Travar;
begin
  if Assigned(Owner) then
  begin
    with TACBrLibETQ(Owner) do
      ETQDM.Travar;
  end;
end;

procedure TLibETQConfig.Destravar;
begin
  if Assigned(Owner) then
  begin
    with TACBrLibETQ(Owner) do
      ETQDM.Destravar;
  end;
end;

end.

