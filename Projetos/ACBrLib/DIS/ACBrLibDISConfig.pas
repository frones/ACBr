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

unit ACBrLibDISConfig;

interface

uses
  Classes, SysUtils, IniFiles, SynaChar,
  ACBrLibConfig, ACBrDIS;

type

  { TDISConfig }
  TDISConfig = class
  private
    FAlinhamento: TACBrDISAlinhamento;
    FColunas: Integer;
    FIntervalo: Integer;
    FIntervaloEnvioBytes: Integer;
    FLinhasCount: Integer;
    FModelo: TACBrDISModelo;
    FPassos: Integer;
    FPorta: String;
    FRemoveAcentos: Boolean;

  public
    constructor Create;
    destructor Destroy; override;
    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);

    property Modelo: TACBrDISModelo           read FModelo              write FModelo;
    property Porta: String                    read FPorta               write FPorta;
    property LinhasCount: Integer             read FLinhasCount         write FLinhasCount;
    property Colunas: Integer                 read FColunas             write FColunas;
    property Alinhamento: TACBrDISAlinhamento read FAlinhamento         write FAlinhamento;
    property Intervalo: Integer               read FIntervalo           write FIntervalo;
    property Passos: Integer                  read FPassos              write FPassos;
    property RemoveAcentos: Boolean           read FRemoveAcentos       write FRemoveAcentos;
    property IntervaloEnvioBytes: Integer     read FIntervaloEnvioBytes write FIntervaloEnvioBytes;

  end;

  { TLibDISConfig }
  TLibDISConfig = class(TLibConfig)
  private
    FDISConfig: TDISConfig;

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

    property DISConfig: TDISConfig read FDISConfig;
  end;

implementation

uses
  ACBrLibDISClass, ACBrLibDISConsts, ACBrLibConsts, ACBrLibComum, ACBrUtil;

{ TDISConfig }

constructor TDISConfig.Create;
begin
  FPorta               := '';
  FModelo              := disNenhum;
  FLinhasCount         := 2;
  FColunas             := 20;
  FAlinhamento         := alEsquerda;
  FIntervalo           := 300;
  FPassos              := 1;
  FRemoveAcentos       := True;
  FIntervaloEnvioBytes := 0;
end;

destructor TDISConfig.Destroy;
begin
  inherited Destroy;
end;

procedure TDISConfig.LerIni(const AIni: TCustomIniFile);
begin
  FPorta               := AIni.ReadString(CSessaoDIS, CChavePorta, FPorta);
  FModelo              := TACBrDISModelo(AIni.ReadInteger(CSessaoDIS, CChaveModelo, Integer(FModelo)));
  FAlinhamento         := TACBrDISAlinhamento(AIni.ReadInteger(CSessaoDIS, CChaveAlinhamento, Integer(FAlinhamento)));
  FLinhasCount         := AIni.ReadInteger(CSessaoDIS, CChaveLinhasCount, FLinhasCount);
  FColunas             := AIni.ReadInteger(CSessaoDIS, CChaveColunas, FColunas);
  FIntervalo           := AIni.ReadInteger(CSessaoDIS, CChaveIntervalo, FIntervalo);
  FPassos              := AIni.ReadInteger(CSessaoDIS, CChavePassos, FPassos);
  FIntervaloEnvioBytes := AIni.ReadInteger(CSessaoDIS, CChaveIntervaloEnvioBytes, FIntervaloEnvioBytes);
  FRemoveAcentos       := AIni.ReadBool(CSessaoDIS, CChaveIntervaloRemoveAcentos, FRemoveAcentos);
end;

procedure TDISConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteString(CSessaoDIS, CChavePorta, FPorta);
  AIni.WriteInteger(CSessaoDIS, CChaveModelo, Integer(FModelo));
  AIni.WriteInteger(CSessaoDIS, CChaveAlinhamento, Integer(FAlinhamento));
  AIni.WriteInteger(CSessaoDIS, CChaveLinhasCount, FLinhasCount);
  AIni.WriteInteger(CSessaoDIS, CChaveColunas, FColunas);
  AIni.WriteInteger(CSessaoDIS, CChaveIntervalo, FIntervalo);
  AIni.WriteInteger(CSessaoDIS, CChavePassos, FPassos);
  AIni.WriteInteger(CSessaoDIS, CChaveIntervaloEnvioBytes, FIntervaloEnvioBytes);
  AIni.WriteBool(CSessaoDIS, CChaveIntervaloRemoveAcentos, FRemoveAcentos);
end;

{ TLibDISConfig }

constructor TLibDISConfig.Create(AOwner: TObject; ANomeArquivo: String; AChaveCrypt: AnsiString);
begin
  inherited Create(AOwner, ANomeArquivo, AChaveCrypt);

  FDISConfig := TDISConfig.Create;
end;

destructor TLibDISConfig.Destroy;
begin
  FDISConfig.Free;

  inherited Destroy;
end;

function TLibDISConfig.AtualizarArquivoConfiguracao: Boolean;
var
  Versao: String;
begin
  Versao := Ini.ReadString(CSessaoVersao, CLibDISNome, '0');
  Result := (CompareVersions(CLibDISVersao, Versao) > 0) or
            (inherited AtualizarArquivoConfiguracao);
end;

procedure TLibDISConfig.INIParaClasse;
begin
  inherited INIParaClasse;

  FDISConfig.LerIni(Ini);
end;

procedure TLibDISConfig.ClasseParaINI;
begin
  inherited ClasseParaINI;

  Ini.WriteString(CSessaoVersao, CLibDISNome, CLibDISVersao);

  FDISConfig.GravarIni(Ini);
end;

procedure TLibDISConfig.ClasseParaComponentes;
begin
  if Assigned(Owner) then
    TACBrLibDIS(Owner).DISDM.AplicarConfiguracoes;
end;

procedure TLibDISConfig.Travar;
begin
  if Assigned(Owner) then
  begin
    with TACBrLibDIS(Owner) do
      DISDM.Travar;
  end;
end;

procedure TLibDISConfig.Destravar;
begin
  if Assigned(Owner) then
  begin
    with TACBrLibDIS(Owner) do
      DISDM.Destravar;
  end;
end;

end.

