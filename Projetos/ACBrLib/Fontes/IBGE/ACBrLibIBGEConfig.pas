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

unit ACBrLibIBGEConfig;

interface

uses
  Classes, SysUtils, IniFiles, SynaChar,
  ACBrLibConfig;

type

  { TIBGEConfig }
  TIBGEConfig = class
  private
    FCacheArquivo: String;
    FCacheDiasValidade: Integer;
    FIgnorarCaixaEAcentos: Boolean;

  public
    constructor Create;
    destructor Destroy; override;
    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);

    property CacheArquivo: String read FCacheArquivo write FCacheArquivo;
    property CacheDiasValidade: Integer read FCacheDiasValidade
      write FCacheDiasValidade;
    property IgnorarCaixaEAcentos: Boolean read FIgnorarCaixaEAcentos
      write FIgnorarCaixaEAcentos;
  end;

  { TLibIBGEConfig }
  TLibIBGEConfig = class(TLibConfig)
  private
    FIBGEConfig: TIBGEConfig;

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

    property IBGEConfig: TIBGEConfig read FIBGEConfig;
  end;

implementation

uses
  ACBrLibIBGEClass, ACBrLibIBGEConsts, ACBrLibConsts, ACBrLibComum, ACBrUtil;

{ TIBGEConfig }

constructor TIBGEConfig.Create;
begin
  FCacheArquivo         := '';
  FCacheDiasValidade    := 0; // 0-não expira
  FIgnorarCaixaEAcentos := False;
end;

destructor TIBGEConfig.Destroy;
begin
  inherited Destroy;
end;

procedure TIBGEConfig.LerIni(const AIni: TCustomIniFile);
begin
  FCacheArquivo         := AIni.ReadString(CSessaoIBGE, CChaveCacheArquivo, FCacheArquivo);
  FCacheDiasValidade    := AIni.ReadInteger(CSessaoIBGE, CChaveCacheDiasValidade, FCacheDiasValidade);
  FIgnorarCaixaEAcentos := AIni.ReadBool(CSessaoIBGE, CChaveIgnorarCaixaEAcentos, FIgnorarCaixaEAcentos);
end;

procedure TIBGEConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteString(CSessaoIBGE, CChaveCacheArquivo, FCacheArquivo);
  AIni.WriteInteger(CSessaoIBGE, CChaveCacheDiasValidade, FCacheDiasValidade);
  AIni.WriteBool(CSessaoIBGE, CChaveIgnorarCaixaEAcentos, FIgnorarCaixaEAcentos);
end;

{ TLibIBGEConfig }

constructor TLibIBGEConfig.Create(AOwner: TObject; ANomeArquivo: String; AChaveCrypt: AnsiString);
begin
  inherited Create(AOwner, ANomeArquivo, AChaveCrypt);

  FIBGEConfig := TIBGEConfig.Create;
end;

destructor TLibIBGEConfig.Destroy;
begin
  FIBGEConfig.Free;

  inherited Destroy;
end;

function TLibIBGEConfig.AtualizarArquivoConfiguracao: Boolean;
var
  Versao: String;
begin
  Versao := Ini.ReadString(CSessaoVersao, CLibIBGENome, '0');
  Result := (CompareVersions(CLibIBGEVersao, Versao) > 0) or
            (inherited AtualizarArquivoConfiguracao);
end;

procedure TLibIBGEConfig.INIParaClasse;
begin
  inherited INIParaClasse;

  FIBGEConfig.LerIni(Ini);
end;

procedure TLibIBGEConfig.ClasseParaINI;
begin
  inherited ClasseParaINI;

  Ini.WriteString(CSessaoVersao, CLibIBGENome, CLibIBGEVersao);

  FIBGEConfig.GravarIni(Ini);
end;

procedure TLibIBGEConfig.ClasseParaComponentes;
begin
  if Assigned(Owner) then
    TACBrLibIBGE(Owner).IBGEDM.AplicarConfiguracoes;
end;

procedure TLibIBGEConfig.Travar;
begin
  if Assigned(Owner) then
  begin
    with TACBrLibIBGE(Owner) do
      IBGEDM.Travar;
  end;
end;

procedure TLibIBGEConfig.Destravar;
begin
  if Assigned(Owner) then
  begin
    with TACBrLibIBGE(Owner) do
      IBGEDM.Destravar;
  end;
end;

end.

