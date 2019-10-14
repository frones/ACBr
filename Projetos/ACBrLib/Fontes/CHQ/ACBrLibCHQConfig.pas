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

unit ACBrLibCHQConfig;

interface

uses
  Classes, SysUtils, IniFiles, SynaChar,
  ACBrLibConfig, ACBrCHQ;

type

  { TCHQConfig }
  TCHQConfig = class
  private
    FArqLog: String;
    FModelo: TACBrCHQModelo;
    FPaginaDeCodigo: Word;
    FPorta: String;

  public
    constructor Create;
    destructor Destroy; override;
    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);

    property ArqLog: String         read FArqLog         write FArqLog;
    property Modelo: TACBrCHQModelo read FModelo         write FModelo;
    property Porta: String          read FPorta          write FPorta;
    property PaginaDeCodigo: Word   read FPaginaDeCodigo write FPaginaDeCodigo;
  end;

  { TLibCHQConfig }
  TLibCHQConfig = class(TLibConfig)
  private
    FCHQConfig: TCHQConfig;

  protected
    procedure INIParaClasse; override;
    procedure ClasseParaINI; override;
    procedure ClasseParaComponentes; override;

    procedure Travar; override;
    procedure Destravar; override;

  public
    constructor Create(AOwner: TObject; ANomeArquivo: String = ''; AChaveCrypt: AnsiString = ''); override;
    destructor Destroy; override;

    property CHQConfig: TCHQConfig read FCHQConfig;
  end;

implementation

uses
  ACBrLibCHQClass, ACBrLibCHQConsts, ACBrLibConsts, ACBrLibComum, ACBrUtil;

{ TCHQConfig }

constructor TCHQConfig.Create;
begin
  FPorta            := '';
  FModelo           := chqNenhuma;
  FPaginaDeCodigo   := 0;
end;

destructor TCHQConfig.Destroy;
begin
  inherited Destroy;
end;

procedure TCHQConfig.LerIni(const AIni: TCustomIniFile);
begin
  FArqLog         := AIni.ReadString(CSessaoCHQ, CChaveLog, FArqLog);
  FPorta          := AIni.ReadString(CSessaoCHQ, CChavePorta, FPorta);
  FModelo         := TACBrCHQModelo(AIni.ReadInteger(CSessaoCHQ, CChaveModelo, Integer(FModelo)));
  FPaginaDeCodigo := AIni.ReadInteger(CSessaoCHQ, CChavePaginaDeCodigo, FPaginaDeCodigo);
end;

procedure TCHQConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteString(CSessaoCHQ, CChaveLog, FArqLog);
  AIni.WriteString(CSessaoCHQ, CChavePorta, FPorta);
  AIni.WriteInteger(CSessaoCHQ, CChaveModelo, Integer(FModelo));
  AIni.WriteInteger(CSessaoCHQ, CChavePaginaDeCodigo, FPaginaDeCodigo);
end;

{ TLibCHQConfig }

constructor TLibCHQConfig.Create(AOwner: TObject; ANomeArquivo: String; AChaveCrypt: AnsiString);
begin
  inherited Create(AOwner, ANomeArquivo, AChaveCrypt);

  FCHQConfig := TCHQConfig.Create;
end;

destructor TLibCHQConfig.Destroy;
begin
  FCHQConfig.Free;

  inherited Destroy;
end;

procedure TLibCHQConfig.INIParaClasse;
begin
  inherited INIParaClasse;

  FCHQConfig.LerIni(Ini);
end;

procedure TLibCHQConfig.ClasseParaINI;
begin
  inherited ClasseParaINI;

  FCHQConfig.GravarIni(Ini);
end;

procedure TLibCHQConfig.ClasseParaComponentes;
begin
  if Assigned(Owner) then
    TACBrLibCHQ(Owner).CHQDM.AplicarConfiguracoes;
end;

procedure TLibCHQConfig.Travar;
begin
  if Assigned(Owner) then
  begin
    with TACBrLibCHQ(Owner) do
      CHQDM.Travar;
  end;
end;

procedure TLibCHQConfig.Destravar;
begin
  if Assigned(Owner) then
  begin
    with TACBrLibCHQ(Owner) do
      CHQDM.Destravar;
  end;
end;

end.

