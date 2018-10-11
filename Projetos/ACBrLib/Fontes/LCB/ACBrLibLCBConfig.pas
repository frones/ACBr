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

unit ACBrLibLCBConfig;

interface

uses
  Classes, SysUtils, IniFiles, SynaChar,
  ACBrLibConfig, ACBrLCB;

type

  { TLCBConfig }
  TLCBConfig = class
  private
    FExcluirSufixo: Boolean;
    FFilaMaxItens: Integer;
    FIntervalo: Integer;
    FPorta: String;
    FPrefixoAExcluir: String;
    FSufixo: String;
    FUsarFila: Boolean;

  public
    constructor Create;
    destructor Destroy; override;
    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);

    property Porta: String           read FPorta           write FPorta;
    property PrefixoAExcluir: String read FPrefixoAExcluir write FPrefixoAExcluir;
    property Sufixo: String          read FSufixo          write FSufixo;
    property ExcluirSufixo: Boolean  read FExcluirSufixo   write FExcluirSufixo;
    property UsarFila: Boolean       read FUsarFila        write FUsarFila;
    property FilaMaxItens: Integer   read FFilaMaxItens    write FFilaMaxItens;
    property Intervalo: Integer      read FIntervalo       write FIntervalo;
  end;

  { TLibLCBConfig }
  TLibLCBConfig = class(TLibConfig)
  private
    FLCBConfig: TLCBConfig;

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

    property LCBConfig: TLCBConfig read FLCBConfig;
  end;

implementation

uses
  ACBrLibLCBClass, ACBrLibLCBConsts, ACBrLibConsts, ACBrLibComum, ACBrUtil;

{ TLCBConfig }

constructor TLCBConfig.Create;
begin
  FPorta           := '';
  FExcluirSufixo   := True;
  FFilaMaxItens    := 0;
  FIntervalo       := 200;
  FPrefixoAExcluir := '';
  FSufixo          := '';
  FUsarFila        := False;
end;

destructor TLCBConfig.Destroy;
begin
  inherited Destroy;
end;

procedure TLCBConfig.LerIni(const AIni: TCustomIniFile);
begin
  FPorta           := AIni.ReadString(CSessaoLCB, CChavePorta, FPorta);
  FExcluirSufixo   := AIni.ReadBool(CSessaoLCB, CChaveExcluirSufixo, FExcluirSufixo);
  FUsarFila        := AIni.ReadBool(CSessaoLCB, CChaveUsarFila, FUsarFila);
  FPrefixoAExcluir := AIni.ReadString(CSessaoLCB, CChavePrefixoAExcluir, FPrefixoAExcluir);
  FSufixo          := AIni.ReadString(CSessaoLCB, CChaveSufixo, FSufixo);
  FFilaMaxItens    := AIni.ReadInteger(CSessaoLCB, CChaveFilaMaxItens, FFilaMaxItens);
  FIntervalo       := AIni.ReadInteger(CSessaoLCB, CChaveIntervalo, FIntervalo);
end;

procedure TLCBConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteString(CSessaoLCB, CChavePorta, FPorta);
  AIni.WriteBool(CSessaoLCB, CChaveExcluirSufixo, FExcluirSufixo);
  AIni.WriteBool(CSessaoLCB, CChaveUsarFila, FUsarFila);
  AIni.WriteString(CSessaoLCB, CChavePrefixoAExcluir, FPrefixoAExcluir);
  AIni.WriteString(CSessaoLCB, CChaveSufixo, FSufixo);
  AIni.WriteInteger(CSessaoLCB, CChaveFilaMaxItens, FFilaMaxItens);
  AIni.WriteInteger(CSessaoLCB, CChaveIntervalo, FIntervalo);
end;

{ TLibLCBConfig }

constructor TLibLCBConfig.Create(AOwner: TObject; ANomeArquivo: String; AChaveCrypt: AnsiString);
begin
  inherited Create(AOwner, ANomeArquivo, AChaveCrypt);

  FLCBConfig := TLCBConfig.Create;
end;

destructor TLibLCBConfig.Destroy;
begin
  FLCBConfig.Free;

  inherited Destroy;
end;

function TLibLCBConfig.AtualizarArquivoConfiguracao: Boolean;
var
  Versao: String;
begin
  Versao := Ini.ReadString(CSessaoVersao, CLibLCBNome, '0');
  Result := (CompareVersions(CLibLCBVersao, Versao) > 0) or
            (inherited AtualizarArquivoConfiguracao);
end;

procedure TLibLCBConfig.INIParaClasse;
begin
  inherited INIParaClasse;

  FLCBConfig.LerIni(Ini);
end;

procedure TLibLCBConfig.ClasseParaINI;
begin
  inherited ClasseParaINI;

  Ini.WriteString(CSessaoVersao, CLibLCBNome, CLibLCBVersao);

  FLCBConfig.GravarIni(Ini);
end;

procedure TLibLCBConfig.ClasseParaComponentes;
begin
  if Assigned(Owner) then
    TACBrLibLCB(Owner).LCBDM.AplicarConfiguracoes;
end;

procedure TLibLCBConfig.Travar;
begin
  if Assigned(Owner) then
  begin
    with TACBrLibLCB(Owner) do
      LCBDM.Travar;
  end;
end;

procedure TLibLCBConfig.Destravar;
begin
  if Assigned(Owner) then
  begin
    with TACBrLibLCB(Owner) do
      LCBDM.Destravar;
  end;
end;

end.

