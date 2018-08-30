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

unit ACBrLibSedexConfig;

interface

uses
  Classes, SysUtils, IniFiles, SynaChar,
  ACBrLibConfig;

type

  { TSedexConfig }
  TSedexConfig = class
  private
    FCodContrato: String;
    FSenha: String;

  public
    constructor Create;
    destructor Destroy; override;
    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);

    property CodContrato: String read FCodContrato write FCodContrato;
    property Senha: String       read FSenha       write FSenha;
  end;

  { TLibSedexConfig }
  TLibSedexConfig = class(TLibConfig)
  private
    FSedexConfig: TSedexConfig;

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

    property SedexConfig: TSedexConfig read FSedexConfig;
  end;

implementation

uses
  ACBrLibSedexClass, ACBrLibSedexConsts, ACBrLibConsts, ACBrLibComum, ACBrUtil;

{ TSedexConfig }

constructor TSedexConfig.Create;
begin
  FCodContrato := '';
  FSenha       := '';
end;

destructor TSedexConfig.Destroy;
begin
  inherited Destroy;
end;

procedure TSedexConfig.LerIni(const AIni: TCustomIniFile);
begin
  FCodContrato := AIni.ReadString(CSessaoSedex, CChaveCodContrato, FCodContrato);
  FSenha       := AIni.ReadString(CSessaoSedex, CChaveSenha, FSenha);
end;

procedure TSedexConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteString(CSessaoSedex, CChaveCodContrato, FCodContrato);
  AIni.WriteString(CSessaoSedex, CChaveSenha, FSenha);
end;

{ TLibSedexConfig }

constructor TLibSedexConfig.Create(AOwner: TObject; ANomeArquivo: String; AChaveCrypt: AnsiString);
begin
  inherited Create(AOwner, ANomeArquivo, AChaveCrypt);

  FSedexConfig := TSedexConfig.Create;
end;

destructor TLibSedexConfig.Destroy;
begin
  FSedexConfig.Free;

  inherited Destroy;
end;

function TLibSedexConfig.AtualizarArquivoConfiguracao: Boolean;
var
  Versao: String;
begin
  Versao := Ini.ReadString(CSessaoVersao, CLibSedexNome, '0');
  Result := (CompareVersions(CLibSedexVersao, Versao) > 0) or
            (inherited AtualizarArquivoConfiguracao);
end;

procedure TLibSedexConfig.INIParaClasse;
begin
  inherited INIParaClasse;

  FSedexConfig.LerIni(Ini);
end;

procedure TLibSedexConfig.ClasseParaINI;
begin
  inherited ClasseParaINI;

  Ini.WriteString(CSessaoVersao, CLibSedexNome, CLibSedexVersao);

  FSedexConfig.GravarIni(Ini);
end;

procedure TLibSedexConfig.ClasseParaComponentes;
begin
  if Assigned(Owner) then
    TACBrLibSedex(Owner).SedexDM.AplicarConfiguracoes;
end;

procedure TLibSedexConfig.Travar;
begin
  if Assigned(Owner) then
  begin
    with TACBrLibSedex(Owner) do
      SedexDM.Travar;
  end;
end;

procedure TLibSedexConfig.Destravar;
begin
  if Assigned(Owner) then
  begin
    with TACBrLibSedex(Owner) do
      SedexDM.Destravar;
  end;
end;

end.

