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

unit ACBrLibNFComConfig;

interface

uses
  Classes, SysUtils, IniFiles, synachar,
  ACBrLibComum, ACBrLibConfig, ACBrNFCom, ACBrNFCom.DANFComRLClass,
  ACBrNFComConversao, ACBrNFComConfiguracoes, ACBrXmlBase, DFeReportConfig, ACBrDFeReport;

type

{ TDANFComReportConfig }
TDANFComReportConfig = class(TDFeReportConfig<TACBrDFeReport>)
  private
    FMostraPreview: Boolean;
    FMargemInferior: Double;
    FMargemSuperior: Double;
    FMargemEsquerda: Double;
    FMargemDireita: Double;

  protected
    procedure LerIniChild(const AIni: TCustomIniFile); override;
    procedure GravarIniChild(const AIni: TCustomIniFile); override;
    procedure ApplyChild(const DFeReport: TACBrDFeReport; const Lib: TACBrLib); override;
    procedure DefinirValoresPadroesChild; override;

  public
    constructor Create;
    destructor Destroy; override;

    property MostraPreview: Boolean read FMostraPreview write FMostraPreview;
    property MargemInferior: Double read FMargemInferior write FMargemInferior;
    property MargemSuperior: Double read FMargemSuperior write FMargemSuperior;
    property MargemEsquerda: Double read FMargemEsquerda write FMargemEsquerda;
    property MargemDireita: Double read FMargemDireita write FMargemDireita;
end;

{ TLibNFComConfig }
TLibNFComConfig = class(TLibConfig)
  private
    FNFComConfig: TConfiguracoesNFCom;
    FDANFComConfig: TDANFComReportConfig;

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

    property NFComConfig: TConfiguracoesNFCom read FNFComConfig;
    property DANFComConfig: TDANFComReportConfig read FDANFComConfig;
end;

implementation

Uses
  ACBrLibNFComBase, ACBrLibNFComConsts, ACBrLibConsts, ACBrUtil.FilesIO;

{ TDANFComReportConfig }
procedure TDANFComReportConfig.LerIniChild(const AIni: TCustomIniFile);
begin
  FMostraPreview := AIni.ReadBool(CSessaoDANFCom, CChaveMostraPreview, FMostraPreview);
  FMargemInferior := AIni.ReadFloat(CSessaoDANFCom, CChaveMargemInferior, FMargemInferior);
  FMargemSuperior := AIni.ReadFloat(CSessaoDANFCom, CChaveMargemSuperior, FMargemSuperior);
  FMargemEsquerda := AIni.ReadFloat(CSessaoDANFCom, CChaveMargemEsquerda, FMargemEsquerda);
  FMargemDireita := AIni.ReadFloat(CSessaoDANFCom, CChaveMargemDireita, FMargemDireita);
end;

procedure TDANFComReportConfig.GravarIniChild(const AIni: TCustomIniFile);
begin
  AIni.WriteBool(CSessaoDANFCom, CChaveMostraPreview, FMostraPreview);
  AIni.WriteFloat(CSessaoDANFCom, CChaveMargemInferior, FMargemInferior);
  AIni.WriteFloat(CSessaoDANFCom, CChaveMargemSuperior, FMargemSuperior);
  AIni.WriteFloat(CSessaoDANFCom, CChaveMargemEsquerda, FMargemEsquerda);
  AIni.WriteFloat(CSessaoDANFCom, CChaveMargemDireita, FMargemDireita);
end;

procedure TDANFComReportConfig.ApplyChild(const DFeReport: TACBrDFeReport;
  const Lib: TACBrLib);
var
  LDANFCom: TACBrNFComDANFComRL;
begin
  LDANFCom := TACBrNFComDANFComRL(DFeReport);
  with LDANFCom do
  begin
    MostraPreview := FMostraPreview;
    MargemInferior := FMargemInferior;
    MargemSuperior := FMargemSuperior;
    MargemEsquerda := FMargemEsquerda;
    MargemDireita  := FMargemDireita;
  end;
end;

procedure TDANFComReportConfig.DefinirValoresPadroesChild;
begin
  FMostraPreview := False;
  FMargemInferior := 0;
  FMargemSuperior := 0;
  FMargemEsquerda := 0;
  FMargemDireita := 0;
end;

constructor TDANFComReportConfig.Create;
begin
  inherited Create(CSessaoDANFCom)
end;

destructor TDANFComReportConfig.Destroy;
begin
  inherited Destroy;
end;

{ TLibNFComConfig }
function TLibNFComConfig.AtualizarArquivoConfiguracao: Boolean;
var
  Versao: String;
begin
  Versao := Ini.ReadString(CSessaoVersao, CLibNFComNome, '0');
  Result := (CompareVersions(CLibNFComVersao, Versao) > 0) or
            (inherited AtualizarArquivoConfiguracao);
end;

procedure TLibNFComConfig.INIParaClasse;
begin
  inherited INIParaClasse;

  FNFComConfig.ChaveCryptINI := ChaveCrypt;
  FNFComConfig.LerIni(Ini);
  FDANFComConfig.LerIni(Ini);
end;

procedure TLibNFComConfig.ClasseParaINI;
begin
  inherited ClasseParaINI;

  FNFComConfig.ChaveCryptINI := ChaveCrypt;
  FNFComConfig.GravarIni(Ini);
  FDANFComConfig.GravarIni(Ini);
end;

procedure TLibNFComConfig.ClasseParaComponentes;
begin
  FNFComConfig.ChaveCryptINI := ChaveCrypt;

  if Assigned(Owner) then
    TACBrLibNFCom(Owner).NFComDM.AplicarConfiguracoes;
end;

procedure TLibNFComConfig.Travar;
begin
  if Assigned(Owner) then
    TACBrLibNFCom(Owner).NFComDM.Travar;
end;

procedure TLibNFComConfig.Destravar;
begin
  if Assigned(Owner) then
    TACBrLibNFCom(Owner).NFComDM.Destravar;
end;

constructor TLibNFComConfig.Create(AOwner: TObject; ANomeArquivo: String;
  AChaveCrypt: AnsiString);
begin
  inherited Create(AOwner, ANomeArquivo, AChaveCrypt);
  FNFComConfig := TConfiguracoesNFCom.Create(nil);
  FNFComConfig.ChaveCryptINI := AChaveCrypt;
  FDANFComConfig := TDANFComReportConfig.Create;
end;

destructor TLibNFComConfig.Destroy;
begin
  FNFComConfig.Free;
  FDANFComConfig.Free;
  inherited Destroy;
end;

end.

