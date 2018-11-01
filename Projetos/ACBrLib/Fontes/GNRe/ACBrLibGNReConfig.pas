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

unit ACBrLibGNReConfig;

interface

uses
  Classes, SysUtils, IniFiles,
  ACBrGNReConfiguracoes,
  pcnConversao,
  ACBrLibConfig;

type

  { TGuiaConfig }

  TGuiaConfig = class
  private
    FeMail: String;
    FFax: String;
    FImpressora: String;
    FMargemInferior: Double;
    FMargemSuperior: Double;
    FMargemEsquerda: Double;
    FMargemDireita: Double;
    FMostraPreview: Boolean;
    FMostraStatus: Boolean;
    FNumCopias: Integer;
    FPathPDF: String;
    FPrintDialog: Boolean;
    FSistema: String;
    FSite: String;
    FTamanhoPapel: TpcnTamanhoPapel;
    FUsuario: String;
    FUsarSeparadorPathPDF: Boolean;
  public
    constructor Create;
    procedure DefinirValoresPadroes;
    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);

    property eMail: String read FeMail write FeMail;
    property Fax: String read FFax write FFax;
    property Impressora: String read FImpressora write FImpressora;
    property MargemInferior: Double read FMargemInferior write FMargemInferior;
    property MargemSuperior: Double read FMargemSuperior write FMargemSuperior;
    property MargemEsquerda: Double read FMargemEsquerda write FMargemEsquerda;
    property MargemDireita: Double read FMargemDireita write FMargemDireita;
    property MostraPreview: Boolean read FMostraPreview write FMostraPreview;
    property MostraStatus: Boolean read FMostraStatus write FMostraStatus;
    property NumCopias: Integer read FNumCopias write FNumCopias;
    property PathPDF: String read FPathPDF write FPathPDF;
    property PrintDialog: Boolean read FPrintDialog write FPrintDialog;
    property Sistema: String read FSistema write FSistema;
    property Site: String read FSite write FSite;
    property TamanhoPapel: TpcnTamanhoPapel read FTamanhoPapel write FTamanhoPapel;
    property Usuario: String read FUsuario write FUsuario;
    property UsarSeparadorPathPDF: Boolean read FUsarSeparadorPathPDF write FUsarSeparadorPathPDF;
  end;

  { TLibGNReConfig }

  TLibGNReConfig = class(TLibConfig)
  private
    FGuiaConfig: TGuiaConfig;
    FGNReConfig: TConfiguracoesGNRe;
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

    property GNReConfig: TConfiguracoesGNRe read FGNReConfig;
    property GuiaConfig: TGuiaConfig read FGuiaConfig;
  end;

implementation

uses
  ACBrLibGNReClass, ACBrLibGNReConsts, ACBrLibConsts, ACBrLibComum,
  ACBrUtil;

{ TGuiaConfig }

constructor TGuiaConfig.Create;
begin
  inherited Create;
  DefinirValoresPadroes;
end;

procedure TGuiaConfig.DefinirValoresPadroes;
begin
  FeMail := '';
  FFax := '';
  FImpressora := '';
  FMargemInferior := 0.7;
  FMargemSuperior := 0.7;
  FMargemEsquerda := 0.7;
  FMargemDireita := 0.7;
  FMostraPreview := True;
  FMostraStatus := True;
  FNumCopias := 1;
  FPathPDF := '';
  FPrintDialog := False;
  FSistema := '';
  FSite := '';
  FTamanhoPapel := tpA4;
  FUsuario := '';
  FUsarSeparadorPathPDF := False;
end;

procedure TGuiaConfig.LerIni(const AIni: TCustomIniFile);
begin
  FeMail := AIni.ReadString(CSessaoGuia, CChaveeMail, FeMail);
  FFax := AIni.ReadString(CSessaoGuia, CChaveFax, FFax);
  FImpressora := AIni.ReadString(CSessaoGuia, CChaveImpressora, FImpressora);
  FMargemInferior := AIni.ReadFloat(CSessaoGuia, CChaveMargemInferior, FMargemInferior);
  FMargemSuperior := AIni.ReadFloat(CSessaoGuia, CChaveMargemSuperior, FMargemSuperior);
  FMargemEsquerda := AIni.ReadFloat(CSessaoGuia, CChaveMargemEsquerda, FMargemEsquerda);
  FMargemDireita := AIni.ReadFloat(CSessaoGuia, CChaveMargemDireita, FMargemDireita);
  FMostraPreview := AIni.ReadBool(CSessaoGuia, CChaveMostraPreview, FMostraPreview);
  FMostraStatus := AIni.ReadBool(CSessaoGuia, CChaveMostraStatus, FMostraStatus);
  FNumCopias := AIni.ReadInteger(CSessaoGuia, CChaveCopias, FNumCopias);
  FPathPDF := AIni.ReadString(CSessaoGuia, CChavePathPDF, FPathPDF);
  FPrintDialog := AIni.ReadBool(CSessaoGuia, CChavePrintDialog, FPrintDialog);
  FSistema := AIni.ReadString(CSessaoGuia, CChaveSistema, FSistema);
  FSite := AIni.ReadString(CSessaoGuia, CChaveSite, FSite);
  FTamanhoPapel := TpcnTamanhoPapel(AIni.ReadInteger(CSessaoGuia, CChaveTamanhoPapel, Integer(FTamanhoPapel)));
  FUsuario := AIni.ReadString(CSessaoGuia, CChaveUsuario, FUsuario);
  FUsarSeparadorPathPDF := AIni.ReadBool(CSessaoGuia, CChaveUsarSeparadorPathPDF, FUsarSeparadorPathPDF);
end;

procedure TGuiaConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteString(CSessaoGuia, CChaveeMail, FeMail);
  AIni.WriteString(CSessaoGuia, CChaveFax, FFax);
  AIni.WriteString(CSessaoGuia, CChaveImpressora, FImpressora);
  AIni.WriteFloat(CSessaoGuia, CChaveMargemInferior, FMargemInferior);
  AIni.WriteFloat(CSessaoGuia, CChaveMargemSuperior, FMargemSuperior);
  AIni.WriteFloat(CSessaoGuia, CChaveMargemEsquerda, FMargemEsquerda);
  AIni.WriteFloat(CSessaoGuia, CChaveMargemDireita, FMargemDireita);
  AIni.WriteBool(CSessaoGuia, CChaveMostraPreview, FMostraPreview);
  AIni.WriteBool(CSessaoGuia, CChaveMostraStatus, FMostraStatus);
  AIni.WriteInteger(CSessaoGuia, CChaveCopias, FNumCopias);
  AIni.WriteString(CSessaoGuia, CChavePathPDF, FPathPDF);
  AIni.WriteBool(CSessaoGuia, CChavePrintDialog, FPrintDialog);
  AIni.WriteString(CSessaoGuia, CChaveSistema, FSistema);
  AIni.WriteString(CSessaoGuia, CChaveSite, FSite);
  AIni.WriteInteger(CSessaoGuia, CChaveTamanhoPapel, Integer(FTamanhoPapel));
  AIni.WriteString(CSessaoGuia, CChaveUsuario, FUsuario);
  AIni.WriteBool(CSessaoGuia, CChaveUsarSeparadorPathPDF, FUsarSeparadorPathPDF);
end;

{ TLibGNReConfig }

constructor TLibGNReConfig.Create(AOwner: TObject; ANomeArquivo: String; AChaveCrypt: AnsiString);
begin
  inherited Create(AOwner, ANomeArquivo, AChaveCrypt);

  FGNReConfig := TConfiguracoesGNRe.Create(nil);
  FGuiaConfig := TGuiaConfig.Create;
end;

destructor TLibGNReConfig.Destroy;
begin
  FGNReConfig.Destroy;
  FGuiaConfig.Free;

  inherited Destroy;
end;

function TLibGNReConfig.AtualizarArquivoConfiguracao: Boolean;
var
  Versao: String;
begin
  Versao := Ini.ReadString(CSessaoVersao, CLibGNReNome, '0');
  Result := (CompareVersions(CLibGNReVersao, Versao) > 0) or
            (inherited AtualizarArquivoConfiguracao);
end;

procedure TLibGNReConfig.INIParaClasse;
begin
  inherited INIParaClasse;

  FGNReConfig.LerIni(Ini);
  FGuiaConfig.LerIni(Ini);
end;

procedure TLibGNReConfig.ClasseParaINI;
begin
  inherited ClasseParaINI;

  Ini.WriteString(CSessaoVersao, CLibGNReNome, CLibGNReVersao);

  FGNReConfig.GravarIni(Ini);
  FGuiaConfig.GravarIni(Ini);
end;

procedure TLibGNReConfig.ClasseParaComponentes;
begin
  if Assigned(Owner) then
    TACBrLibGNRe(Owner).GNReDM.AplicarConfiguracoes;
end;

procedure TLibGNReConfig.Travar;
begin
  if Assigned(Owner) then
  begin
    with TACBrLibGNRe(Owner) do
      GNReDM.Travar;
  end;
end;

procedure TLibGNReConfig.Destravar;
begin
  if Assigned(Owner) then
  begin
    with TACBrLibGNRe(Owner) do
      GNReDM.Destravar;
  end;
end;

end.

