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

unit ACBrLibCTeConfig;

interface

uses
  Classes, SysUtils, IniFiles,
  ACBrCTeConfiguracoes, //ACBrCTeDACTeRLClass,
  pcnConversao,
  ACBrLibConfig;

type

  { TDACTeConfig }

  TDACTeConfig = class
  private
    FCTeCancelada: Boolean;
    FeMail: String;
    FEPECEnviado: Boolean;
    FFax: String;
    FImprimirHoraSaida: Boolean;
    FImprimirHoraSaida_Hora: String;
    FPathLogo: String;
    FPathPDF: String;
    FImpressora: String;
    FMostraPreview: Boolean;
    FMostraStatus: Boolean;
    FPrintDialog: Boolean;
    FProtocoloCTe: String;
    FSistema: String;
    FSite: String;
    FTamanhoPapel: TpcnTamanhoPapel;
    FTipoDACTe: TpcnTipoImpressao;
    FNumCopias: Integer;
    FExpandeLogoMarca: Boolean;
    FImprimeDescPorc: Boolean;
    FMargemInferior: Double;
    FMargemSuperior: Double;
    FMargemEsquerda: Double;
    FMargemDireita: Double;
    FExibeResumoCanhoto: Boolean;
    FPosCanhoto: TPosRecibo;
    FUsarSeparadorPathPDF: Boolean;
    FUsuario: String;
  public
    constructor Create;
    procedure DefinirValoresPadroes;
    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);

    property PathLogo: String read FPathLogo write FPathLogo;
    property PathPDF: String read FPathPDF write FPathPDF;
    property Impressora: String read FImpressora write FImpressora;
    property MostraPreview: Boolean read FMostraPreview write FMostraPreview;
    property MostraStatus: Boolean read FMostraStatus write FMostraStatus;
    property TipoDACTe: TpcnTipoImpressao read FTipoDACTe write FTipoDACTe;
    property NumCopias: Integer read FNumCopias write FNumCopias;
    property ImprimeDescPorc: Boolean read FImprimeDescPorc write FImprimeDescPorc;
    property MargemInferior: Double read FMargemInferior write FMargemInferior;
    property MargemSuperior: Double read FMargemSuperior write FMargemSuperior;
    property MargemEsquerda: Double read FMargemEsquerda write FMargemEsquerda;
    property MargemDireita: Double read FMargemDireita write FMargemDireita;
    property ExibeResumoCanhoto: Boolean read FExibeResumoCanhoto write FExibeResumoCanhoto;
    property ExpandeLogoMarca: Boolean read FExpandeLogoMarca write FExpandeLogoMarca;
    property PosCanhoto: TPosRecibo read FPosCanhoto write FPosCanhoto;
    property CTeCancelada: Boolean read FCTeCancelada write FCTeCancelada;
    property EPECEnviado: Boolean read FEPECEnviado write FEPECEnviado;
    property ImprimirHoraSaida: Boolean read FImprimirHoraSaida write FImprimirHoraSaida;
    property PrintDialog: Boolean read FPrintDialog write FPrintDialog;
    property UsarSeparadorPathPDF: Boolean read FUsarSeparadorPathPDF write FUsarSeparadorPathPDF;
    property eMail: String read FeMail write FeMail;
    property Fax: String read FFax write FFax;
    property ImprimirHoraSaida_Hora: String read FImprimirHoraSaida_Hora write FImprimirHoraSaida_Hora;
    property ProtocoloCTe: String read FProtocoloCTe write FProtocoloCTe;
    property Sistema: String read FSistema write FSistema;
    property Site: String read FSite write FSite;
    property Usuario: String read FUsuario write FUsuario;
    property TamanhoPapel: TpcnTamanhoPapel read FTamanhoPapel write FTamanhoPapel;
  end;

  { TLibCTeConfig }

  TLibCTeConfig = class(TLibConfig)
  private
    FDACTeConfig: TDACTeConfig;
    FCTeConfig: TConfiguracoesCTe;
  protected
    function AtualizarArquivoConfiguracao: Boolean; override;
//    procedure AplicarConfiguracoes; override;

    procedure INIParaClasse; override;
    procedure ClasseParaINI; override;
    procedure ClasseParaComponentes; override;

    procedure Travar; override;
    procedure Destravar; override;

  public
    constructor Create(AOwner: TObject; ANomeArquivo: String = ''; AChaveCrypt: AnsiString = ''); override;
    destructor Destroy; override;

    property CTeConfig: TConfiguracoesCTe read FCTeConfig;
    property DACTeConfig: TDACTeConfig read FDACTeConfig;
  end;

implementation

uses
  ACBrLibCTeClass, ACBrLibCTeConsts, ACBrLibConsts, ACBrLibComum,
  ACBrUtil;

{ TDACTeConfig }

constructor TDACTeConfig.Create;
begin
  inherited Create;
  DefinirValoresPadroes;
end;

procedure TDACTeConfig.DefinirValoresPadroes;
begin
  FPathLogo := '';
  FPathPDF := '';
  FImpressora := '';
  FTipoDACTe := tiRetrato;
  FMostraPreview := True;
  FMostraStatus := True;
  FNumCopias := 1;
  FImprimeDescPorc := False;
  FExpandeLogoMarca := False;
  FMargemInferior := 0.7;
  FMargemSuperior := 0.7;
  FMargemEsquerda := 0.7;
  FMargemDireita := 0.7;
  FExibeResumoCanhoto := False;
  FPosCanhoto := prCabecalho;
  FCTeCancelada := False;
  FEPECEnviado := False;
  FImprimirHoraSaida := False;
  FPrintDialog := False;
  FUsarSeparadorPathPDF := False;
  FeMail := '';
  FFax := '';
  FImprimirHoraSaida_Hora := '';
  FProtocoloCTe := '';
  FSistema := '';
  FSite := '';
  FUsuario := '';
  FTamanhoPapel := tpA4;
end;

procedure TDACTeConfig.LerIni(const AIni: TCustomIniFile);
begin
  FPathLogo := AIni.ReadString(CSessaoDACTe, CChavePathLogo, FPathLogo);
  FPathPDF := AIni.ReadString(CSessaoDACTe, CChavePathPDF, FPathPDF);
  FImpressora := AIni.ReadString(CSessaoDACTe, CChaveImpressora, FImpressora);
  FTipoDACTe := TpcnTipoImpressao(AIni.ReadInteger(CSessaoDACTe, CChaveTipoDACTe, Integer(FTipoDACTe)));
  FMostraPreview := AIni.ReadBool(CSessaoDACTe, CChaveMostraPreview, FMostraPreview);
  FMostraStatus := AIni.ReadBool(CSessaoDACTe, CChaveMostraStatus, FMostraStatus);
  FNumCopias := AIni.ReadInteger(CSessaoDACTe, CChaveCopias, FNumCopias);
  FExpandeLogoMarca := AIni.ReadBool(CSessaoDACTe, CChaveExpandeLogoMarca, FExpandeLogoMarca);
  FMargemInferior := AIni.ReadFloat(CSessaoDACTe, CChaveMargemInferior, FMargemInferior);
  FMargemSuperior := AIni.ReadFloat(CSessaoDACTe, CChaveMargemSuperior, FMargemSuperior);
  FMargemEsquerda := AIni.ReadFloat(CSessaoDACTe, CChaveMargemEsquerda, FMargemEsquerda);
  FMargemDireita := AIni.ReadFloat(CSessaoDACTe, CChaveMargemDireita, FMargemDireita);
  FExibeResumoCanhoto := AIni.ReadBool(CSessaoDACTe, CChaveExibeResumoCanhoto, FExibeResumoCanhoto);
  FPosCanhoto := TPosRecibo(AIni.ReadInteger(CSessaoDACTe, CChavePosCanhoto, Integer(FPosCanhoto)));
  FCTeCancelada := AIni.ReadBool(CSessaoDACTe, CChaveCTeCancelada, FCTeCancelada);
  FEPECEnviado := AIni.ReadBool(CSessaoDACTe, CChaveEPECEnviado, FEPECEnviado);
  FImprimirHoraSaida := AIni.ReadBool(CSessaoDACTe, CChaveImprimirHoraSaida, FImprimirHoraSaida);
  FPrintDialog := AIni.ReadBool(CSessaoDACTe, CChavePrintDialog, FPrintDialog);
  FUsarSeparadorPathPDF := AIni.ReadBool(CSessaoDACTe, CChaveUsarSeparadorPathPDF, FUsarSeparadorPathPDF);
  FeMail := AIni.ReadString(CSessaoDACTe, CChaveeMail, FeMail);
  FFax := AIni.ReadString(CSessaoDACTe, CChaveFax, FFax);
  FImprimirHoraSaida_Hora := AIni.ReadString(CSessaoDACTe, CChaveImprimirHoraSaida_Hora, FImprimirHoraSaida_Hora);
  FProtocoloCTe := AIni.ReadString(CSessaoDACTe, CChaveProtocoloCTe, FProtocoloCTe);
  FSistema := AIni.ReadString(CSessaoDACTe, CChaveSistema, FSistema);
  FSite := AIni.ReadString(CSessaoDACTe, CChaveSite, FSite);
  FUsuario := AIni.ReadString(CSessaoDACTe, CChaveUsuario, FUsuario);
  FTamanhoPapel := TpcnTamanhoPapel(AIni.ReadInteger(CSessaoDACTe, CChaveTamanhoPapel, Integer(FTamanhoPapel)));
end;

procedure TDACTeConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteString(CSessaoDACTe, CChavePathLogo, FPathLogo);
  AIni.WriteString(CSessaoDACTe, CChavePathPDF, FPathPDF);
  AIni.WriteString(CSessaoDACTe, CChaveImpressora, FImpressora);
  AIni.WriteInteger(CSessaoDACTe, CChaveTipoDACTe, Integer(FTipoDACTe));
  AIni.WriteBool(CSessaoDACTe, CChaveMostraPreview, FMostraPreview);
  AIni.WriteBool(CSessaoDACTe, CChaveMostraStatus, FMostraStatus);
  AIni.WriteInteger(CSessaoDACTe, CChaveCopias, FNumCopias);
  AIni.WriteBool(CSessaoDACTe, CChaveExpandeLogoMarca, FExpandeLogoMarca);
  AIni.WriteFloat(CSessaoDACTe, CChaveMargemInferior, FMargemInferior);
  AIni.WriteFloat(CSessaoDACTe, CChaveMargemSuperior, FMargemSuperior);
  AIni.WriteFloat(CSessaoDACTe, CChaveMargemEsquerda, FMargemEsquerda);
  AIni.WriteFloat(CSessaoDACTe, CChaveMargemDireita, FMargemDireita);
  AIni.WriteBool(CSessaoDACTe, CChaveExibeResumoCanhoto, FExibeResumoCanhoto);
  AIni.WriteInteger(CSessaoDACTe, CChavePosCanhoto, Integer(FPosCanhoto));
  AIni.WriteBool(CSessaoDACTe, CChaveCTeCancelada, FCTeCancelada);
  AIni.WriteBool(CSessaoDACTe, CChaveEPECEnviado, FEPECEnviado);
  AIni.WriteBool(CSessaoDACTe, CChaveImprimirHoraSaida, FImprimirHoraSaida);
  AIni.WriteBool(CSessaoDACTe, CChavePrintDialog, FPrintDialog);
  AIni.WriteBool(CSessaoDACTe, CChaveUsarSeparadorPathPDF, FUsarSeparadorPathPDF);
  AIni.WriteString(CSessaoDACTe, CChaveeMail, FeMail);
  AIni.WriteString(CSessaoDACTe, CChaveFax, FFax);
  AIni.WriteString(CSessaoDACTe, CChaveImprimirHoraSaida_Hora, FImprimirHoraSaida_Hora);
  AIni.WriteString(CSessaoDACTe, CChaveProtocoloCTe, FProtocoloCTe);
  AIni.WriteString(CSessaoDACTe, CChaveSistema, FSistema);
  AIni.WriteString(CSessaoDACTe, CChaveSite, FSite);
  AIni.WriteString(CSessaoDACTe, CChaveUsuario, FUsuario);
  AIni.WriteInteger(CSessaoDACTe, CChaveTamanhoPapel, Integer(FTamanhoPapel));
end;

{ TLibCTeConfig }

constructor TLibCTeConfig.Create(AOwner: TObject; ANomeArquivo: String; AChaveCrypt: AnsiString);
begin
  inherited Create(AOwner, ANomeArquivo, AChaveCrypt);

  FCTeConfig := TConfiguracoesCTe.Create(nil);
  FDACTeConfig := TDACTeConfig.Create;
end;

destructor TLibCTeConfig.Destroy;
begin
  FCTeConfig.Destroy;
  FDACTeConfig.Free;

  inherited Destroy;
end;

function TLibCTeConfig.AtualizarArquivoConfiguracao: Boolean;
var
  Versao: String;
begin
  Versao := Ini.ReadString(CSessaoVersao, CLibCTeNome, '0');
  Result := (CompareVersions(CLibCTeVersao, Versao) > 0) or
            (inherited AtualizarArquivoConfiguracao);
end;

procedure TLibCTeConfig.INIParaClasse;
begin
  inherited INIParaClasse;

  FCTeConfig.LerIni(Ini);
  FDACTeConfig.LerIni(Ini);
end;

procedure TLibCTeConfig.ClasseParaINI;
begin
  inherited ClasseParaINI;

  Ini.WriteString(CSessaoVersao, CLibCTeNome, CLibCTeVersao);

  FCTeConfig.GravarIni(Ini);
  FDACTeConfig.GravarIni(Ini);
end;

procedure TLibCTeConfig.ClasseParaComponentes;
begin
  if Assigned(Owner) then
    TACBrLibCTe(Owner).CTeDM.AplicarConfiguracoes;
end;

procedure TLibCTeConfig.Travar;
begin
  if Assigned(Owner) then
  begin
    with TACBrLibCTe(Owner) do
      CTeDM.Travar;
  end;
end;

procedure TLibCTeConfig.Destravar;
begin
  if Assigned(Owner) then
  begin
    with TACBrLibCTe(Owner) do
      CTeDM.Destravar;
  end;
end;

end.

