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

unit ACBrLibMDFeConfig;

interface

uses
  Classes, SysUtils, IniFiles,
  ACBrMDFeConfiguracoes, //ACBrMDFeDAMDFeRLClass,
  pcnConversao,
  ACBrLibConfig;

type

  { TDAMDFeConfig }

  TDAMDFeConfig = class
  private
    FMDFeCancelado: Boolean;
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
    FProtocoloMDFe: String;
    FSistema: String;
    FSite: String;
    FTamanhoPapel: TpcnTamanhoPapel;
    FTipoDAMDFe: TpcnTipoImpressao;
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
    property TipoDAMDFe: TpcnTipoImpressao read FTipoDAMDFe write FTipoDAMDFe;
    property NumCopias: Integer read FNumCopias write FNumCopias;
    property ImprimeDescPorc: Boolean read FImprimeDescPorc write FImprimeDescPorc;
    property MargemInferior: Double read FMargemInferior write FMargemInferior;
    property MargemSuperior: Double read FMargemSuperior write FMargemSuperior;
    property MargemEsquerda: Double read FMargemEsquerda write FMargemEsquerda;
    property MargemDireita: Double read FMargemDireita write FMargemDireita;
    property ExibeResumoCanhoto: Boolean read FExibeResumoCanhoto write FExibeResumoCanhoto;
    property ExpandeLogoMarca: Boolean read FExpandeLogoMarca write FExpandeLogoMarca;
    property PosCanhoto: TPosRecibo read FPosCanhoto write FPosCanhoto;
    property MDFeCancelado: Boolean read FMDFeCancelado write FMDFeCancelado;
    property EPECEnviado: Boolean read FEPECEnviado write FEPECEnviado;
    property ImprimirHoraSaida: Boolean read FImprimirHoraSaida write FImprimirHoraSaida;
    property PrintDialog: Boolean read FPrintDialog write FPrintDialog;
    property UsarSeparadorPathPDF: Boolean read FUsarSeparadorPathPDF write FUsarSeparadorPathPDF;
    property eMail: String read FeMail write FeMail;
    property Fax: String read FFax write FFax;
    property ImprimirHoraSaida_Hora: String read FImprimirHoraSaida_Hora write FImprimirHoraSaida_Hora;
    property ProtocoloMDFe: String read FProtocoloMDFe write FProtocoloMDFe;
    property Sistema: String read FSistema write FSistema;
    property Site: String read FSite write FSite;
    property Usuario: String read FUsuario write FUsuario;
    property TamanhoPapel: TpcnTamanhoPapel read FTamanhoPapel write FTamanhoPapel;
  end;

  { TLibMDFeConfig }

  TLibMDFeConfig = class(TLibConfig)
  private
    FDAMDFeConfig: TDAMDFeConfig;
    FMDFeConfig: TConfiguracoesMDFe;
  protected
    procedure INIParaClasse; override;
    procedure ClasseParaINI; override;
    procedure ClasseParaComponentes; override;

    procedure Travar; override;
    procedure Destravar; override;

  public
    constructor Create(AOwner: TObject; ANomeArquivo: String = ''; AChaveCrypt: AnsiString = ''); override;
    destructor Destroy; override;

    property MDFe: TConfiguracoesMDFe read FMDFeConfig;
    property DAMDFe: TDAMDFeConfig read FDAMDFeConfig;
  end;

implementation

uses
  ACBrLibMDFeClass, ACBrLibMDFeConsts, ACBrLibConsts, ACBrLibComum,
  ACBrUtil;

{ TDAMDFeConfig }

constructor TDAMDFeConfig.Create;
begin
  inherited Create;
  DefinirValoresPadroes;
end;

procedure TDAMDFeConfig.DefinirValoresPadroes;
begin
  FPathLogo := '';
  FPathPDF := '';
  FImpressora := '';
  FTipoDAMDFe := tiRetrato;
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
  FMDFeCancelado := False;
  FEPECEnviado := False;
  FImprimirHoraSaida := False;
  FPrintDialog := False;
  FUsarSeparadorPathPDF := False;
  FeMail := '';
  FFax := '';
  FImprimirHoraSaida_Hora := '';
  FProtocoloMDFe := '';
  FSistema := '';
  FSite := '';
  FUsuario := '';
  FTamanhoPapel := tpA4;
end;

procedure TDAMDFeConfig.LerIni(const AIni: TCustomIniFile);
begin
  FPathLogo := AIni.ReadString(CSessaoDAMDFe, CChavePathLogo, FPathLogo);
  FPathPDF := AIni.ReadString(CSessaoDAMDFe, CChavePathPDF, FPathPDF);
  FImpressora := AIni.ReadString(CSessaoDAMDFe, CChaveImpressora, FImpressora);
  FTipoDAMDFe := TpcnTipoImpressao(AIni.ReadInteger(CSessaoDAMDFe, CChaveTipoDAMDFe, Integer(FTipoDAMDFe)));
  FMostraPreview := AIni.ReadBool(CSessaoDAMDFe, CChaveMostraPreview, FMostraPreview);
  FMostraStatus := AIni.ReadBool(CSessaoDAMDFe, CChaveMostraStatus, FMostraStatus);
  FNumCopias := AIni.ReadInteger(CSessaoDAMDFe, CChaveCopias, FNumCopias);
  FExpandeLogoMarca := AIni.ReadBool(CSessaoDAMDFe, CChaveExpandeLogoMarca, FExpandeLogoMarca);
  FMargemInferior := AIni.ReadFloat(CSessaoDAMDFe, CChaveMargemInferior, FMargemInferior);
  FMargemSuperior := AIni.ReadFloat(CSessaoDAMDFe, CChaveMargemSuperior, FMargemSuperior);
  FMargemEsquerda := AIni.ReadFloat(CSessaoDAMDFe, CChaveMargemEsquerda, FMargemEsquerda);
  FMargemDireita := AIni.ReadFloat(CSessaoDAMDFe, CChaveMargemDireita, FMargemDireita);
  FExibeResumoCanhoto := AIni.ReadBool(CSessaoDAMDFe, CChaveExibeResumoCanhoto, FExibeResumoCanhoto);
  FPosCanhoto := TPosRecibo(AIni.ReadInteger(CSessaoDAMDFe, CChavePosCanhoto, Integer(FPosCanhoto)));
  FMDFeCancelado := AIni.ReadBool(CSessaoDAMDFe, CChaveMDFeCancelado, FMDFeCancelado);
  FEPECEnviado := AIni.ReadBool(CSessaoDAMDFe, CChaveEPECEnviado, FEPECEnviado);
  FImprimirHoraSaida := AIni.ReadBool(CSessaoDAMDFe, CChaveImprimirHoraSaida, FImprimirHoraSaida);
  FPrintDialog := AIni.ReadBool(CSessaoDAMDFe, CChavePrintDialog, FPrintDialog);
  FUsarSeparadorPathPDF := AIni.ReadBool(CSessaoDAMDFe, CChaveUsarSeparadorPathPDF, FUsarSeparadorPathPDF);
  FeMail := AIni.ReadString(CSessaoDAMDFe, CChaveeMail, FeMail);
  FFax := AIni.ReadString(CSessaoDAMDFe, CChaveFax, FFax);
  FImprimirHoraSaida_Hora := AIni.ReadString(CSessaoDAMDFe, CChaveImprimirHoraSaida_Hora, FImprimirHoraSaida_Hora);
  FProtocoloMDFe := AIni.ReadString(CSessaoDAMDFe, CChaveProtocoloMDFe, FProtocoloMDFe);
  FSistema := AIni.ReadString(CSessaoDAMDFe, CChaveSistema, FSistema);
  FSite := AIni.ReadString(CSessaoDAMDFe, CChaveSite, FSite);
  FUsuario := AIni.ReadString(CSessaoDAMDFe, CChaveUsuario, FUsuario);
  FTamanhoPapel := TpcnTamanhoPapel(AIni.ReadInteger(CSessaoDAMDFe, CChaveTamanhoPapel, Integer(FTamanhoPapel)));
end;

procedure TDAMDFeConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteString(CSessaoDAMDFe, CChavePathLogo, FPathLogo);
  AIni.WriteString(CSessaoDAMDFe, CChavePathPDF, FPathPDF);
  AIni.WriteString(CSessaoDAMDFe, CChaveImpressora, FImpressora);
  AIni.WriteInteger(CSessaoDAMDFe, CChaveTipoDAMDFe, Integer(FTipoDAMDFe));
  AIni.WriteBool(CSessaoDAMDFe, CChaveMostraPreview, FMostraPreview);
  AIni.WriteBool(CSessaoDAMDFe, CChaveMostraStatus, FMostraStatus);
  AIni.WriteInteger(CSessaoDAMDFe, CChaveCopias, FNumCopias);
  AIni.WriteBool(CSessaoDAMDFe, CChaveExpandeLogoMarca, FExpandeLogoMarca);
  AIni.WriteFloat(CSessaoDAMDFe, CChaveMargemInferior, FMargemInferior);
  AIni.WriteFloat(CSessaoDAMDFe, CChaveMargemSuperior, FMargemSuperior);
  AIni.WriteFloat(CSessaoDAMDFe, CChaveMargemEsquerda, FMargemEsquerda);
  AIni.WriteFloat(CSessaoDAMDFe, CChaveMargemDireita, FMargemDireita);
  AIni.WriteBool(CSessaoDAMDFe, CChaveExibeResumoCanhoto, FExibeResumoCanhoto);
  AIni.WriteInteger(CSessaoDAMDFe, CChavePosCanhoto, Integer(FPosCanhoto));
  AIni.WriteBool(CSessaoDAMDFe, CChaveMDFeCancelado, FMDFeCancelado);
  AIni.WriteBool(CSessaoDAMDFe, CChaveEPECEnviado, FEPECEnviado);
  AIni.WriteBool(CSessaoDAMDFe, CChaveImprimirHoraSaida, FImprimirHoraSaida);
  AIni.WriteBool(CSessaoDAMDFe, CChavePrintDialog, FPrintDialog);
  AIni.WriteBool(CSessaoDAMDFe, CChaveUsarSeparadorPathPDF, FUsarSeparadorPathPDF);
  AIni.WriteString(CSessaoDAMDFe, CChaveeMail, FeMail);
  AIni.WriteString(CSessaoDAMDFe, CChaveFax, FFax);
  AIni.WriteString(CSessaoDAMDFe, CChaveImprimirHoraSaida_Hora, FImprimirHoraSaida_Hora);
  AIni.WriteString(CSessaoDAMDFe, CChaveProtocoloMDFe, FProtocoloMDFe);
  AIni.WriteString(CSessaoDAMDFe, CChaveSistema, FSistema);
  AIni.WriteString(CSessaoDAMDFe, CChaveSite, FSite);
  AIni.WriteString(CSessaoDAMDFe, CChaveUsuario, FUsuario);
  AIni.WriteInteger(CSessaoDAMDFe, CChaveTamanhoPapel, Integer(FTamanhoPapel));
end;

{ TLibMDFeConfig }

constructor TLibMDFeConfig.Create(AOwner: TObject; ANomeArquivo: String; AChaveCrypt: AnsiString);
begin
  inherited Create(AOwner, ANomeArquivo, AChaveCrypt);

  FMDFeConfig := TConfiguracoesMDFe.Create(nil);
  FMDFeConfig.ChaveCryptINI := AChaveCrypt;

  FDAMDFeConfig := TDAMDFeConfig.Create;
end;

destructor TLibMDFeConfig.Destroy;
begin
  FMDFeConfig.Destroy;
  FDAMDFeConfig.Free;

  inherited Destroy;
end;

procedure TLibMDFeConfig.INIParaClasse;
begin
  inherited INIParaClasse;

  FMDFeConfig.ChaveCryptINI := ChaveCrypt;

  FMDFeConfig.LerIni(Ini);
  FDAMDFeConfig.LerIni(Ini);
end;

procedure TLibMDFeConfig.ClasseParaINI;
begin
  inherited ClasseParaINI;

  FMDFeConfig.ChaveCryptINI := ChaveCrypt;

  FMDFeConfig.GravarIni(Ini);
  FDAMDFeConfig.GravarIni(Ini);
end;

procedure TLibMDFeConfig.ClasseParaComponentes;
begin
  FMDFeConfig.ChaveCryptINI := ChaveCrypt;

  if Assigned(Owner) then
    TACBrLibMDFe(Owner).MDFeDM.AplicarConfiguracoes;
end;

procedure TLibMDFeConfig.Travar;
begin
  if Assigned(Owner) then
  begin
    with TACBrLibMDFe(Owner) do
      MDFeDM.Travar;
  end;
end;

procedure TLibMDFeConfig.Destravar;
begin
  if Assigned(Owner) then
  begin
    with TACBrLibMDFe(Owner) do
      MDFeDM.Destravar;
  end;
end;

end.

