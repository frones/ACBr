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

unit ACBrLibBPeConfig;

interface

uses
  Classes, SysUtils, IniFiles, Graphics,
  pcnConversao,
  ACBrDFeReport, ACBrBPeDABPEClass, ACBrBPeConfiguracoes,
  ACBrLibConfig, ACBrDeviceConfig, DFeReportConfig;

type

  { TDABPeConfig }

  TDABPeConfig = class
  private
    FImprimeNomeFantasia: Boolean;
    FImprimirTotalLiquido: Boolean;
    FMostrarPreview: Boolean;
    FMostrarStatus: Boolean;
    FTipoDABPe: TpcnTipoImpressao;
    FNumCopias: Integer;
    FImprimeDescPorc: Boolean;
    FMargemInferior: Double;
    FMargemSuperior: Double;
    FMargemEsquerda: Double;
    FMargemDireita: Double;

    FBPeCancelada: Boolean;
    FeMail: String;       //
    FEPECEnviado: Boolean;  //
    FFax: String;             //
    FImprimirHoraSaida: Boolean;//
    FImprimirHoraSaida_Hora: String;//
    FPathLogo: String;  //
    FPathPDF: String;   //
    FImpressora: String; //
    FPrintDialog: Boolean; //
    FProtocoloBPe: String; //
    FSistema: String;      //
    FSite: String;         //
    FTamanhoPapel: TpcnTamanhoPapel;//
    FExpandeLogoMarca: Boolean;    //
    FExibeResumoCanhoto: Boolean;
    FPosCanhoto: TPosRecibo;       //
    FUsarSeparadorPathPDF: Boolean; //
    FUsuario: String;

  public
    constructor Create;
    procedure DefinirValoresPadroes;
    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);
    procedure Assign(const DFeReport: TACBrBPeDABPEClass);

    property PathLogo: String read FPathLogo write FPathLogo;
    property PathPDF: String read FPathPDF write FPathPDF;
    property Impressora: String read FImpressora write FImpressora;
    property MostrarPreview: Boolean read FMostrarPreview write FMostrarPreview;
    property MostrarStatus: Boolean read FMostrarStatus write FMostrarStatus;
    property TipoDABPe: TpcnTipoImpressao read FTipoDABPe write FTipoDABPe;
    property NumCopias: Integer read FNumCopias write FNumCopias;
    property ImprimeDescPorc: Boolean read FImprimeDescPorc write FImprimeDescPorc;
    property MargemInferior: Double read FMargemInferior write FMargemInferior;
    property MargemSuperior: Double read FMargemSuperior write FMargemSuperior;
    property MargemEsquerda: Double read FMargemEsquerda write FMargemEsquerda;
    property MargemDireita: Double read FMargemDireita write FMargemDireita;
    property ExibeResumoCanhoto: Boolean read FExibeResumoCanhoto write FExibeResumoCanhoto;
    property ExpandeLogoMarca: Boolean read FExpandeLogoMarca write FExpandeLogoMarca;
    property PosCanhoto: TPosRecibo read FPosCanhoto write FPosCanhoto;
    property BPeCancelada: Boolean read FBPeCancelada write FBPeCancelada;
    property EPECEnviado: Boolean read FEPECEnviado write FEPECEnviado;
    property ImprimirHoraSaida: Boolean read FImprimirHoraSaida write FImprimirHoraSaida;
    property PrintDialog: Boolean read FPrintDialog write FPrintDialog;
    property UsarSeparadorPathPDF: Boolean read FUsarSeparadorPathPDF write FUsarSeparadorPathPDF;
    property eMail: String read FeMail write FeMail;
    property Fax: String read FFax write FFax;
    property ImprimirHoraSaida_Hora: String read FImprimirHoraSaida_Hora write FImprimirHoraSaida_Hora;
    property ProtocoloBPe: String read FProtocoloBPe write FProtocoloBPe;
    property Sistema: String read FSistema write FSistema;
    property Site: String read FSite write FSite;
    property Usuario: String read FUsuario write FUsuario;
    property TamanhoPapel: TpcnTamanhoPapel read FTamanhoPapel write FTamanhoPapel;
    property ImprimeNomeFantasia: Boolean read FImprimeNomeFantasia write FImprimeNomeFantasia;
    property ImprimirTotalLiquido: Boolean read FImprimirTotalLiquido write FImprimirTotalLiquido;
  end;

  { TDABPeReportConfig }

  TDABPeReportConfig = class(TDFeReportConfig<TACBrBPeDABPEClass>)
  private
    FTipoDABPE: TpcnTipoImpressao;
    FImprimeTotalLiquido: Boolean;
    FvTribFed: currency;
    FvTribEst: currency;
    FvTribMun: currency;
    FFonteTributos: String;
    FChaveTributos: String;
    FImprimeNomeFantasia: Boolean;
    FImprimeEmUmaLinha: Boolean;
    FBPeConfig: TDABPeConfig;

  protected
    procedure LerIniChild(const AIni: TCustomIniFile); override;
    procedure GravarIniChild(const AIni: TCustomIniFile); override;
    procedure AssignChild(const DFeReport: TACBrBPeDABPEClass); override;
    procedure DefinirValoresPadroesChild; override;

  public
    constructor Create;
    destructor Destroy; override;

    property TipoDABPE: TpcnTipoImpressao read FTipoDABPE write FTipoDABPE;
    property ImprimeTotalLiquido: Boolean read FImprimeTotalLiquido write FImprimeTotalLiquido;
    property ImprimeNomeFantasia: Boolean read FImprimeNomeFantasia write FImprimeNomeFantasia;
    property ImprimeEmUmaLinha: Boolean read FImprimeEmUmaLinha write FImprimeEmUmaLinha;
    property BPeConfig: TDABPeConfig read FBPeConfig;
  end;

  { TLibBPeConfig }

  TLibBPeConfig = class(TLibConfig)
  private
    FBPeConfig: TConfiguracoesBPe;
    FDABPeConfig: TDABPeConfig;
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

    property BPeConfig: TConfiguracoesBPe read FBPeConfig;
    property DABPeConfig: TDABPeConfig read FDABPeConfig;
    property PosDeviceConfig: TDeviceConfig read FDeviceConfig write FDeviceConfig;
  end;

implementation

uses
  ACBrLibBPeClass, ACBrLibBPeConsts, ACBrLibConsts, ACBrLibComum,
  ACBrUtil;

{ TDABPeReportConfig }

procedure TDABPeReportConfig.LerIniChild(const AIni: TCustomIniFile);
begin
  FTipoDABPE := TpcnTipoImpressao(AIni.ReadInteger(CSessaoDABPE, CChaveTipoDABPE, Integer(FTipoDABPE)));
  FImprimeNomeFantasia := AIni.ReadBool(CSessaoDABPE, CChaveImprimeNomeFantasia, FImprimeNomeFantasia);

  FBPeConfig.LerIni(AIni);
end;

procedure TDABPeReportConfig.GravarIniChild(const AIni: TCustomIniFile);
begin
  AIni.WriteInteger(CSessaoDABPE, CChaveTipoDABPE, Integer(FTipoDABPE));
  AIni.WriteBool(CSessaoDABPE, CChaveImprimeNomeFantasia, FImprimeNomeFantasia);

  FBPeConfig.GravarIni(AIni);
end;

procedure TDABPeReportConfig.AssignChild(const DFeReport: TACBrBPeDABPEClass);
var
  pLibConfig: TLibBPeConfig;
begin
  pLibConfig := TLibBPeConfig(pLib.Config);
  {
  if DFeReport is TACBrBPeDABPEClass then
  begin
    pLibConfig.DABPeConfig.BPeConfig.Assign(TACBrBPeDABPEClass(DFeReport));
  end;
  }
end;

procedure TDABPeReportConfig.DefinirValoresPadroesChild;
begin
  FTipoDABPE := tiRetrato;
  FImprimeTotalLiquido := True;
  FvTribFed := 0.0;
  FvTribEst := 0.0;
  FvTribMun := 0.0;
  FFonteTributos := '';
  FChaveTributos := '';
  FImprimeNomeFantasia := False;
  FImprimeEmUmaLinha := False;

  if not Assigned(FBPeConfig) then
    FBPeConfig := TDABPeConfig.Create
  else
    FBPeConfig.DefinirValoresPadroes;
end;

constructor TDABPeReportConfig.Create;
begin
  inherited Create(CSessaoDABPE);
end;

destructor TDABPeReportConfig.Destroy;
begin
  FBPeConfig.Destroy;

  inherited Destroy;
end;

{ TDABPeConfig }

constructor TDABPeConfig.Create;
begin
  inherited Create;
  DefinirValoresPadroes;
end;

procedure TDABPeConfig.DefinirValoresPadroes;
begin
  FPathLogo := '';
  FPathPDF := '';
  FImpressora := '';
  FTipoDABPe := tiRetrato;
  FMostrarPreview := True;
  FMostrarStatus := True;
  FNumCopias := 1;
  FImprimeDescPorc := False;
  FExpandeLogoMarca := False;
  FMargemInferior := 0.7;
  FMargemSuperior := 0.7;
  FMargemEsquerda := 0.7;
  FMargemDireita := 0.7;
  FExibeResumoCanhoto := False;
  FPosCanhoto := prCabecalho;
  FBPeCancelada := False;
  FEPECEnviado := False;
  FImprimirHoraSaida := False;
  FPrintDialog := False;
  FUsarSeparadorPathPDF := False;
  FeMail := '';
  FFax := '';
  FImprimirHoraSaida_Hora := '';
  FProtocoloBPe := '';
  FSistema := '';
  FSite := '';
  FUsuario := '';
  FTamanhoPapel := tpA4;
  FImprimeNomeFantasia := True;
  FImprimirTotalLiquido := True;
end;

procedure TDABPeConfig.LerIni(const AIni: TCustomIniFile);
begin
  FPathLogo := AIni.ReadString(CSessaoDABPe, CChavePathLogo, FPathLogo);
  FPathPDF := AIni.ReadString(CSessaoDABPe, CChavePathPDF, FPathPDF);
  FImpressora := AIni.ReadString(CSessaoDABPe, CChaveImpressora, FImpressora);
  FTipoDABPe := TpcnTipoImpressao(AIni.ReadInteger(CSessaoDABPe, CChaveTipoDABPe, Integer(FTipoDABPe)));
  FMostrarPreview := AIni.ReadBool(CSessaoDABPe, CChaveMostraPreview, FMostrarPreview);
  FMostrarStatus := AIni.ReadBool(CSessaoDABPe, CChaveMostraStatus, FMostrarStatus);
  FNumCopias := AIni.ReadInteger(CSessaoDABPe, CChaveCopias, FNumCopias);
  FExpandeLogoMarca := AIni.ReadBool(CSessaoDABPe, CChaveExpandeLogoMarca, FExpandeLogoMarca);
  FMargemInferior := AIni.ReadFloat(CSessaoDABPe, CChaveMargemInferior, FMargemInferior);
  FMargemSuperior := AIni.ReadFloat(CSessaoDABPe, CChaveMargemSuperior, FMargemSuperior);
  FMargemEsquerda := AIni.ReadFloat(CSessaoDABPe, CChaveMargemEsquerda, FMargemEsquerda);
  FMargemDireita := AIni.ReadFloat(CSessaoDABPe, CChaveMargemDireita, FMargemDireita);
  FExibeResumoCanhoto := AIni.ReadBool(CSessaoDABPe, CChaveExibeResumoCanhoto, FExibeResumoCanhoto);
  FPosCanhoto := TPosRecibo(AIni.ReadInteger(CSessaoDABPe, CChavePosCanhoto, Integer(FPosCanhoto)));
  FBPeCancelada := AIni.ReadBool(CSessaoDABPe, CChaveBPeCancelada, FBPeCancelada);
  FEPECEnviado := AIni.ReadBool(CSessaoDABPe, CChaveEPECEnviado, FEPECEnviado);
  FImprimirHoraSaida := AIni.ReadBool(CSessaoDABPe, CChaveImprimirHoraSaida, FImprimirHoraSaida);
  FPrintDialog := AIni.ReadBool(CSessaoDABPe, CChavePrintDialog, FPrintDialog);
  FUsarSeparadorPathPDF := AIni.ReadBool(CSessaoDABPe, CChaveUsarSeparadorPathPDF, FUsarSeparadorPathPDF);
  FeMail := AIni.ReadString(CSessaoDABPe, CChaveeMail, FeMail);
  FFax := AIni.ReadString(CSessaoDABPe, CChaveFax, FFax);
  FImprimirHoraSaida_Hora := AIni.ReadString(CSessaoDABPe, CChaveImprimirHoraSaida_Hora, FImprimirHoraSaida_Hora);
  FProtocoloBPe := AIni.ReadString(CSessaoDABPe, CChaveProtocoloBPe, FProtocoloBPe);
  FSistema := AIni.ReadString(CSessaoDABPe, CChaveSistema, FSistema);
  FSite := AIni.ReadString(CSessaoDABPe, CChaveSite, FSite);
  FUsuario := AIni.ReadString(CSessaoDABPe, CChaveUsuario, FUsuario);
  FTamanhoPapel := TpcnTamanhoPapel(AIni.ReadInteger(CSessaoDABPe, CChaveTamanhoPapel, Integer(FTamanhoPapel)));
  FImprimeNomeFantasia := AIni.ReadBool(CSessaoDABPe, CChaveImprimeNomeFantasia, FImprimeNomeFantasia);
  FImprimirTotalLiquido := AIni.ReadBool(CSessaoDABPe, CChaveImprimirTotalLiquido, FImprimirTotalLiquido);
end;

procedure TDABPeConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteString(CSessaoDABPe, CChavePathLogo, FPathLogo);
  AIni.WriteString(CSessaoDABPe, CChavePathPDF, FPathPDF);
  AIni.WriteString(CSessaoDABPe, CChaveImpressora, FImpressora);
  AIni.WriteInteger(CSessaoDABPe, CChaveTipoDABPe, Integer(FTipoDABPe));
  AIni.WriteBool(CSessaoDABPe, CChaveMostraPreview, FMostrarPreview);
  AIni.WriteBool(CSessaoDABPe, CChaveMostraStatus, FMostrarStatus);
  AIni.WriteInteger(CSessaoDABPe, CChaveCopias, FNumCopias);
  AIni.WriteBool(CSessaoDABPe, CChaveExpandeLogoMarca, FExpandeLogoMarca);
  AIni.WriteFloat(CSessaoDABPe, CChaveMargemInferior, FMargemInferior);
  AIni.WriteFloat(CSessaoDABPe, CChaveMargemSuperior, FMargemSuperior);
  AIni.WriteFloat(CSessaoDABPe, CChaveMargemEsquerda, FMargemEsquerda);
  AIni.WriteFloat(CSessaoDABPe, CChaveMargemDireita, FMargemDireita);
  AIni.WriteBool(CSessaoDABPe, CChaveExibeResumoCanhoto, FExibeResumoCanhoto);
  AIni.WriteInteger(CSessaoDABPe, CChavePosCanhoto, Integer(FPosCanhoto));
  AIni.WriteBool(CSessaoDABPe, CChaveBPeCancelada, FBPeCancelada);
  AIni.WriteBool(CSessaoDABPe, CChaveEPECEnviado, FEPECEnviado);
  AIni.WriteBool(CSessaoDABPe, CChaveImprimirHoraSaida, FImprimirHoraSaida);
  AIni.WriteBool(CSessaoDABPe, CChavePrintDialog, FPrintDialog);
  AIni.WriteBool(CSessaoDABPe, CChaveUsarSeparadorPathPDF, FUsarSeparadorPathPDF);
  AIni.WriteString(CSessaoDABPe, CChaveeMail, FeMail);
  AIni.WriteString(CSessaoDABPe, CChaveFax, FFax);
  AIni.WriteString(CSessaoDABPe, CChaveImprimirHoraSaida_Hora, FImprimirHoraSaida_Hora);
  AIni.WriteString(CSessaoDABPe, CChaveProtocoloBPe, FProtocoloBPe);
  AIni.WriteString(CSessaoDABPe, CChaveSistema, FSistema);
  AIni.WriteString(CSessaoDABPe, CChaveSite, FSite);
  AIni.WriteString(CSessaoDABPe, CChaveUsuario, FUsuario);
  AIni.WriteInteger(CSessaoDABPe, CChaveTamanhoPapel, Integer(FTamanhoPapel));
  AIni.WriteBool(CSessaoDABPe, CChaveImprimeNomeFantasia, FImprimeNomeFantasia);
  AIni.WriteBool(CSessaoDABPe, CChaveImprimirTotalLiquido, FImprimirTotalLiquido);
end;

procedure TDABPeConfig.Assign(const DFeReport: TACBrBPeDABPEClass);
begin
  {
  with DFeReport do
  begin
    LarguraBobina := FLarguraBobina;
    ImprimeDescAcrescItem := FImprimeDescAcrescItem;
    ImprimeItens := FImprimeItens;
    ViaConsumidor := FViaConsumidor;
    vTroco := FvTroco;
    ImprimeQRCodeLateral := FImprimeQRCodeLateral;
    ImprimeLogoLateral := FImprimeLogoLateral;
    EspacoFinal := FEspacoFinal;
  end;

  if DFeReport is TACBrNFeDANFCeFortes then
  begin
    with TACBrNFeDANFCeFortes(DFeReport) do
    begin
      TamanhoLogoHeight := FTamanhoLogoHeight;
      TamanhoLogoWidth := FTamanhoLogoWidth;
    end;
  end;
  }
end;

{ TLibBPeConfig }

constructor TLibBPeConfig.Create(AOwner: TObject; ANomeArquivo: String; AChaveCrypt: AnsiString);
begin
  inherited Create(AOwner, ANomeArquivo, AChaveCrypt);

  FBPeConfig := TConfiguracoesBPe.Create(nil);
  FDABPeConfig := TDABPeConfig.Create;
end;

destructor TLibBPeConfig.Destroy;
begin
  FBPeConfig.Destroy;
  FDABPeConfig.Free;
  if FDeviceConfig <> nil then FDeviceConfig.Free;

  inherited Destroy;
end;

function TLibBPeConfig.AtualizarArquivoConfiguracao: Boolean;
var
  Versao: String;
begin
  Versao := Ini.ReadString(CSessaoVersao, CLibBPeNome, '0');
  Result := (CompareVersions(CLibBPeVersao, Versao) > 0) or
            (inherited AtualizarArquivoConfiguracao);
end;

procedure TLibBPeConfig.INIParaClasse;
begin
  inherited INIParaClasse;

  FBPeConfig.LerIni(Ini);
  FDABPeConfig.LerIni(Ini);
end;

procedure TLibBPeConfig.ClasseParaINI;
begin
  inherited ClasseParaINI;

  Ini.WriteString(CSessaoVersao, CLibBPeNome, CLibBPeVersao);

  FBPeConfig.GravarIni(Ini);
  FDABPeConfig.GravarIni(Ini);
end;

procedure TLibBPeConfig.ClasseParaComponentes;
begin
  if Assigned(Owner) then
    TACBrLibBPe(Owner).BPeDM.AplicarConfiguracoes;
end;

procedure TLibBPeConfig.Travar;
begin
  if Assigned(Owner) then
  begin
    with TACBrLibBPe(Owner) do
      BPeDM.Travar;
  end;
end;

procedure TLibBPeConfig.Destravar;
begin
  if Assigned(Owner) then
  begin
    with TACBrLibBPe(Owner) do
      BPeDM.Destravar;
  end;
end;

end.

