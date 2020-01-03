{*******************************************************************************}
{ Projeto: Componentes ACBr                                                     }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa-  }
{ mentos de Automação Comercial utilizados no Brasil                            }
{                                                                               }
{ Direitos Autorais Reservados (c) 2018 Daniel Simoes de Almeida                }
{                                                                               }
{ Colaboradores nesse arquivo: Rafael Teno Dias                                 }
{                                                                               }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr     }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr       }
{                                                                               }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la  }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela   }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério)  }
{ qualquer versão posterior.                                                    }
{                                                                               }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM    }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU       }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor }
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)               }
{                                                                               }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto }
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,   }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.           }
{ Você também pode obter uma copia da licença em:                               }
{ http://www.opensource.org/licenses/gpl-license.php                            }
{                                                                               }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br }
{        Rua Cel.Aureliano de Camargo, 963 - Tatuí - SP - 18270-170             }
{                                                                               }
{*******************************************************************************}

{$I ACBr.inc}

unit DFeReportConfig;

interface

uses
  Classes, SysUtils, Math, IniFiles,
  pcnConversao, ACBrDFeReport,
  ACBrLibComum, ACBrLibConsts;

type

 TDFeReportConfig<T: TACBrDFeReport> = class
 private
    FPathPDF: String;
    FUsaSeparadorPathPDF: Boolean;
    FImpressora: String;
    FNumCopias: Integer;
    FMostraSetup: Boolean;
    FMostraPreview: Boolean;
    FMostraStatus: Boolean;
    FLogo: String;
    FMargemInferior: Double;
    FMargemSuperior: Double;
    FMargemEsquerda: Double;
    FMargemDireita: Double;
    FCasasDecimais: TCasasDecimais;
    FExpandeLogoMarca: Boolean;
    FNomeDocumento: String;

    procedure SetNumCopias(const Value: Integer);

 protected
    FSessao: String;

    procedure DefinirValoresPadroesChild; virtual; abstract;
    procedure ImportChild(const AIni: TCustomIniFile); virtual; abstract;
    procedure LerIniChild(const AIni: TCustomIniFile); virtual; abstract;
    procedure GravarIniChild(const AIni: TCustomIniFile); virtual; abstract;
    procedure ApplyChild(const DFeReport: T); virtual; abstract;

 public
   constructor Create(ASessao: String);
   destructor Destroy; override;

   procedure DefinirValoresPadroes;
   procedure Import(const AIni: TCustomIniFile);
   procedure LerIni(const AIni: TCustomIniFile);
   procedure GravarIni(const AIni: TCustomIniFile);
   procedure Apply(const DFeReport: T);

   property Impressora: String read FImpressora write FImpressora;
   property NomeDocumento: String read FNomeDocumento write FNomeDocumento;
   property NumCopias: Integer read FNumCopias write SetNumCopias;
   property MostraPreview: Boolean read FMostraPreview write FMostraPreview;
   property MostraSetup: Boolean read FMostraSetup write FMostraSetup;
   property MostraStatus: Boolean read FMostraStatus write FMostraStatus;
   property PathPDF: String read FPathPDF write FPathPDF;
   property UsaSeparadorPathPDF: Boolean read FUsaSeparadorPathPDF write FUsaSeparadorPathPDF;
   property Logo: String read FLogo write FLogo;
   property MargemInferior: Double read FMargemInferior write FMargemInferior;
   property MargemSuperior: Double read FMargemSuperior write FMargemSuperior;
   property MargemEsquerda: Double read FMargemEsquerda write FMargemEsquerda;
   property MargemDireita: Double read FMargemDireita write FMargemDireita;
   property ExpandeLogoMarca: Boolean read FExpandeLogoMarca write FExpandeLogoMarca;
   property CasasDecimais: TCasasDecimais read FCasasDecimais;
 end;

implementation

uses
  ACBrMonitorConsts;

constructor TDFeReportConfig<T>.Create(ASessao: String);
begin
  FSessao := ASessao;
  DefinirValoresPadroes;
end;

destructor TDFeReportConfig<T>.Destroy;
begin
  FCasasDecimais.Destroy;

  inherited Destroy;
end;

procedure TDFeReportConfig<T>.SetNumCopias(const Value: Integer);
begin
  FNumCopias := max(Value, 1);
end;

procedure TDFeReportConfig<T>.DefinirValoresPadroes;
begin
  FPathPDF := '';
  FUsaSeparadorPathPDF := False;
  FImpressora := '';
  FNomeDocumento := '';
  FMostraSetup := False;
  FMostraPreview := True;
  FMostraStatus := True;
  FNumCopias := 1;
  FLogo := '';
  FMargemInferior := 8;
  FMargemSuperior := 8;
  FMargemEsquerda := 6;
  FMargemDireita := 5.1;
  ExpandeLogoMarca := False;

  if Assigned(FCasasDecimais) then FCasasDecimais.Free;
  FCasasDecimais := TCasasDecimais.Create(nil);

  DefinirValoresPadroesChild;
end;

procedure TDFeReportConfig<T>.Import(const AIni: TCustomIniFile);
begin
  //Arquivos
  UsaSeparadorPathPDF := AIni.ReadBool(CSecArquivos, CKeyArquivosUsarSeparadorPathPDF, UsaSeparadorPathPDF);

  //Geral
  Impressora := AIni.ReadString(CSecGeral, CKeyImpressora, Impressora);
  Logo := AIni.ReadString(CSecGeral, CKeyLogomarca, Logo);

  //DANFe
  PathPDF := AIni.ReadString(CSecDANFE, CKeyDANFEPathPDF, PathPDF);
  MargemInferior := AIni.ReadFloat(CSecDANFE, CKeyDANFEMargem, MargemInferior);
  MargemSuperior := AIni.ReadFloat(CSecDANFE, CKeyDANFEMargemSup, MargemSuperior);
  MargemEsquerda := AIni.ReadFloat(CSecDANFE, CKeyDANFEMargemEsq, MargemEsquerda);
  MargemDireita := AIni.ReadFloat(CSecDANFE, CKeyDANFEMargemDir, MargemDireita);
  NumCopias := AIni.ReadInteger(CSecDANFE, CKeyDANFECopias, NumCopias);
  MostraPreview := AIni.ReadBool(CSecDANFE, CKeyDANFEMostrarPreview, MostraPreview);
  MostraStatus := AIni.ReadBool(CSecDANFE, CKeyDANFEMostrarStatus, MostraStatus);
  ExpandeLogoMarca := AIni.ReadBool(CSecDANFE, CKeyDANFEExpandirLogo, ExpandeLogoMarca);

  ImportChild(AIni);
end;

procedure TDFeReportConfig<T>.LerIni(const AIni: TCustomIniFile);
begin
  FPathPDF := AIni.ReadString(FSessao, CChavePathPDF, FPathPDF);
  FUsaSeparadorPathPDF := AIni.ReadBool(FSessao, CChaveUsaSeparadorPathPDF, FUsaSeparadorPathPDF);
  FImpressora := AIni.ReadString(FSessao, CChaveImpressora, FImpressora);
  FNomeDocumento := AIni.ReadString(FSessao, CChaveNomeDocumento, FNomeDocumento);
  FMostraSetup := AIni.ReadBool(FSessao, CChaveMostraSetup, FMostraSetup);
  FMostraPreview := AIni.ReadBool(FSessao, CChaveMostraPreview, FMostraPreview);
  FMostraStatus := AIni.ReadBool(FSessao, CChaveMostraStatus, FMostraStatus);
  FNumCopias := AIni.ReadInteger(FSessao, CChaveCopias, FNumCopias);
  FLogo := AIni.ReadString(FSessao, CChavePathLogo, FLogo);
  FMargemInferior := AIni.ReadFloat(FSessao, CChaveMargemInferior, FMargemInferior);
  FMargemSuperior := AIni.ReadFloat(FSessao, CChaveMargemSuperior, FMargemSuperior);
  FMargemEsquerda := AIni.ReadFloat(FSessao, CChaveMargemEsquerda, FMargemEsquerda);
  FMargemDireita := AIni.ReadFloat(FSessao, CChaveMargemDireita, FMargemDireita);
  FExpandeLogoMarca := AIni.ReadBool(FSessao, CChaveExpandeLogoMarca, FExpandeLogoMarca);

  with FCasasDecimais do
  begin
    Formato := TDetFormato(AIni.ReadInteger(FSessao, CChaveCasasDecimaisFormato, Integer(Formato)));
    MaskqCom := AIni.ReadString(FSessao, CChaveCasasDecimaisMaskqCom, MaskqCom);
    MaskvUnCom := AIni.ReadString(FSessao, CChaveCasasDecimaisMaskvUnCom, MaskvUnCom);
    qCom := AIni.ReadInteger(FSessao, CChaveCasasDecimaisqCom, qCom);
    vUnCom := AIni.ReadInteger(FSessao, CChaveCasasDecimaisvUnCom, vUnCom);
  end;

  LerIniChild(AIni);
end;

procedure TDFeReportConfig<T>.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteString(FSessao, CChavePathPDF, FPathPDF);
  AIni.WriteBool(FSessao, CChaveUsaSeparadorPathPDF, FUsaSeparadorPathPDF);
  AIni.WriteString(FSessao, CChaveImpressora, FImpressora);
  AIni.WriteString(FSessao, CChaveNomeDocumento, FNomeDocumento);
  AIni.WriteBool(FSessao, CChaveMostraSetup, FMostraSetup);
  AIni.WriteBool(FSessao, CChaveMostraPreview, FMostraPreview);
  AIni.WriteBool(FSessao, CChaveMostraStatus, FMostraStatus);
  AIni.WriteInteger(FSessao, CChaveCopias, FNumCopias);
  AIni.WriteString(FSessao, CChavePathLogo, FLogo);
  AIni.WriteFloat(FSessao, CChaveMargemInferior, FMargemInferior);
  AIni.WriteFloat(FSessao, CChaveMargemSuperior, FMargemSuperior);
  AIni.WriteFloat(FSessao, CChaveMargemEsquerda, FMargemEsquerda);
  AIni.WriteFloat(FSessao, CChaveMargemDireita, FMargemDireita);
  AIni.WriteBool(FSessao, CChaveExpandeLogoMarca, FExpandeLogoMarca);

  with FCasasDecimais do
  begin
    AIni.WriteInteger(FSessao, CChaveCasasDecimaisFormato, Integer(Formato));
    AIni.WriteString(FSessao, CChaveCasasDecimaisMaskqCom, MaskqCom);
    AIni.WriteString(FSessao, CChaveCasasDecimaisMaskvUnCom, MaskvUnCom);
    AIni.WriteInteger(FSessao, CChaveCasasDecimaisqCom, qCom);
    AIni.WriteInteger(FSessao, CChaveCasasDecimaisvUnCom, vUnCom);
  end;

  GravarIniChild(AIni);
end;

procedure TDFeReportConfig<T>.Apply(const DFeReport: T);
begin
  if not Assigned(DFeReport) or (DFeReport = nil) then Exit;

  DFeReport.PathPDF := FPathPDF;
  DFeReport.UsaSeparadorPathPDF := FUsaSeparadorPathPDF;
  DFeReport.Impressora := FImpressora;
  DFeReport.NomeDocumento := FNomeDocumento;
  DFeReport.MostraSetup := FMostraSetup;
  DFeReport.MostraPreview := FMostraPreview;
  DFeReport.MostraStatus := FMostraStatus;
  DFeReport.NumCopias := FNumCopias;
  DFeReport.Logo := FLogo;
  DFeReport.MargemInferior := FMargemInferior;
  DFeReport.MargemSuperior := FMargemSuperior;
  DFeReport.MargemEsquerda := FMargemEsquerda;
  DFeReport.MargemDireita := FMargemDireita;
  DFeReport.ExpandeLogoMarca := FExpandeLogoMarca;

  With DFeReport.CasasDecimais do
  begin
    Formato := FCasasDecimais.Formato;
    MaskqCom := FCasasDecimais.MaskqCom;
    MaskvUnCom := FCasasDecimais.MaskvUnCom;
    qCom := FCasasDecimais.qCom;
    vUnCom := FCasasDecimais.vUnCom;
  end;

  DFeReport.Sistema := pLib.Config.Sistema.Nome;
  DFeReport.Site := pLib.Config.Emissor.WebSite;
  DFeReport.Email := pLib.Config.Emissor.Email;
  DFeReport.Fax := pLib.Config.Emissor.Telefone;

  ApplyChild(DFeReport);
end;

end.

