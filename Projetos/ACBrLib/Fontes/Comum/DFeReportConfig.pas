{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Rafael Teno Dias                                }
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
    FAlterarEscalaPadrao: Boolean;
    FNovaEscala: Integer;
    FExpandeLogoMarca: Boolean;
    FExpandeLogoMarcaConfig: TExpandeLogoMarcaConfig;
    FNomeDocumento: String;
    FSessao: String;

    procedure SetNumCopias(const Value: Integer);

 protected
    procedure DefinirValoresPadroesChild; virtual; abstract;
    procedure LerIniChild(const AIni: TCustomIniFile); virtual; abstract;
    procedure GravarIniChild(const AIni: TCustomIniFile); virtual; abstract;
    procedure ApplyChild(const DFeReport: T; const Lib: TACBrLib); virtual; abstract;

 public
   constructor Create(ASessao: String);
   destructor Destroy; override;

   procedure DefinirValoresPadroes;
   procedure LerIni(const AIni: TCustomIniFile);
   procedure GravarIni(const AIni: TCustomIniFile);
   procedure Apply(const DFeReport: T; const Lib: TACBrLib);

   property Sessao: String read FSessao;
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
   property AlterarEscalaPadrao: Boolean read FAlterarEscalaPadrao write FAlterarEscalaPadrao;
   property NovaEscala: Integer read FNovaEscala write FNovaEscala;
   property ExpandeLogoMarca: Boolean read FExpandeLogoMarca write FExpandeLogoMarca;
   property ExpandeLogoMarcaConfig: TExpandeLogoMarcaConfig read FExpandeLogoMarcaConfig;
   property CasasDecimais: TCasasDecimais read FCasasDecimais;
 end;

implementation

constructor TDFeReportConfig<T>.Create(ASessao: String);
begin
  FSessao := ASessao;
  DefinirValoresPadroes;
end;

destructor TDFeReportConfig<T>.Destroy;
begin
  FCasasDecimais.Free;
  FExpandeLogoMarcaConfig.Free;

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
  FAlterarEscalaPadrao := False;
  FNovaEscala := 96;
  ExpandeLogoMarca := False;

  if Assigned(FExpandeLogoMarcaConfig) then FreeAndNil(FExpandeLogoMarcaConfig);
  FExpandeLogoMarcaConfig := TExpandeLogoMarcaConfig.Create(nil);

  if Assigned(FCasasDecimais) then FreeAndNil(FCasasDecimais);
  FCasasDecimais := TCasasDecimais.Create(nil);

  DefinirValoresPadroesChild;
end;

procedure TDFeReportConfig<T>.LerIni(const AIni: TCustomIniFile);
begin
  FPathPDF := AIni.ReadString(Sessao, CChavePathPDF, FPathPDF);
  FUsaSeparadorPathPDF := AIni.ReadBool(Sessao, CChaveUsaSeparadorPathPDF, FUsaSeparadorPathPDF);
  FImpressora := AIni.ReadString(Sessao, CChaveImpressora, FImpressora);
  FNomeDocumento := AIni.ReadString(Sessao, CChaveNomeDocumento, FNomeDocumento);
  FMostraSetup := AIni.ReadBool(Sessao, CChaveMostraSetup, FMostraSetup);
  FMostraPreview := AIni.ReadBool(Sessao, CChaveMostraPreview, FMostraPreview);
  FMostraStatus := AIni.ReadBool(Sessao, CChaveMostraStatus, FMostraStatus);
  FNumCopias := AIni.ReadInteger(Sessao, CChaveCopias, FNumCopias);
  FLogo := AIni.ReadString(Sessao, CChavePathLogo, FLogo);
  FMargemInferior := AIni.ReadFloat(Sessao, CChaveMargemInferior, FMargemInferior);
  FMargemSuperior := AIni.ReadFloat(Sessao, CChaveMargemSuperior, FMargemSuperior);
  FMargemEsquerda := AIni.ReadFloat(Sessao, CChaveMargemEsquerda, FMargemEsquerda);
  FMargemDireita := AIni.ReadFloat(Sessao, CChaveMargemDireita, FMargemDireita);
  FAlterarEscalaPadrao := AIni.ReadBool(Sessao, CChaveAlterarEscalaPadrao, FAlterarEscalaPadrao);
  FNovaEscala := AIni.ReadInteger(Sessao, CChaveNovaEscala, FNovaEscala);
  FExpandeLogoMarca := AIni.ReadBool(Sessao, CChaveExpandeLogoMarca, FExpandeLogoMarca);

  with FExpandeLogoMarcaConfig do
  begin
    Altura := AIni.ReadInteger(Sessao, CChaveExpandeLogoMarcaAltura, Altura);
    Esquerda := AIni.ReadInteger(Sessao, CChaveExpandeLogoMarcaEsquerda, Esquerda);
    Topo := AIni.ReadInteger(Sessao, CChaveExpandeLogoMarcaTopo, Topo);
    Largura := AIni.ReadInteger(Sessao, CChaveExpandeLogoMarcaLargura, Largura);
    Dimensionar := AIni.ReadBool(Sessao, CChaveExpandeLogoMarcaDimensionar, Dimensionar);
    Esticar := AIni.ReadBool(Sessao, CChaveExpandeLogoMarcaEsticar, Esticar);
  end;

  with FCasasDecimais do
  begin
    Formato := TDetFormato(AIni.ReadInteger(Sessao, CChaveCasasDecimaisFormato, Integer(Formato)));
    MaskqCom := AIni.ReadString(Sessao, CChaveCasasDecimaisMaskqCom, MaskqCom);
    MaskvUnCom := AIni.ReadString(Sessao, CChaveCasasDecimaisMaskvUnCom, MaskvUnCom);
    qCom := AIni.ReadInteger(Sessao, CChaveCasasDecimaisqCom, qCom);
    vUnCom := AIni.ReadInteger(Sessao, CChaveCasasDecimaisvUnCom, vUnCom);
    MaskAliquota := AIni.ReadString(Sessao, CChaveCasasDecimaisMaskAliquota, MaskAliquota);
    Aliquota := AIni.ReadInteger(Sessao, CChaveCasasDecimaisAliquota, Aliquota);
  end;

  LerIniChild(AIni);
end;

procedure TDFeReportConfig<T>.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteString(Sessao, CChavePathPDF, FPathPDF);
  AIni.WriteBool(Sessao, CChaveUsaSeparadorPathPDF, FUsaSeparadorPathPDF);
  AIni.WriteString(Sessao, CChaveImpressora, FImpressora);
  AIni.WriteString(Sessao, CChaveNomeDocumento, FNomeDocumento);
  AIni.WriteBool(Sessao, CChaveMostraSetup, FMostraSetup);
  AIni.WriteBool(Sessao, CChaveMostraPreview, FMostraPreview);
  AIni.WriteBool(Sessao, CChaveMostraStatus, FMostraStatus);
  AIni.WriteInteger(Sessao, CChaveCopias, FNumCopias);
  AIni.WriteString(Sessao, CChavePathLogo, FLogo);
  AIni.WriteFloat(Sessao, CChaveMargemInferior, FMargemInferior);
  AIni.WriteFloat(Sessao, CChaveMargemSuperior, FMargemSuperior);
  AIni.WriteFloat(Sessao, CChaveMargemEsquerda, FMargemEsquerda);
  AIni.WriteFloat(Sessao, CChaveMargemDireita, FMargemDireita);
  AIni.WriteBool(Sessao, CChaveAlterarEscalaPadrao, FAlterarEscalaPadrao);
  AIni.WriteInteger(Sessao, CChaveNovaEscala, FNovaEscala);
  AIni.WriteBool(Sessao, CChaveExpandeLogoMarca, FExpandeLogoMarca);

  with FExpandeLogoMarcaConfig do
  begin
    AIni.WriteInteger(Sessao, CChaveExpandeLogoMarcaAltura, Altura);
    AIni.WriteInteger(Sessao, CChaveExpandeLogoMarcaEsquerda, Esquerda);
    AIni.WriteInteger(Sessao, CChaveExpandeLogoMarcaTopo, Topo);
    AIni.WriteInteger(Sessao, CChaveExpandeLogoMarcaLargura, Largura);
    AIni.WriteBool(Sessao, CChaveExpandeLogoMarcaDimensionar, Dimensionar);
    AIni.WriteBool(Sessao, CChaveExpandeLogoMarcaEsticar, Esticar);
  end;

  with FCasasDecimais do
  begin
    AIni.WriteInteger(Sessao, CChaveCasasDecimaisFormato, Integer(Formato));
    AIni.WriteString(Sessao, CChaveCasasDecimaisMaskqCom, MaskqCom);
    AIni.WriteString(Sessao, CChaveCasasDecimaisMaskvUnCom, MaskvUnCom);
    AIni.WriteInteger(Sessao, CChaveCasasDecimaisqCom, qCom);
    AIni.WriteInteger(Sessao, CChaveCasasDecimaisvUnCom, vUnCom);
    AIni.WriteString(Sessao, CChaveCasasDecimaisMaskAliquota, MaskAliquota);
    AIni.WriteInteger(Sessao, CChaveCasasDecimaisAliquota, Aliquota);
  end;

  GravarIniChild(AIni);
end;

procedure TDFeReportConfig<T>.Apply(const DFeReport: T; const Lib: TACBrLib);
var
  IntAliquota: Integer;
  StrMaskAliquota: string;
begin
  if not Assigned(DFeReport) then Exit;

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
  DFeReport.AlterarEscalaPadrao := FAlterarEscalaPadrao;
  DFeReport.NovaEscala := FNovaEscala;
  DFeReport.ExpandeLogoMarca := FExpandeLogoMarca;

  With DFeReport.CasasDecimais do
  begin
    Formato := FCasasDecimais.Formato;
    MaskqCom := FCasasDecimais.MaskqCom;
    MaskvUnCom := FCasasDecimais.MaskvUnCom;
    qCom := FCasasDecimais.qCom;
    vUnCom := FCasasDecimais.vUnCom;
    MaskAliquota := FCasasDecimais.MaskAliquota;
    Aliquota := FCasasDecimais.Aliquota;
  end;
  StrMaskAliquota := DFeReport.CasasDecimais.MaskAliquota;
  IntAliquota := DFeReport.CasasDecimais.Aliquota;

{$IFDEF Demo}
  DFeReport.Sistema := Lib.Nome + ' v' + Lib.Versao;
  DFeReport.Email := 'sac@projetoacbr.com.br';
  DFeReport.Site := 'www.projetoacbr.com.br';

{$ELSE}
  DFeReport.Sistema := Lib.Config.Sistema.Nome;
  DFeReport.Site := Lib.Config.Emissor.WebSite;
  DFeReport.Email := Lib.Config.Emissor.Email;

{$ENDIF}

  ApplyChild(DFeReport, Lib);
end;

end.

