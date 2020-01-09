{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2004 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Rafael Dias                                                }
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

{******************************************************************************
|* Historico
|*
|* 19/01/2018: Rafael Dias/DSA
|*  - Criação do componente
******************************************************************************}

{$I ACBr.inc}

unit ACBrDFeReportFortes;

interface

uses
  Classes, SysUtils, math, Graphics,
  RLReport, RLPrinters, RLPDFFilter,
  ACBrUtil, ACBrDFeReport;

type
  TDFeReportFortes = class
  public
    class procedure AjustarReport(FReport: TRLReport; AConfig: TACBrDFeReport);
    class procedure AjustarMargem(FReport: TRLReport; AConfig: TACBrDFeReport);
    class procedure AjustarFiltroPDF(PDFFilter: TRLPDFFilter; AConfig: TACBrDFeReport; const AFile: String);
    class function CarregarLogo(ALogoImage: TRLImage; const ALogo: string): Boolean;
    class procedure DiminuirFonteSeNecessario(ARLMemo: TRLMemo; TamanhoMinimo: Integer);
    class function EspacejarTextoGrafico(const AText: String; AWidth: Integer;
      AFonte: TFont): String;
    class procedure AjustarLogo(ALogoImage: TRLImage; APropExpandeLogoMarca: TExpandeLogoMarcaConfig); static;
  end;


implementation

class procedure TDFeReportFortes.AjustarReport(FReport: TRLReport; AConfig: TACBrDFeReport);
begin
  FReport.ShowProgress := AConfig.MostraStatus;
  FReport.PrintDialog := AConfig.MostraSetup and (not AConfig.MostraPreview);

  if RLPrinter.PrinterName <> AConfig.Impressora then
    RLPrinter.PrinterName := AConfig.Impressora;

  if RLPrinter.SupportsDuplex Then
     RLPrinter.Duplex := false;

  if (AConfig.NumCopias > 0) and (RLPrinter.Copies <> AConfig.NumCopias) then
    RLPrinter.Copies := AConfig.NumCopias;
end;

class procedure TDFeReportFortes.AjustarMargem(FReport: TRLReport; AConfig: TACBrDFeReport);
begin
  // AJuste das Margens
  with FReport.Margins do
  begin
    TopMargin    := AConfig.MargemSuperior;
    BottomMargin := AConfig.MargemInferior;
    LeftMargin   := AConfig.MargemEsquerda;
    RightMargin  := AConfig.MargemDireita;
  end;
end;

class procedure TDFeReportFortes.AjustarFiltroPDF(PDFFilter: TRLPDFFilter; AConfig: TACBrDFeReport; const AFile: String);
Var
  ADir: String;
  NomeArquivoFinal: String;
begin
  NomeArquivoFinal := Trim(AFile);
  if EstaVazio(NomeArquivoFinal) then
    raise Exception.Create('Erro ao gerar PDF. Arquivo não informado');

  ADir := ExtractFilePath(NomeArquivoFinal);
  if EstaVazio(ADir) then
    NomeArquivoFinal := ApplicationPath + ExtractFileName(NomeArquivoFinal)
  else
  begin
    if not ForceDirectories(ADir) then
      raise Exception.Create('Erro ao gerar PDF. Diretório: ' + ADir + ' não pode ser criado');
  end;

  PDFFilter.ShowProgress := AConfig.MostraStatus;
  PDFFilter.FileName := NomeArquivoFinal;
end;

class function TDFeReportFortes.CarregarLogo(ALogoImage: TRLImage; const ALogo: string): Boolean;
var
  LogoStream: TStringStream;
begin
  Result := False;
  ALogoImage.Picture := nil;
  if EstaVazio(Trim(ALogo)) then
    Exit;

  if FileExists(ALogo) then
  begin
    ALogoImage.Picture.LoadFromFile(ALogo);
    Result := True;
  end
  else
  begin
    LogoStream := TStringStream.Create(ALogo);
    try
      try
        ALogoImage.Picture.Bitmap.LoadFromStream(LogoStream);
        Result := True;
      except
        ALogoImage.Picture := nil;
      end;
    finally
      LogoStream.Free;
    end;
  end;
end;

class procedure TDFeReportFortes.DiminuirFonteSeNecessario(ARLMemo: TRLMemo; TamanhoMinimo: Integer);
var
  ABmp: TBitmap;
begin
  ABmp := TBitmap.Create;
  try
    ABmp.Canvas.Font.Assign(ARLMemo.Font);
    TamanhoMinimo := max(1, TamanhoMinimo);

    while ABmp.Canvas.Font.Size > TamanhoMinimo do
    begin
      if ABmp.Canvas.TextWidth( ARLMemo.Lines.Text ) <= ARLMemo.ClientWidth then
        Break;

      ABmp.Canvas.Font.Size := ABmp.Canvas.Font.Size - 1;
    end;
  finally
    ARLMemo.Font.Size := ABmp.Canvas.Font.Size;
    ABmp.Free;
  end;
end;

class function TDFeReportFortes.EspacejarTextoGrafico(const AText: String; AWidth: Integer; AFonte: TFont): String;
var
  ABMP: TBitmap;
  LenText, TextWidth: Integer;
  TextSpaced: String;
begin
  ABMP := TBitmap.Create;
  try
    ABMP.Canvas.Font.Assign(AFonte);
    LenText := Length(AText);
    TextWidth := 0;
    while (TextWidth < AWidth) do
    begin
      Inc(LenText);
      TextSpaced := ACBrStr(PadSpace(AText, LenText, '|'));
      TextWidth := ABMP.Canvas.TextWidth(TextSpaced);
    end;

    Result := ACBrStr(PadSpace(AText, LenText-1, '|'));
  finally
    ABMP.Free;
  end;
end;

class procedure TDFeReportFortes.AjustarLogo(ALogoImage: TRLImage; APropExpandeLogoMarca: TExpandeLogoMarcaConfig);
begin
  ALogoImage.Scaled  := APropExpandeLogoMarca.Dimensionar;
  ALogoImage.Stretch := APropExpandeLogoMarca.Esticar;

  // Se não for configurado, permanece o padrão.
  if ( APropExpandeLogoMarca.Altura > 0 ) or ( APropExpandeLogoMarca.Esquerda > 0 ) or
     ( APropExpandeLogoMarca.Topo > 0 ) or ( APropExpandeLogoMarca.Largura > 0 ) then
  begin
    ALogoImage.Top    := APropExpandeLogoMarca.Topo;
    ALogoImage.Left   := APropExpandeLogoMarca.Esquerda;
    ALogoImage.Height := APropExpandeLogoMarca.Altura;
    ALogoImage.Width  := APropExpandeLogoMarca.Largura;
  end;
end;

end.
