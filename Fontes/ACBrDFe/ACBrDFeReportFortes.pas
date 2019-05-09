{******************************************************************************}
{ Projeto: Componente ACBrNFe                                                  }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Nota Fiscal}
{ eletrônica - NFe - http://www.nfe.fazenda.gov.br                             }
{ Direitos Autorais Reservados (c) 2008 Wemerson Souto                         }
{                                       Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }
{ Colaboradores nesse arquivo:                                                 }
{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }
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
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
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
  Classes, SysUtils, math,
  RLReport, RLPrinters, RLPDFFilter,
  ACBrUtil, ACBrDFeReport;

type
  TDFeReportFortes = class
  public
    class procedure AjustarReport(FReport: TRLReport; AConfig: TACBrDFeReport);
    class procedure AjustarMargem(FReport: TRLReport; AConfig: TACBrDFeReport);
    class procedure AjustarFiltroPDF(PDFFilter: TRLPDFFilter; AConfig: TACBrDFeReport; const AFile: String);
    class function CarregarLogo(ALogoImage: TRLImage; const ALogo: string): Boolean;
  end;


implementation

class procedure TDFeReportFortes.AjustarReport(FReport: TRLReport; AConfig: TACBrDFeReport);
begin
  FReport.ShowProgress := AConfig.MostraStatus;
  FReport.PrintDialog := (not (AConfig.MostraPreview)) and EstaVazio(AConfig.Impressora) or (AConfig.MostraSetup);

  if NaoEstaVazio(AConfig.Impressora) then
      RLPrinter.PrinterName := AConfig.Impressora;

  if RLPrinter.SupportsDuplex Then
     RLPrinter.Duplex := false;
	 
  if RLPrinter.Copies <> AConfig.NumCopias then
  begin
    RLPrinter.Copies := AConfig.NumCopias;
  end;
end;

class procedure TDFeReportFortes.AjustarMargem(FReport: TRLReport; AConfig: TACBrDFeReport);
begin
  // AJuste das Margens
  with FReport.Margins do
  begin
    TopMargin := AConfig.MargemSuperior * 10;
    BottomMargin := AConfig.MargemInferior * 10;
    LeftMargin := AConfig.MargemEsquerda * 10;
    RightMargin := AConfig.MargemDireita * 10;
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

end.
