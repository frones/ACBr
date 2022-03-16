{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
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

unit ACBrETQEscLabel;

interface

uses
  Classes,
  ACBrETQZplII, ACBrDevice
  {$IFDEF NEXTGEN}
   ,ACBrBase
  {$ENDIF};

type

  { TACBrETQEscLabel }

  TACBrETQEscLabel = class(TACBrETQZplII)
  private
  protected
    function ConverterPaginaDeCodigo(aPaginaDeCodigo: TACBrETQPaginaCodigo): String; override;
    function ComandoTemperatura: AnsiString; override;
    function ComandoResolucao: AnsiString; override;
    function ComandoVelocidade: AnsiString; override;
  public
    constructor Create(AOwner: TComponent);
    function ComandoCarregarImagem(aStream: TStream; var aNomeImagem: String;
      aFlipped: Boolean; aTipo: String): AnsiString; override;
    function ComandoGravaRFIDHexaDecimal(aValue:String): AnsiString; override;
    function ComandoGravaRFIDASCII( aValue:String ): AnsiString; override;
  end;

implementation

uses
  SysUtils,
  synautil,
  ACBrUtil, ACBrImage;

{ TACBrETQEscLabel }

constructor TACBrETQEscLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fpModeloStr := 'EscLabel';
  fpLimiteCopias := 99999999;
end;

function TACBrETQEscLabel.ComandoCarregarImagem(aStream: TStream;
  var aNomeImagem: String; aFlipped: Boolean; aTipo: String): AnsiString;
begin
  aNomeImagem := AjustarNomeArquivoImagem(aNomeImagem, aTipo);

  if (aTipo = 'PNG') then       // EscLabel suporta PNG colorido
  begin
    aStream.Position := 0;
    if not IsPNG(aStream) then
      raise Exception.Create(ACBrStr('Imagem não é PNG'));

    Result := '~DY'+                // Download Graphics command
              'R:' +                // File Location
              aNomeImagem + ',' +   // Filename
              'B,' +                // Format - B = uncompressed (binary),
              'P,' +                // Extension - P = store as compressed (.PNG),
              IntToStr(aStream.Size) + ',' + // Bytes total
              '0,' +                // Bytes per Row (BMP only)
              ReadStrFromStream(aStream, aStream.Size);

    Result := '^IDR:' + aNomeImagem + '^FS' +  // Apaga a imagem existente com o mesmo nome
              Result;
  end
  else
    Result := inherited ComandoCarregarImagem(aStream, aNomeImagem, aFlipped, aTipo)
end;

function TACBrETQEscLabel.ConverterPaginaDeCodigo(aPaginaDeCodigo: TACBrETQPaginaCodigo): String;
begin
  case aPaginaDeCodigo of
    pce1250: Result := '31';
  else
    Result := inherited ConverterPaginaDeCodigo(aPaginaDeCodigo);
  end;
end;

function TACBrETQEscLabel.ComandoTemperatura: AnsiString;
begin
  Result := EmptyStr;  // Não suportado em EscLabel (não usado)
end;

function TACBrETQEscLabel.ComandoResolucao: AnsiString;
var
  ResDPI: String;
begin
  case DPI of
    dpi300: ResDPI := '300';
    dpi600: ResDPI := '600';
  else
    ResDPI := '200';
  end;

  Result := '^S(CLR,R,'+ ResDPI + sLineBreak + // Sets the format base in dots per inch
            '^S(CLR,P,'+ ResDPI + sLineBreak + // Sets the print Resolution
            '^S(CLR,Z,'+ ResDPI;               // Sets Print Resolution of replaced printer
end;

function TACBrETQEscLabel.ComandoVelocidade: AnsiString;
begin
  if (Velocidade > 14) then
    raise Exception.Create('Velocidade deve ser de 1 a 14');

  if (Velocidade > 0) then
    Result := '^S(CMP,S,' + IntToStr(Velocidade)
  else
    Result := EmptyStr;
end;

function TACBrETQEscLabel.ComandoGravaRFIDHexaDecimal(aValue: String): AnsiString;
begin
  Result := EmptyStr;  // Não suportado em EscLabel (não usado)
end;

function TACBrETQEscLabel.ComandoGravaRFIDASCII(aValue: String): AnsiString;
begin
  Result := EmptyStr;  // Não suportado em EscLabel (não usado)
end;

end.

