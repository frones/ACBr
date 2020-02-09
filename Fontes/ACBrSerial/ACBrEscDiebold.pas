{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
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

{******************************************************************************
|* Historico
|*
|* 20/04/2013:  Daniel Simões de Almeida
|*   Inicio do desenvolvimento
******************************************************************************}

{$I ACBr.inc}

unit ACBrEscDiebold;

interface

uses
  Classes, SysUtils,
  ACBrPosPrinter, ACBrEscPosEpson
  {$IFDEF NEXTGEN}
   ,ACBrBase
  {$ENDIF};

type

  { TACBrEscDiebold }

  TACBrEscDiebold = class(TACBrEscPosEpson)
  private
  public
    constructor Create(AOwner: TACBrPosPrinter);

    function ComandoCodBarras(const ATag: String; const ACodigo: AnsiString): AnsiString;
      override;
    function ComandoQrCode(const ACodigo: AnsiString): AnsiString; override;
    function ComandoLogo: AnsiString; override;

    function LerInfo: String; override;
  end;


implementation

uses
  strutils, math,
  ACBrConsts, ACBrUtil;

{ TACBrEscDiebold }

constructor TACBrEscDiebold.Create(AOwner: TACBrPosPrinter);
begin
  inherited Create(AOwner);

  fpModeloStr := 'EscDiebold';

{(*}
  with Cmd  do
  begin
    DesligaExpandido  := ESC + '!' + #0;
    LigaItalico       := ESC + '4';
    DesligaItalico    := ESC + '5';
  end;
  {*)}

  TagsNaoSuportadas.Add( cTagBarraCode128c );
end;

function TACBrEscDiebold.ComandoCodBarras(const ATag: String;
  const ACodigo: AnsiString): AnsiString;
begin
  with fpPosPrinter.ConfigBarras do
  begin
    Result := ComandoCodBarrasEscPosNo128ABC(ATag, ACodigo, MostrarCodigo, Altura, LarguraLinha);
  end ;
end;

function TACBrEscDiebold.ComandoQrCode(const ACodigo: AnsiString): AnsiString;
begin
  with fpPosPrinter.ConfigQRCode do
  begin
    Result := ESC + '(k' + #3 + #0 + '1B' +
              ifthen(fpPosPrinter.Alinhamento = alEsquerda, '0', '1'); // 0 - A esquerda, 1 - Centralizar

    Result := Result +
              ESC + '(k' + #3 + #0 + '1C' + AnsiChr(LarguraModulo) +   // Largura Modulo
              ESC + '(k' + #3 + #0 + '1E' + IntToStr(ErrorLevel) + // Error Level
              ESC + '(k' + IntToLEStr(length(ACodigo)+3)+'1P0' + ACodigo +  // Codifica
              ESC + '(k' + #3 + #0 +'1Q0';  // Imprime
  end;
end;

function TACBrEscDiebold.ComandoLogo: AnsiString;
begin
  with fpPosPrinter.ConfigLogo do
  begin
    Result := GS + '09' + AnsiChr( min(StrToIntDef( chr(KeyCode1) + chr(KeyCode2), 0), 9));
  end;
end;

function TACBrEscDiebold.LerInfo: String;
var
  //Ret: AnsiString;
  Info: String;
  //B: Byte;

  Procedure AddInfo( Titulo: String; AInfo: AnsiString);
  begin
    Info := Info + Titulo+'='+AInfo + sLineBreak;
  end;

begin
  Info := '';

  AddInfo('Fabricante', 'Diebold');

  // Aparentemente, Diebold não tem comandos para retornar Informações sobre o equipamento
  //
  //Ret := fpPosPrinter.TxRx( GS + 'IA', 0, 500, True );
  //AddInfo('Firmware', Ret);
  //
  //Ret := fpPosPrinter.TxRx( GS + 'IC', 0, 500, True );
  //AddInfo('Modelo', Ret);
  //
  //Ret := fpPosPrinter.TxRx( GS + 'I1', 1, 500 );
  //B := Ord(Ret[1]);
  //Info := Info + 'Guilhotina='+IfThen(TestBit(B, 1),'1','0') + sLineBreak ;

  Result := Info;
end;

end.

