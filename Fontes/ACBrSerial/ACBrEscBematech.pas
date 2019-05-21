{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }

{ Direitos Autorais Reservados (c) 2004 Daniel Simoes de Almeida               }

{ Colaboradores nesse arquivo:                                                 }

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
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }

{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 20/04/2013:  Daniel Simões de Almeida
|*   Inicio do desenvolvimento
******************************************************************************}

{$I ACBr.inc}

unit ACBrEscBematech;

interface

uses
  Classes, SysUtils,
  ACBrPosPrinter, ACBrConsts;

const
  ModoEscBema = GS + #249 + #32 + #0;

type

  { TACBrEscBematech }

  TACBrEscBematech = class(TACBrPosPrinterClass)
  private
    procedure VerificarKeyCodes;
  public
    constructor Create(AOwner: TACBrPosPrinter);

    function ComandoCodBarras(const ATag: String; const ACodigo: AnsiString): AnsiString;
      override;
    function ComandoQrCode(const ACodigo: AnsiString): AnsiString; override;
    function ComandoPaginaCodigo(APagCodigo: TACBrPosPaginaCodigo): AnsiString;
      override;
    function ComandoImprimirImagemRasterStr(const RasterStr: AnsiString;
      AWidth: Integer; AHeight: Integer): AnsiString; override;
    function ComandoLogo: AnsiString; override;
    function ComandoGravarLogoRasterStr(const RasterStr: AnsiString; AWidth: Integer;
      AHeight: Integer): AnsiString; override;
    function ComandoApagarLogo: AnsiString; override;
    function ComandoGaveta(NumGaveta: Integer = 1): AnsiString; override;
    function ComandoInicializa: AnsiString; override;

    procedure LerStatus(var AStatus: TACBrPosPrinterStatus); override;
    function LerInfo: String; override;
  end;


implementation

Uses
  strutils, math,
  ACBrUtil, ACBrEscPosEpson;


{ TACBrEscBematech }

constructor TACBrEscBematech.Create(AOwner: TACBrPosPrinter);
begin
  inherited Create(AOwner);

  fpModeloStr := 'EscBematech';

{(*}
  with Cmd  do
  begin
    Zera                    := ESC + '@';
    PuloDeLinha             := LF;
    EspacoEntreLinhasPadrao := ESC + '2';
    EspacoEntreLinhas       := ESC + '3';
    LigaNegrito             := ESC + 'E';
    DesligaNegrito          := ESC + 'F';
    LigaExpandido           := ESC + 'W' + #1;
    DesligaExpandido        := ESC + 'W' + #0;
    LigaAlturaDupla         := ESC + 'd' + #1;
    DesligaAlturaDupla      := ESC + 'd' + #0;
    LigaSublinhado          := ESC + '-' + #1;
    DesligaSublinhado       := ESC + '-' + #0;
    LigaInvertido           := '';  // Modo EscBema não suporta
    DesligaInvertido        := '';  // Modo EscBema não suporta
    LigaItalico             := ESC + '4';
    DesligaItalico          := ESC + '5';
    LigaCondensado          := ESC + SI;
    DesligaCondensado       := ESC + 'H';
    AlinhadoEsquerda        := ESC + 'a' + #0;
    AlinhadoCentro          := ESC + 'a' + #1;
    AlinhadoDireita         := ESC + 'a' + #2;
    CorteTotal              := ESC + 'w';
    CorteParcial            := ESC + 'm';
    FonteNormal             := ESC + '!' + #0 + DesligaCondensado + DesligaItalico;
    FonteA                  := DesligaCondensado;
    FonteB                  := LigaCondensado;
    Beep                    := ESC + '(A' + #4 + #0 + #1 + #2 + #1 + #0;
  end;
  {*)}

  TagsNaoSuportadas.Add( cTagBarraMSI );
  TagsNaoSuportadas.Add( cTagBarraCode128c );
  TagsNaoSuportadas.Add( cTagModoPaginaLiga );
end;

function TACBrEscBematech.ComandoCodBarras(const ATag: String;
  const ACodigo: AnsiString): AnsiString;
begin
  with fpPosPrinter.ConfigBarras do
  begin
    Result := ComandoCodBarrasEscPosNo128ABC(ATag, ACodigo, MostrarCodigo, Altura, LarguraLinha);
  end ;
end;

function TACBrEscBematech.ComandoQrCode(const ACodigo: AnsiString): AnsiString;
begin
  with fpPosPrinter.ConfigQRCode do
  begin
    Result := GS  + 'kQ' + // Codigo QRCode
              AnsiChr(ErrorLevel) +         // N1 Error correction level 0 - L, 1 - M, 2 - Q, 3 - H
              AnsiChr(LarguraModulo * 2) +  // N2 - MSB; 0 = default = 4
              AnsiChr(0) +                  // N3 - Precisa computar Version QRCode ???
              AnsiChr(1) +                  // N4, Encoding modes: 0 – Numeric only, 1 – Alphanumeric, 2 – Binary (8 bits), 3 – Kanji,
              IntToLEStr( Length(ACodigo) ) +  // N5 e N6
              ACodigo;
  end;
end;

function TACBrEscBematech.ComandoPaginaCodigo(APagCodigo: TACBrPosPaginaCodigo
  ): AnsiString;
var
  CmdPag: Integer;
begin
  case APagCodigo of
    pc437: CmdPag := 3;
    pc850: CmdPag := 2;
    pc860: CmdPag := 4;
    pcUTF8: CmdPag := 8;
  else
    begin
      Result := '';
      Exit;
    end;
  end;

  Result := ESC + 't' + AnsiChr( CmdPag );
end;

procedure TACBrEscBematech.VerificarKeyCodes;
begin
  with fpPosPrinter.ConfigLogo do
  begin
    if (KeyCode1 <> 1) or (KeyCode2 <> 0) then
      raise EPosPrinterException.Create('Bematech apenas aceitas KeyCode1=1, KeyCode2=0');
  end;
end;

function TACBrEscBematech.ComandoImprimirImagemRasterStr(
  const RasterStr: AnsiString; AWidth: Integer; AHeight: Integer): AnsiString;
begin
  Result := ComandoImprimirImagemColumnStr(fpPosPrinter, RasterStr, AWidth, AHeight)
end;

function TACBrEscBematech.ComandoLogo: AnsiString;
var
  m, KeyCode: Integer;
begin
  with fpPosPrinter.ConfigLogo do
  begin
    if (KeyCode2 = 0) then
    begin
      if (KeyCode1 >= 48) and (KeyCode1 <= 57) then  // '0'..'9'
        KeyCode := StrToInt( chr(KeyCode1) )
      else
        KeyCode := KeyCode1 ;
    end
    else
      KeyCode := StrToIntDef( chr(KeyCode1) + chr(KeyCode2), 1);

    m := 0;
    if FatorX > 1 then
      m := m + 1;
    if Fatory > 1 then
      m := m + 2;

    Result := FS + 'p' + AnsiChr(KeyCode) + AnsiChr(m);
  end;
end;

function TACBrEscBematech.ComandoGravarLogoRasterStr(
  const RasterStr: AnsiString; AWidth: Integer; AHeight: Integer): AnsiString;
var
  KeyCode: Byte;
begin
  with fpPosPrinter.ConfigLogo do
  begin
    KeyCode := AjustarKeyCodeUnico(KeyCode1);
    VerificarKeyCodes;

    Result := ComandoGravarLogoColumnStr(RasterStr, AWidth, AHeight, KeyCode);
  end;
end;

function TACBrEscBematech.ComandoApagarLogo: AnsiString;
var
  KeyCode: Byte;
begin
  with fpPosPrinter.ConfigLogo do
  begin
    KeyCode := AjustarKeyCodeUnico(KeyCode1);
    VerificarKeyCodes;

    KeyCode := AjustarKeyCodeUnico(KeyCode1);
    Result := ComandoGravarLogoColumnStr(#0, 1, 1, KeyCode);
  end;
end;

function TACBrEscBematech.ComandoGaveta(NumGaveta: Integer): AnsiString;
var
  Tempo: Integer;
begin
  with fpPosPrinter.ConfigGaveta do
  begin
    Tempo := max(TempoON, TempoOFF);

    if NumGaveta > 1 then
      Result := ESC + #128 + AnsiChr( Tempo )
    else
      Result := ESC + 'v' + AnsiChr( Tempo )
  end;
end;

function TACBrEscBematech.ComandoInicializa: AnsiString;
begin
  Result := inherited ComandoInicializa ;
  Result := ModoEscBema + Result;
end;

procedure TACBrEscBematech.LerStatus(var AStatus: TACBrPosPrinterStatus);
var
  B: Byte;
  Ret: AnsiString;
begin
  try
    Ret := fpPosPrinter.TxRx( GS + #248 + '1', 5, 500 );
    B := Ord(Ret[1]);

    if TestBit(B, 2) then
      AStatus := AStatus + [stImprimindo];  // Overrun
    if TestBit(B, 3) then
      AStatus := AStatus + [stOffLine];
    if TestBit(B, 4) then
      AStatus := AStatus + [stImprimindo];

    B := Ord(Ret[2]);
    if TestBit(B, 1) then
      AStatus := AStatus + [stPoucoPapel];
    if TestBit(B, 2) then
      AStatus := AStatus + [stSemPapel];
    if not TestBit(B, 4) then
      AStatus := AStatus + [stGavetaAberta];
    if TestBit(B, 5) then
      AStatus := AStatus + [stSemPapel];
    if TestBit(B, 6) then
      AStatus := AStatus + [stErro];
    if not TestBit(B, 7) then
      AStatus := AStatus + [stTampaAberta];
  except
    AStatus := AStatus + [stErroLeitura];
  end;
end;

function TACBrEscBematech.LerInfo: String;
var
  Ret: AnsiString;
  InfoCmd, Info: String;
  B: Byte;

  Procedure AddInfo( Titulo: String; AInfo: AnsiString);
  begin
    Info := Info + Titulo+'='+AInfo + sLineBreak;
  end;

begin
  Info := '';

  InfoCmd := GS + #249 + #39;

  Ret := fpPosPrinter.TxRx( InfoCmd + #0, 10, 500 );
  AddInfo('Modelo', Ret);

  Ret := fpPosPrinter.TxRx( InfoCmd + #1, 0, 500 );
  AddInfo('Serial', Ret);

  Ret := fpPosPrinter.TxRx( InfoCmd + #3, 3, 500 );
  AddInfo('Firmware', Ret);

  Ret := fpPosPrinter.TxRx( GS + #248 + '1', 5, 500 );
  B := Ord(Ret[3]);
  Info := Info + 'Guilhotina='+IfThen(TestBit(B, 2),'0','1') + sLineBreak ;

  Result := Info;
end;

end.

