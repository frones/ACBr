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

unit ACBrEscPosEpson;

interface

uses
  Classes, SysUtils, ACBrPosPrinter;

type

  { TACBrEscPosEpson }

  TACBrEscPosEpson = class(TACBrPosPrinterClass)
  private
  public
    constructor Create(AOwner: TACBrPosPrinter);

    function ComandoCodBarras(const ATag: String; ACodigo: AnsiString): AnsiString;
      override;
    function ComandoQrCode(ACodigo: AnsiString): AnsiString; override;
    function ComandoEspacoEntreLinhas(Espacos: Byte): AnsiString; override;
    function ComandoPaginaCodigo(APagCodigo: TACBrPosPaginaCodigo): AnsiString;
      override;
    function ComandoLogo: AnsiString; override;
    function ComandoGaveta(NumGaveta: Integer = 1): AnsiString; override;

    procedure LerStatus(var AStatus: TACBrPosPrinterStatus); override;
    function LerInfo: String; override;
  end;


implementation

uses
  strutils, math,
  ACBrConsts, ACBrUtil;

{ TACBrEscPosEpson }

constructor TACBrEscPosEpson.Create(AOwner: TACBrPosPrinter);
begin
  inherited Create(AOwner);

  fpModeloStr := 'EscPosEpson';

{(*}
  with Cmd  do
  begin
    Zera                    := ESC + '@';
    EspacoEntreLinhasPadrao := ESC + '2';
    EspacoEntreLinhas       := ESC + '3';
    FonteNormal             := ESC + '!' + #0;
    FonteA                  := ESC + 'M' + #0;
    FonteB                  := ESC + 'M' + #1;
    LigaNegrito             := ESC + 'E' + #1;
    DesligaNegrito          := ESC + 'E' + #0;
    LigaExpandido           := GS  + '!' + #16;
    DesligaExpandido        := GS  + '!' + #0;
    LigaSublinhado          := ESC + '-' + #1;
    DesligaSublinhado       := ESC + '-' + #0;
    LigaInvertido           := GS  + 'B' + #1;
    DesligaInvertido        := GS  + 'B' + #0;
    LigaItalico             := '';        // Não existe ?
    DesligaItalico          := '';        // Não existe ?
    LigaCondensado          := FonteB;
    DesligaCondensado       := FonteA;
    AlinhadoEsquerda        := ESC + 'a' + #0;
    AlinhadoCentro          := ESC + 'a' + #1;
    AlinhadoDireita         := ESC + 'a' + #2;
    CorteTotal              := GS  + 'V' + #0;
    CorteParcial            := GS  + 'V' + #1;
    Beep                    := ESC + '(A' + #4 + #0 + #48 + #55 + #03 + #10;
  end;
  {*)}

  TagsNaoSuportadas.Add( cTagBarraMSI );
end;

function TACBrEscPosEpson.ComandoCodBarras(const ATag: String;
  ACodigo: AnsiString): AnsiString;
var
  L, A, M : Integer ;
  CmdBarCode: Char;
  ACodBar, Cmd128, Code128c: AnsiString;
  i, s: Integer;
begin
  if ATag = cTagBarraUPCA then
    CmdBarCode := 'A'
  else if ATag = cTagBarraUPCE then
    CmdBarCode := 'B'
  else if ATag = cTagBarraEAN13 then
    CmdBarCode := 'C'
  else if ATag = cTagBarraEAN8 then
    CmdBarCode := 'D'
  else if ATag = cTagBarraCode39 then
    CmdBarCode := 'E'
  else if ATag = cTagBarraInter then
    CmdBarCode := 'F'
  else if ATag = cTagBarraCodaBar then
    CmdBarCode := 'G'
  else if ATag = cTagBarraCode93 then
    CmdBarCode := 'H'
  else if (ATag = cTagBarraCode128) or (ATag = cTagBarraCode128b) then
  begin
    CmdBarCode := 'I';
    Cmd128 := '{B';
  end
  else if ATag = cTagBarraCode128a then
  begin
    CmdBarCode := 'I';
    Cmd128 := '{A';
  end
  else if ATag = cTagBarraCode128c then
  begin
    CmdBarCode := 'I';
    Cmd128 := '{C';

    // Apenas números,
    ACodigo := AnsiString(OnlyNumber(String(ACodigo)));

    s := Length(ACodigo);
    if s mod 2 <> 0 then  // Tamanho deve ser Par
    begin
      ACodigo := '0'+ACodigo;
      Inc(s);
    end;

    Code128c := '';
    i := 1;
    while i < s do
    begin
      Code128c := Code128c + AnsiChr(StrToInt(copy(String(ACodigo),i,2)));
      i := i + 2;
    end;

    ACodigo := Code128c;
  end
  else if ATag = cTagBarraMSI then     // Apenas Bematech suporta
    CmdBarCode := 'R'
  else
  begin
    Result := ACodigo;
    Exit;
  end;

  ACodBar := ACodigo;

  if CmdBarCode = 'I' then // Cod128
  begin
    if Copy(String(ACodBar),1,1) <> '{' then
      ACodBar := Cmd128 + ACodBar;
  end;

  with fpPosPrinter.ConfigBarras do
  begin
    L := IfThen( LarguraLinha = 0, 2, max(min(LarguraLinha,4),1) );
    A := IfThen( Altura = 0, 50, max(min(Altura,255),1) );
    M := IfThen( MostrarCodigo, 2, 0 );
  end ;

  Result := GS + 'w' + AnsiChr( L ) + // Largura
            GS + 'h' + AnsiChr( A ) + // Altura
            GS + 'H' + AnsiChr( M ) + // HRI (numero impresso abaixo do cod.barras)
            GS + 'k' + CmdBarCode + AnsiChr( Length(ACodBar) ) + ACodBar;
end;

function TACBrEscPosEpson.ComandoQrCode(ACodigo: AnsiString): AnsiString;
begin
  with fpPosPrinter.ConfigQRCode do
  begin
     Result := GS + '(k' + #4 + #0 + '1A' + IntToStr(Tipo) + #0 +  // Tipo
               GS + '(k' + #3 + #0 + '1C' + AnsiChr(LarguraModulo) +   // Largura Modulo
               GS + '(k' + #3 + #0 + '1E' + IntToStr(ErrorLevel) + // Error Level
               GS + '(k' + IntToLEStr(length(ACodigo)+3)+'1P0' + ACodigo +  // Codifica
               GS + '(k' + #3 + #0 +'1Q0';  // Imprime
  end;
end;

function TACBrEscPosEpson.ComandoEspacoEntreLinhas(Espacos: Byte): AnsiString;
begin
  if Espacos = 0 then
    Result := Cmd.EspacoEntreLinhasPadrao
  else
    Result := Cmd.EspacoEntreLinhas + AnsiChr(Espacos);
end;

function TACBrEscPosEpson.ComandoPaginaCodigo(APagCodigo: TACBrPosPaginaCodigo
  ): AnsiString;
var
  CmdPag: Integer;
begin
  case APagCodigo of
    pc437: CmdPag := 0;
    pc850: CmdPag := 2;
    pc852: CmdPag := 18;
    pc860: CmdPag := 3;
    pc1252: CmdPag := 16;
  else
    begin
      Result := '';
      Exit;
    end;
  end;

  Result := ESC + 't' + AnsiChr( CmdPag );
end;

function TACBrEscPosEpson.ComandoLogo: AnsiString;
var
  m, KeyCode: Integer;
begin
  with fpPosPrinter.ConfigLogo do
  begin
    {
      Verificando se informou o KeyCode compatível com o comando Novo ou Antigo.

      Nota: O Comando novo da Epson "GS + '(L'", não é compatível em alguns
      Equipamentos (não Epson), mas que usam EscPosEpson...
      Nesse caso, vamos usar o comando "FS + 'p'", para tal, informe:
      KeyCode1 := 1..255; KeyCode2 := 0
    }

    if (KeyCode2 = 0) then
    begin
      if (KeyCode1 >= 48) and (KeyCode1 <= 57) then  // '0'..'9'
        KeyCode := StrToInt( chr(KeyCode1) )
      else
        KeyCode := KeyCode1 ;

      m := 0;
      if FatorX > 1 then
        m := m + 1;
      if Fatory > 1 then
        m := m + 2;

      Result := FS + 'p' + AnsiChr(KeyCode) + AnsiChr(m);
    end
    else
    begin
      Result := GS + '(L' + #6 + #0 + #48 + #69 +
                AnsiChr(KeyCode1) + AnsiChr(KeyCode2) +
                AnsiChr(FatorX)   + AnsiChr(FatorY);
    end;
  end;
end;

function TACBrEscPosEpson.ComandoGaveta(NumGaveta: Integer): AnsiString;
var
  CharGav: AnsiChar;
begin
  if NumGaveta > 1 then
    CharGav := #1
  else
    CharGav := #0;

  with fpPosPrinter.ConfigGaveta do
  begin
    Result := ESC + 'p' + CharGav + AnsiChar(TempoON) + AnsiChar(TempoOFF);
  end;
end;

procedure TACBrEscPosEpson.LerStatus(var AStatus: TACBrPosPrinterStatus);
var
  B: Byte;
  Falhas: Integer;
begin
  if not (fpPosPrinter.Device.IsSerialPort or fpPosPrinter.Device.IsTCPPort) then
    exit;

  Falhas := 0;
  while Falhas < 5 do
  begin
    try
      fpPosPrinter.Ativo := True;

      B := Ord(fpPosPrinter.TxRx( DLE + EOT + #1 )[1]);
      if not TestBit(B, 2) then
        AStatus := AStatus + [stGavetaAberta];
      if TestBit(B, 3) then
        AStatus := AStatus + [stOffLine];
      if TestBit(B, 5) then
        AStatus := AStatus + [stErro];  // Waiting for online recovery
      if TestBit(B, 6) then
        AStatus := AStatus + [stImprimindo]; // Paper is being fed by the paper feed button

      B := Ord(fpPosPrinter.TxRx( DLE + EOT + #2 )[1]);
      if TestBit(B, 2) then
        AStatus := AStatus + [stTampaAberta];
      if TestBit(B, 3) then
        AStatus := AStatus + [stImprimindo]; // Paper is being fed by the paper feed button
      if TestBit(B, 5) then
        AStatus := AStatus + [stSemPapel];
      if TestBit(B, 6) then
        AStatus := AStatus + [stErro];

      B := Ord(fpPosPrinter.TxRx( DLE + EOT + #4 )[1]);
      if TestBit(B, 2) and TestBit(B, 3) then
        AStatus := AStatus + [stPoucoPapel];
      if TestBit(B, 5) and TestBit(B, 6) then
        AStatus := AStatus + [stSemPapel];

      Break;
    except
      Inc( Falhas );
      if Falhas >= 5 then;
        AStatus := AStatus + [stErro];
    end;
  end;
end;

function TACBrEscPosEpson.LerInfo: String;
var
  Ret: AnsiString;
  Info: String;
  B: Byte;

  Procedure AddInfo( Titulo: String; AInfo: AnsiString);
  begin
    Info := Info + Titulo+'='+copy(AInfo, 2, Length(AInfo)) + sLineBreak;
  end;

begin
  Info := '';

  Ret := fpPosPrinter.TxRx( GS + 'IB', 0, 500, True );
  AddInfo('Fabricante', Ret);

  Ret := fpPosPrinter.TxRx( GS + 'IA', 0, 500, True );
  AddInfo('Firmware', Ret);

  Ret := fpPosPrinter.TxRx( GS + 'IC', 0, 500, True );
  AddInfo('Modelo', Ret);

  Ret := fpPosPrinter.TxRx( GS + 'ID', 0, 500, True );
  AddInfo('Serial', Ret);

  Ret := fpPosPrinter.TxRx( GS + 'I2', 1, 500 );
  B := Ord(Ret[1]);
  Info := Info + 'Guilhotina='+IfThen(TestBit(B, 1),'1','0') + sLineBreak ;

  Result := Info;
end;

(*
procedure TACBrEscPosEpson.ProgramarLogo(ALogoFileBMP: String);
var
  AFileStream: TFileStream;
  ABitMap: TBitmap;
  Col, Row, W, H, P: Integer;
  ACmd: AnsiString;
begin
  // Inspiração: http://stackoverflow.com/questions/13715950/writing-a-bitmap-to-epson-tm88iv-through-esc-p-commands-write-to-nvram

  ABitMap := TBitmap.Create;
  AFileStream := TFileStream.Create(ALogoFileBMP, fmOpenRead);
  try
    AFileStream.Position := 0;
    ABitMap.LoadFromStream(AFileStream);

    W := ABitMap.Width;
    H := ABitMap.Height;

    ACmd := #48 + #67 + #48 + 'AC' + #01 +
            IntToLEStr(W) +
            IntToLEStr(H) +
            #49;  // 1 Cor, Mono

    For Col := 0 to W do
      For Row := 0 to H do
      begin
        P := ABitMap.Canvas.Pixels[Col, Row];
        ACmd := ACmd + AnsiChr( P );
      end;

    ACmd := GS + 'L(' + IntToLEStr(Length(ACmd)) + ACmd;

    fpPosPrinter.ImprimirCmd( ACmd ) ;

    fpPosPrinter.ImprimirCmd( GS  + '(L' + #6 + #0 + #48 + #69 + 'AC' + #01 + #01 );

  finally
    AFileStream.Free;
    ABitMap.Free;
  end;
end;
*)
end.

