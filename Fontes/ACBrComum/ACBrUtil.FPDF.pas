  {******************************************************************************}
  { Projeto: Componentes ACBr                                                    }
  {  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
  { mentos de Automação Comercial utilizados no Brasil                           }
  { Direitos Autorais Reservados (c) 2023 Daniel Simoes de Almeida               }

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
  { http://www.opensource.org/licenses/lgpl-license.php                          }

  { Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
  {       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
  {******************************************************************************}

  {  Algumas funçoes dessa Unit foram extraidas de outras Bibliotecas, veja no   }
  { cabeçalho das Funçoes no código abaixo a origem das informaçoes, e autores...}

  {******************************************************************************}

{$I ACBr.inc}
unit ACBrUtil.FPDF;

interface

uses
  ACBr_fpdf,
  ACBr_fpdf_ext,
  ACBrUtil.Strings;

type
  TACBrFPDFExt = class(TFPDFExt)
    procedure SetFont(const AFamily: String; ASize: Double = 0.0; const AStyle: String = ''); overload;
  public
    function WordWrap(var AText: string; AMaxWidth: Double): integer;
    function GetNumLines(const AText: string; AWidth: Double; const AFontFamily: string = 'Times'; const AFontStyle: string = ''; AFontSize: Double = 8): integer;
    function TextBox(x, y, w, h: Double; const AText: string = ''; const vAlign: char = 'T'; const hAlign: char = 'L'; border: Double = 1; const link: string = '';
      force: boolean = true; hmax: Double = 0; vOffSet: Double = 0): Double;
    function TextBox90(x, y, w, h: Double; const AText: string = ''; const vAlign: char = 'T'; const hAlign: char = 'L'; border: Double = 1; const link: string = '';
      force: boolean = true; hmax: Double = 0; vOffSet: Double = 0): Double;
    procedure DashedHLine(x, y, w, h: Double; n: integer);
    procedure DashedVLine(x, y1, y2: Double; n: integer);
    procedure Cell(vWidth: Double; vHeight: Double = 0; const vText: String = ''; const vBorder: String = '0'; vLineBreak: integer = 0; const vAlign: String = '';
      vFill: boolean = False; vLink: String = ''); override;
  end;

implementation

uses
  SysUtils,
  RegularExpressions;

  { TACBrFPDFExt }

procedure TACBrFPDFExt.Cell(vWidth, vHeight: Double; const vText, vBorder: String; vLineBreak: integer; const vAlign: String; vFill: boolean; vLink: String);
begin
  inherited Cell(vWidth, vHeight, NativeStringToAnsi(vText), vBorder, vLineBreak, vAlign, vFill, vLink);
end;

procedure TACBrFPDFExt.DashedHLine(x, y, w, h: Double; n: integer);
var
  wDash: Double;
  i, j : Double;
begin
  SetDrawColor(110);
  SetLineWidth(h);
  wDash := (w / n) / 2; // Altura dos traços (width)
  i     := x;
  while i <= x + w do
  begin
    j := i;
    while j <= i + wDash do
    begin
      if j <= x + w - 1 then
        Self.Line(j, y, j + 1, y);
      j := j + 1;
    end;
    i := i + wDash + wDash;
  end;
  SetDrawColor(0);
end;

procedure TACBrFPDFExt.DashedVLine(x, y1, y2: Double; n: integer);
var
  hDash: Double;
  j, y : Double;
begin
  SetDrawColor(110);
  SetLineWidth(0.1);
  hDash := ((y2 - y1) / n) / 2; // Comprimento dos traços (height)
  y     := y1;
  while y <= y2 - hDash do
  begin
    j := y;
    while j <= y + hDash do
    begin
      if j <= y + hDash - 1 then
        Self.Line(x, j, x, j + 1);
      j := j + 1;
    end;
    y := y + hDash + hDash;
  end;
  SetDrawColor(0);
end;

function TACBrFPDFExt.GetNumLines(const AText: string; AWidth: Double; const AFontFamily, AFontStyle: string; AFontSize: Double): integer;
var
  LocalText: string;
begin
  LocalText := Trim(AText);
  Self.SetFont(AFontFamily, AFontStyle, AFontSize);
  Result := WordWrap(LocalText, AWidth - 0.2);
end;

function TACBrFPDFExt.TextBox(x, y, w, h: Double; const AText: string; const vAlign, hAlign: char; border: Double; const link: string; force: boolean;
  hmax, vOffSet: Double): Double;
var
  text, texto                               : string;
  oldY, incY, altText, x1, y1, comp, newSize: Double;
  n                                         : integer;
  temObs, resetou                           : boolean;
  Lines                                     : TArray<string>;
  Line                                      : string;
begin
  text    := AText;
  oldY    := y;
  temObs  := False;
  resetou := False;
  if w < 0 then
    Exit(y);
    //  if (is_object($text)) {
    //      $text = '';
    //  }
    //  if (is_string($text)) {
    //      //remover espaços desnecessários
  text := Trim(text);
    //      //converter o charset para o fpdf
    //      $text = utf8_decode($text);
    //  } else {
    //      $text = (string) $text;
    //  }
    //desenhar a borda da caixa
  if border > 0 then
    Self.RoundedRect(x, y, w, h, 0.8, '', 'D');
    //calcular o incremento
  incY := Self.FontSize; //tamanho da fonte na unidade definida
  if not force then
      //verificar se o texto cabe no espaço
    n := Self.WordWrap(text, w)
  else
    n := 1;
    //calcular a altura do conjunto de texto
  altText := incY * n;
    //separar o texto em linhas
  Lines := text.Split([ sLineBreak ]);
    //verificar o alinhamento vertical
  if vAlign = 'T' then
      //alinhado ao topo
    y1 := y + incY;
  if vAlign = 'C' then
      //alinhado ao centro
    y1 := y + incY + ((h - altText) / 2);
  if vAlign = 'B' then
      //alinhado a base
    y1 := (y + h) - 0.5;
    //para cada linha
  for Line in Lines do
  begin
      //verificar o comprimento da frase
    texto := Trim(Line);
    comp  := Self.GetStringWidth(texto);
    if force then
    begin
      newSize := Self.FontSizePt;
      while comp > w do
      begin
          //estabelecer novo fonte
        newSize := newSize - 1;
        Self.SetFont(Self.FontFamily, Self.FontStyle, newSize);
        comp := Self.GetStringWidth(texto);
      end;
    end;
      //ajustar ao alinhamento horizontal
    if (hAlign = 'L') then
      x1 := x + 0.5;
    if (hAlign = 'C') then
      x1 := x + ((w - comp) / 2);
    if (hAlign = 'R') then
      x1 := x + w - (comp + 0.5);

      //escrever o texto
    if vOffSet > 0 then
    begin
      if (y1 > (oldY + vOffSet)) then
      begin
        if (not resetou) then
        begin
          y1      := oldY;
          resetou := true;
        end;
        Self.text(x1, y1, texto);
      end;
    end
    else
      Self.text(x1, y1, texto);

      //incrementar para escrever o proximo
    y1 := y1 + incY;
    if ((hmax > 0) and (y1 > (y + (hmax - 1)))) then
    begin
      temObs := true;
      break;
    end;
  end;
  Result := (y1 - y) - incY;
end;

function TACBrFPDFExt.TextBox90(x, y, w, h: Double; const AText: string; const vAlign, hAlign: char; border: Double; const link: string; force: boolean;
  hmax, vOffSet: Double): Double;
begin
  Self.Rotate(90, x, y);
  try
    Result := TextBox(x, y, w, h, AText, vAlign, hAlign, border, link, force, hmax, vOffSet);
  finally
    Self.Rotate(0, x, y);
  end;
end;

procedure TACBrFPDFExt.SetFont(const AFamily: String; ASize: Double; const AStyle: String);
begin
  Self.SetFont(AFamily, AStyle, ASize);
end;

function TACBrFPDFExt.WordWrap(var AText: string; AMaxWidth: Double): integer;
var
  Space, Width, WordWidth: Double;
  Lines, Words           : TArray<string>;
  Line, Word             : string;
  i, j, L                : integer;
begin
  AText := Trim(AText);
  if AText = '' then
    Exit(0);
  Space  := Self.GetStringWidth(' ');
  Lines  := AText.Split([ sLineBreak ]);
  AText  := '';
  Result := 0;
  for i  := 0 to Length(Lines) - 1 do
  begin
    Line  := Lines[ i ];
    Words := TRegEx.Split(Line, ' +');
    Width := 0;
    for j := 0 to Length(Words) - 1 do
    begin
      Word      := Words[ j ];
      WordWidth := Self.GetStringWidth(Word);
      if WordWidth > AMaxWidth then
      begin
          // Word is too long, we cut it
        for L := 1 to Length(Word) do
        begin
          WordWidth := Self.GetStringWidth(Copy(Word, L, 1));
          if (Width + WordWidth <= AMaxWidth) then
          begin
            Width := Width + WordWidth;
            AText := AText + Copy(Word, L, 1);
          end
          else
          begin
            Width := WordWidth;
            AText := TrimRight(AText) + sLineBreak + Copy(Word, L, 1);
            Inc(Result);
          end;
        end;
      end
      else
        if (Width + WordWidth <= AMaxWidth) then
        begin
          Width := Width + WordWidth + Space;
          AText := AText + Word + ' ';
        end
        else
        begin
          Width := WordWidth + Space;
          AText := TrimRight(AText) + sLineBreak + Word + ' ';
          Inc(Result);
        end;
    end;
    AText := TrimRight(AText) + sLineBreak;
    Inc(Result);
  end;
  AText := TrimRight(AText);
end;

end.
