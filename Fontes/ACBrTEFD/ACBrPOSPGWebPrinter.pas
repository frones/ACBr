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

{$I ACBr.inc}

unit ACBrPOSPGWebPrinter;

interface

uses
  Classes, SysUtils,
  ACBrPosPrinter, ACBrPOS, ACBrPOSPGWebAPI, ACBrConsts;

const
  // Essas Tags serão interpretadas no momento da Impressão, pois usam comandos específicos
  CTAGS_POST_PROCESS: array[0..2] of string =
    ( cTagFonteAlinhadaEsquerda, cTagfonteAlinhadaCentro, cTagFonteAlinhadaDireita );
  CTAGS_AVANCO: array[0..3] of string =
    (cTagCorte, cTagCorteParcial, cTagCorteTotal, cTagPulodePagina);

type

  { TACBrPOSPGWebPrinter }

  TACBrPOSPGWebPrinter = class(TACBrPosPrinterClass)
  private
    FACBrPOS: TACBrPOS;
    FTerminalId: String;
  protected
    procedure Imprimir(const LinhasImpressao: String; var Tratado: Boolean);
  public
    constructor Create(AOwner: TACBrPosPrinter; AACBrPOS: TACBrPOS);

    procedure Configurar; override;
    function TraduzirTag(const ATag: AnsiString; var TagTraduzida: AnsiString): Boolean;
      override;
    function TraduzirTagBloco(const ATag, ConteudoBloco: AnsiString;
      var BlocoTraduzido: AnsiString): Boolean; override;

    property TerminalId: String read FTerminalId write FTerminalId;
  end;

implementation

uses
  StrUtils, ACBrUtil.Strings, ACBrUtil.XMLHTML;

constructor TACBrPOSPGWebPrinter.Create(AOwner: TACBrPosPrinter;
  AACBrPOS: TACBrPOS);
begin
  inherited Create(AOwner);

  FACBrPOS := AACBrPOS;
  FTerminalId := '';

  fpModeloStr := 'POSPGWebPrinter';
  RazaoColunaFonte.Condensada := 1;
  RazaoColunaFonte.Expandida := 2;

  {(*}
    with Cmd  do
    begin
      LigaExpandido := #11;
    end;
  {*)}

  TagsNaoSuportadas.Add( cTagBarraCode128c );
end;

procedure TACBrPOSPGWebPrinter.Configurar;
begin
  fpPosPrinter.ColunasFonteNormal := CACBrPOSPGWebColunasImpressora;
  fpPosPrinter.PaginaDeCodigo := pcNone;
  fpPosPrinter.Porta := 'NULL';
  fpPosPrinter.OnEnviarStringDevice := Imprimir;
end;

procedure TACBrPOSPGWebPrinter.Imprimir(const LinhasImpressao: String;
  var Tratado: Boolean);
var
  BufferImpressao, ATag: AnsiString;
  AlinhamentoImpressao: TAlignment;
  SL: TStringList;
  i, PosTag: Integer;
  Linha: String;
  TipoCod: TACBrPOSPGWebCodBarras;

  function AjustarAlinhamentoLinha(const ALinha: String): String;
  var
    l: Integer;
  begin
    Result := ALinha;

    if Length(ALinha) < CACBrPOSPGWebColunasImpressora then
    begin
      if AlinhamentoImpressao = taRightJustify then
        Result := PadLeft(ALinha, CACBrPOSPGWebColunasImpressora)
      else if AlinhamentoImpressao = taCenter then
        Result := PadCenter(ALinha, CACBrPOSPGWebColunasImpressora)
    end
    else
    begin
      l := Length(Result);
      // Remove espaços em branco, do final da String, para evitar pulo de linhas desenecessários
      while (l > CACBrPOSPGWebColunasImpressora) and (Result[l] = ' ') do
      begin
        Delete(Result, l, 1);
        Dec(l);
      end
    end;
  end;

  procedure ImprimirBuffer;
  begin
    if (BufferImpressao <> '') then
      FACBrPOS.ImprimirTexto(TerminalId, BufferImpressao);

    BufferImpressao := '';
  end;

  function AcharConteudoBloco(const ATag, ALinha: String): String;
  var
    TagFim, LLinha: String;
    PosTagIni, PosTagFim: Integer;
    LenTag: Integer;
  begin
    LLinha := LowerCase(ALinha);
    LenTag := Length(ATag);
    PosTagIni := pos(ATag, LLinha);
    if PosTagIni = 0 then
      PosTagIni := 1;

    TagFim := '</'+ copy(ATag, 2, LenTag);
    PosTagFim := PosEx(TagFim, LLinha, PosTag+LenTag );
    if (PosTagFim = 0) then
      PosTagFim := Length(ALinha);

    Result := copy(ALinha, PosTagIni+LenTag, PosTagFim-PosTagIni-LenTag);
  end;

begin
  // DEBUG
  //WriteToFile('c:\temp\relat1.txt', LinhasImpressao);

  if not Assigned(FACBrPOS) then
    raise EPosPrinterException.Create(ACBrStr('ACBrPOS não atribuido a TACBrPOSPGWebPrinter'));

  if (TerminalId = '') then
    raise EPosPrinterException.Create(ACBrStr('TerminalId não atribuido a TACBrPOSPGWebPrinter'));

  Tratado := True;
  BufferImpressao := '';
  AlinhamentoImpressao := taLeftJustify;

  SL := TStringList.Create;
  try
    SL.Text := LinhasImpressao;
    for i := 0 to SL.Count-1 do
    begin
      Linha := SL[i];
      PosTag := 0;
      ATag := '';
      AcharProximaTag(Linha, 1, ATag, PosTag);
      if (PosTag > 0) then
      begin
        if (PosTag = 1) then  // É Tag pós processada ?
        begin
          if (ATag = cTagFonteAlinhadaEsquerda) then
            AlinhamentoImpressao := taLeftJustify
          else if (ATag = cTagfonteAlinhadaCentro) then
            AlinhamentoImpressao := taCenter
          else if (ATag = cTagFonteAlinhadaDireita) then
            AlinhamentoImpressao := taRightJustify

          else if (ATag = cTagPuloDeLinhas) then
          begin
            ImprimirBuffer;
            FACBrPOS.AvancarPapel(TerminalId);
            Continue;
          end

          else if (ATag = cTagQRCode) or (ATag = cTagBarraCode128) or (ATag = cTagBarraInter)  then
          begin
            ImprimirBuffer;
            if (ATag = cTagBarraInter) then
              TipoCod := codeITF
            else if (ATag = cTagBarraCode128) then
              TipoCod := code128
            else
              TipoCod := codeQRCODE;

            Linha := AcharConteudoBloco(ATag, Linha);
            FACBrPOS.ImprimirCodBarras(TerminalId, Linha, TipoCod);
            Continue;
          end;
        end;

        Linha := StringReplace(Linha, ATag, '', [rfReplaceAll]);
      end;

      if (pos(#11, Linha) > 1) then    // Expandido só funciona no inicio da Linha (coluna 1)
        Linha := StringReplace(Linha, #11, '', [rfReplaceAll]);

      BufferImpressao := BufferImpressao +
                         AjustarAlinhamentoLinha(Linha) + LF;
    end;
  finally
    SL.Free;
  end;

  ImprimirBuffer;
end;

function TACBrPOSPGWebPrinter.TraduzirTag(const ATag: AnsiString;
  var TagTraduzida: AnsiString): Boolean;
begin
  TagTraduzida := '';
  Result := True;
  if MatchText(ATag, CTAGS_POST_PROCESS) then
    TagTraduzida := ATag
  else if MatchText(ATag, CTAGS_AVANCO) then
    TagTraduzida := cTagPuloDeLinhas
  else
    Result := False;  // Deixa ACBrPosPrinter traduzir...
end;

function TACBrPOSPGWebPrinter.TraduzirTagBloco(const ATag,
  ConteudoBloco: AnsiString; var BlocoTraduzido: AnsiString): Boolean;
const
  // Essas Tags serão interpretadas no momento da Impressão, pois usam comandos específicos
  CTAGS_POST_PROCESS: array[0..2] of string = ( cTagQRCode, cTagBarraCode128, cTagBarraInter );
begin
  BlocoTraduzido := '';
  Result := True;
  if MatchText(ATag, CTAGS_POST_PROCESS) then
    BlocoTraduzido := ATag + ConteudoBloco + '</'+ copy(ATag, 2, Length(ATag))
  else
    Result := False;  // Deixa ACBrPosPrinter traduzir...
end;

end.
