{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2007 Andrews Ricardo Bejatto                }
{                                       Anderson Rogerio Bejatto               }
{                                                                              }
{ Colaboradores nesse arquivo:          Daniel Simooes de Almeida              }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 23/09/2010: Rodinei Marum Ribeiro
|* - Primeira versão ACBrETQZplII
******************************************************************************}

{$I ACBr.inc}

unit ACBrETQZplII;

interface
uses ACBrETQClass, ACBrUtil, ACBrDevice, Classes ;

type

  { TACBrETQZplII }

  TACBrETQZplII = class( TACBrETQClass )
  public
    constructor Create(AOwner: TComponent);

    procedure ImprimirTexto(Orientacao: TACBrETQOrientacao; Fonte, MultiplicadorH,
      MultiplicadorV, Vertical, Horizontal: Integer; Texto: String;
      SubFonte: Integer = 0; ImprimirReverso : Boolean = False); override;
      
    procedure ImprimirBarras(Orientacao: TACBrETQOrientacao; TipoBarras,
      LarguraBarraLarga, LarguraBarraFina: String; Vertical, Horizontal: Integer;
      Texto: String; AlturaCodBarras: Integer = 0;
      ExibeCodigo: TACBrETQBarraExibeCodigo = becPadrao); override;

    procedure ImprimirCaixa(Vertical, Horizontal, Largura, Altura,
      EspessuraVertical, EspessuraHorizontal: Integer); override;

    procedure CalcularComandoAbertura; override;
    procedure CalcularComandoFinaliza(Copias: Integer = 1; AvancoEtq: Integer = 0);
      override;
  end;

implementation
Uses
  math, {$IFNDEF COMPILER6_UP} ACBrD5, Windows, {$ENDIF}
  SysUtils ;

{ TACBrETQPpla }

constructor TACBrETQZplII.Create(AOwner: TComponent);
begin
  inherited Create( AOwner );

  fpModeloStr := 'ZPLII';
end;

procedure TACBrETQZplII.ImprimirTexto(Orientacao: TACBrETQOrientacao; Fonte, MultiplicadorH,
  MultiplicadorV, Vertical, Horizontal: Integer; Texto: String;
  SubFonte: Integer = 0; ImprimirReverso : Boolean = False);
var
   eixoY, eixoX, MultH, MultV, fnt: String;
   wOrientacao: char;
begin
  Cmd := '';

  if not (Fonte in [0..Ord('Z')]) then
     Raise Exception.Create(ACBrStr('Informe um valor entre "" e Z para Fonte'));

{ Multiplicador Horizontal, Multiplicador Vertical:
 De 0 a 9 e de A atÃ© O representa as escalas de multiplicaÃ§Ã£o (A=10, B=11,..., O=24)}

  if (Vertical < 0) or (Vertical > 32000) then
     Raise Exception.Create(ACBrStr('Informe um valor entre 0 e 32000 para Vertical'));
  eixoY := inttostr(Vertical);

  if (Horizontal < 0) or (Horizontal > 32000) then
     Raise Exception.Create(ACBrStr('Informe um valor entre 0 e 32000 para Horizontal'));
  eixoX := inttostr(Horizontal);

  if (Integer(MultiplicadorH) < 0) or (Integer(MultiplicadorH) > 32000) then
     Raise Exception.Create(ACBrStr('Informe um valor entre 0 e 32000 para Multiplicador Horizontal'));
  MultH := IntToStr(MultiplicadorH);

  if (MultiplicadorV < 0) or (MultiplicadorV > 32000) then
     Raise Exception.Create(ACBrStr('Informe um valor entre 0 e 32000 para Multiplicador Vertical'));
  MultV := IntToStr(MultiplicadorV);

  if Length(Texto) > 255 then
     Raise Exception.Create(ACBrStr('Tamanho maximo para o texto 255 caracteres'));

  case Orientacao of
    or270   : wOrientacao := 'B'; //270
    or180   : wOrientacao := 'I'; //180
    or90    : wOrientacao := 'R'; //90
  else
    wOrientacao := 'N'; //normal
  end;

  if Fonte = 0 then
    fnt := ''
  else
    fnt := chr(Fonte)+',';

  ListaCmd.Add('^CF'+fnt+MultH+','+MultV);
  ListaCmd.Add('^FO'+EixoX+','+EixoY);
  ListaCmd.Add('^FW'+ wOrientacao); //Verificar s Ã© aqui mesmo que adiciona ou dentro do FD
  ListaCmd.Add('^FD'+Texto+'^FS');
end;

procedure TACBrETQZplII.ImprimirBarras(Orientacao: TACBrETQOrientacao;
  TipoBarras, LarguraBarraLarga, LarguraBarraFina: String; Vertical,
  Horizontal: Integer; Texto: String; AlturaCodBarras: Integer;
  ExibeCodigo: TACBrETQBarraExibeCodigo);
var
   eixoY, eixoX, Alt: String;
   wOrientacao: Char;
begin
  Cmd := '';

  if ((Integer(Orientacao) + 1) < 1) or ((Integer(Orientacao) + 1) > 4) then
     Raise Exception.Create(ACBrStr('Informe um valor entre 1 e 4 para Orientação'));

{Tipo de CÃ³digo de Barras - vai de 'a' até 't' e de 'A' até 'T'
 Largura da Barra Larga, Largura da Barra Fina - De 0 a 9 e de 'A' até 'O'}

  if (Vertical < 0) or (Vertical > 32000) then
     Raise Exception.Create(ACBrStr('Informe um valor entre 0 e 762 para Vertical'));
  eixoY := IntToStr(Vertical);

  if (Horizontal < 0) or (Horizontal > 32000) then
     Raise Exception.Create(ACBrStr('Informe um valor entre 0 e 762 para Horizontal'));
  eixoX := IntToStr(Horizontal);

  if (AlturaCodBarras < 0) or (AlturaCodBarras > 32000) then
     Raise Exception.Create(ACBrStr('Tamanho máximo para a Altura do Código de Barras 3 caracteres'));
  Alt := IntToStr(AlturaCodBarras);

  case Orientacao of
    or270   : wOrientacao := 'B'; //270
    or180   : wOrientacao := 'I'; //180
    or90    : wOrientacao := 'R'; //90
  else
    wOrientacao := 'N'; //normal
  end;

  ListaCmd.Add('^FO'+EixoX+','+EixoY);
  ListaCmd.Add('^B'+TipoBarras+ wOrientacao+','+Alt+',Y,N');
  ListaCmd.Add('^FD'+Texto+'^FS');
end;

procedure TACBrETQZplII.ImprimirCaixa(Vertical, Horizontal, Largura, Altura,
  EspessuraVertical, EspessuraHorizontal: Integer);
var
   eixoY, eixoX, Larg, Alt, Esp: String;
begin
  Cmd := '';

  if (Vertical < 0) or (Vertical > 32000) then
     Raise Exception.Create(ACBrStr('Informe um valor entre 0 e 32000 para Vertical'));
  eixoY := IntToStr(Vertical);

  if (Horizontal < 0) or (Horizontal > 32000) then
     Raise Exception.Create(ACBrStr('Informe um valor entre 0 e 32000 para Horizontal'));
  eixoX := IntToStr(Horizontal);

  if (Largura < 0) or (Largura > 32000) then
     Raise Exception.Create(ACBrStr('Informe um valor entre 0 e 32000 para Horizontal'));
  Larg := IntToStr(Largura);

  if (Altura < 0) or (Altura > 999) then
     Raise Exception.Create(ACBrStr('Informe um valor entre 0 e 32000 para Horizontal'));
  Alt := PadLeft(IntToStr(Altura), 3, '0');

  if (EspessuraHorizontal < 0) or (EspessuraHorizontal > 999) then
     Raise Exception.Create(ACBrStr('Informe um valor entre 0 e 32000 para Horizontal'));
  Esp := PadLeft(IntToStr(EspessuraHorizontal), 3, '0');


  ListaCmd.Add('^FO'+EixoX+','+EixoY);
  ListaCmd.Add('^GB'+Larg+','+Alt+','+Esp+'^FS');

end;

procedure TACBrETQZplII.CalcularComandoAbertura;
begin
  Cmd := '^XA';
end;

procedure TACBrETQZplII.CalcularComandoFinaliza(Copias: Integer;
  AvancoEtq: Integer);
begin
  if (Copias > 1) then
    Cmd := '^PQ' + IntToStr(min(Copias,999)) + sLineBreak;

  Cmd := Cmd + '^XZ';
end;

end.
