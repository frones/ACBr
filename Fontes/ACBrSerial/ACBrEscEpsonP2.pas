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

unit ACBrEscEpsonP2;

interface

uses
  Classes, SysUtils,
  ACBrPosPrinter, ACBrConsts;

type

  { TACBrEscEpsonP2 }

  TACBrEscEpsonP2 = class(TACBrPosPrinterClass)
  private
  public
    constructor Create(AOwner: TACBrPosPrinter);
  end;


implementation

{ TACBrEscEpsonP2 }

constructor TACBrEscEpsonP2.Create(AOwner: TACBrPosPrinter);
var
  I: Integer;
begin
  inherited Create(AOwner);

  fpModeloStr := 'EscEpsonP2';

{(*}
  with Cmd  do
  begin
    Zera                    := ESC + '@'+SI;
    PuloDeLinha             := LF;
    EspacoEntreLinhasPadrao := ESC + '2';
    EspacoEntreLinhas       := ESC + '0';
    LigaNegrito             := ESC + 'G';
    DesligaNegrito          := ESC + 'H';
    LigaExpandido           := ESC + 'W1';
    DesligaExpandido        := ESC + 'W0';
    LigaSublinhado          := ESC + '-' + #1;
    DesligaSublinhado       := ESC + '-' + #0;
    LigaItalico             := ESC + '4';
    DesligaItalico          := ESC + '5';
    LigaCondensado          := SI;
    DesligaCondensado       := DC2;
    FonteNormal             := LigaCondensado;   //ESC + 'P' + DesligaCondensado + DesligaItalico;
    FonteA                  := DesligaCondensado;
    FonteB                  := LigaCondensado;
  end;
  {*)}

  For I := 0 to Length(cTAGS_BARRAS) do
    TagsNaoSuportadas.Add( cTAGS_BARRAS[I] );

  For I := 0 to Length(cTAGS_ALINHAMENTO) do
    TagsNaoSuportadas.Add( cTAGS_ALINHAMENTO[I] );

  TagsNaoSuportadas.Add( cTagLigaInvertido );
  TagsNaoSuportadas.Add( cTagDesligaInvertido );
  TagsNaoSuportadas.Add( cTagCorteParcial );
  TagsNaoSuportadas.Add( cTagCorteTotal );
  TagsNaoSuportadas.Add( cTagLogoImprimir );
  TagsNaoSuportadas.Add( cTagAbreGaveta );
  TagsNaoSuportadas.Add( cTagAbreGavetaEsp );
  TagsNaoSuportadas.Add( cTagBeep );
end;

end.
