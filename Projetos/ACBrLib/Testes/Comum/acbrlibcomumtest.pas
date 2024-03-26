{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Rafael Teno Dias                                }
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

unit ACBrLibComumTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, Graphics, ACBrLibResposta;

type

  { TACbrLibRespostaDescendenteSimples }

  TACbrLibRespostaDescendenteSimples = class(TACBrLibRespostaBase)
  private
    FFonte: TFont;
  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo);
    destructor Destroy; override;
  published
    property Fonte: TFont read FFonte write FFonte;
  end;

  TACBrLibResposta_Testes= class(TTestCase)
  published
    procedure GravarIni_TesteFonte;
  end;

implementation

uses IniFiles;

{ TACbrLibRespostaDescendenteSimples }

constructor TACbrLibRespostaDescendenteSimples.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo);
begin
  FFonte := TFont.Create;
end;

destructor TACbrLibRespostaDescendenteSimples.Destroy;
begin
  FFonte.Free;
end;

procedure TACBrLibResposta_Testes.GravarIni_TesteFonte;
var
  acrds: TACbrLibRespostaDescendenteSimples;
  Resultado, STeste: string;
  ITeste: Int64;
  AIni: TMemIniFile;
  Astr: TStringStream;
begin
  acrds := TACbrLibRespostaDescendenteSimples.Create('Sessao', resINI);
  try
    acrds.Fonte.Name := 'Arial';
    acrds.Fonte.Pitch := fpFixed;
    acrds.Fonte.Size := 8;
    Resultado := acrds.Gerar;
  finally
    acrds.Free;
  end;
  Astr := TStringStream.Create(Resultado);
  try
    AIni := TMemIniFile.Create(Astr);
    try
      STeste := AIni.ReadString('Fonte', 'Name', '');
      CheckEquals('Arial', STeste, 'Falhou Fonte.Name!');

      STeste := AIni.ReadString('Fonte', 'Pitch', '');
      CheckEquals('2', STeste, 'Falhou Fonte.Pitch (enumerado)!');

      ITeste := AIni.ReadInt64('Fonte', 'Size', -1);
      CheckEquals(8, ITeste, 'Falhou Fonte.Size !');
    finally
      AIni.Free;
    end;
  finally
    Astr.Free;
  end;

end;



initialization

  RegisterTest(TACBrLibResposta_Testes);
end.

